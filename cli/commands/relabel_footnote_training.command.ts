// Relabel the chats to train a footnote extraction model on (see
// core/footnote_relabel.ts and export-footnote-training): ask a stronger
// model, the teacher, for each batch's citations, and write the chats with its
// answers, those it judges worth training on, in place of those saved.
//   npm run cli -- relabel-footnote-training --model <teacher> [--limit <n>] [--insights]
//     [--concurrency <n>] [--interval-ms <ms>] [--in <dir>]
// The teacher's answers are kept as they come in
// (<in>/relabel/<teacher>/answers.jsonl), so a run stopped part way, or run
// again with a higher --limit, asks only for the batches not answered yet
// (and again for those whose requests failed). Then it writes, in the same
// folder, train.jsonl and valid.jsonl (the chats kept), and more.jsonl and
// dropped.jsonl (the batches where the teacher found many more citations than
// were saved, to look over a sample of, and those not kept, with why).
// --limit: only n of the training batches, spread over the books (and as many
// in proportion of the validation ones). --insights: give the teacher the
// saved insights that apply, as importing does; the chats written are given
// the prompt without them. --concurrency: requests at once (4).
// --interval-ms: at least this long between the start of requests, for a
// rate limit (OpenRouter's free models allow 20 a minute: 3200).
import fs from 'node:fs';
import path from 'node:path';
import { setTimeout } from 'node:timers/promises';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import {
  judgeAnswer, relabelledChat, trainingBatch, type TrainingBatch, type Verdict,
} from '../../core/footnote_relabel.ts';
import { stableHash, type BatchFootnote, type TrainingChat } from '../../core/footnote_training.ts';
import { askFootnotes, findEvalInsights, type EvalAnswer, type EvalInsights } from '../../lib/footnote_eval.ts';
import { mapLimited } from '../../lib/lib.ts';

const DEFAULT_IN          = 'output/footnote_training';
const DEFAULT_CONCURRENCY = 4;

// Tries of a request before it's taken to have failed, waiting longer each time
const TRIES         = 3;
const RETRY_WAIT_MS = 5000;

type Options = {
  model?: string, in?: string, limit?: number, insights?: boolean, concurrency?: number, intervalMs?: number,
};

type SavedAnswer = EvalAnswer & { id: string };

const SPLITS = ['train', 'valid'] as const;

const readLines = <T>(file: string): T[] => (
  fs.existsSync(file)
    ? fs.readFileSync(file, 'utf8').split('\n').filter((l) => l.trim().length > 0).map((l) => JSON.parse(l) as T)
    : []
);

// The first n batches in an order that's the same every time but not the books'
const spread = (batches: TrainingBatch[], n: number): TrainingBatch[] => (
  [...batches].sort((a, b) => stableHash(a.id) - stableHash(b.id) || a.id.localeCompare(b.id)).slice(0, n)
);

@Command({
  name:        'relabel-footnote-training',
  description: "Relabel the footnote training chats with a stronger model's answers",
})
export class RelabelFootnoteTrainingCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--model <model>', description: 'the teacher: an OpenRouter model, or "local/<name>"' })
  parseModel(value: string): string {
    return value;
  }

  @Option({ flags: '--in <dir>', description: `the chats exported (${ DEFAULT_IN })` })
  parseIn(value: string): string {
    return value;
  }

  @Option({ flags: '--limit <n>', description: 'only n of the training batches, spread over the books' })
  parseLimit(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--insights', description: 'give the teacher the saved insights, as importing does' })
  parseInsights(): boolean {
    return true;
  }

  @Option({ flags: '--concurrency <n>', description: `requests at once (${ DEFAULT_CONCURRENCY })` })
  parseConcurrency(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--interval-ms <ms>', description: 'at least this long between the starts of requests' })
  parseIntervalMs(value: string): number {
    return Number(value);
  }

  async run(_: string[], options: Options = {}): Promise<void> {
    if (options.model === undefined) {
      throw new Error('give the teacher: --model <model>');
    }

    const model    = options.model;
    const dir      = options.in ?? DEFAULT_IN;
    const out      = path.join(dir, 'relabel', model.replace(/[^\w.-]+/g, '_'));
    const insights = options.insights === true ? await findEvalInsights(this.db) : null;
    const all      = Object.fromEntries(SPLITS.map((s) => [
      s, readLines<TrainingChat>(path.join(dir, `${ s }.jsonl`)).map(trainingBatch),
    ])) as Record<typeof SPLITS[number], TrainingBatch[]>;

    if (all.train.length === 0) {
      throw new Error(`no chats in ${ dir }/train.jsonl: run export-footnote-training first`);
    }

    const limit  = options.limit ?? all.train.length;
    const chosen = {
      train: spread(all.train, limit),
      valid: spread(all.valid, Math.round(limit * all.valid.length / all.train.length)),
    };

    fs.mkdirSync(out, { recursive: true });
    const answersFile = path.join(out, 'answers.jsonl');
    const answers     = new Map(readLines<SavedAnswer>(answersFile).map((a) => [a.id, a]));
    const todo        = [...chosen.train, ...chosen.valid].filter((b) => answers.get(b.id)?.footnotes == null);

    console.log(`${ model }: ${ chosen.train.length } training and ${ chosen.valid.length } validation batches, `
      + `${ todo.length } to ask (${ answers.size } answered before)${ insights === null ? '' : ', with insights' }`);
    await this.ask(todo, model, insights, answersFile, answers, options);
    this.write(out, chosen, answers);
  }

  // Ask the teacher for each batch, keeping each answer as it comes in
  private async ask(
    todo: TrainingBatch[], model: string, insights: EvalInsights, file: string,
    answers: Map<string, SavedAnswer>, options: Options,
  ) {
    const stream   = fs.createWriteStream(file, { flags: 'a' });
    const interval = options.intervalMs ?? 0;
    let nextStart  = Date.now();
    let done       = 0;
    let failed     = 0;

    // the request's turn, at least `interval` after the last one's
    const turn = async() => {
      const at  = Math.max(Date.now(), nextStart);
      nextStart = at + interval;
      await setTimeout(at - Date.now());
    };

    await mapLimited(todo, options.concurrency ?? DEFAULT_CONCURRENCY, async(batch) => {
      let answer: EvalAnswer = await (async() => {
        await turn();
        return askFootnotes(batch.message, model, insights);
      })();

      for (let tried = 1; answer.footnotes === null && tried < TRIES; tried++) {
        await setTimeout(RETRY_WAIT_MS * tried);
        await turn();
        answer = await askFootnotes(batch.message, model, insights);
      }

      const saved = { id: batch.id, ...answer };
      answers.set(batch.id, saved);
      stream.write(JSON.stringify(saved) + '\n');
      done++;
      failed += answer.footnotes === null ? 1 : 0;
      if (done % 25 === 0 || done === todo.length) {
        console.log(`${ done }/${ todo.length } asked, ${ failed } failed`
          + (answer.error === null ? '' : ` (last: ${ answer.error.slice(0, 150) })`));
      }
    });

    await new Promise((resolve) => stream.end(resolve));
  }

  // The chats kept, and the batches to look over and those dropped, with a report
  private write(
    out: string, chosen: Record<typeof SPLITS[number], TrainingBatch[]>, answers: Map<string, SavedAnswer>,
  ) {
    const rows    = [];
    const more    = [];
    const dropped = [];

    for (const split of SPLITS) {
      const counts: Record<Verdict | 'unasked', number> = { keep: 0, more: 0, drop: 0, unasked: 0 };
      const chats: TrainingChat[]                       = [];
      let saved                                         = 0;
      let found                                         = 0;
      let prompt                                        = 0;
      let completion                                    = 0;

      for (const batch of chosen[split]) {
        const answer = answers.get(batch.id);

        if (answer === undefined) {
          counts.unasked++;
        }
        else {
          const judged = judgeAnswer(batch, answer.footnotes);
          counts[judged.verdict]++;
          prompt     += answer.promptTokens ?? 0;
          completion += answer.completionTokens ?? 0;

          if (judged.verdict === 'drop') {
            dropped.push({ id: batch.id, split, reason: answer.error ?? judged.reason });
          }
          else {
            saved += judged.score.expected;
            found += judged.score.answered;
            chats.push(relabelledChat(batch, answer.footnotes as BatchFootnote[]));
            if (judged.verdict === 'more') {
              more.push({ id: batch.id, split, reason: judged.reason });
            }
          }
        }
      }

      fs.writeFileSync(path.join(out, `${ split }.jsonl`), chats.map((c) => JSON.stringify(c) + '\n').join(''));
      rows.push({ split, ...counts, written: chats.length, citationsSaved: saved, citationsFound: found, prompt, completion });
    }

    fs.writeFileSync(path.join(out, 'more.jsonl'), more.map((m) => JSON.stringify(m) + '\n').join(''));
    fs.writeFileSync(path.join(out, 'dropped.jsonl'), dropped.map((d) => JSON.stringify(d) + '\n').join(''));

    const reasons = dropped.reduce((m, d) => {
      const why = d.reason.replace(/\d+/g, 'n').replace(/pages? (n, )*n/, 'page(s)').slice(0, 80);
      return m.set(why, (m.get(why) ?? 0) + 1);
    }, new Map<string, number>());

    console.log(`\nwrote ${ out }/{train,valid,more,dropped}.jsonl. keep: as many citations as saved; more: `
      + 'many more (kept: look over a sample, more.jsonl); drop: not kept (dropped.jsonl); '
      + 'citationsSaved/Found: of the chats written; prompt/completion: tokens');
    console.table(rows);
    if (reasons.size > 0) {
      console.log('dropped:', Object.fromEntries([...reasons].sort((a, b) => b[1] - a[1])));
    }
  }
}
