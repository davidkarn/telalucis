// Relabel the chats to train a footnote extraction model on (see
// core/footnote_relabel.ts and export-footnote-training): ask a stronger
// model, the teacher, for each batch's citations, and write the chats with its
// answers, those it judges worth training on, in place of those saved.
//   npm run cli -- relabel-footnote-training --model <teacher> [--limit <n>] [--insights]
//     [--reasoning <off|tokens|effort>] [--batch [--batch-size <n>]]
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
// --reasoning: how much the teacher thinks, as for eval-footnotes (see
// defaultReasoning). --batch: ask through OpenRouter's Batch API (see
// lib/open_router_batches.ts), for about half the price, --batch-size requests
// a batch (500), waiting for each to be done (most take minutes, at most a
// day). The batches submitted are kept (<in>/relabel/<teacher>/batches.json),
// so a run stopped while they're answered collects them when run again rather
// than asking again. The first batch of a teacher is a probe of PROBE_SIZE
// requests, checked to have kept the response format and the reasoning asked
// for before the rest are sent.
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
import { defaultReasoning } from '../../lib/footnote_citations.ts';
import {
  askFootnotes, findEvalInsights, footnotesAnswer, footnotesRequest, type EvalAnswer, type EvalInsights,
} from '../../lib/footnote_eval.ts';
import { mapLimited } from '../../lib/lib.ts';
import { chatRequestBody, parseReasoning, type ORReasoning } from '../../lib/open_router.ts';
import {
  awaitBatch, batchResults, submitBatch, type BatchRequest, type BatchResult,
} from '../../lib/open_router_batches.ts';

const DEFAULT_IN          = 'output/footnote_training';
const DEFAULT_CONCURRENCY = 4;

// Tries of a request before it's taken to have failed, waiting longer each time
const TRIES         = 3;
const RETRY_WAIT_MS = 5000;

// Batch mode: requests a batch, the requests of the first one (to check), how often a batch is
// asked about, and each request's answer limit (thinking included). OpenRouter estimates a
// batch's cost, to check the balance, from the answer limit: answers are about 5,000 tokens.
const DEFAULT_BATCH_SIZE = 500;
const PROBE_SIZE         = 5;
const BATCH_POLL_MS      = 30_000;
const BATCH_MAX_TOKENS   = 16_000;

type Options = {
  model?: string, in?: string, limit?: number, insights?: boolean, concurrency?: number, intervalMs?: number,
  reasoning?: ORReasoning, batch?: boolean, batchSize?: number,
};

// The batches submitted and not yet collected, each its requests' batch ids by custom_id, and
// whether a probe batch has passed (see checkProbe)
type BatchState = { probed: boolean, pending: { id: string, requests: Record<string, string> }[] };

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

  @Option({ flags: '--reasoning <setting>', description: 'how much the teacher thinks: off, a number of tokens, or an effort (low...)' })
  parseReasoningOption(value: string): ORReasoning {
    return parseReasoning(value);
  }

  @Option({ flags: '--batch', description: "ask through OpenRouter's Batch API, for about half the price" })
  parseBatch(): boolean {
    return true;
  }

  @Option({ flags: '--batch-size <n>', description: `requests a batch (${ DEFAULT_BATCH_SIZE })` })
  parseBatchSize(value: string): number {
    return Number(value);
  }

  async run(_: string[], options: Options = {}): Promise<void> {
    if (options.model === undefined) {
      throw new Error('give the teacher: --model <model>');
    }

    const model     = options.model;
    const reasoning = options.reasoning ?? defaultReasoning(model);
    const dir       = options.in ?? DEFAULT_IN;
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
    if (options.batch === true) {
      await this.askInBatches(todo, model, insights, reasoning, out, answersFile, answers, options);
    }
    else {
      await this.ask(todo, model, insights, reasoning, answersFile, answers, options);
    }
    this.write(out, chosen, answers);
  }

  // Ask the teacher for each batch, keeping each answer as it comes in
  private async ask(
    todo: TrainingBatch[], model: string, insights: EvalInsights, reasoning: ORReasoning | undefined,
    file: string, answers: Map<string, SavedAnswer>, options: Options,
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
        return askFootnotes(batch.message, model, insights, reasoning);
      })();

      for (let tried = 1; answer.footnotes === null && tried < TRIES; tried++) {
        await setTimeout(RETRY_WAIT_MS * tried);
        await turn();
        answer = await askFootnotes(batch.message, model, insights, reasoning);
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

  // Ask the teacher through the Batch API: collect the batches submitted before, then send the
  // batches not answered nor submitted, a probe first when none has passed, keeping each
  // batch's answers as it's collected. Batches whose requests fail are asked again next run.
  private async askInBatches(
    todo: TrainingBatch[], model: string, insights: EvalInsights, reasoning: ORReasoning | undefined,
    out: string, file: string, answers: Map<string, SavedAnswer>, options: Options,
  ) {
    const stateFile = path.join(out, 'batches.json');
    const state     = fs.existsSync(stateFile)
      ? JSON.parse(fs.readFileSync(stateFile, 'utf8')) as BatchState
      : { probed: false, pending: [] };
    const save      = () => fs.writeFileSync(stateFile, JSON.stringify(state, null, 2) + '\n');
    const stream    = fs.createWriteStream(file, { flags: 'a' });

    // a batch's answers, collected once it's done, and taken off the pending
    const collect = async(pending: BatchState['pending'][number]): Promise<SavedAnswer[]> => {
      const batch    = await awaitBatch(pending.id, BATCH_POLL_MS, (b) => console.log(
        `  batch ${ b.id }: ${ b.status }, ${ b.request_counts.completed }/${ b.request_counts.total } done, `
        + `${ b.request_counts.failed } failed`,
      ));
      const results = batch.status === 'completed' ? await batchResults(pending.id) : [];
      const byCustom = new Map(results.map((r) => [r.custom_id, r]));
      const got      = Object.entries(pending.requests).map(([custom, id]) => (
        { id, ...this.answerOf(byCustom.get(custom), batch.status, batch.error?.message) }
      ));

      for (const answer of got) {
        answers.set(answer.id, answer);
        stream.write(JSON.stringify(answer) + '\n');
      }
      state.pending = state.pending.filter((p) => p.id !== pending.id);
      save();
      console.log(`  batch ${ pending.id }: ${ got.filter((a) => a.footnotes !== null).length }/${ got.length } answered`);
      return got;
    };

    const submit = async(chunk: TrainingBatch[]) => {
      const requests: BatchRequest[] = chunk.map((b, i) => {
        const { messages, format } = footnotesRequest(b.message, insights);
        return { custom_id: `r${ i }`, body: chatRequestBody(messages, format, model, reasoning, BATCH_MAX_TOKENS) };
      });
      const batch   = await submitBatch(model, requests);
      const pending = { id: batch.id, requests: Object.fromEntries(chunk.map((b, i) => [`r${ i }`, b.id])) };

      state.pending.push(pending);
      save();
      console.log(`submitted batch ${ batch.id }: ${ chunk.length } requests (${ batch.status })`);
      return pending;
    };

    // the batches submitted before; the first, when no probe has passed, checked as a probe
    for (const pending of [...state.pending]) {
      console.log(`collecting batch ${ pending.id }, submitted before`);
      const got = await collect(pending);

      if (!state.probed) {
        const problem = this.checkProbe(got, reasoning);
        if (problem !== null) {
          await new Promise((resolve) => stream.end(resolve));
          throw new Error(`the probe batch didn't pass: ${ problem }. Its answers are kept; try without --batch`);
        }
        state.probed = true;
        save();
      }
    }

    const submitted = new Set(state.pending.flatMap((p) => Object.values(p.requests)));
    let rest        = todo.filter((b) => answers.get(b.id)?.footnotes == null && !submitted.has(b.id));

    if (!state.probed && rest.length > 0) {
      const probe = rest.slice(0, PROBE_SIZE);
      console.log(`probing with ${ probe.length } requests, to check the batch keeps the response format and reasoning`);
      const problem = this.checkProbe(await collect(await submit(probe)), reasoning);

      if (problem !== null) {
        await new Promise((resolve) => stream.end(resolve));
        throw new Error(`the probe batch didn't pass: ${ problem }. Its answers are kept; try without --batch`);
      }
      state.probed = true;
      save();
      rest = rest.filter((b) => answers.get(b.id)?.footnotes == null && !probe.includes(b));
    }

    const size   = options.batchSize ?? DEFAULT_BATCH_SIZE;
    const chunks = Array.from({ length: Math.ceil(rest.length / size) }, (_, i) => rest.slice(i * size, (i + 1) * size));
    const sent   = [];
    for (const chunk of chunks) {
      sent.push(await submit(chunk));
    }
    for (const pending of sent) {
      await collect(pending);
    }

    await new Promise((resolve) => stream.end(resolve));
  }

  // A batch request's answer: its result's, or a failure saying why there's none
  private answerOf(result: BatchResult | undefined, status: string, batchError?: string): EvalAnswer {
    const failed = (error: string): EvalAnswer => ({
      footnotes: null, error, ms: 0, promptTokens: null, completionTokens: null, reasoningTokens: null,
    });

    if (status !== 'completed') {
      return failed(`batch ${ status }${ batchError ? ': ' + batchError : '' }`);
    }
    else if (result === undefined) {
      return failed('no result for this request');
    }
    else if (result.response === null || result.response.status_code !== 200) {
      return failed(result.error?.message ?? `status ${ result.response?.status_code ?? 'unknown' }`);
    }
    else {
      return footnotesAnswer(result.response.body, 0);
    }
  }

  // Why a probe batch's answers show the batch didn't keep the request, or null when they don't:
  // none answered; citations without the fields the response format requires (it was dropped);
  // or no thinking when reasoning was asked for (the setting was dropped)
  private checkProbe(probe: SavedAnswer[], reasoning: ORReasoning | undefined): string | null {
    const ok        = probe.filter((a) => a.footnotes !== null);
    const citations = ok.flatMap((a) => (a.footnotes ?? []).flatMap((f) => f.citations));
    const thinking  = ok.reduce((n, a) => n + (a.reasoningTokens ?? 0), 0);
    const thinks    = reasoning !== undefined && !('enabled' in reasoning);

    if (ok.length === 0) {
      return `no request answered (${ probe[0]?.error ?? 'no results' })`;
    }
    else if (citations.some((c) => !('collection' in c) || !('locationsCited' in c))) {
      return 'answers without the fields the response format requires';
    }
    else if (thinks && thinking === 0) {
      return 'no thinking tokens, though reasoning was asked for';
    }
    else {
      return null;
    }
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
