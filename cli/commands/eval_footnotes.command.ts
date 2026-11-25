// Measure a model's footnote extraction on the eval set (see
// core/footnote_eval.ts and make-footnote-eval-set): ask it for each batch's
// citations and compare them with those expected, printing how many it found
// and got right, and how fast. The answers are written to a file to look
// over.
//   npm run cli -- eval-footnotes --model <model> [--set <file>] [--checked-only]
//     [--insights] [--reasoning <off|tokens|effort>] [--limit <n>] [--out <file>]
// --model: an OpenRouter model, or "local/<name>" for a local server's (see
// lib/open_router.ts). --insights: give it the saved insights that apply, as
// importing does (to measure the model used now as it's used); without, the
// prompt a trained model is given. --checked-only: only the batches checked by
// hand. --reasoning: how much the model thinks: "off", a budget of tokens, or an
// effort (see ORReasoning); as importing asks the model when not given (see
// defaultReasoning). --out: the answers,
// output/footnote_eval/<model>[_reasoning-<setting>].json when not given.
import fs from 'node:fs/promises';
import path from 'node:path';
import { setTimeout } from 'node:timers/promises';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import {
  addScores, COMPARED_FIELDS, evalSetProblems, NO_SCORE, scoreBatch, scoreSummary, type EvalSet,
} from '../../core/footnote_eval.ts';
import { DEFAULT_FOOTNOTES_MODEL, defaultReasoning } from '../../lib/footnote_citations.ts';
import { askEvalBatch, findEvalInsights } from '../../lib/footnote_eval.ts';
import {
  isLocalModel, parseReasoning, reasoningName, type ORReasoning,
} from '../../lib/open_router.ts';

const DEFAULT_SET = 'eval/footnotes.json';

// OpenRouter allows 20 requests a minute (as lib/footnote_citations.ts)
const REQUEST_INTERVAL_MS = 3200;

type Options = {
  model?: string, set?: string, checkedOnly?: boolean, insights?: boolean, reasoning?: ORReasoning,
  limit?: number, out?: string,
};

const percent = (x: number) => (100 * x).toFixed(1) + '%';

@Command({
  name:        'eval-footnotes',
  description: "Measure a model's footnote extraction on the eval set",
})
export class EvalFootnotesCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--model <model>', description: `the model (${ DEFAULT_FOOTNOTES_MODEL }; "local/<name>" for a local server's)` })
  parseModel(value: string): string {
    return value;
  }

  @Option({ flags: '--set <file>', description: `the eval set (${ DEFAULT_SET })` })
  parseSet(value: string): string {
    return value;
  }

  @Option({ flags: '--checked-only', description: 'only the batches checked by hand' })
  parseCheckedOnly(): boolean {
    return true;
  }

  @Option({ flags: '--insights', description: 'give the model the saved insights, as importing does' })
  parseInsights(): boolean {
    return true;
  }

  @Option({ flags: '--reasoning <setting>', description: 'how much the model thinks: off, a number of tokens, or an effort (low...)' })
  parseReasoningOption(value: string): ORReasoning {
    return parseReasoning(value);
  }

  @Option({ flags: '--limit <n>', description: 'only the first n batches' })
  parseLimit(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--out <file>', description: 'where to write the answers' })
  parseOut(value: string): string {
    return value;
  }

  async run(_: string[], options: Options = {}): Promise<void> {
    const model     = options.model ?? DEFAULT_FOOTNOTES_MODEL;
    const reasoning = options.reasoning ?? defaultReasoning(model);
    const set       = JSON.parse(await fs.readFile(options.set ?? DEFAULT_SET, 'utf8')) as EvalSet;
    const problems  = evalSetProblems(set);
    if (problems.length > 0) {
      throw new Error(`The eval set can't be scored:\n${ problems.join('\n') }`);
    }
    const chosen    = set.batches.filter((b) => options.checkedOnly !== true || b.checked);
    const batches   = options.limit === undefined ? chosen : chosen.slice(0, options.limit);
    const insights  = options.insights === true ? await findEvalInsights(this.db) : null;
    const out       = options.out ?? path.join('output/footnote_eval', model.replace(/[^\w.-]+/g, '_')
      + (reasoning ? '_reasoning-' + reasoningName(reasoning) : '') + '.json');
    const answers  = [];

    let total    = NO_SCORE;
    let checked  = NO_SCORE;
    let failures = 0;
    let ms       = 0;
    let tokens   = 0;
    let timed    = 0;
    let thinking = 0;

    for (const [i, batch] of batches.entries()) {
      const answer = await askEvalBatch(batch, model, insights, reasoning);
      const score  = scoreBatch(batch.expected, answer.footnotes ?? []);

      total     = addScores(total, score);
      checked   = batch.checked ? addScores(checked, score) : checked;
      failures += answer.error === null ? 0 : 1;
      ms       += answer.ms;
      if (answer.completionTokens !== null) {
        tokens   += answer.completionTokens;
        thinking += answer.reasoningTokens ?? 0;
        timed  += answer.ms;
      }
      answers.push({ id: batch.id, checked: batch.checked, score, ...answer });
      console.log(`${ i + 1 }/${ batches.length } ${ batch.id }: ${ score.matched }/${ score.expected } found, `
        + `${ score.answered - score.matched } extra, ${ score.exact } exact, ${ (answer.ms / 1000).toFixed(1) }s`
        + (answer.error === null ? '' : ` — ${ answer.error.slice(0, 200) }`));
      if (!isLocalModel(model) && i < batches.length - 1) {
        await setTimeout(REQUEST_INTERVAL_MS);
      }
    }

    await fs.mkdir(path.dirname(out), { recursive: true });
    await fs.writeFile(out, JSON.stringify({ model, insights: insights !== null, answers }, null, 2) + '\n');

    const rows = [
      { of: 'all', batches: batches.length, score: total },
      { of: 'checked', batches: batches.filter((b) => b.checked).length, score: checked },
    ].filter((r) => r.batches > 0).map(({ of, batches: n, score }) => {
      const s = scoreSummary(score);
      return {
        of,
        batches:   n,
        citations: score.expected,
        precision: percent(s.citations.precision),
        recall:    percent(s.citations.recall),
        f1:        percent(s.citations.f1),
        ...Object.fromEntries(COMPARED_FIELDS.map((f) => [f, percent(s.fields[f])])),
        exact:     percent(s.exact),
        placesF1:  percent(s.places.f1),
      };
    });

    console.log(`\n${ model }${ insights === null ? '' : ' with insights' }: ${ failures } failed requests, `
      + `${ (ms / batches.length / 1000).toFixed(1) }s a batch`
      + (timed > 0 ? `, ${ (tokens / (timed / 1000)).toFixed(0) } answer tokens/s` : '')
      + `, ${ (tokens / batches.length).toFixed(0) } answer tokens a batch (${ (thinking / batches.length).toFixed(0) } thinking)`
      + `; answers in ${ out }`);
    console.log('precision/recall/f1: the citations found; identifier...location: right, of those found; '
      + 'exact: found with every field and place right, of those expected; placesF1: the places cited');
    console.table(rows);
  }
}
