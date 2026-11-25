// Draw the set of batches to measure footnote extraction on (see
// core/footnote_eval.ts) from the test books (see core/footnote_training.ts):
// each batch's footnotes and the citations the collection has of them, to
// be checked by hand (each batch's "checked" set true once its "expected" is
// right). It won't write over a set already made, whose checks would be lost,
// without --force.
//   npm run cli -- make-footnote-eval-set [--count <n>] [--per-book <n>] [--out <file>] [--force]
import fs from 'node:fs/promises';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { footnoteBatches } from '../../core/footnote_batches.ts';
import { footnoteHtml } from '../../core/footnote_extraction.ts';
import {
  evalBatchId, evalSetJson, isArticle, sampleEvalBatches, type EvalBatch, type EvalSet,
} from '../../core/footnote_eval.ts';
import { batchAnswer, citedShare, MIN_COVERAGE, splitOf } from '../../core/footnote_training.ts';
import { FootnoteTrainingQueries } from '../../model/footnote_training.ts';

const DEFAULT_OUT      = 'eval/footnotes.json';
const DEFAULT_COUNT    = 200;
const DEFAULT_PER_BOOK = 3;

const ABOUT = 'Batches of OCR\'d footnotes whose citations are known, for measuring footnote '
  + 'extraction (npm run cli -- eval-footnotes --model <model>). Drawn from the test books '
  + '(never trained on) by make-footnote-eval-set, expected as the collection had them; '
  + 'a batch is "checked" once its expected footnotes are corrected by hand. Each footnote: '
  + 'page, identifier (its marker), citations (author, title, collection and issue (optional), location, raw, locationsCited: '
  + 'groups of {rawLabel, type, values}). Footnotes citing nothing may be left out.';

@Command({
  name:        'make-footnote-eval-set',
  description: 'Draw batches of footnotes from the test books, to measure footnote extraction on',
})
export class MakeFootnoteEvalSetCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--count <n>', description: `batches (${ DEFAULT_COUNT })` })
  parseCount(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--per-book <n>', description: `at most this many a journal article (${ DEFAULT_PER_BOOK })` })
  parsePerBook(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--out <file>', description: `the file (${ DEFAULT_OUT })` })
  parseOut(value: string): string {
    return value;
  }

  @Option({ flags: '--force', description: 'write over a set already made' })
  parseForce(): boolean {
    return true;
  }

  async run(
    _: string[], options: { count?: number, perBook?: number, out?: string, force?: boolean } = {},
  ): Promise<void> {
    const out    = options.out ?? DEFAULT_OUT;
    const exists = await fs.access(out).then(() => true, () => false);

    if (exists && options.force !== true) {
      throw new Error(`${ out } is made already (its checks would be lost): --force to write over it`);
    }

    const bookIds              = (await FootnoteTrainingQueries.findOcrBookIds(this.db)).filter((id) => splitOf(id) === 'test');
    const batches: EvalBatch[] = [];

    for (const bookId of bookIds) {
      const pages     = await FootnoteTrainingQueries.findFootnotePages(this.db, bookId);
      const citations = await FootnoteTrainingQueries.findSavedCitations(this.db, bookId);

      const own = footnoteBatches(pages).map((batch) => ({ batch, answer: batchAnswer(batch, citations) }));

      // as export-footnote-training: not a book whose citations were read poorly
      if (citedShare(own.map((b) => b.answer)) >= MIN_COVERAGE) {
        batches.push(...own.map(({ batch, answer }) => ({
          id:       evalBatchId(bookId, batch),
          bookId,
          checked:  false,
          pages:    batch.map((p) => ({ page: p.page, html: footnoteHtml(p) })),
          expected: answer.filter((f) => f.citations.length > 0),
        })));
      }
    }

    const sampled      = sampleEvalBatches(batches, {
      count: options.count ?? DEFAULT_COUNT, perBook: options.perBook ?? DEFAULT_PER_BOOK,
    });
    const set: EvalSet = { about: ABOUT, batches: sampled };

    await fs.writeFile(out, evalSetJson(set) + '\n');
    console.log(`wrote ${ sampled.length } batches of ${ new Set(sampled.map((b) => b.bookId)).size } books `
      + `(of ${ bookIds.length } test books, ${ batches.length } batches) to ${ out }; `
      + `${ sampled.reduce((n, b) => n + b.expected.reduce((m, f) => m + f.citations.length, 0), 0) } citations; `
      + `${ sampled.filter((b) => isArticle(b.bookId)).length } of journal articles`);
  }
}
