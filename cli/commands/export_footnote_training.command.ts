// Write the chats to fine-tune a footnote extraction model on (see
// core/footnote_training.ts): each batch of each OCR'd book's footnotes with
// the citations saved of it, a line of JSON each, in train.jsonl, valid.jsonl
// and test.jsonl by the book's split. The test books are those the eval set is
// drawn from (make-footnote-eval-set): not to train on.
//   npm run cli -- export-footnote-training [--out <dir>] [--books <n>] [--min-coverage <share>]
// --out: where to write them, output/footnote_training when not given.
// --books: only the first n books, to try it. --min-coverage: leave out books
// with fewer of their footnotes' pages cited (see MIN_COVERAGE, 0.7).
import fs from 'node:fs';
import path from 'node:path';
import { once } from 'node:events';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { footnoteBatches } from '../../core/footnote_batches.ts';
import {
  batchAnswer, citedShare, MIN_COVERAGE, splitOf, trainingChat, type Split,
} from '../../core/footnote_training.ts';
import { FootnoteTrainingQueries } from '../../model/footnote_training.ts';

const DEFAULT_OUT = 'output/footnote_training';

const SPLITS: Split[] = ['train', 'valid', 'test'];

type Counts = { books: number, skipped: number, batches: number, citations: number, emptyPages: number };

@Command({
  name:        'export-footnote-training',
  description: "Write chats to fine-tune a footnote extraction model on, from the OCR'd books' citations",
})
export class ExportFootnoteTrainingCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--out <dir>', description: `where to write the files (${ DEFAULT_OUT })` })
  parseOut(value: string): string {
    return value;
  }

  @Option({ flags: '--books <n>', description: 'only the first n books' })
  parseBooks(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--min-coverage <share>', description: `leave out books with less of their footnotes cited (${ MIN_COVERAGE })` })
  parseMinCoverage(value: string): number {
    return Number(value);
  }

  async run(_: string[], options: { out?: string, books?: number, minCoverage?: number } = {}): Promise<void> {
    const minCoverage = options.minCoverage ?? MIN_COVERAGE;
    const out         = options.out ?? DEFAULT_OUT;
    const all         = await FootnoteTrainingQueries.findOcrBookIds(this.db);
    const bookIds     = options.books === undefined ? all : all.slice(0, options.books);

    fs.mkdirSync(out, { recursive: true });
    const files  = Object.fromEntries(SPLITS.map((s) => [s, fs.createWriteStream(path.join(out, `${ s }.jsonl`))]));
    const counts = Object.fromEntries(SPLITS.map((s) => [s, { books: 0, skipped: 0, batches: 0, citations: 0, emptyPages: 0 }])) as
      Record<Split, Counts>;

    for (const [i, bookId] of bookIds.entries()) {
      const split     = splitOf(bookId);
      const pages     = await FootnoteTrainingQueries.findFootnotePages(this.db, bookId);
      const citations = await FootnoteTrainingQueries.findSavedCitations(this.db, bookId);
      const count     = counts[split];
      const batches   = footnoteBatches(pages).map((batch) => ({ batch, answer: batchAnswer(batch, citations) }));

      if (citedShare(batches.map((b) => b.answer)) < minCoverage) {
        count.skipped++;
        continue;
      }

      count.books++;
      for (const { batch, answer } of batches) {
        count.batches++;
        count.citations  += answer.reduce((n, f) => n + f.citations.length, 0);
        count.emptyPages += new Set(batch.map((p) => p.page)).size
          - new Set(answer.filter((f) => f.citations.length > 0).map((f) => f.page)).size;
        if (!files[split]!.write(JSON.stringify(trainingChat(bookId, batch, answer)) + '\n')) {
          await once(files[split]!, 'drain');
        }
      }
      if ((i + 1) % 100 === 0) {
        console.log(`${ i + 1 } of ${ bookIds.length } books`);
      }
    }

    await Promise.all(SPLITS.map((s) => new Promise((done) => files[s]!.end(done))));
    console.log(`wrote ${ out }/{${ SPLITS.join(',') }}.jsonl (skipped: books with less than ${ minCoverage } of their footnotes' pages cited; emptyPages: pages whose footnotes cite nothing saved)`);
    console.table(counts);
  }
}
