// Write the teacher's answers from relabel-footnote-training back to the collection, in place
// of the citations extracted before (see writtenBackCitations in core/footnote_relabel.ts): on
// the pages of each batch whose answer is kept, its citations; elsewhere, those saved, as they
// were. Each book's citations are saved in one transaction (see replaceExtractedCitations),
// then linked to the books they cite, as importing does.
//   npm run cli -- apply-relabelled-citations --model <teacher> [--dry-run [--show <n>]]
//     [--book <id>] [--limit <n>] [--in <dir>]
// --model: the teacher whose answers to write (its folder in <in>/relabel). --dry-run: say
// what would change, writing nothing, the citations before and after on the pages replaced
// of the first --show books (3). --book: only this book. --limit: only the first n books.
import fs from 'node:fs';
import path from 'node:path';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { matchCitedBooks } from '../../core/cited_books.ts';
import type { JournalArticleRef } from '../../core/collection_links.ts';
import {
  batchBookId, judgeAnswer, trainingBatch, writtenBackCitations, type TrainingBatch,
} from '../../core/footnote_relabel.ts';
import type { TrainingChat } from '../../core/footnote_training.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import type { EvalAnswer } from '../../lib/footnote_eval.ts';
import { BookQueries } from '../../model/books.ts';
import { ExtractedCitationActions, ExtractedCitationQueries } from '../../model/extracted_citations.ts';
import { findCitationsInBook, linkCitations } from '../../model/incoming_citations.ts';
import { JournalQueries } from '../../model/journals.ts';
import { QueuedBookImportsQueries } from '../../model/queued_book_imports.ts';

const DEFAULT_IN   = 'output/footnote_training';
const DEFAULT_SHOW = 3;
const SPLITS       = ['train', 'valid'];

type Options = { model?: string, in?: string, dryRun?: boolean, show?: number, book?: string, limit?: number };

type SavedAnswer = EvalAnswer & { id: string };

const readLines = <T>(file: string): T[] => (
  fs.existsSync(file)
    ? fs.readFileSync(file, 'utf8').split('\n').filter((l) => l.trim().length > 0).map((l) => JSON.parse(l) as T)
    : []
);

@Command({
  name:        'apply-relabelled-citations',
  description: "Replace books' citations with the relabelling teacher's answers",
})
export class ApplyRelabelledCitationsCommand extends CommandRunner {
  // the journal articles in the collection, read once (see linkCitations)
  private articles?: JournalArticleRef[];

  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--model <model>', description: 'the teacher whose answers to write back' })
  parseModel(value: string): string {
    return value;
  }

  @Option({ flags: '--in <dir>', description: `the chats exported and relabelled (${ DEFAULT_IN })` })
  parseIn(value: string): string {
    return value;
  }

  @Option({ flags: '--dry-run', description: 'say what would change, writing nothing' })
  parseDryRun(): boolean {
    return true;
  }

  @Option({ flags: '--show <n>', description: `with --dry-run, the citations before and after of the first n books (${ DEFAULT_SHOW })` })
  parseShow(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--book <id>', description: 'only this book' })
  parseBook(value: string): string {
    return value;
  }

  @Option({ flags: '--limit <n>', description: 'only the first n books' })
  parseLimit(value: string): number {
    return Number(value);
  }

  async run(_: string[], options: Options = {}): Promise<void> {
    if (options.model === undefined) {
      throw new Error('give the teacher: --model <model>');
    }

    const dir     = options.in ?? DEFAULT_IN;
    const out     = path.join(dir, 'relabel', options.model.replace(/[^\w.-]+/g, '_'));
    const answers = new Map(readLines<SavedAnswer>(path.join(out, 'answers.jsonl')).map((a) => [a.id, a]));
    const batches = SPLITS.flatMap((s) => readLines<TrainingChat>(path.join(dir, `${ s }.jsonl`)).map(trainingBatch))
      .filter((b) => answers.has(b.id));
    const byBook  = batches.reduce(
      (m, b) => m.set(batchBookId(b.id), [...m.get(batchBookId(b.id)) ?? [], b]),
      new Map<string, TrainingBatch[]>(),
    );
    const chosen  = [...byBook.keys()].sort()
      .filter((id) => options.book === undefined || id === options.book)
      .slice(0, options.limit ?? Infinity);

    if (answers.size === 0) {
      throw new Error(`no answers in ${ out }/answers.jsonl: run relabel-footnote-training --model ${ options.model } first`);
    }

    console.log(`${ options.model }: ${ answers.size } answers, of ${ byBook.size } books; `
      + `${ chosen.length } books to write back${ options.dryRun ? ' (dry run: nothing is written)' : '' }`);

    const totals = { books: 0, unchanged: 0, pages: 0, before: 0, after: 0, unplaced: 0 };

    for (const [n, bookId] of chosen.entries()) {
      const answered = byBook.get(bookId)!.map((batch) => {
        const footnotes = answers.get(batch.id)!.footnotes;
        return { batch, footnotes, verdict: judgeAnswer(batch, footnotes).verdict };
      });
      const saved    = await ExtractedCitationQueries.findPlacedCitations(this.db, bookId);
      const blocks   = await ExtractedCitationQueries.findFootnoteBlocks(this.db, bookId);
      const result   = writtenBackCitations(bookId, answered, saved, blocks);
      const pages    = new Set(result.replacedPages);
      const before   = saved.filter((s) => pages.has(s.pageNumber));
      const after    = result.citations.filter((c) => pages.has(c.pageNumber));

      if (pages.size === 0) {
        totals.unchanged++;
        continue;
      }

      totals.books++;
      totals.pages    += pages.size;
      totals.before   += before.length;
      totals.after    += after.length;
      totals.unplaced += result.unplaced.length;

      console.log(`${ n + 1 }/${ chosen.length } ${ bookId }: ${ pages.size } pages, `
        + `${ before.length } citations -> ${ after.length }`
        + (result.unplaced.length > 0 ? `, ${ result.unplaced.length } left out (no block with their marker)` : ''));

      if (options.dryRun) {
        if (totals.books <= (options.show ?? DEFAULT_SHOW)) {
          this.show(before, after);
        }
      }
      else {
        await ExtractedCitationActions.replaceExtractedCitations(this.db, bookId, result.citations, []);
        console.log(`  saved; linked ${ await this.linkCitations(bookId) } citations to books in the collection`);
      }
    }

    console.log(`\n${ options.dryRun ? 'would write' : 'wrote' } ${ totals.books } books (${ totals.unchanged } `
      + `with no answer kept, unchanged): ${ totals.pages } pages, ${ totals.before } citations -> `
      + `${ totals.after }, ${ totals.unplaced } left out`);

    if (!options.dryRun && totals.books > 0) {
      console.log('linking citations to the works cited by their parts: '
        + relinkSummary(await linkUnlinkedCitations(this.db)));
    }
  }

  // The citations on the pages replaced, page by page, before and after
  private show(
    before: { pageNumber: number, citation: { raw: string } }[],
    after: { pageNumber: number, citation: { raw: string, title: string, collection?: string | null } }[],
  ) {
    const pages = [...new Set([...before, ...after].map((c) => c.pageNumber))].sort((a, b) => a - b);

    for (const page of pages) {
      console.log(`  page ${ page }`);
      before.filter((c) => c.pageNumber === page).forEach((c) => console.log(`    - ${ c.citation.raw.slice(0, 100) }`));
      after.filter((c) => c.pageNumber === page).forEach((c) => console.log(`    + ${ c.citation.raw.slice(0, 100) }`
        + (c.citation.collection ? `  [${ c.citation.collection }]` : '')));
    }
  }

  // Link the book's citations to the books in the collection they cite, as importing does
  private async linkCitations(bookId: string): Promise<number> {
    const { referencing } = matchCitedBooks(
      await findCitationsInBook(this.db, bookId),
      await BookQueries.findOtherBookNames(this.db, bookId),
      await QueuedBookImportsQueries.findQueuedBookNames(this.db),
      (this.articles ??= await JournalQueries.findArticleRefs(this.db)).filter((a) => a.id !== bookId),
    );

    return linkCitations(this.db, referencing);
  }
}
