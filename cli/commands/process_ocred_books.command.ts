// Import the next OCR'd books (queued, status inProgress): each one's pages and blocks from its
// surya results, then the citations in its footnotes, extracted by an LLM several pages at a time
// (see lib/footnote_citations.ts). Marks each imported, for find-and-queue-cited-books, and
// points the citations of it other books made before at it (see lib/citation_relinking.ts). The
// saving is done by actions (core/ocred_book_import.ts); this reads what they need. Needs
// OPENROUTER_KEY.
//   npm run cli -- process-ocred-books [count] [--model <model>] [--examples]
//     [--batch [--batch-size <n>]]
// count: how many books, 10 when not given. --model: the model extracting
// citations, an OpenRouter one or "local/<name>" for a local server's (see
// lib/open_router.ts). --examples: show it citations in the collection like
// each batch's (see core/citation_examples.ts). --batch: ask for all the
// books' citations at once through OpenRouter's Batch API, for about half the
// price, waiting for the answers (most take minutes, at most a day; see
// lib/footnote_batch_extraction.ts), then import the books one after another.
// The books being extracted are kept in BATCH_DIR/books.json: a run stopped
// while waiting takes them up again (before any more) when run again.
import { Inject } from '@nestjs/common';
import fs from 'node:fs/promises';
import { existsSync, readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import path from 'node:path';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { executeActions } from '../../actions/app_actions.ts';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { build_pages } from '../../core/book_pages.ts';
import { suryaResultsPath } from '../../core/book_files.ts';
import { importOcredBookActions, startImportActions, type ExtractedCitations } from '../../core/ocred_book_import.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import { DEFAULT_BATCH_SIZE, extractInBatches } from '../../lib/footnote_batch_extraction.ts';
import {
  DEFAULT_FOOTNOTES_MODEL, extractFootnoteCitations, type ExtractionOptions,
} from '../../lib/footnote_citations.ts';
import { isLocalModel } from '../../lib/open_router.ts';
import { CitationExampleQueries } from '../../model/citation_examples.ts';
import { QueuedBookImportsQueries, type QueuedBook } from '../../model/queued_book_imports.ts';
import type { SuryaBook, SuryaPage } from '../../types.ts';

// Books imported when no count is given
const DEFAULT_COUNT = 10;

// Where --batch keeps its answers and the API batches and books it's waiting on
const BATCH_DIR = 'output/footnote_batches/ocred_books';

type Options = { model?: string, examples?: boolean, batch?: boolean, batchSize?: number };

// A book claimed from the queue, with its surya results read
type Claimed = { queued: QueuedBook, bookId: string, suryaPages: SuryaPage[] };

// The count argument: a whole number, 1 or more; DEFAULT_COUNT when it isn't given
const countFrom = (raw: string | undefined): number => {
  const count = raw === undefined ? DEFAULT_COUNT : Number(raw);

  if (!Number.isInteger(count) || count < 1) {
    throw new Error(`count must be a whole number, 1 or more, not ${ raw }`);
  }
  else {
    return count;
  }
};

@Command({
  name:        'process-ocred-books',
  arguments:   '[count]',
  description: `Import the next OCR'd books (count, ${ DEFAULT_COUNT } when not given): their pages, `
    + 'and the citations in their footnotes',
})
export class ProcessOcredBooksCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--model <model>', description: `the model extracting citations (${ DEFAULT_FOOTNOTES_MODEL }; "local/<name>" for a local server's)` })
  parseModel(value: string): string {
    return value;
  }

  @Option({ flags: '--examples', description: 'show the model citations like each batch\'s, as read before' })
  parseExamples(): boolean {
    return true;
  }

  @Option({ flags: '--batch', description: "extract through OpenRouter's Batch API, for about half the price" })
  parseBatch(): boolean {
    return true;
  }

  @Option({ flags: '--batch-size <n>', description: `with --batch, requests an API batch (${ DEFAULT_BATCH_SIZE })` })
  parseBatchSize(value: string): number {
    return Number(value);
  }

  async run([count]: string[], options: Options = {}): Promise<void> {
    const books = countFrom(count);
    const how   = {
      ...(options.model ? { model: options.model } : {}),
      ...(options.examples ? { examples: await CitationExampleQueries.findExampleIndex(this.db) } : {}),
    };

    if (options.batch === true && how.model !== undefined && isLocalModel(how.model)) {
      throw new Error(`a local model (${ how.model }) has no Batch API: import without --batch`);
    }
    else if (options.batch === true) {
      await this.importInBatches(books, how, options.batchSize);
    }
    else {
      for (let imported = 0; imported < books; imported++) {
        const claimed = await this.claimNextBook();

        if (claimed === null) {
          console.log(`no more OCR'd books waiting to be imported (imported ${ imported } of ${ books })`);
          break;
        }
        else {
          await this.importBook(claimed, await extractFootnoteCitations(claimed.bookId, claimed.suryaPages, how));
        }
      }
    }
  }

  // Claim up to `count` books, those claimed by a batch run stopped before first; ask for all
  // their citations through the Batch API; then import each
  private async importInBatches(count: number, how: ExtractionOptions, batchSize?: number) {
    const booksFile = path.join(BATCH_DIR, 'books.json');
    const inFlight  = existsSync(booksFile) ? JSON.parse(readFileSync(booksFile, 'utf8')) as string[] : [];
    const saveBooks = (ids: string[]) => {
      mkdirSync(BATCH_DIR, { recursive: true });
      writeFileSync(booksFile, JSON.stringify(ids, null, 2) + '\n');
    };
    const resumed   = await QueuedBookImportsQueries.findQueuedBooksWithIds(this.db, inFlight, 'processingContents');
    const claimed: Claimed[] = [];

    for (const queued of resumed) {
      console.log(`${ queued.id }: taken up again from a batch run stopped before`);
      claimed.push(await this.readBook(queued));
    }
    while (claimed.length < count) {
      const next = await this.claimNextBook();

      if (next === null) {
        console.log(`no more OCR'd books waiting to be imported (${ claimed.length } to import)`);
        break;
      }
      else {
        claimed.push(next);
        saveBooks(claimed.map((c) => c.queued.id));
      }
    }

    saveBooks(claimed.map((c) => c.queued.id));
    if (claimed.length === 0) {
      return;
    }

    const extraction = await extractInBatches(
      claimed.map((c) => ({ bookId: c.bookId, pages: c.suryaPages })),
      { ...how, dir: BATCH_DIR, ...(batchSize ? { batchSize } : {}) },
    );

    for (const [i, book] of claimed.entries()) {
      console.log(`${ i + 1 }/${ claimed.length } ${ book.queued.id }: saving ${ book.queued.title }`);
      await this.importBook(book, await extraction.citationsOf(book.bookId));
      saveBooks(claimed.slice(i + 1).map((c) => c.queued.id));
    }
  }

  // The next book waiting, claimed (so no other run imports it meanwhile) and its surya results
  // read; null when none is waiting
  private async claimNextBook(): Promise<Claimed | null> {
    const queued = await QueuedBookImportsQueries.findNextQueuedBook(this.db, 'inProgress');

    if (!queued) {
      return null;
    }
    else {
      console.log(`${ queued.id }: importing ${ queued.title }`);
      await executeActions(this.db, startImportActions(queued));
      return this.readBook(queued);
    }
  }

  private async readBook(queued: QueuedBook): Promise<Claimed> {
    if (!queued.pdf_url) {
      throw new Error(`queued book ${ queued.id } has no pdf_url`);
    }
    else {
      const surya: SuryaBook     = JSON.parse(await fs.readFile(suryaResultsPath(queued.pdf_url), 'utf8'));
      const [bookId, suryaPages] = Object.entries(surya)[0] ?? [];

      if (!bookId || !suryaPages) {
        throw new Error(`surya results for queued book ${ queued.id } have no pages`);
      }
      else {
        // the book's id is the OCR folder name, surya's key for it
        return { queued, bookId, suryaPages };
      }
    }
  }

  // Save the book's pages and citations, mark it imported, and point other books' citations of it at it
  private async importBook({ queued, bookId, suryaPages }: Claimed, extracted: ExtractedCitations) {
    const pages = build_pages(suryaPages).pages;

    await executeActions(this.db, importOcredBookActions(queued, bookId, pages, extracted));
    // the citations other books made of it before it was imported
    console.log(`  ${ relinkSummary(await linkUnlinkedCitations(this.db, { bookIds: [bookId] })) }`);
  }
}
