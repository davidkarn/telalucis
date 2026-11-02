// Import the next OCR'd books (queued, status inProgress), one after another: each one's pages and
// blocks from its surya results, then the citations in its footnotes, extracted by an LLM a page at
// a time (see core/footnote_extraction.ts), with the insights the model learned doing it. Marks
// each imported, for find-and-queue-cited-books, and points the citations of
// it other books made before at it (see lib/citation_relinking.ts). The saving is done by actions
// (core/ocred_book_import.ts); this reads what they need. Needs OPENROUTER_KEY.
//   npm run cli -- process-ocred-books [count] [--skip-insights]
// count: how many books, 10 when not given. --skip-insights: extract without
// the insights (see footnotesPrompt), for shorter requests.
import { Inject } from '@nestjs/common';
import fs from 'node:fs/promises';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { executeActions } from '../../actions/app_actions.ts';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { build_pages } from '../../core/book_pages.ts';
import { suryaResultsPath } from '../../core/book_files.ts';
import { importOcredBookActions, startImportActions } from '../../core/ocred_book_import.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import { extractFootnoteCitations } from '../../lib/footnote_citations.ts';
import { QueuedBookImportsQueries } from '../../model/queued_book_imports.ts';
import type { SuryaBook } from '../../types.ts';

// Books imported when no count is given
const DEFAULT_COUNT = 10;

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

  @Option({ flags: '--skip-insights', description: 'extract citations without insights, given or learned' })
  parseSkipInsights(): boolean {
    return true;
  }

  async run([count]: string[], options: { skipInsights?: boolean } = {}): Promise<void> {
    const books = countFrom(count);

    for (let imported = 0; imported < books; imported++) {
      if (!await this.importNextBook(options.skipInsights === true)) {
        console.log(`no more OCR'd books waiting to be imported (imported ${ imported } of ${ books })`);
        break;
      }
    }
  }

  // Import the next book waiting; false when none is
  private async importNextBook(skipInsights: boolean): Promise<boolean> {
    const queued = await QueuedBookImportsQueries.findNextQueuedBook(this.db, 'inProgress');

    if (!queued) {
      return false;
    }
    else if (!queued.pdf_url) {
      throw new Error(`queued book ${ queued.id } has no pdf_url`);
    }
    else {
      console.log(`${ queued.id }: importing ${ queued.title }`);
      await executeActions(this.db, startImportActions(queued));

      const surya: SuryaBook     = JSON.parse(await fs.readFile(suryaResultsPath(queued.pdf_url), 'utf8'));
      const [bookId, suryaPages] = Object.entries(surya)[0] ?? [];

      if (!bookId || !suryaPages) {
        throw new Error(`surya results for queued book ${ queued.id } have no pages`);
      }
      else {
        // the book's id is the OCR folder name, surya's key for it
        const pages     = build_pages(suryaPages).pages;
        const extracted = await extractFootnoteCitations(this.db, bookId, suryaPages, { skipInsights });

        await executeActions(this.db, importOcredBookActions(queued, bookId, pages, extracted));
        // the citations other books made of it before it was imported
        console.log(`  ${ relinkSummary(await linkUnlinkedCitations(this.db, { bookIds: [bookId] })) }`);
        return true;
      }
    }
  }
}
