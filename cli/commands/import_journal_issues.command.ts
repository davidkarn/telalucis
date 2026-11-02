// Import the journal issues downloaded from archive.org and OCR'd with surya
// (../thomist, ../the_new_scholasticism, ../the_modern_schoolman,
// ../franciscan_studies; see core/journal_issues.ts), as process-ocred-books
// imports a book: each issue a book of its pages, with the citations in its
// footnotes extracted by an LLM a page at a time. Each is put in its journal
// issue (journal_issues) and journal, found or added. Then its citations are
// pointed at the books in the collection they cite, and earlier citations of
// it at it (see lib/citation_relinking.ts). Issues not OCR'd yet, and
// those imported already (their book is saved), are passed over.
//   npm run cli -- import-journal-issues [folders...] [--limit <n>] [--dir <folder>]
//     [--reimport] [--dry-run] [--skip-insights]
// folders: the journals' folders (all when none are given). The issues' titles
// come from archive.org's metadata, kept in output/journal_issue_metadata.json.
// --skip-insights: extract without the insights (see footnotesPrompt), for
// shorter requests. Needs OPENROUTER_KEY.
import { Inject } from '@nestjs/common';
import fs from 'node:fs/promises';
import path from 'node:path';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { executeActions } from '../../actions/app_actions.ts';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { build_pages } from '../../core/book_pages.ts';
import { matchCitedBooks } from '../../core/cited_books.ts';
import { importJournalIssueActions } from '../../core/journal_issue_import.ts';
import {
  JOURNALS, issueName, issuePeriods, issueTitle, periodOf, type Period,
} from '../../core/journal_issues.ts';
import { archiveItemMetadata } from '../../lib/archive_search.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import { extractFootnoteCitations } from '../../lib/footnote_citations.ts';
import { BookQueries } from '../../model/books.ts';
import { findCitationsInBook, linkCitations } from '../../model/incoming_citations.ts';
import { QueuedBookImportsQueries } from '../../model/queued_book_imports.ts';
import type { SuryaBook } from '../../types.ts';

const DEFAULT_DIR = '..';

const METADATA_CACHE = 'output/journal_issue_metadata.json';

type Metadata = { identifier: string, title: string, date: string } | null;

type Options = {
  limit?: number, dir?: string, reimport?: boolean, dryRun?: boolean, skipInsights?: boolean,
};

@Command({
  name:        'import-journal-issues',
  arguments:   '[folders...]',
  description: "Import the OCR'd journal issues (../thomist and the others), with their footnotes' citations",
})
export class ImportJournalIssuesCommand extends CommandRunner {
  private metadata: Record<string, Metadata> = {};

  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--limit <n>', description: 'import at most this many issues' })
  parseLimit(value: string): number {
    const limit = Number(value);

    if (!Number.isInteger(limit) || limit < 1) {
      throw new Error('--limit must be a whole number, 1 or more');
    }
    else {
      return limit;
    }
  }

  @Option({ flags: '--dir <folder>', description: `the folder the journals' folders are in (${ DEFAULT_DIR })` })
  parseDir(value: string): string {
    return value;
  }

  @Option({ flags: '--reimport', description: 'import issues imported already again' })
  parseReimport(): boolean {
    return true;
  }

  @Option({ flags: '--dry-run', description: 'list what would be imported, without the LLM or saving' })
  parseDryRun(): boolean {
    return true;
  }

  @Option({ flags: '--skip-insights', description: 'extract citations without insights, given or learned' })
  parseSkipInsights(): boolean {
    return true;
  }

  async run(folders: string[], options: Options = {}): Promise<void> {
    const unknown = folders.filter((f) => !JOURNALS.some((j) => j.folder === f));
    if (unknown.length > 0) {
      throw new Error(`no such journals: ${ unknown.join(', ') } (${ JOURNALS.map((j) => j.folder).join(', ') })`);
    }

    const dir      = options.dir ?? DEFAULT_DIR;
    const journals = JOURNALS.filter((j) => folders.length === 0 || folders.includes(j.folder));
    const imported = await BookQueries.findBookIds(this.db);
    let count      = 0;

    this.metadata = await fs.readFile(METADATA_CACHE, 'utf8')
      .then((s) => JSON.parse(s) as Record<string, Metadata>, () => ({}));

    for (const journal of journals) {
      const folder  = path.join(dir, journal.folder);
      const names   = (await pdfsIn(folder)).map((pdf) => path.basename(pdf, '.pdf'));
      const periods = issuePeriods(names.map((n) => periodOf(issueName(n).date ?? '')));
      let waiting   = 0;

      for (const [i, name] of names.entries()) {
        const results = path.join(folder, 'results', 'surya', name, 'results.json');
        const ocred   = await fs.access(results).then(() => true, () => false);

        if (!ocred) {
          waiting++;
        }
        else if ((imported.has(name) && !options.reimport) || count >= (options.limit ?? Infinity)) {
          // imported already, or enough for this run
        }
        else {
          count++;
          await this.importIssue(journal.name, name, results, periods[i] ?? null, options);
        }
      }

      console.log(`${ journal.name }: ${ names.length } issues, ${ waiting } not OCR'd yet`);
    }

    console.log(options.dryRun ? `dry run: ${ count } issues would be imported` : `imported ${ count } issues`);
  }

  private async importIssue(
    journal: string, name: string, resultsFile: string, fromName: Period | null, options: Options,
  ) {
    const surya: SuryaBook     = JSON.parse(await fs.readFile(resultsFile, 'utf8'));
    const [bookId, suryaPages] = Object.entries(surya)[0] ?? [];
    const meta                 = await this.metadataOf(name);
    const period               = fromName ?? periodOf(meta?.title ?? '') ?? periodOf(meta?.date ?? '');

    if (!bookId || !suryaPages) {
      console.error(`${ name }: its surya results have no pages`);
    }
    else if (!period) {
      console.error(`${ name }: no date in its name or its archive.org title; not imported`);
    }
    else {
      const issue         = {
        bookId,
        journal,
        title: issueTitle(journal, issueName(name), meta?.title ?? null),
        url:   meta ? `https://archive.org/details/${ meta.identifier }` : null,
        period,
      };
      const footnotePages = suryaPages.filter((p) => p.blocks.some((b) => b.label === 'Footnote')).length;

      console.log(`${ bookId }: "${ issue.title }", ${ period.start } to ${ period.end }, `
        + `${ suryaPages.length } pages (${ footnotePages } with footnotes)`);

      if (!options.dryRun) {
        const pages     = build_pages(suryaPages).pages;
        const extracted = await extractFootnoteCitations(this.db, bookId, suryaPages, {
          skipInsights: options.skipInsights === true,
        });
        const results   = await executeActions(this.db, importJournalIssueActions(issue, pages, extracted));

        if (results['saveCitations']?.success) {
          console.log(`  linked ${ await this.linkCitations(bookId) } citations to books in the collection`);
          console.log(`  ${ relinkSummary(await linkUnlinkedCitations(this.db, { bookIds: [bookId] })) }`);
        }
      }
    }
  }

  // The issue's archive.org item: named as its file is, or with "sim_" before
  // it (some files were saved without it); kept in METADATA_CACHE
  private async metadataOf(name: string): Promise<Metadata> {
    if (!(name in this.metadata)) {
      const ids           = name.startsWith('sim_') ? [name] : [name, `sim_${ name }`];
      let found: Metadata = null;

      for (const id of ids) {
        found = found ?? await archiveItemMetadata(id).catch(() => null);
      }
      this.metadata[name] = found;
      await fs.mkdir(path.dirname(METADATA_CACHE), { recursive: true });
      await fs.writeFile(METADATA_CACHE, JSON.stringify(this.metadata, null, 1));
    }
    return this.metadata[name] ?? null;
  }

  // Point the book's citations at the books in the collection they cite (see
  // matchCitedBooks); how many were
  private async linkCitations(bookId: string): Promise<number> {
    const { referencing } = matchCitedBooks(
      await findCitationsInBook(this.db, bookId),
      await BookQueries.findOtherBookNames(this.db, bookId),
      await QueuedBookImportsQueries.findQueuedBookNames(this.db),
    );

    return linkCitations(this.db, referencing);
  }
}

// The PDFs in a journal's folder and the folders in it, in name order; not
// those in its OCR results
const pdfsIn = async(folder: string): Promise<string[]> => {
  const entries = await fs.readdir(folder, { withFileTypes: true, recursive: true });

  return entries
    .filter((e) => e.isFile() && e.name.toLowerCase().endsWith('.pdf'))
    .map((e) => path.join(e.parentPath, e.name))
    .filter((p) => !path.relative(folder, p).split(path.sep).includes('results'))
    .sort((a, b) => path.basename(a).localeCompare(path.basename(b)));
};
