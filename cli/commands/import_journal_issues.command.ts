// Import the journal issues downloaded from archive.org and OCR'd with surya
// (../thomist, ../the_new_scholasticism, ../the_modern_schoolman,
// ../franciscan_studies; see core/journal_issues.ts): each article a book of
// its own, by its author and with its title, its pages numbered as the
// journal prints them, and each department (book reviews, notices) a book by
// "Various" (see core/journal_articles.ts). An LLM names an issue's articles
// from an outline of its pages; its answer is kept in
// output/journal_articles/<issue>.json, so an issue is only sent once (and an
// answer can be corrected there). The citations in each article's footnotes
// are extracted by an LLM, as process-ocred-books extracts a book's. Each
// article is put in its journal issue (journal_issues) and journal, found or
// added; its citations are pointed at the books in the collection they cite,
// and earlier citations of it at it (see lib/citation_relinking.ts). An issue
// imported before as one book has that book replaced by its articles. Issues
// not OCR'd yet, and those imported already, are passed over.
//   npm run cli -- import-journal-issues [folders...] [--limit <n>] [--dir <folder>]
//     [--reimport] [--dry-run] [--skip-insights] [--model <model>] [--examples]
// folders: the journals' folders (all when none are given). --dry-run: name
// each issue's articles (asking the LLM for issues not asked before), without
// extracting citations or saving. --skip-insights: extract without the
// insights (see footnotesPrompt), for shorter requests.
// --model: the model extracting citations, an OpenRouter one or "local/<name>"
// for a local server's (see lib/open_router.ts). --examples: show it
// citations in the collection like each batch's (see core/citation_examples.ts).
// Needs OPENROUTER_KEY.
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
import {
  ARTICLES_FORMAT, ARTICLES_PROMPT, articleBookId, articlePages, checkedArticles, isArticleOf, issueOutline,
  pageOffset, renumberCitations, type IssueArticle,
} from '../../core/journal_articles.ts';
import { importJournalArticleActions } from '../../core/journal_issue_import.ts';
import {
  JOURNALS, issueName, issuePeriods, issueTitle, periodOf, type Period,
} from '../../core/journal_issues.ts';
import type { Page } from '../../types.ts';
import { archiveItemMetadata } from '../../lib/archive_search.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import type { ExampleIndex } from '../../core/citation_examples.ts';
import { DEFAULT_FOOTNOTES_MODEL, extractFootnoteCitations } from '../../lib/footnote_citations.ts';
import { CitationExampleQueries } from '../../model/citation_examples.ts';
import { makeOpenRouterRequest, parseJsonResponse } from '../../lib/open_router.ts';
import { BookActions, BookQueries } from '../../model/books.ts';
import { findCitationsInBook, linkCitations } from '../../model/incoming_citations.ts';
import { QueuedBookImportsQueries } from '../../model/queued_book_imports.ts';
import type { SuryaBook } from '../../types.ts';

const DEFAULT_DIR = '..';

const METADATA_CACHE = 'output/journal_issue_metadata.json';

// The LLM's articles of each issue (see core/journal_articles.ts)
const ARTICLES_CACHE = 'output/journal_articles';

const ARTICLES_MODEL = 'openai/gpt-4.1-mini';

type Metadata = { identifier: string, title: string, date: string } | null;

type Options = {
  limit?: number, dir?: string, reimport?: boolean, dryRun?: boolean, skipInsights?: boolean,
  model?: string, examples?: boolean,
};

@Command({
  name:        'import-journal-issues',
  arguments:   '[folders...]',
  description: "Import the OCR'd journal issues (../thomist and the others), with their footnotes' citations",
})
export class ImportJournalIssuesCommand extends CommandRunner {
  private metadata: Record<string, Metadata> = {};
  // the citations shown the model as examples, with --examples
  private examples: ExampleIndex | undefined;

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

  @Option({ flags: '--dry-run', description: "name each issue's articles, without extracting citations or saving" })
  parseDryRun(): boolean {
    return true;
  }

  @Option({ flags: '--skip-insights', description: 'extract citations without insights, given or learned' })
  parseSkipInsights(): boolean {
    return true;
  }

  @Option({ flags: '--model <model>', description: `the model extracting citations (${ DEFAULT_FOOTNOTES_MODEL }; "local/<name>" for a local server's)` })
  parseModel(value: string): string {
    return value;
  }

  @Option({ flags: '--examples', description: 'show the model citations like each batch\'s, as read before' })
  parseExamples(): boolean {
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

    if (options.examples && !options.dryRun) {
      this.examples = await CitationExampleQueries.findExampleIndex(this.db);
    }

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
        else if ((isImported(imported, name) && !options.reimport) || count >= (options.limit ?? Infinity)) {
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
    const surya: SuryaBook = JSON.parse(await fs.readFile(resultsFile, 'utf8'));
    const [, suryaPages]   = Object.entries(surya)[0] ?? [];
    const meta             = await this.metadataOf(name);
    const period           = fromName ?? periodOf(meta?.title ?? '') ?? periodOf(meta?.date ?? '');

    if (!suryaPages) {
      console.error(`${ name }: its surya results have no pages`);
    }
    else if (!period) {
      console.error(`${ name }: no date in its name or its archive.org title; not imported`);
    }
    else {
      const pages    = build_pages(suryaPages).pages;
      const articles = await this.articlesOf(name, pages);

      console.log(`${ name }: "${ issueTitle(journal, issueName(name), meta?.title ?? null) }", `
        + `${ period.start } to ${ period.end }, ${ pages.length } pages, ${ articles.length } articles`);

      if (articles.length === 0) {
        console.log(`  no articles found; not imported (see ${ path.join(ARTICLES_CACHE, `${ name }.json`) })`);
      }

      for (const article of articles) {
        const own    = articlePages(article, pages);
        const offset = pageOffset(pages.filter((p) => p.pageNumber >= article.firstScan && p.pageNumber <= article.lastScan), pages);
        const bookId = articleBookId(name, own[0]!.pageNumber);
        const book   = {
          bookId,
          title:  article.title,
          author: article.author,
          url:    meta ? `https://archive.org/details/${ meta.identifier }/page/n${ article.firstScan - 1 }` : null,
        };

        console.log(`  pp. ${ own[0]!.pageNumber }-${ own.at(-1)!.pageNumber }: ${ book.author || '(unsigned)' }, `
          + `"${ book.title }" (${ bookId })`);

        if (!options.dryRun) {
          const scans      = suryaPages.filter((p) => p.page >= article.firstScan && p.page <= article.lastScan);
          const extracted  = await extractFootnoteCitations(this.db, bookId, scans, {
            skipInsights: options.skipInsights === true,
            ...(options.model ? { model: options.model } : {}),
            ...(this.examples ? { examples: this.examples } : {}),
          });
          const renumbered = { ...extracted, citations: renumberCitations(extracted.citations, offset) };
          const results    = await executeActions(
            this.db, importJournalArticleActions({ journal, period }, book, own, renumbered),
          );

          if (results['saveCitations']?.success) {
            console.log(`    linked ${ await this.linkCitations(bookId) } citations to books in the collection; `
              + relinkSummary(await linkUnlinkedCitations(this.db, { bookIds: [bookId] })));
          }
        }
      }

      if (!options.dryRun && articles.length > 0) {
        await this.removeWholeIssue(name);
      }
    }
  }

  // The issue's articles: the LLM's answer (see ARTICLES_PROMPT), kept in
  // ARTICLES_CACHE, checked (see checkedArticles)
  private async articlesOf(name: string, pages: Page[]): Promise<IssueArticle[]> {
    const file   = path.join(ARTICLES_CACHE, `${ name }.json`);
    const cached = await fs.readFile(file, 'utf8').then((s) => JSON.parse(s) as IssueArticle[], () => null);

    if (cached) {
      return checkedArticles(cached, pages);
    }
    else {
      const response = await makeOpenRouterRequest([
        { role: 'system', content: ARTICLES_PROMPT },
        { role: 'user', content: issueOutline(pages) },
      ], ARTICLES_FORMAT, ARTICLES_MODEL);
      const articles = parseJsonResponse<{ articles: IssueArticle[] }>(response).articles;

      await fs.mkdir(ARTICLES_CACHE, { recursive: true });
      await fs.writeFile(file, JSON.stringify(articles, null, 1));
      return checkedArticles(articles, pages);
    }
  }

  // The book the issue was imported as before it was imported by its
  // articles (its id the issue's name, in a journal issue), deleted with its
  // pages and citations. Only that book: deleting a book deletes its queue
  // row too.
  private async removeWholeIssue(name: string) {
    const book = await this.db.selectFrom('books').select(['id', 'journal_issue_id'])
      .where('id', '=', name).executeTakeFirst();

    if (book && book.journal_issue_id !== null) {
      await BookActions.deleteBook(this.db, name);
      console.log(`  removed ${ name }, the issue imported before as one book`);
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

// Whether an issue is imported by its articles (see articleBookId); one
// imported as a single book, as issues were before, is to be again
const isImported = (imported: Set<string>, name: string): boolean => (
  [...imported].some((id) => isArticleOf(id, name))
);

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
