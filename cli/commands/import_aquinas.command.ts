// Import the English works of Aquinas in ../AquinasOperaOmnia (the Summa
// Theologiae and the Catena Aurea excepted; see core/aquinas_works.ts): a book
// each, its pages, each page cited by its divisions, and the citations in them,
// found by pattern (core/aquinas_html.ts). The works that name their sources in
// prose (llm in core/aquinas_works.ts) also have each page read by an LLM for
// them; its answers are kept in output/aquinas_llm/<book id>.json, so a page is
// only sent once. Then each book's citations are pointed at the books in the
// collection they cite, and the citations of it made before at it (see
// lib/citation_relinking.ts). Re-running replaces each work's pages.
//   npm run cli -- import-aquinas [--only <ids>] [--dir <folder>]
//     [--no-llm] [--dry-run]
// Needs OPENROUTER_KEY for the LLM pass (without it the pass is skipped, with a
// warning).
import { Inject } from '@nestjs/common';
import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import path from 'node:path';
import { setTimeout } from 'node:timers/promises';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { executeActions } from '../../actions/app_actions.ts';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import {
  aquinasPages, foundOnPage, pageProse, PROSE_CITATIONS_FORMAT, proseCitationsPrompt,
  withProseCitations, type AquinasPage, type AquinasWork, type ProseCitations,
} from '../../core/aquinas_html.ts';
import { AQUINAS_WORKS } from '../../core/aquinas_works.ts';
import { importAquinasWorkActions } from '../../core/aquinas_import.ts';
import { matchCitedBooks } from '../../core/cited_books.ts';
import { linkUnlinkedCitations, relinkSummary } from '../../lib/citation_relinking.ts';
import { makeOpenRouterRequest, parseJsonResponse } from '../../lib/open_router.ts';
import { BookQueries } from '../../model/books.ts';
import { findCitationsInBook, linkCitations } from '../../model/incoming_citations.ts';
import { QueuedBookImportsQueries } from '../../model/queued_book_imports.ts';

// OpenRouter allows 20 requests a minute
const REQUEST_INTERVAL_MS = 3200;

const DEFAULT_DIR = '../AquinasOperaOmnia';

// The model's answers, by work, then by a hash of the page's text
const CACHE_DIR = 'output/aquinas_llm';

type Options = { only?: string[], dir?: string, llm?: boolean, dryRun?: boolean };

type Cache = Record<string, ProseCitations>;

@Command({
  name:        'import-aquinas',
  description: 'Import the English works of Aquinas from the Aquinas Opera Omnia mirror, with their citations',
})
export class ImportAquinasCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--only <ids>', description: 'import only these works (book ids, comma-separated)' })
  parseOnly(value: string): string[] {
    const ids     = value.split(',').map((id) => id.trim()).filter((id) => id.length > 0);
    const unknown = ids.filter((id) => !AQUINAS_WORKS.some((w) => w.id === id));

    if (unknown.length > 0) {
      throw new Error(`no such works: ${ unknown.join(', ') } (see core/aquinas_works.ts)`);
    }
    else {
      return ids;
    }
  }

  @Option({ flags: '--dir <folder>', description: `the Opera Omnia folder (${ DEFAULT_DIR })` })
  parseDir(value: string): string {
    return value;
  }

  @Option({ flags: '--no-llm', description: 'find citations by pattern only' })
  parseNoLlm(): boolean {
    return false;
  }

  @Option({ flags: '--dry-run', description: 'read and report, without saving' })
  parseDryRun(): boolean {
    return true;
  }

  async run(_params: string[], options: Options = {}): Promise<void> {
    const dir   = options.dir ?? DEFAULT_DIR;
    const works = AQUINAS_WORKS.filter((w) => !options.only || options.only.includes(w.id));
    const llm   = options.llm !== false && Boolean(process.env.OPENROUTER_KEY);

    if (options.llm !== false && !process.env.OPENROUTER_KEY) {
      console.warn('OPENROUTER_KEY is not set: citations are found by pattern only');
    }

    for (const work of works) {
      const files = await Promise.all(work.files.map(async(file) => ({
        file, html: await fs.readFile(path.join(dir, file.path), 'utf8'),
      })));
      const found = aquinasPages(work, files);
      const pages = llm && work.llm ? await this.readProse(work, found) : found;

      this.report(work, pages);

      if (!options.dryRun) {
        const results = await executeActions(this.db, importAquinasWorkActions(work, pages));
        if (results['saveBook']?.success) {
          console.log(`  linked ${ await this.linkCitations(work.id) } citations to books in the collection`);
          console.log(`  ${ relinkSummary(await linkUnlinkedCitations(this.db, { bookIds: [work.id] })) }`);
        }
      }
    }

    console.log(options.dryRun ? 'dry run: nothing saved' : `imported ${ works.length } works`);
  }

  // The pages with the citations the model finds in their prose, each page
  // asked once (its answer kept in the cache, saved after each page so an
  // interrupted run loses nothing). Pages whose request fails keep only what
  // was found by pattern.
  private async readProse(work: AquinasWork, pages: AquinasPage[]): Promise<AquinasPage[]> {
    const cacheFile           = path.join(CACHE_DIR, `${ work.id }.json`);
    const cache               = await fs.readFile(cacheFile, 'utf8')
      .then((s) => JSON.parse(s) as Cache, () => ({} as Cache));
    const read: AquinasPage[] = [];
    let failed                = 0;

    await fs.mkdir(CACHE_DIR, { recursive: true });

    for (const page of pages) {
      const prose = pageProse(page);
      const key   = createHash('sha1').update(prose).digest('hex');

      if (prose.length < 200) {
        read.push(page);
      }
      else if (cache[key]) {
        read.push(withProseCitations(page, work.id, cache[key]!));
      }
      else {
        try {
          const response = await makeOpenRouterRequest([
            { role: 'system', content: proseCitationsPrompt(work, foundOnPage(page)) },
            { role: 'user', content: prose },
          ], PROSE_CITATIONS_FORMAT, 'openai/gpt-4o-mini');

          cache[key] = parseJsonResponse<ProseCitations>(response);
          await fs.writeFile(cacheFile, JSON.stringify(cache));
          read.push(withProseCitations(page, work.id, cache[key]!));
          console.log(`  ${ work.id }: ${ page.printedPageNumber }: ${ cache[key]!.citations.length } named in prose`);
          await setTimeout(REQUEST_INTERVAL_MS);
        }
        catch (e) {
          failed++;
          console.error(`  ${ work.id }: ${ page.printedPageNumber } failed: ${ (e as Error).message ?? e }`);
          read.push(page);
        }
      }
    }

    if (failed > 0) {
      console.warn(`  ${ work.id }: ${ failed } pages not read by the LLM; re-run to retry them`);
    }
    return read;
  }

  private report(work: AquinasWork, pages: AquinasPage[]) {
    const blocks    = pages.flatMap((p) => p.blocks);
    const citations = blocks.flatMap((b) => b.citations);

    const divided   = pages.filter((p) => p.citationParts.length > 0).length;
    const footnotes = blocks.filter((b) => b.label === 'Footnote').length;
    const scripture = citations.filter((c) => c.author === 'Bible').length;
    const prose     = citations.filter((c) => c.source.footnoteIdentifier === '').length;

    console.log(`${ work.id }: ${ pages.length } pages (${ divided } cited by their divisions), `
      + `${ footnotes } footnotes, ${ citations.length } citations (${ scripture } of scripture, `
      + `${ prose } named in prose); e.g. `
      + pages.slice(0, 3).map((p) => p.printedPageNumber).join(' | '));
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
