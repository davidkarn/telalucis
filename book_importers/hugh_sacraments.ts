// Import Hugh of Saint Victor's De Sacramentis in Roy J. Deferrari's translation (On the
// Sacraments of the Christian Faith, 1951) from its surya OCR: its pages numbered as Deferrari's
// are, each page's book, part and chapter of the original saved to book_pages_to_citations, the
// citations in parentheses in its text read by core/hugh_sacraments.ts, and those in its few
// footnotes extracted by an LLM. Then its citations are pointed at the books they cite, and the
// citations of it in other books at it ("De Sacram., II, 3, 12": book 2, part 3, chapter 12; see
// lib/citation_relinking.ts).
//
// Usage (from src/):
//   node --env-file=.env --import tsx book_importers/hugh_sacraments.ts
//     [--file <results.json>] [--no-footnotes] [--model <model>] [--dry-run]
//   --file          the surya results (../new_works/results/surya/Hugh of St/results2.json)
//   --no-footnotes  without the footnotes' citations (no LLM requests)
//   --model         the model extracting the footnotes' citations (see lib/footnote_citations.ts)
//   --dry-run       read and report, without extracting the footnotes' citations or saving
// Needs DATABASE_URL, and OPENROUTER_KEY for the footnotes. Re-running replaces the book's pages.
import fs from 'node:fs';
import { Kysely, PostgresDialect } from 'kysely';
import { Pool } from 'pg';
import type { Database } from '../api/database.ts';
import { build_pages } from '../core/book_pages.ts';
import { placeCitations } from '../core/footnote_extraction.ts';
import {
  divisionCheck, HUGH_AUTHOR, HUGH_BOOK_ID, HUGH_TITLE, HUGH_TRANSLATOR, pageCitationRows, pageDivisions,
  withInlineCitations, withPlacedCitations,
} from '../core/hugh_sacraments.ts';
import { linkUnlinkedCitations } from '../lib/citation_relinking.ts';
import { extractFootnoteCitations } from '../lib/footnote_citations.ts';
import { replacePageCitations } from '../model/book_pages_to_citations.ts';
import { BookActions } from '../model/books.ts';
import type { Page, SuryaBook } from '../types.ts';

const argv      = process.argv.slice(2);
const option    = (name: string) => (argv.includes(name) ? argv[argv.indexOf(name) + 1] : undefined);
const FILE      = option('--file') ?? '../new_works/results/surya/Hugh of St/results2.json';
const MODEL     = option('--model');
const FOOTNOTES = !argv.includes('--no-footnotes');
const DRY_RUN   = argv.includes('--dry-run');

const main = async() => {
  const surya: SuryaBook        = JSON.parse(fs.readFileSync(FILE, 'utf8'));
  const suryaPages              = Object.values(surya)[0] ?? [];
  const built                   = build_pages(suryaPages).pages;
  const { divisions, warnings } = pageDivisions(built);
  const rows                    = pageCitationRows(divisions);
  const inline                  = withInlineCitations(HUGH_BOOK_ID, built);
  const count                   = (pages: Page[]) => pages.flatMap((p) => p.blocks.flatMap((b) => b.citations)).length;

  console.log(`${ built.length } pages, ${ divisions.size } in the work, ${ rows.length } cited by book, part `
    + `and chapter; ${ count(inline) } citations in parentheses`);
  for (const w of [...warnings, ...divisionCheck(built, divisions)]) {
    console.log(`  ${ w }`);
  }

  if (DRY_RUN) {
    console.log('dry run: nothing extracted or saved');
    return;
  }
  else if (!process.env.DATABASE_URL) {
    throw new Error('DATABASE_URL is not set');
  }

  const db = new Kysely<Database>({
    dialect: new PostgresDialect({ pool: new Pool({ connectionString: process.env.DATABASE_URL }) }),
  });

  try {
    const footnotes            = FOOTNOTES
      ? await extractFootnoteCitations(db, HUGH_BOOK_ID, suryaPages, {
        skipInsights: true, ...(MODEL ? { model: MODEL } : {}),
      })
      : { citations: [], failedPages: [] };
    const { placed, unplaced } = placeCitations(inline, footnotes.citations);
    const pages                = withPlacedCitations(inline, placed);

    console.log(`${ placed.length } citations in footnotes`
      + (unplaced.length > 0 ? `, ${ unplaced.length } not placed in a footnote block` : '')
      + (footnotes.failedPages.length > 0 ? `, ${ footnotes.failedPages.length } pages failed` : ''));

    await BookActions.saveBook(db, {
      id: HUGH_BOOK_ID, title: HUGH_TITLE, author: HUGH_AUTHOR, url: null, translator: HUGH_TRANSLATOR,
    }, pages);
    await replacePageCitations(db, HUGH_BOOK_ID, rows);

    const report  = await linkUnlinkedCitations(db);
    const located = report.located.reduce((n, l) => n + l.citations, 0);
    const linked  = report.linked.reduce((n, l) => n + l.citations, 0);
    console.log(`saved ${ HUGH_BOOK_ID }: ${ count(pages) } citations; linked ${ located + linked } `
      + `unlinked citations in the collection (${ located } with their places read)`);
    for (const l of [...report.located, ...report.linked].slice(0, 15)) {
      console.log(`  ${ String(l.citations).padStart(5) }  ${ l.bookId }`);
    }
  }
  finally {
    await db.destroy();
  }
};

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
