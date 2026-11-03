// Import Anselm's works from The Complete Works of St. Anselm, OCR'd with surya: a book per work,
// a page per chapter, each page's chapter saved to book_pages_to_citations, with the Bible
// citations in parentheses in its text (see core/anselm_works.ts). The e-book's introductions,
// notes, appended Bible chapters and Gaunilon's In Behalf of the Fool are left out. Then the
// books' citations are pointed at the books they cite, and the citations of the works in other
// books at them ("Monolog. 7", "Cur Deus Homo, II, 6"; see lib/citation_relinking.ts).
//
// Usage (from src/):
//   node --env-file=.env --import tsx book_importers/anselm_works.ts [--file <results.json>] [--dry-run]
//   --file     the surya results (../new_works/results/surya/The Complete Works of St/results.json)
//   --dry-run  read and report, without saving
// Needs DATABASE_URL. Re-running replaces each work's pages.
import fs from 'node:fs';
import { Kysely, PostgresDialect } from 'kysely';
import { Pool } from 'pg';
import type { Database } from '../api/database.ts';
import { ANSELM, ANSELM_WORKS, anselmWorks, withBibleCitations } from '../core/anselm_works.ts';
import { build_pages } from '../core/book_pages.ts';
import { linkUnlinkedCitations } from '../lib/citation_relinking.ts';
import { replacePageCitations } from '../model/book_pages_to_citations.ts';
import { BookActions } from '../model/books.ts';
import type { SuryaBook } from '../types.ts';

const argv    = process.argv.slice(2);
const FILE    = argv.includes('--file')
  ? argv[argv.indexOf('--file') + 1]!
  : '../new_works/results/surya/The Complete Works of St/results.json';
const DRY_RUN = argv.includes('--dry-run');

const main = async() => {
  const surya: SuryaBook = JSON.parse(fs.readFileSync(FILE, 'utf8'));
  const works            = [...anselmWorks(build_pages(Object.values(surya)[0] ?? []).pages)]
    .map(([key, pages]) => ({ ...ANSELM_WORKS[key], pages: withBibleCitations(ANSELM_WORKS[key].id, pages) }));

  for (const work of works) {
    const citations = work.pages.flatMap((p) => p.blocks.flatMap((b) => b.citations)).length;
    console.log(`${ work.id }: ${ work.pages.length } pages (${ work.pages[0]?.printedPageNumber } to `
      + `${ work.pages.at(-1)?.printedPageNumber }), ${ citations } Bible citations`);
  }

  if (DRY_RUN) {
    console.log('dry run: nothing saved');
  }
  else if (!process.env.DATABASE_URL) {
    throw new Error('DATABASE_URL is not set');
  }
  else {
    const db = new Kysely<Database>({
      dialect: new PostgresDialect({ pool: new Pool({ connectionString: process.env.DATABASE_URL }) }),
    });

    try {
      for (const work of works) {
        await BookActions.saveBook(db, {
          id: work.id, title: work.title, author: ANSELM, url: null, translator: work.translator,
        }, work.pages);
        await replacePageCitations(db, work.id, work.pages.map((p) => ({ pageNumber: p.pageNumber, parts: p.citationParts })));
      }

      const report  = await linkUnlinkedCitations(db);
      const located = report.located.reduce((n, l) => n + l.citations, 0);
      const linked  = report.linked.reduce((n, l) => n + l.citations, 0);
      console.log(`saved ${ works.length } works; linked ${ located + linked } unlinked citations in the `
        + `collection (${ located } with their places read)`);
      for (const l of [...report.located, ...report.linked]) {
        console.log(`  ${ String(l.citations).padStart(5) }  ${ l.bookId }`);
      }
    }
    finally {
      await db.destroy();
    }
  }
};

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
