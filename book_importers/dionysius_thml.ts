// Import the works of Dionysius the Areopagite in Parker's translation, from
// CCEL's ThML edition (../thml/dionysius.xml): a book per work, a page per
// section, each page's chapter (or letter), part and section saved to
// book_pages_to_citations (see core/dionysius_thml.ts), with the citations in
// its notes. Then the citations of the works in other books are pointed at
// them (see lib/citation_relinking.ts).
//
// Usage (from src/):
//   npx tsx book_importers/dionysius_thml.ts [--file ../thml/dionysius.xml] [--dry-run]
//   --dry-run  read and report, without saving
// DATABASE_URL comes from the environment or .env. Re-running replaces each work's pages.
import 'dotenv/config';
import fs from 'node:fs';
import { Kysely, PostgresDialect } from 'kysely';
import { Pool } from 'pg';
import type { Database } from '../api/database.ts';
import { dionysianWorks, DIONYSIUS_TRANSLATOR } from '../core/dionysius_thml.ts';
import { linkUnlinkedCitations } from '../lib/citation_relinking.ts';
import { replacePageCitations } from '../model/book_pages_to_citations.ts';
import { BookActions } from '../model/books.ts';

const argv    = process.argv.slice(2);
const FILE    = argv.includes('--file') ? argv[argv.indexOf('--file') + 1]! : '../thml/dionysius.xml';
const DRY_RUN = argv.includes('--dry-run');

const main = async() => {
  const works = dionysianWorks(fs.readFileSync(FILE, 'utf8'));
  const db    = DRY_RUN ? null : (() => {
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL is not set');
    }
    return new Kysely<Database>({
      dialect: new PostgresDialect({ pool: new Pool({ connectionString: process.env.DATABASE_URL }) }),
    });
  })();

  try {
    for (const work of works) {
      const cited     = work.pages.filter((p) => p.citationParts.length > 0);
      const blocks    = work.pages.flatMap((p) => p.blocks);
      const footnotes = blocks.filter((b) => b.label === 'Footnote').length;
      const citations = blocks.flatMap((b) => b.citations).length;

      console.log(`${ work.id }: ${ work.pages.length } pages, ${ cited.length } cited by their divisions, `
        + `${ footnotes } footnotes, ${ citations } citations`);

      if (db) {
        await BookActions.saveBook(db, {
          id: work.id, title: work.title, author: work.author, url: work.url, translator: DIONYSIUS_TRANSLATOR,
        }, work.pages);
        await replacePageCitations(db, work.id, cited.map((p) => ({ pageNumber: p.pageNumber, parts: p.citationParts })));
      }
    }

    if (db) {
      const report  = await linkUnlinkedCitations(db, { bookIds: works.map((w) => w.id) });
      const located = report.located.reduce((n, l) => n + l.citations, 0);
      const linked  = report.linked.reduce((n, l) => n + l.citations, 0);
      console.log(`linked ${ located } citations of the works with their places, ${ linked } by title alone, `
        + `${ report.unplaced } left unlinked (their places not read)`);
      for (const l of [...report.located, ...report.linked]) {
        console.log(`  ${ String(l.citations).padStart(5) }  ${ l.bookId }`);
      }
    }
    console.log(DRY_RUN ? 'dry run: nothing saved' : `saved ${ works.length } works`);
  }
  finally {
    await db?.destroy();
  }
};

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
