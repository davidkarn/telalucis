// Link the citations that cite a book in the collection but aren't pointed at
// it: those made before the book was imported, and those whose author the
// extraction left out (filled in from the title where it can be). Citations of
// the Summa and of works cited by Latin aliases (the Fathers, Aristotle,
// Boethius, the Contra Gentiles) get their places read again; the others are
// matched by author and title. See core/citation_relinking.ts. The importers
// do this for the books they import; this does it for every book.
//   npm run cli -- relink-citations [bookIds...] [--dry-run]
// bookIds: link only the citations of these books (every book when none are
// given).
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { linkUnlinkedCitations } from '../../lib/citation_relinking.ts';

// How many books' counts are listed
const LISTED = 30;

@Command({
  name:        'relink-citations',
  arguments:   '[bookIds...]',
  description: 'Link the citations of books in the collection that are not linked to them',
})
export class RelinkCitationsCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--dry-run', description: 'count what would be linked, without saving' })
  parseDryRun(): boolean {
    return true;
  }

  async run(bookIds: string[], options: { dryRun?: boolean } = {}): Promise<void> {
    const report = await linkUnlinkedCitations(this.db, {
      ...(bookIds.length > 0 ? { bookIds } : {}),
      dryRun: options.dryRun === true,
    });
    const total  = (rows: { citations: number }[]) => rows.reduce((n, r) => n + r.citations, 0);

    console.log(`authors filled in or placeholders cleared: ${ report.names }`);
    console.log(`linked with their places read again: ${ total(report.located) }`);
    for (const { bookId, citations } of report.located.slice(0, LISTED)) {
      console.log(`  ${ String(citations).padStart(6) }  ${ bookId }`);
    }
    console.log(`linked by author and title: ${ total(report.linked) }, to ${ report.linked.length } books`);
    for (const { bookId, citations } of report.linked.slice(0, LISTED)) {
      console.log(`  ${ String(citations).padStart(6) }  ${ bookId }`);
    }
    console.log(`left unlinked, their places not read or on no page: ${ report.unplaced }`);
    console.log(options.dryRun ? 'dry run: nothing saved' : 'saved');
  }
}
