import { Kysely, sql } from 'kysely';

// Each citation's source snippet cached: up to 25 words of the citing book's body text around the
// footnote's marker (see footnoteContext in core/citation_pages.ts), shown in the "Cited by" list.
// Null until worked out, the first time a page the citation cites is shown; '' when the marker
// isn't found. It never goes stale: the citing page's text only changes when its book is saved
// again, which deletes the book's pages and so its citations.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`alter table citations add column source_snippet text`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table citations drop column source_snippet`.execute(db);
}
