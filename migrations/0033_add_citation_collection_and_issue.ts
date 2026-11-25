import { Kysely, sql } from 'kysely';

// The journal or other long-running collection a cited work is in ("Acta Apostolicae Sedis",
// "Theological Studies"), and the issue of it cited ("Vol. XXXVI"), as the footnote gives them
// (see footnotesPrompt in core/footnote_extraction.ts). Null for none, and for the citations
// extracted before they were asked for.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`alter table citations add column collection text, add column issue text`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table citations drop column collection, drop column issue`.execute(db);
}
