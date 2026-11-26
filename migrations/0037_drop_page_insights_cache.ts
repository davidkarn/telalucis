import { Kysely, sql } from 'kysely';

// Page insights are gone (an OpenRouter request anyone could set off, for any
// page), and with them their cache. Down makes it again, empty, as
// migrations/0006_create_page_insights_cache.ts did.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`drop table page_insights_cache`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`create table page_insights_cache (
    book_id text not null,
    page_number integer not null,
    insights json not null,
    created_at timestamptz not null default now(),
    constraint page_insights_cache_pkey primary key (book_id, page_number),
    constraint page_insights_cache_page_fkey foreign key (book_id, page_number)
      references pages (book_id, page_number) on delete cascade
  )`.execute(db);
}
