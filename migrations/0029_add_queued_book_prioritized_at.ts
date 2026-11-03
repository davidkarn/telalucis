import { Kysely, sql } from 'kysely';

// When a queued book was moved to the top of the queue from the admin panel: the queue takes the
// books moved to the top first, the one moved last first, then the rest by id (see inQueueOrder in
// model/queued_book_imports.ts). Null for those never moved.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`alter table queued_book_imports add column prioritized_at timestamptz`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table queued_book_imports drop column prioritized_at`.execute(db);
}
