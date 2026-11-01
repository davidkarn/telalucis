import { Kysely, sql } from 'kysely';

// Journals, and their issues (journal_issues), each covering a period from
// start_date to end_date (a quarterly's issue: three months; a volume: a
// year). A journal's articles are books, pointing at the issue they appeared
// in (books.journal_issue_id; null for a book that isn't an article), and so
// at its journal. An issue is deleted with its journal; an article outlives
// its issue, losing the link. updated_at is kept by triggers.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create table journals (
    id bigserial primary key,
    name text not null,
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
  )`.execute(db);

  await sql`create table journal_issues (
    id bigserial primary key,
    journal_id bigint not null references journals(id) on delete cascade,
    start_date date not null,
    end_date date not null,
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now(),
    constraint journal_issues_period_check check (end_date >= start_date)
  )`.execute(db);

  await sql`create index journal_issues_journal_id_idx on journal_issues (journal_id)`.execute(db);

  await sql`alter table books
    add column journal_issue_id bigint references journal_issues(id) on delete set null`.execute(db);

  await sql`create index books_journal_issue_id_idx on books (journal_issue_id)`.execute(db);

  for (const table of ['journals', 'journal_issues']) {
    await sql`create function ${ sql.raw(table) }_set_updated_at() returns trigger as $$
      begin
        new.updated_at = now();
        return new;
      end
      $$ language plpgsql`.execute(db);

    await sql`create trigger ${ sql.raw(table) }_updated_at
      before update on ${ sql.raw(table) }
      for each row execute function ${ sql.raw(table) }_set_updated_at()`.execute(db);
  }
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table books drop column journal_issue_id`.execute(db);
  await sql`drop table journal_issues`.execute(db);
  await sql`drop table journals`.execute(db);
  await sql`drop function journal_issues_set_updated_at()`.execute(db);
  await sql`drop function journals_set_updated_at()`.execute(db);
}
