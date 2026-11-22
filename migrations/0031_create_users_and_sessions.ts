import { Kysely, sql } from 'kysely';

// Readers signed in with Google (users), one row per Google account (google_id,
// its "sub"), its email, name and picture refreshed at each sign-in; and the
// express-session store's sessions (user_sessions, the table connect-pg-simple
// expects: sess holds the passport user id and the OAuth state while signing
// in). An expired session is pruned by the store. updated_at is kept by a
// trigger.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create table users (
    id bigserial primary key,
    google_id text not null unique,
    email text not null,
    email_verified boolean not null,
    name text not null,
    picture_url text,
    last_signed_in_at timestamptz not null default now(),
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
  )`.execute(db);

  await sql`create function users_set_updated_at() returns trigger as $$
    begin
      new.updated_at = now();
      return new;
    end
    $$ language plpgsql`.execute(db);

  await sql`create trigger users_updated_at
    before update on users
    for each row execute function users_set_updated_at()`.execute(db);

  await sql`create table user_sessions (
    sid varchar not null primary key,
    sess json not null,
    expire timestamp(6) not null
  )`.execute(db);

  await sql`create index user_sessions_expire_idx on user_sessions (expire)`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`drop table user_sessions`.execute(db);
  await sql`drop table users`.execute(db);
  await sql`drop function users_set_updated_at()`.execute(db);
}
