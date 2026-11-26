import { Kysely, sql } from 'kysely';

// Readers' chats with an LLM about the collection (see core/chats.ts):
// conversations, each a signed-in user's, and their messages. Each answer
// keeps the passages it was given (passages: their numbers, books, pages and
// text as they were then), so its citations still show after the passages are
// cut again. A conversation the reader deletes is only marked deleted
// (deleted_at), so its messages still count toward the day's limits.
// conversation_messages.error: why an answer stopped short, with what came of
// it in content. updated_at is kept by a trigger.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create table conversations (
    id bigserial primary key,
    user_id bigint not null references users (id) on delete cascade,
    title text not null,
    deleted_at timestamptz,
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
  )`.execute(db);

  await sql`create index conversations_user_idx on conversations (user_id, updated_at desc)
    where deleted_at is null`.execute(db);

  await sql`create function conversations_set_updated_at() returns trigger as $$
    begin
      new.updated_at = now();
      return new;
    end
    $$ language plpgsql`.execute(db);

  await sql`create trigger conversations_updated_at
    before update on conversations
    for each row execute function conversations_set_updated_at()`.execute(db);

  await sql`create table conversation_messages (
    id bigserial primary key,
    conversation_id bigint not null references conversations (id) on delete cascade,
    role text not null check (role in ('user', 'assistant')),
    content text not null,
    passages jsonb not null default '[]',
    model text,
    prompt_tokens integer,
    completion_tokens integer,
    error text,
    created_at timestamptz not null default now()
  )`.execute(db);

  await sql`create index conversation_messages_conversation_idx
    on conversation_messages (conversation_id, id)`.execute(db);
  // the messages sent today, counted against the limits
  await sql`create index conversation_messages_sent_idx
    on conversation_messages (created_at) where role = 'user'`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`drop table conversation_messages`.execute(db);
  await sql`drop table conversations`.execute(db);
  await sql`drop function conversations_set_updated_at`.execute(db);
}
