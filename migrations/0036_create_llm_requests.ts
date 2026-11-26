import { Kysely, sql } from 'kysely';

// Every request the web app makes of an LLM (see model/llm_requests.ts): what
// asked for it (source: 'chat', 'page_insights'), the reader it was for when
// signed in, what it was about (context: the conversation and message, the
// book and page), the model, the messages sent and the other parameters, what
// came back (response: the answer's text, as far as it got), how it ended
// (status: ok, error, stopped), the tokens counted and the cost in US dollars
// as OpenRouter reports them (null when it doesn't), OpenRouter's id for it
// (its generation id, to look up on openrouter.ai), and how long it took.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create table llm_requests (
    id bigserial primary key,
    source text not null,
    user_id bigint references users (id) on delete set null,
    context jsonb not null default '{}',
    model text not null,
    messages jsonb not null,
    parameters jsonb not null default '{}',
    response text,
    status text not null check (status in ('ok', 'error', 'stopped')),
    error text,
    prompt_tokens integer,
    completion_tokens integer,
    reasoning_tokens integer,
    cached_tokens integer,
    cost numeric(14, 8),
    generation_id text,
    duration_ms integer not null,
    created_at timestamptz not null default now()
  )`.execute(db);

  await sql`create index llm_requests_created_idx on llm_requests (created_at)`.execute(db);
  await sql`create index llm_requests_user_idx on llm_requests (user_id, created_at) where user_id is not null`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`drop table llm_requests`.execute(db);
}
