import { Kysely, sql } from 'kysely';

// Each book's body text in passages of a few paragraphs (see core/passages.ts), for searching the
// collection: by meaning, with each passage's embedding (pgvector), and by words, with its text's
// tsvector. A passage is on the pages page_number to end_page_number; it goes with its first page,
// so a book saved again (its pages replaced) loses its passages until they're cut again
// (index-passages). embedding: null until worked out; embedding_model: the model it's from, as
// embeddings of different models can't be compared. Needs the vector extension (Postgres built
// from docker/postgres.Dockerfile). halfvec: half the space of vector, as good for search; 1024
// numbers, bge-m3's (EMBEDDING_DIMENSIONS in core/passages.ts).

export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create extension if not exists vector`.execute(db);

  await sql`
    create table passages (
      id              bigserial primary key,
      book_id         text not null,
      page_number     integer not null,
      end_page_number integer not null,
      position        integer not null,
      text            text not null,
      word_count      integer not null,
      search          tsvector generated always as (to_tsvector('english', text)) stored,
      embedding       halfvec(1024),
      embedding_model text,
      created_at      timestamptz not null default now(),
      foreign key (book_id, page_number) references pages (book_id, page_number) on delete cascade,
      unique (book_id, position)
    )
  `.execute(db);

  await sql`create index passages_search_idx on passages using gin (search)`.execute(db);
  await sql`create index passages_embedding_idx on passages using hnsw (embedding halfvec_cosine_ops)`.execute(db);
  // those still to embed
  await sql`create index passages_unembedded_idx on passages (id) where embedding is null`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`drop table passages`.execute(db);
}
