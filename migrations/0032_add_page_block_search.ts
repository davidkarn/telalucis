import { Kysely, sql } from 'kysely';

// Searching the blocks of text by their words (see model/block_search.ts): each
// body text, heading and footnote block's English tsvector, of its html without
// its footnote markers (<sup>) and tags, as core/passages.ts's blockText reads
// it. Null for page headers and footers, which the index leaves out.
export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`
    alter table page_blocks add column search tsvector generated always as (
      case when label in ('Text', 'SectionHeader', 'Footnote') then to_tsvector('english'::regconfig,
        regexp_replace(regexp_replace(regexp_replace(html,
          '<sup>.*?</sup>', ' ', 'gi'), '<br\\s*/?>', ' ', 'gi'), '<[^>]+>', ' ', 'g'))
      end
    ) stored
  `.execute(db);

  await sql`create index page_blocks_search_idx on page_blocks using gin (search)
    where search is not null`.execute(db);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table page_blocks drop column search`.execute(db);
}
