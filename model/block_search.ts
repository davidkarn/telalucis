// The searches of the blocks of text (see core/block_search.ts and
// migrations/0032_add_page_block_search.ts): those with a search term's words,
// the blocks on the pages of passages found by meaning, and what's shown of a
// block found: its page, book, authors and journal issue. Only blocks of books
// shown in the library's searches (not withheld for copyright) are found.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { BlockDetails, PassageBlock } from '../core/block_search.ts';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

// SQL: the search term as a text search query, as a search box takes it ("natural law" both
// words, "\"natural law\"" the phrase, "-grace" without it, "or" either)
const termQuery = (term: string) => sql`websearch_to_tsquery('english', ${ term })`;

// SQL: a query of any one of the term's words, for scoring a block by how many it has
const anyWordQuery = (term: string) => sql`
  nullif(replace(plainto_tsquery('english', ${ term })::text, ' & ', ' | '), '')::tsquery`;

const notWithheld = (bookIdColumn: string) => (
  sql`${ sql.ref(bookIdColumn) } not in ${ CopyrightStatusCheckSql.withheldBookIds() }`
);

// The `limit` blocks with the term's words, the best first (ts_rank_cd, which counts how close
// together they are, scaled for the block's length); their ids
const findWithWords = async(db: Kysely<Database>, term: string, limit: number): Promise<string[]> => (
  (await sql<{ id: string }>`
    select pb.id::text as id
    from page_blocks pb
    where pb.search @@ ${ termQuery(term) } and ${ notWithheld('pb.book_id') }
    order by ts_rank_cd(pb.search, ${ termQuery(term) }, 32) desc, pb.id
    limit ${ limit }
  `.execute(db)).rows.map((r) => r.id)
);

export type PassagePages = { passageId: string, bookId: string, pageNumber: number, endPageNumber: number };

// The body text and heading blocks on each passage's pages, by passage, each scored by how many of
// the term's words it has (see passageBlock)
const findPassageBlocks = async(
  db: Kysely<Database>, passages: PassagePages[], term: string,
): Promise<Map<string, PassageBlock[]>> => {
  if (passages.length === 0) {
    return new Map();
  }
  else {
    const rows = await sql<PassageBlock & { passage_id: string, page_number: number, word_score: number }>`
      select p.passage_id, pb.id::text as "blockId", pb.page_number, pb.position, pb.html,
        coalesce(ts_rank_cd(pb.search, ${ anyWordQuery(term) }, 32), 0)::float8 as word_score
      from (values ${ sql.join(passages.map((p) => sql`(${ p.passageId }::text, ${ p.bookId }::text, ${ p.pageNumber }::integer, ${ p.endPageNumber }::integer)`)) })
        as p(passage_id, book_id, page_number, end_page_number)
      join page_blocks pb on pb.book_id = p.book_id
        and pb.page_number between p.page_number and p.end_page_number
        and pb.label in ('Text', 'SectionHeader')
    `.execute(db);

    return rows.rows.reduce((byPassage, r) => byPassage.set(r.passage_id, [
      ...byPassage.get(r.passage_id) ?? [],
      { blockId: r.blockId, pageNumber: r.page_number, position: r.position, html: r.html, wordScore: r.word_score },
    ]), new Map<string, PassageBlock[]>());
  }
};

const HEADLINE_OPTIONS = 'MaxFragments=2, MaxWords=35, MinWords=15, StartSel=<mark>, StopSel=</mark>';

// What's shown of these blocks, in no order
const findBlockDetails = async(db: Kysely<Database>, ids: string[], term: string): Promise<BlockDetails[]> => (
  ids.length === 0 ? [] : (await sql<BlockDetails>`
    select pb.id::text as id, pb.label, pb.html,
      ts_headline('english',
        regexp_replace(regexp_replace(regexp_replace(pb.html,
          '<sup>.*?</sup>', ' ', 'gi'), '<br\\s*/?>', ' ', 'gi'), '<[^>]+>', ' ', 'g'),
        coalesce(${ anyWordQuery(term) }, ''::tsquery), ${ HEADLINE_OPTIONS }) as snippet,
      pb.book_id, pb.page_number, pg.printed_page_number, b.title, b.author,
      (select json_agg(json_build_object('id', a.id::text, 'name', a.name) order by ba.position)
        from book_authors ba join authors a on a.id = ba.author_id
        where ba.book_id = pb.book_id) as authors,
      i.id::text as journal_issue_id, j.id::text as journal_id, j.name as journal_name,
      to_char(i.start_date, 'YYYY-MM-DD') as issue_start_date,
      to_char(i.end_date, 'YYYY-MM-DD') as issue_end_date
    from page_blocks pb
    join pages pg on pg.book_id = pb.book_id and pg.page_number = pb.page_number
    join books b on b.id = pb.book_id
    left join journal_issues i on i.id = b.journal_issue_id
    left join journals j on j.id = i.journal_id
    where pb.id in (${ sql.join(ids.map((id) => sql`${ id }::bigint`)) })
  `.execute(db)).rows
);

export const BlockSearchQueries = { findWithWords, findPassageBlocks, findBlockDetails };
