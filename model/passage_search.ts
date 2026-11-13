// The searches of the passages (see core/passage_search.ts): those nearest a question in meaning,
// those with its words, and those citing a page. Only passages of books shown in the library's
// searches (not withheld for copyright) are found.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { Candidate } from '../core/passage_search.ts';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';
import { citationsOfPage } from './page_insights.ts';

type CandidateRow = {
  id: string,
  book_id: string,
  title: string,
  author: string,
  author_ids: string[] | null,
  page_number: number,
  end_page_number: number,
  printed_page_number: string,
  text: string,
};

const toCandidate = (r: CandidateRow): Candidate => ({
  passageId:         r.id,
  bookId:            r.book_id,
  title:             r.title,
  author:            r.author,
  authorIds:         r.author_ids ?? [],
  pageNumber:        r.page_number,
  endPageNumber:     r.end_page_number,
  printedPageNumber: r.printed_page_number,
  text:              r.text,
});

// The columns of a candidate, of passages `p`
const CANDIDATE_COLUMNS = sql`
  p.id::text as id, p.book_id, b.title, b.author,
  (select array_agg(ba.author_id::text order by ba.position) from book_authors ba where ba.book_id = p.book_id) as author_ids,
  p.page_number, p.end_page_number, pg.printed_page_number, p.text`;

const CANDIDATE_JOINS = sql`
  join books b on b.id = p.book_id
  join pages pg on pg.book_id = p.book_id and pg.page_number = p.page_number`;

// How many neighbours the vector index looks through: more finds more of the true nearest
const EF_SEARCH = 200;

// The `limit` passages nearest the embedding in meaning (cosine distance), the nearest first. The
// index is scanned on (pgvector's iterative scan) past passages of withheld books, so they don't
// leave fewer than `limit`.
const findNearest = (db: Kysely<Database>, embedding: number[], limit: number) => (
  db.transaction().execute(async(trx) => {
    await sql`set local hnsw.ef_search = ${ sql.lit(EF_SEARCH) }`.execute(trx);
    await sql`set local hnsw.iterative_scan = relaxed_order`.execute(trx);

    const vector = `[${ embedding.join(',') }]`;
    const rows   = await sql<CandidateRow & { distance: number }>`
      with nearest as materialized (
        select p.id, p.embedding <=> ${ vector }::halfvec as distance
        from passages p
        where p.embedding is not null and ${ CopyrightStatusCheckSql.isShownInSearches('p.book_id') }
        order by p.embedding <=> ${ vector }::halfvec
        limit ${ limit }
      )
      select ${ CANDIDATE_COLUMNS }, n.distance
      from nearest n join passages p on p.id = n.id ${ CANDIDATE_JOINS }
      order by n.distance
    `.execute(trx);

    return rows.rows.map(toCandidate);
  })
);

// The `limit` passages with the question's words (English stemming, common words left out), the
// most first (ts_rank_cd, which counts how close together they are, scaled for the passage's
// length): those with all of them, and while they're very few (FEW_FOUND), then those with all
// but one, then all but two. Never those with any one of them: a word such as "God" is in half the
// passages, and ranking them takes seconds.
const FEW_FOUND = 20;

// A text search query of these lexemes, all of them (&), as a tsquery's text
const allOf = (lexemes: string[]) => lexemes.map((l) => `'${ l.replace(/'/g, "''") }'`).join(' & ');

// A text search query of all but `left` of these lexemes: any such set of them (|)
const allBut = (lexemes: string[], left: number): string => {
  const sets = (from: number, kept: string[], toLeave: number): string[][] => (
    from === lexemes.length
      ? (toLeave === 0 ? [kept] : [])
      : [
          ...sets(from + 1, [...kept, lexemes[from]!], toLeave),
          ...(toLeave > 0 ? sets(from + 1, kept, toLeave - 1) : []),
        ]
  );
  return sets(0, [], left).map((s) => `(${ allOf(s) })`).join(' | ');
};

const findWithWords = async(db: Kysely<Database>, question: string, limit: number) => {
  const lexemes = (await sql<{ lexeme: string }>`
    select unnest(tsvector_to_array(to_tsvector('english', ${ question }))) as lexeme
  `.execute(db)).rows.map((r) => r.lexeme);

  const search = async(query: string, excluded: string[]) => (query.length === 0 ? [] : (await sql<CandidateRow>`
    select ${ CANDIDATE_COLUMNS }
    from passages p ${ CANDIDATE_JOINS }
    where p.search @@ ${ query }::tsquery and ${ CopyrightStatusCheckSql.isShownInSearches('p.book_id') }
      ${ excluded.length > 0 ? sql`and p.id not in (${ sql.join(excluded.map((id) => sql`${ id }::bigint`)) })` : sql`` }
    order by ts_rank_cd(p.search, ${ query }::tsquery, 32) desc, p.id
    limit ${ limit - excluded.length }
  `.execute(db)).rows);

  // all the words, then all but one, then all but two, while few are found (keeping at least two
  // words of the question's several)
  let found: CandidateRow[] = [];
  for (let left = 0; left <= 2 && found.length < FEW_FOUND && lexemes.length > 0 && (left === 0 || lexemes.length - left >= 2); left++) {
    found = [...found, ...await search(allBut(lexemes, left), found.map((r) => r.id))];
  }

  return found.map(toCandidate);
};

// The passages of other books citing the page (their footnotes' citations of it; see
// citationsOfPage), at most `limit`, each with the page it cites
const findCiting = async(
  db: Kysely<Database>, page: { bookId: string, pageNumber: number, printedPageNumber: string }, limit: number,
): Promise<Candidate[]> => {
  const printed   = /^\d+$/.test(page.printedPageNumber) ? Number(page.printedPageNumber) : null;
  const citations = await citationsOfPage(db, page.bookId, { pageNumber: page.pageNumber, printedNumber: printed })
    .select(['citations.source_book_id', 'citations.source_footnote_page'])
    .where('citations.source_book_id', '!=', page.bookId)
    .execute();

  // a citing book's footnotes on one page are one passage
  const places = [...new Map(citations.map((c) => [`${ c.source_book_id }\u0000${ c.source_footnote_page }`, c])).values()]
    .slice(0, limit);

  if (places.length === 0) {
    return [];
  }
  else {
    const rows = await sql<CandidateRow>`
      select distinct on (p.id) ${ CANDIDATE_COLUMNS }
      from passages p ${ CANDIDATE_JOINS }
      join (values ${ sql.join(places.map((c) => sql`(${ c.source_book_id }::text, ${ c.source_footnote_page }::integer)`)) })
        as cited(book_id, page_number)
        on cited.book_id = p.book_id and cited.page_number between p.page_number and p.end_page_number
      order by p.id
    `.execute(db);

    return rows.rows.map((r) => ({ ...toCandidate(r), cites: { bookId: page.bookId, pageNumber: page.pageNumber } }));
  }
};

// The pages of a book cited by these parts, outermost first (a book_pages_to_citations row having
// them as its first parts), in order
const findPagesByParts = async(db: Kysely<Database>, bookId: string, parts: [string, number][]) => {
  const rows = await db.selectFrom('book_pages_to_citations')
    .select('book_pages_to_citations.page_number')
    .distinct()
    .where('book_pages_to_citations.book_id', '=', bookId)
    .where((eb) => eb.and(parts.flatMap(([type, value], i) => [
      eb(sql.ref(`book_pages_to_citations.citation_part_${ i + 1 }_type`), '=', type),
      eb(sql.ref(`book_pages_to_citations.citation_part_${ i + 1 }_value`), '=', value),
    ])))
    .orderBy('book_pages_to_citations.page_number')
    .execute();

  return rows.map((r) => r.page_number);
};

export const PassageSearchQueries = { findNearest, findWithWords, findCiting, findPagesByParts };
