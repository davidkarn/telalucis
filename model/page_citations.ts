// Database reads for the citations of a book's pages: those in other books
// that cite a page, how many cite each of a book's pages, and the citing
// books' titles and authors.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { AlternateIdsSql } from './alternate_ids.ts';
import { withScopes } from './model_utils.js';
import { CITATION_RANGE, CitationScopes } from './citations.js';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

// SQL: the citation_groups row `grp`'s parts as a table (n, type, value), to join laterally; a
// part that is null is left out by the caller (where type is not null)
const groupPartRows = (grp: string) => sql`(values ${ sql.join(CITATION_RANGE.map((n) => (
  sql`(${ n }, ${ sql.ref(`${ grp }.part${ n }_type`) }, ${ sql.ref(`${ grp }.part${ n }_value`) })`
))) })`;

// Citations in other books of a book's page (pointing at the book or one of its alternate ids, see
// model/alternate_ids.ts): those with a "page" location equal to its printed page number (when that
// is a number), or that cite it by the parts of one of its book_pages_to_citations rows (see
// citesPageByParts). Callers choose the columns.
const citationsOfPage = (
  db: Kysely<Database>, bookId: string, page: { pageNumber: number, printedNumber: number | null }
) => (
  withScopes(db.selectFrom('citations'), [
    CitationScopes.citesBook(bookId),
    CitationScopes.citesPage(bookId, page.printedNumber, page.pageNumber)
  ])
    // not those in withheld books (likely under copyright), whose pages can't be shown
    .where(CopyrightStatusCheckSql.isShownInSearches('citations.source_book_id'))
    .orderBy('citations.source_book_id')
    .orderBy('citations.source_footnote_page')
    .orderBy('citations.id')
);

// SQL: part n of the book_pages_to_citations row `bpc` is null (asks for nothing) or is part n
// of the citation_groups row `grp`
const rowPartMatches = (n: number) => {
  const type  = sql.ref(`bpc.citation_part_${ n }_type`);
  const value = sql.ref(`bpc.citation_part_${ n }_value`);
  return sql`(${ type } is null or ${ type } = ${ sql.ref(`grp.part${ n }_type`) })
    and (${ value } is null or ${ value } = ${ sql.ref(`grp.part${ n }_value`) })`;
};

// How many citations in other books cite each of a book's pages (as citationsOfPage finds them,
// the foreignCitations of GET /books/{bookId}/pages/{pageId}): by the parts of one of the page's
// book_pages_to_citations rows, part by part, or by a "page" part equal to its printed number;
// not those in withheld books (likely under copyright). Pages no citation cites are left out.
// GET .../pageOrder caches it (page_cited_counts_cache).
//
// Found from the book's rows rather than from its citations (the Bible has 98,879): a row with a
// second part finds the citation_groups rows with its first two parts through
// citation_groups_parts_lookup_idx, one without, those with its first part.
const citedCountsByPage = async(
  db: Kysely<Database>, bookId: string
): Promise<Map<number, number>> => {
  const rows = await sql<{ page_number: number, n: string }>`
    with cited_as as (
      select id from ${ AlternateIdsSql.idsCitedAs(bookId) } ids(id)
    ),
    withheld as (
      select book_id from ${ CopyrightStatusCheckSql.withheldBookIds() } w(book_id)
    ),
    by_two_parts as (
      select bpc.page_number, grp.citation_id
      from book_pages_to_citations bpc
      join citation_groups grp
        on grp.part1_type = bpc.citation_part_1_type
        and grp.part1_value = bpc.citation_part_1_value
        and grp.part2_type = bpc.citation_part_2_type
        and grp.part2_value = bpc.citation_part_2_value
      where bpc.book_id = ${ bookId } and bpc.citation_part_2_type is not null
        and ${ sql.join(CITATION_RANGE.slice(2).map(rowPartMatches), sql` and `) }
    ),
    by_one_part as (
      select bpc.page_number, grp.citation_id
      from book_pages_to_citations bpc
      join citation_groups grp
        on grp.part1_type = bpc.citation_part_1_type
        and grp.part1_value = bpc.citation_part_1_value
      where bpc.book_id = ${ bookId } and bpc.citation_part_2_type is null
    ),
    -- citations in other books, not withheld ones, that point at the book
    counted as (
      select c.id from citations c
      where c.reference_book_id in (select id from cited_as)
        and c.source_book_id not in (select id from cited_as)
        and c.source_book_id not in (select book_id from withheld)
    ),
    by_number as (
      select p.page_number, grp.citation_id
      from counted
      join citation_groups grp on grp.citation_id = counted.id
      cross join lateral ${ groupPartRows('grp') } part(n, type, value)
      join pages p
        on p.book_id = ${ bookId }
        and case when p.printed_page_number ~ '^[0-9]+$'
                 then p.printed_page_number::numeric end = part.value
      where part.type = 'page'
    )
    select page_number, count(distinct citation_id) as n
    from (
      select by_parts.* from (
        select * from by_two_parts union all select * from by_one_part
      ) by_parts
      join citations c on c.id = by_parts.citation_id
      where c.reference_book_id in (select id from cited_as)
        and c.source_book_id not in (select id from cited_as)
        and c.source_book_id not in (select book_id from withheld)
      union all
      select * from by_number
    ) cites
    group by page_number`.execute(db);

  return new Map(rows.rows.map((r) => [r.page_number, Number(r.n)]));
};

const findBooks = async(db: Kysely<Database>, bookIds: string[]) => {
  if (bookIds.length === 0) {
    return new Map<string, { id: string, title: string, author: string }>();
  }
  else {
    const rows = await db.selectFrom('books')
      .select(['books.id', 'books.title', 'books.author'])
      .where('books.id', 'in', bookIds)
      .execute();

    return new Map(rows.map((r) => [r.id, r]));
  }
};

export const PageCitationQueries = { citationsOfPage, citedCountsByPage, findBooks };
