// The books and pages that citations point at, for linking a footnote's
// citation to the page it cites: the citation's book (its reference_book_id,
// or the book that id is an alternate id of), and the page of it that the
// citation's first place on one is found on, as citationsOfPage finds a page's
// citations the other way: a book_pages_to_citations row each of whose parts
// is one of the place's (the row with the most parts, a page's own, over one
// of a whole question or chapter), else a "page" part equal to the page's
// printed number. Withheld books (likely under copyright), whose pages can't
// be shown, aren't linked.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { CITATION_RANGE } from './citations.js';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

export type CitedPage = { citationId: string, bookId: string, pageId: number | null };

const groupPartPairs = (grp: string) => sql.join(CITATION_RANGE.map((n) => (
  sql`(${ sql.ref(`${ grp }.part${ n }_type`) }, ${ sql.ref(`${ grp }.part${ n }_value`) })`
)));

const groupPartRows = (grp: string) => sql`(values ${ sql.join(CITATION_RANGE.map((n) => (
  sql`(${ sql.ref(`${ grp }.part${ n }_type`) }, ${ sql.ref(`${ grp }.part${ n }_value`) })`
))) })`;

// SQL: every part of the row is one of the group's, and how many parts it has
const rowPartsInGroup = sql.join(CITATION_RANGE.map((n) => {
  const type  = sql.ref(`bpc.citation_part_${ n }_type`);
  const value = sql.ref(`bpc.citation_part_${ n }_value`);
  return sql`(${ type } is null or (${ type }, ${ value }) in (${ groupPartPairs('grp') }))`;
}), sql` and `);
const rowPartCount = sql.join(CITATION_RANGE.map((n) => (
  sql`(${ sql.ref(`bpc.citation_part_${ n }_type`) } is not null)::int`
)), sql` + `);

// The books and pages these citations point at; those pointing at no book in
// the collection, at their own book or at a withheld one are left out
export const findCitedPages = async(
  db: Kysely<Database>, citationIds: string[],
): Promise<CitedPage[]> => {
  if (citationIds.length === 0) {
    return [];
  }
  else {
    const { rows } = await sql<{ citation_id: string, book_id: string, page_id: number | null }>`
      with cited as (
        select c.id, b.id as book_id
        from citations c
        join books b on b.id = coalesce(
          (select a.book_id from alternate_ids a where a.alternate_id = c.reference_book_id limit 1),
          c.reference_book_id)
        where c.id in (${ sql.join(citationIds) })
          and b.id <> c.source_book_id
          and b.id not in ${ CopyrightStatusCheckSql.withheldBookIds() }
      )
      select cited.id as citation_id, cited.book_id, (
        select found.page_number from (
          select grp.id as group_id, bpc.page_number, ${ rowPartCount } as parts
          from citation_groups grp
          join book_pages_to_citations bpc
            on bpc.book_id = cited.book_id
            and bpc.citation_part_1_type = grp.part1_type
            and bpc.citation_part_1_value = grp.part1_value
          where grp.citation_id = cited.id and ${ rowPartsInGroup }
          union all
          select grp.id, p.page_number, 0
          from citation_groups grp
          cross join lateral ${ groupPartRows('grp') } part(type, value)
          join pages p
            on p.book_id = cited.book_id
            and case when p.printed_page_number ~ '^[0-9]+$'
                     then p.printed_page_number::numeric end = part.value
          where grp.citation_id = cited.id and part.type = 'page'
        ) found
        order by found.group_id, found.parts desc, found.page_number
        limit 1
      ) as page_id
      from cited
    `.execute(db);

    return rows.map((r) => ({ citationId: r.citation_id, bookId: r.book_id, pageId: r.page_id }));
  }
};

export const CitedPageQueries = { findCitedPages };
