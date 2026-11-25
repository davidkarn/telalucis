// What training and measuring a footnote extraction model reads (see
// core/footnote_training.ts and core/footnote_eval.ts): the OCR'd books whose
// footnotes' citations were extracted, each one's pages' footnotes and the
// citations saved of them. A book is OCR'd when its footnote blocks have
// boxes on the page; those of books read from markup (ThML, EPUB) don't, and
// their citations were never the model's.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { FootnotePage } from '../core/footnote_extraction.ts';
import type { SavedCitation } from '../core/footnote_training.ts';

// The OCR'd books with citations, by id
const findOcrBookIds = async(db: Kysely<Database>): Promise<string[]> => (
  (await sql<{ id: string }>`
    select distinct c.source_book_id as id
    from citations c
    join page_blocks pb on pb.id = c.page_block_id
    where pb.bbox_x1 > 0
    order by 1
  `.execute(db)).rows.map((r) => r.id)
);

// A book's pages with footnotes, each with its Footnote blocks, in order,
// their positions as their reading order
const findFootnotePages = async(db: Kysely<Database>, bookId: string): Promise<FootnotePage[]> => {
  const rows = await db.selectFrom('page_blocks')
    .select(['page_number', 'position', 'html'])
    .where('book_id', '=', bookId)
    .where('label', '=', 'Footnote')
    .orderBy('page_number')
    .orderBy('position')
    .execute();

  return [...rows.reduce((pages, r) => pages.set(r.page_number, [
    ...pages.get(r.page_number) ?? [],
    { label: 'Footnote' as const, html: r.html, reading_order: r.position },
  ]), new Map<number, FootnotePage['blocks']>())]
    .map(([page, blocks]) => ({ page, blocks }));
};

// A book's citations, in the order saved, each with its footnote block's page
// and position and its citation_groups rows in order
const findSavedCitations = async(db: Kysely<Database>, bookId: string): Promise<SavedCitation[]> => (
  (await sql<SavedCitation>`
    select pb.page_number as "pageNumber", pb.position, c.source_footnote_identifier as identifier,
      c.author, c.title, c.collection, c.issue, c.location, c.raw,
      coalesce((
        select json_agg(json_build_object('raw', g.raw, 'parts', array_remove(array[
          ${ sql.join([1, 2, 3, 4, 5, 6, 7, 8].map((n) => sql`
            case when ${ sql.ref(`g.part${ n }_type`) } is null then null
            else json_build_object('type', ${ sql.ref(`g.part${ n }_type`) },
              'value', ${ sql.ref(`g.part${ n }_value`) })::jsonb end`)) }
        ], null)) order by g.id)
        from citation_groups g where g.citation_id = c.id
      ), '[]'::json) as groups
    from citations c
    join page_blocks pb on pb.id = c.page_block_id
    where c.source_book_id = ${ bookId }
    order by c.id
  `.execute(db)).rows
);

export const FootnoteTrainingQueries = { findOcrBookIds, findFootnotePages, findSavedCitations };
