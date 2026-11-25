// Saving the citations extracted from an imported book's footnotes (see core/footnote_extraction.ts)
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { placesOf } from '../core/citation_groups.ts';
import type { PlacedCitation } from '../core/footnote_extraction.ts';
import type { FootnoteBlocks } from '../core/footnote_relabel.ts';
import { locationsFromPlaces } from '../core/footnote_training.ts';
import type { PlaceGroup } from '../core/citation_groups.ts';
import { groupRow } from './citation_groups.ts';
import { PageCitedCountsCacheActions } from './page_cited_counts_cache.ts';

const blockKey = (pageNumber: number, position: number) => pageNumber + ':' + position;

// Replace a book's citations with these, each in its footnote block, with a citation_groups row
// per place each of its location groups cites (see core/citation_groups.ts). In one transaction,
// after the book's pages are saved. Returns the counts saved, and how many location values were
// left out of the places.
const replaceExtractedCitations = (
  db: Kysely<Database>,
  bookId: string,
  placed: PlacedCitation[],
) => db.transaction().execute(async(trx) => {
  const counts = { citations: 0, groups: 0, skippedValues: 0 };

  const blocks   = await trx.selectFrom('page_blocks')
    .select(['id', 'page_number', 'position'])
    .where('book_id', '=', bookId)
    .execute();
  const blockIds = new Map(blocks.map((b) => [blockKey(b.page_number, b.position), b.id]));

  await trx.deleteFrom('citations').where('source_book_id', '=', bookId).execute();

  for (const { pageNumber, position, citation: c } of placed) {
    const blockId = blockIds.get(blockKey(pageNumber, position));
    if (blockId === undefined) {
      throw new Error(`${ bookId } has no block ${ position } on page ${ pageNumber }`);
    }

    const { id: citationId } = await trx.insertInto('citations')
      .values({
        page_block_id:              blockId,
        source_book_id:             bookId,
        source_footnote_identifier: c.source.footnoteIdentifier,
        source_footnote_page:       c.source.footnotePage,
        reference_book_id:          c.referenceBookId,
        author:                     c.author,
        title:                      c.title,
        collection:                 c.collection ?? null,
        issue:                      c.issue ?? null,
        location:                   c.location,
        raw:                        c.raw,
      })
      .returning('id')
      .executeTakeFirstOrThrow();
    counts.citations++;

    for (const group of c.locationsCited) {
      const { places, skipped } = placesOf(group);
      counts.skippedValues     += skipped;

      if (places.length > 0) {
        await trx.insertInto('citation_groups')
          .values(places.map((place) => groupRow(citationId, place)))
          .execute();
        counts.groups += places.length;
      }
    }
  }

  // the book's citations count toward the pages they cite
  await PageCitedCountsCacheActions.clearCachedCounts(trx);

  return counts;
});

export const ExtractedCitationActions = { replaceExtractedCitations };

// A book's citations as saved, each in its footnote block (its page and position), with its
// link and its locations from its citation_groups rows (see locationsFromPlaces), in the order
// saved: what replaceExtractedCitations would save again
const findPlacedCitations = async(db: Kysely<Database>, bookId: string): Promise<PlacedCitation[]> => {
  const rows = (await sql<{
    pageNumber: number, position: number, identifier: string, footnotePage: number,
    referenceBookId: string | null, author: string, title: string, collection: string | null,
    issue: string | null, location: string, raw: string, groups: PlaceGroup[],
  }>`
    select pb.page_number as "pageNumber", pb.position, c.source_footnote_identifier as identifier,
      c.source_footnote_page as "footnotePage", c.reference_book_id as "referenceBookId",
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
  `.execute(db)).rows;

  return rows.map((r) => ({
    pageNumber: r.pageNumber,
    position:   r.position,
    citation:   {
      source:          { bookId, footnoteIdentifier: r.identifier, footnotePage: r.footnotePage },
      referenceBookId: r.referenceBookId,
      author:          r.author,
      title:           r.title,
      collection:      r.collection,
      issue:           r.issue,
      location:        r.location,
      raw:             r.raw,
      locationsCited:  locationsFromPlaces(r.groups),
    },
  }));
};

// A book's Footnote blocks, by page, each with its position among the page's blocks
const findFootnoteBlocks = async(db: Kysely<Database>, bookId: string): Promise<FootnoteBlocks> => {
  const rows = await db.selectFrom('page_blocks')
    .select(['page_number', 'position', 'html'])
    .where('book_id', '=', bookId)
    .where('label', '=', 'Footnote')
    .orderBy('page_number')
    .orderBy('position')
    .execute();

  return rows.reduce((m, r) => m.set(r.page_number, [
    ...m.get(r.page_number) ?? [], { position: r.position, html: r.html },
  ]), new Map() as FootnoteBlocks);
};

export const ExtractedCitationQueries = { findPlacedCitations, findFootnoteBlocks };
