// The citations given the footnote extraction model as worked examples (see
// core/citation_examples.ts): those linked to a book in the collection.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { exampleIndex, type ExampleIndex } from '../core/citation_examples.ts';

export const findExampleIndex = async(db: Kysely<Database>): Promise<ExampleIndex> => {
  const rows = await db.selectFrom('citations')
    .select(['id', 'source_book_id', 'raw', 'author', 'title', 'location'])
    .where('reference_book_id', 'is not', null)
    .where('raw', '<>', '')
    .orderBy('id')
    .execute();

  return exampleIndex(rows.map((r) => ({
    id:           String(r.id),
    sourceBookId: r.source_book_id,
    raw:          r.raw,
    author:       r.author,
    title:        r.title,
    location:     r.location,
  })));
};

export const CitationExampleQueries = { findExampleIndex };
