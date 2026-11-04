// What core/book_text.ts makes a book's text of, read from the database: the book, its pages and
// their blocks in reading order, how its pages are cited (book_pages_to_citations), its parts'
// names (book_part_names) and the citations of its footnotes.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { BookSource } from '../core/book_text.ts';
import type { CitationPart } from '../core/summa_thml.ts';

// The parts of a row with numbered part columns (prefix 1 to count), up to the first missing one
const partsOf = (row: Record<string, unknown>, prefix: (i: number) => string, count: number): CitationPart[] => {
  const parts: CitationPart[] = [];
  for (let i = 1; i <= count && row[`${ prefix(i) }_type`] !== null && row[`${ prefix(i) }_type`] !== undefined; i++) {
    parts.push({ type: String(row[`${ prefix(i) }_type`]), value: Number(row[`${ prefix(i) }_value`]) });
  }
  return parts;
};

// The book's source for its text; null when there's no such book
const findBookSource = async(db: Kysely<Database>, bookId: string): Promise<BookSource | null> => {
  const book = await db.selectFrom('books')
    .select(['books.id', 'books.title', 'books.author'])
    .where('books.id', '=', bookId)
    .executeTakeFirst();

  if (book === undefined) {
    return null;
  }
  else {
    const [pages, blocks, pageParts, partNames, citations] = await Promise.all([
      db.selectFrom('pages').select(['pages.page_number', 'pages.printed_page_number'])
        .where('pages.book_id', '=', bookId).orderBy('pages.page_number').execute(),
      db.selectFrom('page_blocks').select(['page_blocks.page_number', 'page_blocks.label', 'page_blocks.html'])
        .where('page_blocks.book_id', '=', bookId)
        .orderBy('page_blocks.page_number').orderBy('page_blocks.position').execute(),
      db.selectFrom('book_pages_to_citations').selectAll()
        .where('book_pages_to_citations.book_id', '=', bookId)
        .orderBy('book_pages_to_citations.page_number').orderBy('book_pages_to_citations.id').execute(),
      db.selectFrom('book_part_names').selectAll().where('book_part_names.book_id', '=', bookId).execute(),
      db.selectFrom('citations')
        .select([
          'citations.source_footnote_page', 'citations.source_footnote_identifier', 'citations.author',
          'citations.title', 'citations.location', 'citations.raw', 'citations.reference_book_id',
        ])
        .where('citations.source_book_id', '=', bookId)
        .orderBy('citations.id')
        .execute(),
    ]);

    const blocksByPage = new Map<number, BookSource['pages'][number]['blocks']>();
    for (const b of blocks) {
      blocksByPage.set(b.page_number, [...(blocksByPage.get(b.page_number) ?? []), { label: b.label, html: b.html }]);
    }

    return {
      ...book,
      pages: pages.map((p) => ({
        pageNumber: p.page_number, printedPageNumber: p.printed_page_number, blocks: blocksByPage.get(p.page_number) ?? [],
      })),
      pageParts: pageParts.map((r) => ({ pageNumber: r.page_number, parts: partsOf(r, (i) => `citation_part_${ i }`, 8) })),
      partNames: partNames.map((r) => ({ name: r.name, parts: partsOf(r, (i) => `part${ i }`, 4) })),
      citations: citations.map((c) => ({
        footnotePage:       c.source_footnote_page,
        footnoteIdentifier: c.source_footnote_identifier,
        author:             c.author,
        title:              c.title,
        location:           c.location,
        raw:                c.raw,
        referenceBookId:    c.reference_book_id,
      })),
    };
  }
};

export const BookTextQueries = { findBookSource };
