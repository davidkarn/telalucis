// Linking the citations that cite a book in the collection but aren't pointed
// at it: those made before the book was imported (a book's citations are
// linked when it is crawled, against the books there then), and those whose
// author the extraction left out. Each unlinked citation, in reading order:
//
// 1. gets the author its title says, when it has none (fillCitationAuthors);
// 2. if it cites the Summa, or a work cited by its Latin aliases (the
//    Fathers', Aristotle's, Boethius's, the Contra Gentiles: see
//    core/fathers_citations.ts), is pointed at it with its places read again,
//    as summa_update_incoming_citations.ts and
//    fathers_update_incoming_citations.ts do; one whose places can't be read,
//    or are on no page of the work, is left unlinked;
// 3. else is matched by author and title (matchCitedBooks), keeping its
//    locations.
//
// A citation is never pointed at its own book. Pure functions;
// lib/citation_relinking.ts reads the citations and saves the plan.
import type { PlaceGroup } from './citation_groups.ts';
import { fillCitationAuthors } from './citation_authors.ts';
import { matchCitedBooks } from './cited_books.ts';
import { ARISTOTLE_WORKS } from './aristotle_citations.ts';
import {
  citedFatherWork, divisionTypes, FATHER_WORKS, locationText, parseFatherLocation, placeOnPage,
  type FatherWork,
} from './fathers_citations.ts';
import { BOETHIUS_WORKS, CONTRA_GENTILES_WORKS } from './medieval_citations.ts';
import { isIbid, isSummaCitation, lastPlace, parseSummaCitation, type SummaPlace } from './summa_citations.ts';
import { SUMMA_BOOK_ID, type CitationPart } from './summa_thml.ts';

// The works cited by aliases, tried in this order
export const ALIAS_WORKS: FatherWork[] = [
  ...FATHER_WORKS, ...ARISTOTLE_WORKS, ...BOETHIUS_WORKS, ...CONTRA_GENTILES_WORKS,
];

export type RelinkCitation = {
  id: string,
  source_book_id: string,
  author: string,
  title: string,
  location: string,
  raw: string,
  reference_book_id: string | null,
};

export type Book = { id: string, title: string, author: string };

// A work's pages, by the parts they're cited by (book_pages_to_citations)
export type WorkPages = Map<string, { pageId: number, parts: CitationPart[] }[]>;

export type RelinkPlan = {
  // authors filled in (and placeholder titles blanked), to save
  names: { id: string, author: string, title: string }[],
  // citations pointed at the Summa or an alias work, with their places
  located: { bookId: string, citations: { id: string, groups: PlaceGroup[] }[] }[],
  // citations pointed at a book matched by author and title
  links: { citationId: string, bookId: string }[],
  // citations of the Summa or an alias work whose places couldn't be read, or
  // are on no page of it: left unlinked
  unplaced: number,
};

// The citations with their authors filled in (see fillCitationAuthors), each
// book's together: whether a book cites Aquinas by name decides some
const withAuthors = (citations: RelinkCitation[]): RelinkCitation[] => {
  const byBook = new Map<string, RelinkCitation[]>();
  for (const c of citations) {
    byBook.set(c.source_book_id, [...(byBook.get(c.source_book_id) ?? []), c]);
  }

  const filled = new Map([...byBook.values()].flatMap((cs) => fillCitationAuthors(cs).map((c) => [c.id, c])));
  return citations.map((c) => filled.get(c.id) ?? c);
};

// The places an alias work's citation cites on its pages, as
// fathers_update_incoming_citations.ts reads them; none when they can't be
// read or are on none of its pages
const aliasPlaces = (c: RelinkCitation, work: FatherWork, pages: WorkPages): PlaceGroup[] => {
  const rows   = pages.get(work.bookId) ?? [];
  const prefix = work.prefix ?? [];
  const types  = (work.types ?? divisionTypes(rows)).slice(prefix.length);
  const raw    = c.location || c.raw;

  return parseFatherLocation(locationText(c, work.title), types)
    .map((parts) => placeOnPage([...prefix, ...parts], rows))
    .flatMap((parts) => (parts === undefined ? [] : [{ raw, parts }]));
};

// What to link of the citations (every citation, linked or not, in reading
// order: by source book, footnote page, then id, as an "Ibid." needs), to
// these books. pages: the pages of the Summa and the alias works among the
// books, those imported with their divisions.
export const relinkPlan = (
  citations: RelinkCitation[], books: Book[], pages: WorkPages,
): RelinkPlan => {
  const filled  = withAuthors(citations);
  const linking = new Set(books.map((b) => b.id));
  const summa   = linking.has(SUMMA_BOOK_ID);
  const aliases = ALIAS_WORKS.filter((w) => linking.has(w.bookId) && (pages.get(w.bookId) ?? []).length > 0);

  const located                   = new Map<string, { id: string, groups: PlaceGroup[] }[]>();
  const byTitle: RelinkCitation[] = [];
  let unplaced                    = 0;
  // the last place in the Summa cited in each book, for "Ibid." to refer back to
  let previous: { bookId: string, place: SummaPlace | undefined } | undefined;

  for (const c of filled) {
    const isSumma = summa && c.source_book_id !== SUMMA_BOOK_ID && isSummaCitation(c.title, c.author);
    const work    = isSumma ? undefined : citedFatherWork(c.author, c.title, c.location, aliases);

    if (isSumma) {
      const before = previous?.bookId === c.source_book_id ? previous.place : undefined;
      const groups = parseSummaCitation(c.raw, isIbid(c.raw) ? before : undefined);

      if (groups.length > 0) {
        previous = { bookId: c.source_book_id, place: lastPlace(groups) };
      }
      if (c.reference_book_id !== null) {
        // linked already
      }
      else if (groups.length > 0) {
        const raw = c.location || c.raw;
        located.set(SUMMA_BOOK_ID, [
          ...(located.get(SUMMA_BOOK_ID) ?? []),
          { id: c.id, groups: groups.map((parts) => ({ raw, parts })) },
        ]);
      }
      else {
        unplaced++;
      }
    }
    else if (c.reference_book_id !== null) {
      // linked already
    }
    else if (work !== undefined && work.bookId !== c.source_book_id) {
      const groups = aliasPlaces(c, work, pages);

      if (groups.length > 0) {
        located.set(work.bookId, [...(located.get(work.bookId) ?? []), { id: c.id, groups }]);
      }
      else {
        unplaced++;
      }
    }
    else {
      byTitle.push(c);
    }
  }

  const sources = new Map(byTitle.map((c) => [c.id, c.source_book_id]));
  const links   = matchCitedBooks(byTitle, books, []).referencing
    .filter((l) => sources.get(l.citationId) !== l.bookId);
  const before  = new Map(citations.map((c) => [c.id, c]));

  return {
    names: filled
      .filter((c) => c.reference_book_id === null)
      .filter((c) => before.get(c.id)!.author !== c.author || before.get(c.id)!.title !== c.title)
      .map(({ id, author, title }) => ({ id, author, title })),
    located: [...located].map(([bookId, cs]) => ({ bookId, citations: cs })),
    links,
    unplaced,
  };
};
