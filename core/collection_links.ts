// The book a citation of a collection cites (its collection and issue, see footnotesPrompt in
// core/footnote_extraction.ts): an article of a journal in the collection, in the issue cited
// (by its volume, else its year), whose pages take in the page cited (else whose title is the
// citation's); or a volume of a collection held as books, one a volume ("Patrologia Graeca
// 100"). Pure functions; matchCitedBooks (core/cited_books.ts) links citations by them first.
import { sameTitle } from './citation_matching.ts';

// A journal article in the collection: its book, the journal and issue it's in (the issue's
// volume and year, null when not known) and its pages
export type JournalArticleRef = {
  id: string, title: string, journal: string,
  volume: number | null, year: number | null, firstPage: number, lastPage: number,
};

export type CollectionCitation = { title: string, collection?: string | null, issue?: string | null, location: string };

const ROMAN: Record<string, number> = { i: 1, v: 5, x: 10, l: 50, c: 100, d: 500, m: 1000 };

const romanValue = (r: string): number => [...r.toLowerCase()].reduce((n, ch, k, all) => {
  const v    = ROMAN[ch]!;
  const next = ROMAN[all[k + 1] ?? ''] ?? 0;
  return n + (v < next ? -v : v);
}, 0);

// A number as written, arabic or roman ("23", "XXIX"); null for neither
const numberOf = (s: string): number | null => (
  /^\d+$/.test(s) ? Number(s) : /^[ivxlcdm]+$/i.test(s) ? romanValue(s) : null
);

const MONTH = '(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\.?';

// A day of a month as written ("October 16", "16 October", "12th and 19th November")
const DAY = new RegExp(`\\b${ MONTH }\\s+\\d{1,2}(?:st|nd|rd|th)?\\b|\\b\\d{1,2}(?:st|nd|rd|th)?\\s+${ MONTH }|\\b\\d{1,2}(?:st|nd|rd|th)\\b`, 'gi');

// An issue's volume and year as written ("XXIX (April, 1955)" -> 29, 1955; "2 (1941)" -> 2,
// 1941; "t. iii." -> 3; "1892" -> null, 1892). A day of a month isn't a volume: an issue named
// by its date ("October 16, 1906") has none.
export const issueNumbers = (issue: string): { volume: number | null, year: number | null } => {
  const year   = /\b(1[5-9]\d\d|20\d\d)\b/.exec(issue);
  const rest   = issue
    .replace(/\b(1[5-9]\d\d|20\d\d)\b/g, ' ')
    .replace(DAY, ' ')
    .replace(/\bpart\s+[ivx\d]+\b|\bn\.\s*s\.|\bnew series\b|\bn\.?\s*f\.|\bseries\b|\bserie\b/gi, ' ');
  const tokens = [...rest.matchAll(/[A-Za-z]+|\d+/g)].map((m) => m[0])
    .filter((t) => !/^(vol|vols|volume|tom|tome|t|no|nr|n|fasc|quad|bd|jahrg)$/i.test(t));
  const first  = tokens.map(numberOf).find((n) => n !== null) ?? null;

  return { volume: first, year: year === null ? null : Number(year[1]) };
};

// The first page a location cites, written in arabic numerals ("pp. 37—53" -> 37); null for none
export const firstCitedPage = (location: string): number | null => {
  const m = /\d+/.exec(location);
  return m === null ? null : Number(m[0]);
};

// The articles of each journal, worked out once for each list of articles
const journalsOf = new WeakMap<JournalArticleRef[], Map<string, JournalArticleRef[]>>();

const articlesByJournal = (articles: JournalArticleRef[]): Map<string, JournalArticleRef[]> => {
  const known = journalsOf.get(articles);
  const found = known ?? articles.reduce(
    (m, a) => m.set(a.journal, [...m.get(a.journal) ?? [], a]),
    new Map<string, JournalArticleRef[]>(),
  );

  journalsOf.set(articles, found);
  return found;
};

// The article a citation cites among the collection's (see the top), or undefined when there's
// none, or more than one with nothing to tell them apart
export const citedArticle = (
  c: CollectionCitation, articles: JournalArticleRef[],
): JournalArticleRef | undefined => {
  const issue  = c.issue ? issueNumbers(c.issue) : { volume: null, year: null };
  const page   = firstCitedPage(c.location);
  const byName = articlesByJournal(articles);
  // the journal named once, not each of its articles' (matching titles is slow)
  const ofIt   = c.collection
    ? [...byName.keys()].filter((j) => sameTitle(c.collection!, j)).flatMap((j) => byName.get(j)!)
    : [];
  const inIt   = ofIt.filter((a) => (
    issue.volume !== null && a.volume !== null ? a.volume === issue.volume
      : issue.year !== null && a.year !== null ? a.year === issue.year
        : false
  ));
  const byPage  = page === null ? inIt : inIt.filter((a) => a.firstPage <= page && page <= a.lastPage);
  const byTitle = c.title.trim().length > 0 ? byPage.filter((a) => sameTitle(c.title, a.title)) : [];

  if (byPage.length === 1) {
    return byPage[0];
  }
  else if (byTitle.length === 1) {
    return byTitle[0];
  }
  else {
    return undefined;
  }
};

// A title naming a volume of a collection ("Patrologia Graeca 100", "Acta Apostolicae Sedis,
// vol. 17"): the collection and the volume; null for one that doesn't end in a volume
export const collectionVolume = (title: string): { name: string, volume: number } | null => {
  const m = /^(.*?)[\s,:.]+(?:vol\.?|volume|tom\.?|tome|t\.)?\s*(\d+|[ivxlc]+)\s*$/i.exec(title.trim());
  const n = m === null ? null : numberOf(m[2]!);
  return m === null || n === null || m[1]!.trim().length === 0 ? null : { name: m[1]!.trim(), volume: n };
};

// The book, a volume of the collection cited, that holds the issue cited; undefined for none, or
// for more than one
export const citedCollectionVolume = <B extends { id: string, title: string }>(
  c: CollectionCitation, books: B[],
): B | undefined => {
  const volume = c.issue ? issueNumbers(c.issue).volume : null;
  const found  = volume === null || !c.collection ? [] : volumesOf(books).filter((v) => (
    v.volume === volume && sameTitle(c.collection!, v.name)
  ));

  return found.length === 1 ? found[0]!.book as B : undefined;
};

// The books that are volumes of a collection (see collectionVolume), worked out once for each list
// of books
const volumesIn = new WeakMap<object[], { book: object, name: string, volume: number }[]>();

const volumesOf = <B extends { title: string }>(books: B[]): { book: B, name: string, volume: number }[] => {
  const known = volumesIn.get(books) as { book: B, name: string, volume: number }[] | undefined;
  const found = known ?? books.flatMap((book) => {
    const named = collectionVolume(book.title);
    return named === null ? [] : [{ book, ...named }];
  });

  volumesIn.set(books, found);
  return found;
};
