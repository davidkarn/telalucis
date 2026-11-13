// Finding the passages of the collection that answer a question (see migrations/0030_create_passages.ts),
// from three lists of candidates: those nearest it in meaning (their embeddings), those with its
// words (their text's tsvector), and those citing the pages of the best of these (their footnotes'
// citations: the commentators on a passage). The lists are merged by reciprocal rank fusion, the
// merged candidates may be scored again by a reranking model, copies of one text (two translations'
// printings, a book held twice) are made one, and no author or book is given more than a few
// places, so the answer hears several voices. Pure functions; lib/passage_search.ts runs the
// searches.

// Where a candidate was found: by meaning, by words, or citing a page found by either
export type SearchSource = 'meaning' | 'words' | 'cites';

export type Candidate = {
  passageId: string,
  bookId: string,
  title: string,
  author: string,
  // the book's authors (book_authors); none for a book whose author isn't known
  authorIds: string[],
  pageNumber: number,
  endPageNumber: number,
  printedPageNumber: string,
  text: string,
  // for a candidate found citing a page: the page it cites
  cites?: { bookId: string, pageNumber: number },
};

// A candidate's place in each list it's in, from 0
export type Ranked = Candidate & { ranks: Partial<Record<SearchSource, number>> };

export type SearchResult = Candidate & {
  score: number,
  foundBy: SearchSource[],
  // the other copies of its text, left out
  copies: { bookId: string, title: string, pageNumber: number, printedPageNumber: string }[],
};

// Reciprocal rank fusion's constant: a place's score is 1 / (RRF_K + place)
export const RRF_K = 60;

// How much each list counts in the fusion: a commentator found by citation is worth less than a
// passage found for itself, as it's found for the page it cites rather than for the question
export const SOURCE_WEIGHTS: Record<SearchSource, number> = { meaning: 1, words: 1, cites: 0.5 };

// The candidates of the lists, each once, with its places in them, by their fused score
// (reciprocal rank fusion), the best first
export const fused = (lists: Partial<Record<SearchSource, Candidate[]>>): (Ranked & { fusedScore: number })[] => {
  const byId = new Map<string, Ranked>();

  for (const [source, list] of Object.entries(lists) as [SearchSource, Candidate[]][]) {
    list.forEach((c, rank) => {
      const known = byId.get(c.passageId);
      byId.set(c.passageId, known === undefined
        ? { ...c, ranks: { [source]: rank } }
        : { ...known, ...(c.cites && !known.cites ? { cites: c.cites } : {}), ranks: { ...known.ranks, [source]: rank } });
    });
  }

  return [...byId.values()]
    .map((r) => ({
      ...r,
      fusedScore: (Object.entries(r.ranks) as [SearchSource, number][])
        .reduce((s, [source, rank]) => s + SOURCE_WEIGHTS[source] / (RRF_K + rank), 0),
    }))
    .sort((a, b) => b.fusedScore - a.fusedScore);
};

// ---------------------------------------------------------------------------
// Copies
// ---------------------------------------------------------------------------

// The text's runs of SHINGLE words, lower-cased, letters and digits only, for telling copies of a
// text apart from passages that only share some phrases
const SHINGLE = 5;

export const shingles = (text: string): Set<string> => {
  const words = text.toLowerCase().normalize('NFKD').replace(/[̀-ͯ]/g, '')
    .split(/[^\p{L}\p{N}]+/u).filter((w) => w.length > 0);
  return new Set(Array.from({ length: Math.max(0, words.length - SHINGLE + 1) }, (_, i) => words.slice(i, i + SHINGLE).join(' ')));
};

// Two passages are copies of one text when this share of the shorter one's shingles is in the
// other's: a printing's page breaks and OCR slips change some, but most remain
export const COPY_OVERLAP = 0.5;

export const areCopies = (a: Set<string>, b: Set<string>): boolean => {
  const [small, large] = a.size <= b.size ? [a, b] : [b, a];
  if (small.size === 0) {
    return false;
  }
  else {
    let shared = 0;
    for (const s of small) {
      if (large.has(s)) {
        shared++;
      }
    }
    return shared / small.size >= COPY_OVERLAP;
  }
};

// The results, in order, each copy of a text after the first made one of its copies
export const withoutCopies = <R extends Candidate & { copies?: SearchResult['copies'] }>(results: R[]): (R & { copies: SearchResult['copies'] })[] => {
  const kept: { result: R & { copies: SearchResult['copies'] }, shingles: Set<string> }[] = [];

  for (const result of results) {
    const own      = shingles(result.text);
    const original = kept.find((k) => k.result.bookId !== result.bookId && areCopies(k.shingles, own));

    if (original) {
      original.result.copies.push({
        bookId: result.bookId, title: result.title, pageNumber: result.pageNumber, printedPageNumber: result.printedPageNumber,
      });
    }
    else {
      kept.push({ result: { ...result, copies: [...(result.copies ?? [])] }, shingles: own });
    }
  }

  return kept.map((k) => k.result);
};

// ---------------------------------------------------------------------------
// Voices
// ---------------------------------------------------------------------------

// Who a candidate's author is, for limiting each one's places: their author ids, or their name as
// printed (lower-cased, its dates and punctuation left out) for a book with none
export const authorKeys = (c: Pick<Candidate, 'authorIds' | 'author'>): string[] => (
  c.authorIds.length > 0
    ? c.authorIds.map((id) => `id:${ id }`)
    : [`name:${ c.author.toLowerCase().replace(/[\d\-–?,.]+/g, ' ').replace(/\s+/g, ' ').trim() }`]
);

export type VoiceLimits = { limit: number, perAuthor: number, perBook: number };

export const DEFAULT_LIMITS: VoiceLimits = { limit: 12, perAuthor: 3, perBook: 2 };

// The first `limit` results, in order, giving no author more than perAuthor places and no book
// more than perBook; when that leaves fewer than `limit`, the rest of the places go to those left
// out, in order
export const withVoices = <R extends Candidate>(results: R[], limits: VoiceLimits = DEFAULT_LIMITS): R[] => {
  const byAuthor = new Map<string, number>();
  const byBook   = new Map<string, number>();
  const chosen   = new Set<R>();

  for (const r of results) {
    if (chosen.size < limits.limit) {
      const keys = authorKeys(r);
      const full = keys.some((k) => (byAuthor.get(k) ?? 0) >= limits.perAuthor) || (byBook.get(r.bookId) ?? 0) >= limits.perBook;

      if (!full) {
        chosen.add(r);
        keys.forEach((k) => byAuthor.set(k, (byAuthor.get(k) ?? 0) + 1));
        byBook.set(r.bookId, (byBook.get(r.bookId) ?? 0) + 1);
      }
    }
  }

  for (const r of results) {
    if (chosen.size < limits.limit) {
      chosen.add(r);
    }
  }

  return results.filter((r) => chosen.has(r));
};

// The final results: the candidates scored (by a reranker's scores, given by passage id, else by
// their fused score), the best first, copies made one, and the places shared out among the voices
export const searchResults = (
  candidates: (Ranked & { fusedScore: number })[],
  rerankScores: Map<string, number> | null,
  limits: VoiceLimits = DEFAULT_LIMITS,
): SearchResult[] => {
  const scored = candidates
    .map((c) => ({
      ...c,
      score:   rerankScores?.get(c.passageId) ?? (rerankScores === null ? c.fusedScore : -Infinity),
      foundBy: (Object.keys(c.ranks) as SearchSource[]),
    }))
    .sort((a, b) => b.score - a.score);

  return withVoices(withoutCopies(scored), limits).map((r): SearchResult => ({
    passageId:         r.passageId,
    bookId:            r.bookId,
    title:             r.title,
    author:            r.author,
    authorIds:         r.authorIds,
    pageNumber:        r.pageNumber,
    endPageNumber:     r.endPageNumber,
    printedPageNumber: r.printedPageNumber,
    text:              r.text,
    ...(r.cites ? { cites: r.cites } : {}),
    score:             r.score,
    foundBy:           r.foundBy,
    copies:            r.copies,
  }));
};

// ---------------------------------------------------------------------------
// Measuring the search (eval/passage_search.json)
// ---------------------------------------------------------------------------

// An expected answer: a book's pages (those cited by its expected parts)
export type ExpectedAnswer = { bookId: string, pages: number[] };

// Whether a result is on one of the answer's pages
const answers = (r: Pick<Candidate, 'bookId' | 'pageNumber' | 'endPageNumber'>, a: ExpectedAnswer) => (
  r.bookId === a.bookId && a.pages.some((p) => p >= r.pageNumber && p <= r.endPageNumber)
);

// How well the results answer a question: the place of the first result answering it (from 1;
// null if none does), how many of its expected answers some result gives, and how many authors
// the results have
export const answerScore = (results: (Pick<Candidate, 'bookId' | 'pageNumber' | 'endPageNumber' | 'authorIds' | 'author'>)[], expected: ExpectedAnswer[]) => {
  const first = results.findIndex((r) => expected.some((a) => answers(r, a)));
  return {
    firstAnswer:  first < 0 ? null : first + 1,
    answersFound: expected.filter((a) => results.some((r) => answers(r, a))).length,
    authors:      new Set(results.map((r) => authorKeys(r).join('+'))).size,
  };
};
