// Measuring a model's extraction of footnotes' citations (see
// core/footnote_extraction.ts) against a set of batches whose citations are
// known (eval/footnotes.json): each batch's pages' footnotes as the model is
// given them, and the footnotes and citations expected, first as the
// collection has them (drawn from the test books, see core/footnote_training.ts),
// then checked by hand (checked: true). The citations answered are matched to
// those expected on the same page by their text (raw), and those matched are
// compared field by field, and by the places their locations cite. Pure
// functions; cli/commands/make_footnote_eval_set.command.ts makes the set,
// and cli/commands/eval_footnotes.command.ts asks a model and scores it.
import { placesOf } from './citation_groups.ts';
import type { BatchFootnote } from './footnote_training.ts';
import { batchId, stableHash } from './footnote_training.ts';
import type { CitationLocation } from '../types.ts';

// A batch whose citations are known
export type EvalBatch = {
  id: string,
  bookId: string,
  // whether the expected footnotes have been checked by hand
  checked: boolean,
  note?: string,
  // each page's footnotes, as the model is given them (see batchMessage)
  pages: { page: number, html: string }[],
  expected: BatchFootnote[],
};

export type EvalSet = { about: string, batches: EvalBatch[] };

// An eval batch's id: its book's, with its pages
export const evalBatchId = batchId;

// `count` of the batches, at most `perBook` a book, spread over each book's:
// a batch of each book, then a second of each, and so on, the books in an
// order that's the same every time but not theirs (so not all of one
// journal's)
export const sampleBatches = <B extends { bookId: string }>(
  batches: B[], { count, perBook }: { count: number, perBook: number },
): B[] => {
  const byBook = batches.reduce(
    (m, b) => m.set(b.bookId, [...m.get(b.bookId) ?? [], b]),
    new Map<string, B[]>(),
  );
  const picks  = [...byBook.keys()]
    .sort((a, b) => stableHash(a) - stableHash(b) || a.localeCompare(b))
    .map((book) => {
      const own  = byBook.get(book)!;
      const take = Math.min(perBook, own.length);
      return Array.from({ length: take }, (_, i) => own[Math.floor(i * own.length / take)]!);
    });

  return Array.from({ length: perBook }, (_, round) => picks.flatMap((p) => p.slice(round, round + 1)))
    .flat()
    .slice(0, count);
};

// The widest an object or array is written on one line in the eval set's file
const INLINE_WIDTH = 100;

// JSON as the eval set's file is written, to check by hand: indented, but an
// object or array that fits in INLINE_WIDTH on one line (a location, a short
// citation's group)
export const evalSetJson = (value: unknown, indent = ''): string => {
  const inline = JSON.stringify(value);
  const inner  = indent + '  ';

  if (value === null || typeof value !== 'object' || inline.length + indent.length <= INLINE_WIDTH) {
    return inline;
  }
  else if (Array.isArray(value)) {
    return value.length === 0
      ? '[]'
      : `[\n${ value.map((v) => inner + evalSetJson(v, inner)).join(',\n') }\n${ indent }]`;
  }
  else {
    const entries = Object.entries(value).filter(([, v]) => v !== undefined);
    return `{\n${ entries.map(([k, v]) => `${ inner }${ JSON.stringify(k) }: ${ evalSetJson(v, inner) }`).join(',\n') }\n${ indent }}`;
  }
};

// Whether a book is a journal article (see splitKey): there are many more of
// them than of other books, each with a few batches
export const isArticle = (bookId: string): boolean => /_p\d+$/.test(bookId);

// `count` of the batches, half of journal articles (at most perBook an
// article) and half of other books (as many a book as it takes), as there
// are, each half as sampleBatches takes them
export const sampleEvalBatches = <B extends { bookId: string }>(
  batches: B[], { count, perBook }: { count: number, perBook: number },
): B[] => {
  const articles = batches.filter((b) => isArticle(b.bookId));
  const books    = batches.filter((b) => !isArticle(b.bookId));
  const bookIds  = new Set(books.map((b) => b.bookId)).size;
  const ofBooks  = sampleBatches(books, {
    count: Math.ceil(count / 2), perBook: Math.max(perBook, Math.ceil(count / 2 / Math.max(1, bookIds))),
  });

  return [...ofBooks, ...sampleBatches(articles, { count: count - ofBooks.length, perBook })];
};

// A text as compared: character forms, case, quotes, spacing and the
// punctuation ending it ignored
export const comparable = (text: string): string => (
  text.normalize('NFKC').toLowerCase()
    .replace(/[‘’`]/g, "'")
    .replace(/[“”]/g, '"')
    .replace(/\s+/g, ' ')
    .replace(/[\s.,;:]+$/, '')
    .trim()
);

const bigrams = (text: string): Map<string, number> => {
  const t = comparable(text).replace(/\s/g, '');
  return Array.from({ length: Math.max(0, t.length - 1) }, (_, i) => t.slice(i, i + 2))
    .reduce((m, g) => m.set(g, (m.get(g) ?? 0) + 1), new Map<string, number>());
};

// How alike two texts are, 0 to 1: the Dice coefficient of their character
// pairs, spacing ignored
export const similarity = (a: string, b: string): number => {
  const x     = bigrams(a);
  const y     = bigrams(b);
  const total = [...x.values(), ...y.values()].reduce((n, c) => n + c, 0);
  const both  = [...x].reduce((n, [g, c]) => n + Math.min(c, y.get(g) ?? 0), 0);

  if (total === 0) {
    return comparable(a) === comparable(b) ? 1 : 0;
  }
  else {
    return 2 * both / total;
  }
};

// Two citations' texts this alike (see similarity) are the same citation
export const MATCH_SIMILARITY = 0.6;

// The places a citation's locations cite, each its types and values
// ("chapter=1/verse=2"): what's saved of them (see placesOf)
export const placeKeys = (locations: CitationLocation[][]): string[] => [...new Set(
  locations.flatMap((group) => placesOf(group).places.map((p) => (
    p.parts.map((part) => `${ part.type }=${ part.value }`).join('/')
  ))),
)];

type Cited = { page: number, identifier: string, citation: BatchFootnote['citations'][number] };

const citedOf = (footnotes: BatchFootnote[]): Cited[] => footnotes.flatMap((f) => (
  f.citations.map((citation) => ({ page: f.page, identifier: f.identifier, citation }))
));

// The expected and answered citations matched: on the same page, the most
// alike first (a footnote's marker the same counting a little), each once,
// none less alike than MATCH_SIMILARITY
export const matchCitations = (
  expected: BatchFootnote[], answered: BatchFootnote[],
): { pairs: [Cited, Cited][], missed: Cited[], extra: Cited[] } => {
  const want       = citedOf(expected);
  const got        = citedOf(answered);
  const candidates = want.flatMap((w, i) => got.flatMap((g, j) => {
    const score = similarity(w.citation.raw, g.citation.raw)
      + (comparable(w.identifier) === comparable(g.identifier) ? 0.05 : 0);
    return w.page === g.page && score >= MATCH_SIMILARITY ? [{ i, j, score }] : [];
  })).sort((a, b) => b.score - a.score || a.i - b.i || a.j - b.j);

  const usedWant = new Set<number>();
  const usedGot  = new Set<number>();
  const pairs    = candidates.flatMap(({ i, j }): [Cited, Cited][] => {
    if (usedWant.has(i) || usedGot.has(j)) {
      return [];
    }
    else {
      usedWant.add(i);
      usedGot.add(j);
      return [[want[i]!, got[j]!]];
    }
  });

  return {
    pairs,
    missed: want.filter((_, i) => !usedWant.has(i)),
    extra:  got.filter((_, j) => !usedGot.has(j)),
  };
};

export const COMPARED_FIELDS = [
  'identifier', 'author', 'title', 'collection', 'issue', 'location',
] as const;
export type ComparedField = typeof COMPARED_FIELDS[number];

// The fields an expected citation may leave out, labelled before they were
// asked for: compared only when it has them (null for none)
const OPTIONAL_FIELDS: ComparedField[] = ['collection', 'issue'];

// How a batch's answer compares with what's expected: citations expected,
// answered and matched; of those matched, how many have each field expected
// (labelled: all but the optional fields left out) and right,
// and how many have every field and place right; and the places cited,
// expected, answered and found in both (of the citations matched, and of
// the others, all expected or all extra)
export type BatchScore = {
  expected: number,
  answered: number,
  matched: number,
  labelled: Record<ComparedField, number>,
  fields: Record<ComparedField, number>,
  exact: number,
  places: { expected: number, answered: number, matched: number },
};

// A field's text, "" for none
const fieldOf = (cited: Cited, field: ComparedField): string => (
  field === 'identifier' ? cited.identifier : cited.citation[field] ?? ''
);

// Whether an expected citation has a field to compare (see OPTIONAL_FIELDS)
const isLabelled = (cited: Cited, field: ComparedField): boolean => (
  field === 'identifier' || !OPTIONAL_FIELDS.includes(field) || cited.citation[field] !== undefined
);

const fieldCounts = (count: (f: ComparedField) => number): Record<ComparedField, number> => (
  Object.fromEntries(COMPARED_FIELDS.map((f) => [f, count(f)])) as Record<ComparedField, number>
);

export const scoreBatch = (expected: BatchFootnote[], answered: BatchFootnote[]): BatchScore => {
  const { pairs, missed, extra } = matchCitations(expected, answered);
  const placeCount               = (c: Cited) => placeKeys(c.citation.locationsCited).length;

  const compared = pairs.map(([w, g]) => {
    const wantPlaces = placeKeys(w.citation.locationsCited);
    const gotPlaces  = new Set(placeKeys(g.citation.locationsCited));
    const labelled   = COMPARED_FIELDS.filter((f) => isLabelled(w, f));
    const fields     = labelled.filter((f) => comparable(fieldOf(w, f)) === comparable(fieldOf(g, f)));
    const shared     = wantPlaces.filter((p) => gotPlaces.has(p)).length;

    return {
      labelled,
      fields,
      exact:  fields.length === labelled.length
        && shared === wantPlaces.length && shared === gotPlaces.size,
      places: { expected: wantPlaces.length, answered: gotPlaces.size, matched: shared },
    };
  });

  return {
    expected: pairs.length + missed.length,
    answered: pairs.length + extra.length,
    matched:  pairs.length,
    labelled: fieldCounts((f) => compared.filter((c) => c.labelled.includes(f)).length),
    fields:   fieldCounts((f) => compared.filter((c) => c.fields.includes(f)).length),
    exact:  compared.filter((c) => c.exact).length,
    places: {
      expected: compared.reduce((n, c) => n + c.places.expected, 0) + missed.reduce((n, c) => n + placeCount(c), 0),
      answered: compared.reduce((n, c) => n + c.places.answered, 0) + extra.reduce((n, c) => n + placeCount(c), 0),
      matched:  compared.reduce((n, c) => n + c.places.matched, 0),
    },
  };
};

export const NO_SCORE: BatchScore = {
  expected: 0,
  answered: 0,
  matched:  0,
  labelled: fieldCounts(() => 0),
  fields:   fieldCounts(() => 0),
  exact:    0,
  places:   { expected: 0, answered: 0, matched: 0 },
};

export const addScores = (a: BatchScore, b: BatchScore): BatchScore => ({
  expected: a.expected + b.expected,
  answered: a.answered + b.answered,
  matched:  a.matched + b.matched,
  labelled: fieldCounts((f) => a.labelled[f] + b.labelled[f]),
  fields:   fieldCounts((f) => a.fields[f] + b.fields[f]),
  exact:  a.exact + b.exact,
  places: {
    expected: a.places.expected + b.places.expected,
    answered: a.places.answered + b.places.answered,
    matched:  a.places.matched + b.places.matched,
  },
});

const ratio = (n: number, d: number): number => (d === 0 ? 0 : n / d);

const f1 = (precision: number, recall: number): number => (
  precision + recall === 0 ? 0 : 2 * precision * recall / (precision + recall)
);

// The measures of a total score, each 0 to 1: the citations found
// (precision, recall, F1), each field right of those matched that have it
// labelled, every field and
// place right of those expected (exact), and the places cited found
export const scoreSummary = (score: BatchScore) => {
  const precision      = ratio(score.matched, score.answered);
  const recall         = ratio(score.matched, score.expected);
  const placePrecision = ratio(score.places.matched, score.places.answered);
  const placeRecall    = ratio(score.places.matched, score.places.expected);

  return {
    citations: { precision, recall, f1: f1(precision, recall) },
    fields:    fieldCounts((f) => ratio(score.fields[f], score.labelled[f])),
    exact:  ratio(score.exact, score.expected),
    places: { precision: placePrecision, recall: placeRecall, f1: f1(placePrecision, placeRecall) },
  };
};
