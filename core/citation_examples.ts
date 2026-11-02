// Worked examples for the footnote extraction prompt: citations already in
// the collection like those in a batch's footnotes, each its text as printed
// and the author, title and location extracted from it, so that a model shown
// "Sent. IV, d. 17" reads it as the citations before it were read. Found by
// the words they share (BM25 over the citations' raw text: abbreviations like
// "Sent.", "Opusc.", "Migne" are rare, so weigh most), among citations
// linked to a book in the collection, whose title their text bears out (see
// titleInRaw): some were read wrong ("In II Sent." as the Summa), and an
// example teaches its mistake. Pure functions; model/citation_examples.ts reads the citations.

export type CitationExample = {
  id: string,
  sourceBookId: string,
  raw: string,
  author: string,
  title: string,
  location: string,
};

export type ExampleIndex = {
  examples: CitationExample[],
  // each example's words, with how often each occurs in it
  terms: Map<string, number>[],
  // in how many examples each word occurs
  documentFrequency: Map<string, number>,
  averageLength: number,
};

// BM25's constants, the usual ones
const K1 = 1.2;
const B  = 0.75;

// Words too common in citations to tell them apart
const COMMON = new Set([
  'the', 'and', 'of', 'in', 'on', 'to', 'a', 'de', 'la', 'le', 'et', 'des', 'du', 'der', 'die', 'und',
  'cf', 'cfr', 'see', 'p', 'pp', 'ibid', 'ib', 'op', 'cit', 'loc', 'vol', 'ed', 'n', 'nn', 'f', 'ff',
  'sqq', 'seq', 'also', 'st', 's',
]);

// A Roman numeral of two letters or more, of i, v and x (one letter is as
// likely an abbreviation: "d." for distinctio, "c." for caput; one with l or
// c, a word: "civ." for civitate)
const ROMAN = /^[ivx]{2,}$/;

// A text's words, lowercased, accents folded, numbers (Roman ones too) and
// common words left out ("Sent. IV, d. 17, q. 3" -> sent, d, q)
export const citationTerms = (text: string): string[] => (
  text.normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase()
    .split(/[^a-z]+/)
    .filter((w) => w.length > 0 && !COMMON.has(w) && !ROMAN.test(w))
);

const counts = (words: string[]): Map<string, number> => (
  words.reduce((m, w) => m.set(w, (m.get(w) ?? 0) + 1), new Map<string, number>())
);

// Whether a citation's text bears out the title read from it: a word of the
// text (of 3 letters or more) begins a word of the title, or the other way
// round ("De civ. D." -> "De civitate Dei", "Metaph." -> "Metaphysics"; not
// "In II Sent." -> "Summa Theologica", "Cf. above note 2" -> anything)
export const titleInRaw = (raw: string, title: string): boolean => {
  const long  = (text: string) => citationTerms(text).filter((w) => w.length >= 3);
  const words = long(title);
  return long(raw).some((r) => words.some((t) => t.startsWith(r) || r.startsWith(t)));
};

// The index of the examples whose text bears out their title (see titleInRaw)
export const exampleIndex = (all: CitationExample[]): ExampleIndex => {
  const examples = all.filter((e) => titleInRaw(e.raw, e.title));
  const terms             = examples.map((e) => counts(citationTerms(e.raw)));
  const documentFrequency = new Map<string, number>();
  for (const t of terms) {
    for (const w of t.keys()) {
      documentFrequency.set(w, (documentFrequency.get(w) ?? 0) + 1);
    }
  }
  const lengths = terms.map((t) => [...t.values()].reduce((a, b) => a + b, 0));

  return {
    examples,
    terms,
    documentFrequency,
    averageLength: lengths.reduce((a, b) => a + b, 0) / Math.max(1, lengths.length),
  };
};

// The examples most like a footnote's text, best first, at most `limit`, one
// to a work (by author and title), none from the book being extracted
export const similarExamples = (
  index: ExampleIndex, text: string, { limit, excludeBookId }: { limit: number, excludeBookId?: string },
): CitationExample[] => {
  const query = [...new Set(citationTerms(text))];
  const n     = index.examples.length;
  const idf   = (w: string) => {
    const df = index.documentFrequency.get(w) ?? 0;
    return Math.log(1 + (n - df + 0.5) / (df + 0.5));
  };

  const scored: { i: number, score: number }[] = [];
  index.terms.forEach((terms, i) => {
    if (index.examples[i]!.sourceBookId !== excludeBookId) {
      const length = [...terms.values()].reduce((a, b) => a + b, 0);
      const score  = query.reduce((sum, w) => {
        const tf = terms.get(w) ?? 0;
        return tf === 0 ? sum : sum + idf(w) * (tf * (K1 + 1)) / (tf + K1 * (1 - B + B * length / index.averageLength));
      }, 0);
      if (score > 0) {
        scored.push({ i, score });
      }
    }
  });

  const seen = new Set<string>();
  return scored.sort((a, b) => b.score - a.score)
    .map((s) => index.examples[s.i]!)
    .filter((e) => {
      const work = `${ e.author }|${ e.title }`.toLowerCase();
      return seen.has(work) ? false : (seen.add(work), true);
    })
    .slice(0, limit);
};

// The examples for a batch: those most like each of its footnotes (perFootnote
// each), at most `limit` in all, one to a work
export const batchExamples = (
  index: ExampleIndex, footnotes: string[],
  { perFootnote = 2, limit = 12, excludeBookId }: { perFootnote?: number, limit?: number, excludeBookId?: string },
): CitationExample[] => {
  const seen = new Set<string>();
  return footnotes
    .flatMap((f) => similarExamples(index, f, { limit: perFootnote, ...(excludeBookId ? { excludeBookId } : {}) }))
    .filter((e) => (seen.has(e.id) ? false : (seen.add(e.id), true)))
    .slice(0, limit);
};

// The examples as a part of the prompt; '' when there are none
export const examplesPart = (examples: CitationExample[]): string => (
  examples.length === 0
    ? ''
    : `\nCitations in other books like these were read as follows. They show how such abbreviations are read;
the same abbreviation may stand for another author's work here, so take the author from these footnotes:\n${
      examples.map((e) => `- ${ JSON.stringify(e.raw) } -> author ${ JSON.stringify(e.author) }, title ${
        JSON.stringify(e.title) }, location ${ JSON.stringify(e.location) }`).join('\n')
    }\n`
);
