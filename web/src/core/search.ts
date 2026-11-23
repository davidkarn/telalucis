// The search page's pure helpers (see pages/search.ts)

// A run of a snippet's text, and whether it's one of the search term's words
export type SnippetPart = { text: string, marked: boolean };

const NAMED_ENTITIES: Record<string, string> = {
  amp: '&', lt: '<', gt: '>', quot: '"', apos: "'", nbsp: ' ',
};

// A snippet's HTML entities as the characters they stand for (&amp; &#39; &#x2019;), any it
// doesn't know left as written
export const decodeEntities = (s: string): string => s.replace(
  /&(#x[0-9a-f]+|#\d+|[a-z]+);/gi,
  (whole, name: string) => {
    const code = name[0] === '#'
      ? (name[1] === 'x' || name[1] === 'X' ? parseInt(name.slice(2), 16) : parseInt(name.slice(1), 10))
      : NaN;

    if (name[0] !== '#') {
      return NAMED_ENTITIES[name.toLowerCase()] ?? whole;
    }
    else if (Number.isInteger(code) && code > 0 && code <= 0x10ffff) {
      return String.fromCodePoint(code);
    }
    else {
      return whole;
    }
  },
);

// A snippet (GET /search's: its text, the term's words in <mark>) as runs of plain text, to be
// shown as text rather than set as HTML; anything else tag-like is left out
export const snippetParts = (snippet: string): SnippetPart[] => (
  snippet
    .split(/(<mark>.*?<\/mark>)/s)
    .map((piece) => {
      const marked = piece.startsWith('<mark>') && piece.endsWith('</mark>');
      const inner  = marked ? piece.slice('<mark>'.length, -'</mark>'.length) : piece;
      return { text: decodeEntities(inner.replace(/<[^>]*>?/g, '')), marked };
    })
    .filter((p) => p.text.length > 0)
);

// A journal issue's name for a result: "The New Scholasticism, vol. 35, no. 3 (1961-07)"
export const issueLabel = (issue: {
  journalName: string, volume: string | null, number: string | null, startDate: string,
}): string => (
  issue.journalName
    + (issue.volume === null ? '' : `, vol. ${issue.volume}`)
    + (issue.number === null ? '' : `, no. ${issue.number}`)
    + (issue.startDate.length >= 7 ? ` (${issue.startDate.slice(0, 7)})` : '')
);

// The page's ?page= as a page number from 1
export const pageOf = (raw: string | null): number => {
  const n = Number(raw);
  return Number.isInteger(n) && n >= 1 ? n : 1;
};
