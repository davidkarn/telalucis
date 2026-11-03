// Hugh of Saint Victor's De Sacramentis Christianae Fidei in Roy J. Deferrari's translation (On the
// Sacraments of the Christian Faith, 1951), OCR'd with surya: its pages numbered as Deferrari's
// are, each also cited by the book, part and chapter of the original on it (book_pages_to_citations;
// "De Sacram. II, 3, 12" is book 2, part 3, chapter 12), and the citations in its text, which
// are inline, in parentheses: "(Cf. Rom. 8, 28)", "(Psal. 132, 1)", "(Gen. 1, 4 and 6)". Pure
// functions; book_importers/hugh_sacraments.ts reads the OCR and saves the book.
//
// The work is two books, each a prologue and its parts (Book One twelve, Book Two eighteen), each
// part's chapters numbered from I ("XXVII. Why the works of foundation are recounted first").
// The OCR labels some of these headings as text, and misses some parts' headings entirely, so:
//   - a book begins at "Prologue of the First Book" or "Here beginneth the first book"; its
//     prologue is part 0 (its chapters cited "Prol. c. 6");
//   - a part heading is any block reading "Part Four", "Sixth Part", "PART XII";
//   - a chapter heading is a short block, a section header or wholly in italics or bold, beginning with
//     its number ("XIV.", or "1." for some parts' first);
//   - a chapter numbered I when the part already has chapters begins the next part, its heading
//     missed;
// The "General Index" after the work ends it. The right-hand pages' running heads ("Book One, Part Two") are compared with the parts
// read (see divisionCheck).
import type { Citation, Page, PageBlock } from '../types.ts';
import { strip_tags } from './book_pages.ts';
import { bibleCitation, biblePlaces, verseList } from './inline_bible_citations.ts';
import type { PlacedCitation } from './footnote_extraction.ts';
import type { CitationPart } from './summa_thml.ts';

export const HUGH_BOOK_ID = 'hugh-of-st-victor-de-sacramentis';
export const HUGH_TITLE = 'On the Sacraments of the Christian Faith (De Sacramentis)';
export const HUGH_AUTHOR = 'Hugh of Saint Victor';
export const HUGH_TRANSLATOR = 'Roy J. Deferrari';

// A place in the work: book 1 or 2, part (0 for a book's prologue), chapter (0 before the first)
export type Division = { book: number, part: number, chapter: number };

const ROMAN_VALUES: Record<string, number> = { i: 1, v: 5, x: 10, l: 50, c: 100 };

export const romanValue = (roman: string): number => [...roman.toLowerCase()].reduce((n, ch, i, all) => {
  const v    = ROMAN_VALUES[ch] ?? 0;
  const next = ROMAN_VALUES[all[i + 1] ?? ''] ?? 0;
  return n + (v < next ? -v : v);
}, 0);

const NUMBER_WORDS  = [
  'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve',
  'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen',
];
const ORDINAL_WORDS = [
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth',
  'eleventh', 'twelfth', 'thirteenth', 'fourteenth', 'fifteenth', 'sixteenth', 'seventeenth',
  'eighteenth',
];

// "four", "sixth", "XII", "12" -> 4, 6, 12, 12; null for anything else
export const numberWord = (word: string): number | null => {
  const w = word.toLowerCase().replace(/\.$/, '');
  const n = NUMBER_WORDS.indexOf(w) + 1 || ORDINAL_WORDS.indexOf(w) + 1;
  return n > 0 ? n
    : /^[ivxlc]+$/.test(w) ? romanValue(w)
      : /^\d+$/.test(w) ? Number(w)
        : null;
};

const text = (html: string) => strip_tags(html).replace(/\s+/g, ' ').trim();

// The book a block begins: "Prologue of the First Book on the Sacraments", "HERE BEGINNETH THE
// FIRST BOOK", "Prologue of theSecond Book" (its words run together)
export const bookHeading = (block: string): number | null => {
  const m = block.match(/^(?:prologue\s+of\s+the|here\s+beginneth\s+the)\s*(first|second)\s*book\b/i);
  return m ? ORDINAL_WORDS.indexOf(m[1]!.toLowerCase()) + 1 : null;
};

// The part a block heads: "PART FOUR", "SIXTH PART", "Sixteenth Part", "PART XII", "FOURTEENTH
// PART."
export const partHeading = (block: string): number | null => {
  const m = block.match(/^(?:part\s+(\w+)|(\w+)\s+part)\.?$/i);
  return m ? numberWord(m[1] ?? m[2]!) : null;
};

const MAX_HEADING = 300;

// A Roman numeral as chapters are numbered (to XLIX); not a misprint like "XVIIII"
const WELL_FORMED = /^(?:XL|L?X{0,3})(?:IX|IV|V?I{0,3})$/;

// What chapterHeading gives a chapter whose numeral is misprinted: it's taken to be the next
export const MISPRINTED = -1;

// The chapter a block heads ("XIV. On the knowledge...", "1. On the Cause of Man's Creation"):
// a section header, or a block wholly in italics or bold ("<b>XII. <i>That both...</i></b>"),
// beginning with its number
export const chapterHeading = (block: Pick<PageBlock, 'label' | 'html'>): number | null => {
  const t       = text(block.html);
  const styled  = /^\s*(?:<p>)?\s*<(b|i)>[\s\S]*<\/\1>\s*(?:<\/p>)?\s*$/.test(block.html);
  const m       = t.match(/^([IVXLC]+|\d{1,2})\.\s*["'“A-Z]/);
  const heading = block.label === 'SectionHeader' || styled;
  return m && heading && t.length <= MAX_HEADING
    ? (/^\d/.test(m[1]!) ? Number(m[1]) : WELL_FORMED.test(m[1]!) ? romanValue(m[1]!) : MISPRINTED)
    : null;
};

// The division a chapter heading begins, after `at`: the next chapter, for one whose numeral is
// misprinted; the next part's first, for a chapter I when the part already has chapters and no
// part heading came before it (partSeen); else the chapter numbered. warnings: those two, and a
// chapter out of order.
const nextChapter = (at: Division, chapter: number, partSeen: boolean): { at: Division, warnings: string[] } => {
  if (chapter === MISPRINTED) {
    return {
      at:       { ...at, chapter: at.chapter + 1 },
      warnings: [`a chapter's numeral misprinted, taken to be ${ at.chapter + 1 }`],
    };
  }
  else if (chapter === 1 && at.chapter > 1 && !partSeen) {
    return {
      at:       { book: at.book, part: at.part + 1, chapter: 1 },
      warnings: [`chapter I after ${ at.chapter }, no part heading: part ${ at.part + 1 } taken to begin`],
    };
  }
  else {
    const inOrder = chapter === at.chapter + 1 || (partSeen && chapter === 1);
    return {
      at:       { ...at, chapter },
      warnings: inOrder ? [] : [`chapter ${ chapter } after ${ at.chapter } (book ${ at.book }, part ${ at.part })`],
    };
  }
};

// Each page's divisions: the one it opens in (when it has text before its first heading) and
// each beginning on it, in order; none before the first book. warnings: chapters numbered out
// of order, parts begun by a chapter I without a heading.
export const pageDivisions = (pages: Page[]): {
  divisions: Map<number, Division[]>, warnings: string[],
} => {
  const divisions          = new Map<number, Division[]>();
  const warnings: string[] = [];
  let at: Division | null  = null;
  let partSeen             = false;

  for (const page of pages) {
    const own: Division[] = [];
    const add             = (d: Division) => {
      const last = own.at(-1);
      if (!last || last.book !== d.book || last.part !== d.part || last.chapter !== d.chapter) {
        own.push(d);
      }
    };

    for (const block of page.blocks.filter((b) => b.label !== 'PageHeader' && b.label !== 'PageFooter')) {
      const t       = text(block.html);
      const index   = /^general index$/i.test(t);
      const book    = index ? null : bookHeading(t);
      const part    = index || book !== null ? null : partHeading(t);
      const chapter = index || book !== null || part !== null ? null : chapterHeading(block);

      if (index) {
        at = null;
      }
      else if (book !== null) {
        at       = { book, part: 0, chapter: 0 };
        partSeen = true;
        add(at);
      }
      else if (at === null) {
        // front matter
      }
      else if (part !== null) {
        at       = { book: at.book, part, chapter: 0 };
        partSeen = true;
        add(at);
      }
      else if (chapter !== null) {
        const next = nextChapter(at, chapter, partSeen);
        at         = next.at;
        partSeen   = false;
        warnings.push(...next.warnings.map((w) => `scan ${ page.pageNumber }: ${ w }`));
        add(at);
      }
      else if (own.length === 0 && t.length > 0) {
        // text before the page's first heading: the division it opens in
        add(at);
      }
    }
    if (own.length > 0) {
      divisions.set(page.pageNumber, own);
    }
  }
  return { divisions, warnings };
};

// A division as a page's citation parts: book, part, chapter (the part alone before its first
// chapter)
export const divisionParts = (d: Division): CitationPart[] => [
  { type: 'book', value: d.book },
  { type: 'part', value: d.part },
  ...(d.chapter > 0 ? [{ type: 'chapter', value: d.chapter }] : []),
];

// Where the right-hand pages' running heads ("Book One, Part Two") disagree with the parts read:
// a running head names the part of the page's text
export const divisionCheck = (pages: Page[], divisions: Map<number, Division[]>): string[] => (
  pages.flatMap((page) => {
    const head = page.blocks.filter((b) => b.label === 'PageHeader').map((b) => text(b.html))
      .map((t) => t.match(/^book\s+(\w+),\s*part\s+(\w+)$/i)).find((m) => m !== null);
    const own  = divisions.get(page.pageNumber) ?? [];
    if (!head || own.length === 0) {
      return [];
    }
    else {
      const book = numberWord(head[1]!);
      const part = numberWord(head[2]!);
      return own.some((d) => d.book === book && d.part === part)
        ? []
        : [`scan ${ page.pageNumber }: running head "${ head[0] }", parts read `
          + own.map((d) => `${ d.book }.${ d.part }`).join(', ')];
    }
  })
);

// The inline citations ---------------------------------------------------------------------------

// What isn't a Bible place, for a citation all the same: an author, a title, then a book and
// lines ("Vergil, Aeneid, 6, 740-743", the only such)
const OTHER_WORK = /^(?:cf\.?\s+)?([A-Z][A-Za-z.]+(?:\s+[A-Z][A-Za-z.]+)*),\s+([A-Z][^,]+?),\s+([\d,\s.\-–]+)$/;

// The citations in a block's parentheses, on this page: a Bible citation per place (see
// core/inline_bible_citations.ts), and a work cited by author and title
export const inlineCitations = (bookId: string, pageNumber: number, html: string): Citation[] => {
  const source = { bookId, footnoteIdentifier: '', footnotePage: pageNumber };

  return [...text(html).matchAll(/\(([^()]{2,160})\)/g)].flatMap((m) => {
    const inner  = m[1]!.trim();
    const places = biblePlaces(inner);
    const other  = places.length === 0 ? inner.match(OTHER_WORK) : null;

    if (places.length > 0) {
      return places.map((p) => bibleCitation(source, p));
    }
    else if (other) {
      const [book, ...lines] = other[3]!.split(/\s*[,.]\s*/).filter((n) => n.length > 0);
      const lineNumbers      = verseList(lines.join(', ')).filter((n) => !Number.isNaN(n));
      return [{
        source,
        referenceBookId: null,
        author:          other[1]!,
        title:           other[2]!.trim(),
        location:        other[3]!.trim(),
        raw:             inner,
        locationsCited:  /^\d+$/.test(book ?? '')
          ? [[
              { rawLabel: book!, type: 'book', values: [Number(book)] },
              ...(lineNumbers.length > 0
                ? [{ rawLabel: lines.join(', '), type: 'line' as const, values: lineNumbers }]
                : []),
            ]]
          : [],
      }];
    }
    else {
      return [];
    }
  });
};

// The pages with their text blocks' inline citations on them
export const withInlineCitations = (bookId: string, pages: Page[]): Page[] => pages.map((page) => ({
  ...page,
  blocks: page.blocks.map((block) => (
    block.label === 'Text' || block.label === 'SectionHeader'
      ? { ...block, citations: [...block.citations, ...inlineCitations(bookId, page.pageNumber, block.html)] }
      : block
  )),
}));

// The pages with these citations in the blocks they're placed in (see placeCitations in
// core/footnote_extraction.ts: the footnotes' citations, extracted by an LLM)
export const withPlacedCitations = (pages: Page[], placed: PlacedCitation[]): Page[] => pages.map((page) => ({
  ...page,
  blocks: page.blocks.map((block, position) => {
    const own = placed.filter((p) => p.pageNumber === page.pageNumber && p.position === position);
    return own.length === 0 ? block : { ...block, citations: [...block.citations, ...own.map((p) => p.citation)] };
  }),
}));

// Each page's citation parts, a row per division on it (see pageDivisions)
export const pageCitationRows = (divisions: Map<number, Division[]>): { pageNumber: number, parts: CitationPart[] }[] => (
  [...divisions].flatMap(([pageNumber, own]) => own.map((d) => ({ pageNumber, parts: divisionParts(d) })))
);
