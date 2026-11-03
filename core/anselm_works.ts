// The works of Anselm of Canterbury in an e-book of three older translations (The Complete Works
// of St. Anselm), OCR'd with surya: a book per work, a page per chapter (its pages in the e-book
// run together, their headings and pictures left out), each cited by its chapters (see
// book_pages_to_citations), with the Bible citations printed in parentheses in its text. Pure
// functions; book_importers/anselm_works.ts reads the OCR and saves the books.
//
// The e-book holds:
//   - St. Anselm's Book of Meditations and Prayers (tr. M. R., 1872): twenty-one meditations;
//   - The Devotions of Saint Anselm (tr. Clement C. J. Webb, 1903): the Proslogion (a preface and
//     twenty-six chapters), Anselm's preface to his meditations and four meditations (the first
//     in fourteen sections), four prayers, and five letters;
//   - Proslogium; Monologium; An Appendix in Behalf of the Fool by Gaunilon; and Cur Deus Homo
//     (tr. Sidney Norton Deane, 1903): the Proslogium, the Monologium (a preface and seventy-nine
//     chapters), Anselm's reply to Gaunilon (ten chapters), and Cur Deus Homo (a preface, then two
//     books of chapters).
// Each collection's introduction, Webb's note on the Proslogion and his note on the letters, the
// contents, the notes, and the chapters of the Bible the e-book appends (a page per reference)
// are left out: only Anselm's works are imported. So is Gaunilon's In Behalf of the Fool.
//
// Every heading is a SectionHeader block. The e-book heads each chapter with its collection's or
// work's name ("Monologium - On The Being Of God") on the page before it, then the chapter's
// heading ("CHAPTER XII"), then, in Deane's works, its title; a heading names its chapter in the
// work of the name before it.
import type { Citation, Page, PageBlock } from '../types.ts';
import { strip_tags } from './book_pages.ts';
import { bibleCitation, biblePlaces, type BiblePlace } from './inline_bible_citations.ts';
import type { CitationPart } from './summa_thml.ts';

export const ANSELM = 'Anselm of Canterbury';

const WEBB  = 'Clement C. J. Webb';
const DEANE = 'Sidney Norton Deane';

export type WorkKey =
  | 'meditationsAndPrayers' | 'proslogion' | 'meditations' | 'prayers' | 'letters' | 'proslogium'
  | 'monologium' | 'reply' | 'curDeusHomo';

export type AnselmWork = { id: string, title: string, translator: string };

export const ANSELM_WORKS: Record<WorkKey, AnselmWork> = {
  meditationsAndPrayers: {
    id: 'anselm-book-of-meditations-and-prayers', title: 'Book of Meditations and Prayers', translator: 'M. R.',
  },
  proslogion: {
    id:         'anselm-proslogion-webb',
    title:      'Proslogion, or Address to God concerning His Existence',
    translator: WEBB,
  },
  meditations: { id: 'anselm-meditations-webb', title: 'Meditations', translator: WEBB },
  prayers:     { id: 'anselm-prayers-webb', title: 'Prayers', translator: WEBB },
  letters:     { id: 'anselm-letters-of-spiritual-counsel', title: 'Letters of Spiritual Counsel', translator: WEBB },
  proslogium:  {
    id: 'anselm-proslogium', title: 'Proslogium, or Discourse on the Existence of God', translator: DEANE,
  },
  monologium: { id: 'anselm-monologium', title: 'Monologium', translator: DEANE },
  reply:      {
    id: 'anselm-reply-to-gaunilon', title: "In Reply to Gaunilon's Answer in Behalf of the Fool", translator: DEANE,
  },
  curDeusHomo: { id: 'anselm-cur-deus-homo', title: 'Cur Deus Homo', translator: DEANE },
};

const ROMAN_VALUES: Record<string, number> = { i: 1, v: 5, x: 10, l: 50, c: 100 };

export const romanValue = (roman: string): number => [...roman.toLowerCase()].reduce((n, ch, i, all) => {
  const v    = ROMAN_VALUES[ch] ?? 0;
  const next = ROMAN_VALUES[all[i + 1] ?? ''] ?? 0;
  return n + (v < next ? -v : v);
}, 0);

const toRoman = (n: number): string => (
  [[100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']] as [number, string][]
).reduce((out, [v, r]) => ({ rest: out.rest % v, text: out.text + r.repeat(Math.floor(out.rest / v)) }),
         { rest: n, text: '' }).text;

const ORDINALS = [
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh',
  'twelfth', 'thirteenth', 'fourteenth', 'fifteenth', 'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth',
  'twentieth',
];

// "FIRST", "TWENTY-FIRST" -> 1, 21
const ordinal = (word: string): number | null => {
  const w   = word.toLowerCase();
  const one = ORDINALS.indexOf(w) + 1;
  const m   = w.match(/^twenty-(\w+)$/);
  return one > 0 ? one : m ? 20 + (ORDINALS.indexOf(m[1]!) + 1 || NaN) || null : null;
};

// A place in a work: its key, and its divisions (chapter 0 for a preface; Cur Deus Homo's books;
// the sections of Webb's first meditation)
export type Place = { work: WorkKey, book?: number, chapter: number, section?: number };

// What a heading is: the name of a collection or work, heading the pages of its chapters
// (context: which work the numbered headings after it are in, null for a collection's own name);
// a heading beginning a place, or a part to leave out; or anything else (a chapter's title),
// kept in the page
export type Heading =
  | { kind: 'name', context: WorkKey | null, place?: Place }
  | { kind: 'place', place: Place }
  | { kind: 'leave out' }
  | { kind: 'text' };

// The books of the Bible as the e-book's appended chapters name them ("Song of Solomon 1",
// "1 Kings 2")
const BIBLE_CHAPTER = /^(?:[1-3] )?[A-Z][a-z]+(?: of [A-Z][a-z]+)? \d+$/;

const LEFT_OUT = [
  /^introduction$/i, /^table of contents$/i, /^contents$/i, /^footnotes$/i, /^introductory note$/i,
  /^note on the argument/i, /^anselm's philosophy/i, /^criticisms of anselm/i, /^bibliography/i,
  /^in behalf of the fool/i,
];

const NAMES: [RegExp, WorkKey | null][] = [
  [/^st\. anselm's book of meditations\s*and prayers$/i, 'meditationsAndPrayers'],
  [/^the devotions of saint anselm$/i, null],
  [/^proslogion or address to god/i, 'proslogion'],
  [/^prayers of st anselm$/i, 'prayers'],
  [/^letters of spiritual counsel$/i, 'letters'],
  [/^proslogium; monologium/i, null],
  [/^proslogium - or discourse/i, 'proslogium'],
  [/^monologium - on the being of god$/i, 'monologium'],
  [/^appendix$/i, 'reply'],
  [/^cur deus homo$/i, 'curDeusHomo'],
];

// A heading (its text), in the work of the last name before it (context)
export const classifyHeading = (heading: string, context: WorkKey | null, book: number): Heading => {
  const t       = heading.replace(/\[\d+\]/g, '').replace(/\s+/g, ' ').trim();
  const name    = NAMES.find(([pattern]) => pattern.test(t));
  const medit   = t.match(/^([\w-]+) meditation$/i);
  const webb    = t.match(/^meditation ([IVX]+)$/i);
  const chapter = t.match(/^(?:book (first|second): )?chapter ([IVXLC]+)(?: \([ab]\))?$/i);
  const roman   = t.match(/^([IVXL]+)$/);

  if (BIBLE_CHAPTER.test(t) || LEFT_OUT.some((p) => p.test(t))) {
    return { kind: 'leave out' };
  }
  else if (name) {
    return { kind: 'name', context: name[1] };
  }
  else if (medit && ordinal(medit[1]!) !== null) {
    return { kind: 'place', place: { work: 'meditationsAndPrayers', chapter: ordinal(medit[1]!)! } };
  }
  else if (/^preface to the meditations$/i.test(t)) {
    return { kind: 'place', place: { work: 'meditations', chapter: 0 } };
  }
  else if (webb) {
    // "Meditation I" heads each page of the first meditation's sections; "MEDITATION II" begins
    // the second
    const n = romanValue(webb[1]!);
    return n === 1
      ? { kind: 'name', context: 'meditations' }
      : { kind: 'place', place: { work: 'meditations', chapter: n } };
  }
  else if (/^in reply to gaunilon/i.test(t)) {
    return { kind: 'place', place: { work: 'reply', chapter: 0 } };
  }
  else if (context === null) {
    return { kind: 'text' };
  }
  else if (/^preface$/i.test(t)) {
    return { kind: 'place', place: { work: context, ...(context === 'curDeusHomo' ? { book: 0 } : {}), chapter: 0 } };
  }
  else if (chapter) {
    const inBook = chapter[1] ? ORDINALS.indexOf(chapter[1].toLowerCase()) + 1 : book;
    return {
      kind:  'place',
      place: { work: context, ...(context === 'curDeusHomo' ? { book: inBook } : {}), chapter: romanValue(chapter[2]!) },
    };
  }
  else if (roman && context === 'meditations') {
    return { kind: 'place', place: { work: 'meditations', chapter: 1, section: romanValue(roman[1]!) } };
  }
  else if (roman && (context === 'prayers' || context === 'letters')) {
    return { kind: 'place', place: { work: context, chapter: romanValue(roman[1]!) } };
  }
  else {
    return { kind: 'text' };
  }
};

// A place as its page's citation parts: chapter (0 a preface); Cur Deus Homo's book first, Webb's
// first meditation's section after
export const placeParts = (place: Place): CitationPart[] => [
  ...(place.book === undefined ? [] : [{ type: 'book', value: place.book }]),
  { type: 'chapter', value: place.chapter },
  ...(place.section === undefined ? [] : [{ type: 'position', value: place.section }]),
];

// A place as its page is labelled: "Preface", "Chapter XII", "Book II, Chapter III", "Meditation
// XXI", "Meditation I, Section IV", "Prayer II", "Letter V"
export const placeLabel = (place: Place): string => {
  const unit = place.work === 'meditationsAndPrayers' || place.work === 'meditations' ? 'Meditation'
    : place.work === 'prayers' ? 'Prayer'
      : place.work === 'letters' ? 'Letter'
        : 'Chapter';
  const main = place.chapter === 0
    ? 'Preface'
    : `${ place.book ? `Book ${ toRoman(place.book) }, ` : '' }${ unit } ${ toRoman(place.chapter) }`;
  return place.section === undefined ? main : `${ main }, Section ${ toRoman(place.section) }`;
};

const placeKey = (place: Place) => `${ place.work }|${ place.book ?? '' }|${ place.chapter }|${ place.section ?? '' }`;

export type WorkPage = Page & { citationParts: CitationPart[] };

// The e-book's footnote markers ("[37]"): their notes are left out
const NOTE_MARKER = /\[\d{1,3}\]/g;

// Each work's pages, a page per chapter (or section), its heading and then its blocks from the
// e-book's pages, in order, numbered from 1 in each work; the names heading them, the e-book's
// page headers and pictures, and the parts left out, left out. Blocks are those of build_pages.
export const anselmWorks = (pages: Page[]): Map<WorkKey, WorkPage[]> => {
  const works                 = new Map<WorkKey, Map<string, WorkPage>>();
  let context: WorkKey | null = null;
  let place: Place | null     = null;
  let book                    = 0;

  for (const block of pages.flatMap((p) => p.blocks)) {
    const heading: Heading = block.label === 'SectionHeader'
      ? classifyHeading(strip_tags(block.html), context, book)
      : { kind: 'text' };

    if (heading.kind === 'leave out') {
      place = null;
    }
    else if (heading.kind === 'name') {
      context = heading.context;
    }
    else if (heading.kind === 'place') {
      place   = heading.place;
      context = heading.place.work;
      book    = heading.place.book ?? book;
    }

    const kept = (heading.kind === 'text' || heading.kind === 'place') && place !== null
      && (block.label === 'Text' || block.label === 'SectionHeader');

    if (kept) {
      const pagesOf = works.get(place!.work) ?? new Map<string, WorkPage>();
      const key     = placeKey(place!);
      const page    = pagesOf.get(key) ?? {
        pageNumber:        pagesOf.size + 1,
        printedPageNumber: placeLabel(place!),
        blocks:            [],
        citationParts:     placeParts(place!),
      };
      page.blocks.push({ ...block, html: block.html.replace(NOTE_MARKER, ''), citations: [] });
      pagesOf.set(key, page);
      works.set(place!.work, pagesOf);
    }
  }

  return new Map([...works].map(([work, byKey]) => [work, [...byKey.values()]]));
};

// The Bible citations in a work's text, each in its block: "(ib.)" and a place without its book
// refer to the place cited before it in the work
export const withBibleCitations = (bookId: string, pages: WorkPage[]): WorkPage[] => {
  let previous: BiblePlace | null = null;

  return pages.map((page) => ({
    ...page,
    blocks: page.blocks.map((block): PageBlock => {
      const text      = strip_tags(block.html).replace(/\s+/g, ' ');
      const source    = { bookId, footnoteIdentifier: '', footnotePage: page.pageNumber };
      const citations = [...text.matchAll(/\(([^()]{2,120})\)/g)].flatMap((m): Citation[] => {
        const places = biblePlaces(m[1]!, previous);
        previous     = places.at(-1) ?? previous;
        return places.map((p) => bibleCitation(source, p));
      });
      return { ...block, citations: [...block.citations, ...citations] };
    }),
  }));
};
