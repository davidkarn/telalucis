// Bible citations printed inline, in parentheses, as translations of the Fathers and the
// schoolmen give them: "(Cf. Rom. 8, 28)", "(Psal. 132, 1)" (Deferrari's Hugh of Saint Victor),
// "(1 St. Pet. iv. 8.)", "(ib. viii. 10)", "(Ps. lxxxiii. 3)" (the translations of Anselm). Each
// place is read as the collection cites the Bible: the book's number in the Douay canon, the
// chapter, the verses (see core/bible.ts). Pure functions; core/hugh_sacraments.ts and
// core/anselm_works.ts read their books' parentheses with them.
import type { Citation } from '../types.ts';
import { BIBLE_BOOK_ID, bookName, printedBookNumber } from './bible.ts';

const ROMAN_VALUES: Record<string, number> = { i: 1, v: 5, x: 10, l: 50, c: 100 };

const romanValue = (roman: string): number => [...roman.toLowerCase()].reduce((n, ch, i, all) => {
  const v    = ROMAN_VALUES[ch] ?? 0;
  const next = ROMAN_VALUES[all[i + 1] ?? ''] ?? 0;
  return n + (v < next ? -v : v);
}, 0);

// A chapter's Roman numeral as chapters are numbered (to CL, the Psalms)
const WELL_FORMED = /^c?(?:xc|xl|l?x{0,3})(?:ix|iv|v?i{0,3})$/;

// A chapter's number as printed: Arabic, or Roman in either case, read through the OCR's
// slips: a lower-case l read as 1 ("1xviii") or as I in a lower-case numeral ("Iv", lv); null
// for a numeral that isn't one
const chapterNumber = (printed: string): number | null => {
  if (/^\d+$/.test(printed)) {
    return Number(printed);
  }
  else {
    const lower   = /[a-z]/.test(printed) ? printed.replace(/I/g, 'l') : printed;
    const numeral = lower.replace(/^1(?=[ivxlc])/i, 'l').toLowerCase();
    return numeral.length > 0 && WELL_FORMED.test(numeral) ? romanValue(numeral) : null;
  }
};

// "22, and 23", "4 and 6", "8-10", "1." -> [22, 23], [4, 6], [8, 9, 10], [1]; NaN for what
// isn't a number
export const verseList = (verses: string): number[] => verses
  .split(/\s*(?:,\s*and|,|and)\s*/)
  .map((v) => v.replace(/\.$/, '').trim())
  .filter((v) => v.length > 0)
  .flatMap((v) => {
    const range = v.match(/^(\d+)\s*[-–]\s*(\d+)$/);
    return range
      ? Array.from({ length: Number(range[2]) - Number(range[1]) + 1 }, (_, i) => Number(range[1]) + i)
      : /^\d+$/.test(v) ? [Number(v)] : [NaN];
  });

// A Bible book as printed ("1 Cor", "I. John", "2 Paralip", "1 St. Pet.", "St. John"): its
// number in the Douay canon, or null. The number is set off by a space or a period, so the I of
// "Isa." isn't one.
const bibleBook = (printed: string): number | null => {
  const m      = printed.trim().match(/^(?:(IV|I{1,3}|[1-4])(?:\.\s*|\s+))?(?:St\.\s*)?([A-Za-z]+)\.?,?$/);
  const number = m?.[1] === undefined ? '' : /\d/.test(m[1]) ? m[1] : String(romanValue(m[1]));
  return m ? printedBookNumber(`${ number } ${ m[2] }`.trim()) : null;
};

// A Bible book as printed, its number before it and a period or comma after: "Rom.", "1 Cor.",
// "I. John", "Wisdom,", "1 St. Pet."
const BOOK = String.raw`(?:(?:IV|I{1,3}|[1-4])(?:\.\s*|\s+))?(?:St\.\s*)?[A-Z][a-z]+\.?,?`;

// The book of the place before: "ib.", "Ib.", "ibid."
const IBID = String.raw`[Ii]b(?:id)?\.?,?`;

const CHAPTER = String.raw`\d+|[IVXLCivxlc1]+`;

// A Bible place as printed: book, chapter, verses ("Rom. 8, 28", "Gen. I, 4 and 6", "Eccli. 18.
// 1.", "John III, 21", "Acts 8", "Rom. viii. 32.", "ib. viii. 10"); or, without its book, a
// place in the book before it, chapter and verses ("24, 13" in "Matt. 10, 22; 24, 13")
const BIBLE_PLACE = new RegExp(
  String.raw`^(?:(${ IBID })\s*|(${ BOOK })\s+)?(${ CHAPTER })(?:\s*[,.]\s*(\d[\d\s,.\-–and]*?))?\.?$`,
);

// Between places: a semicolon or colon, or a comma or "and" before a book and its chapter
// ("Matt. 26, 41, Mark 14, 38", "Deut. 12, 16 and Levit. 7, 15")
const BETWEEN_PLACES = new RegExp(
  String.raw`\s*(?:[;:]|,|\band\b)\s*(?=(?:[Cc]f\.\s*)?(?:${ BOOK }|${ IBID })\s+[\dIVXLCivxlc])|\s*[;:]\s*`,
);

// What may follow a place, not part of it: "ff.", "etc."
const AFTER_PLACE = /\s*,?\s*(?:ff|etc)\.?$/i;

export type BiblePlace = { book: number, bookName: string, chapter: number, verses: number[], raw: string };

// The Bible places a parenthesis cites, without its "Cf." or "see": one or more (see
// BETWEEN_PLACES); none when any isn't one (it's an aside, or cites something else). previous:
// the place cited before it, which "ib." and a place without its book refer to ("(ib.)" alone is
// that place again, "ib. 39" its chapter's verse 39); a place within the parenthesis is before
// those after it.
export const biblePlaces = (inner: string, previous: BiblePlace | null = null): BiblePlace[] => {
  const bare = inner.replace(/^\s*(?:cf\.?|see)\s+/i, '').trim();

  if (new RegExp(`^${ IBID }$`).test(bare)) {
    return previous === null ? [] : [{ ...previous, raw: bare }];
  }
  else {
    const places = bare.split(BETWEEN_PLACES).reduce<(BiblePlace | null)[]>((out, part) => {
      const raw    = part.replace(/^\s*cf\.?\s+/i, '').replace(AFTER_PLACE, '').trim();
      const m      = raw.match(BIBLE_PLACE);
      const before = out.length > 0 ? out.at(-1) ?? null : previous;
      const named  = m?.[2];
      const book   = named !== undefined ? bibleBook(named) : before?.book ?? null;
      const name   = named?.trim() ?? before?.bookName ?? null;
      // "ib. 39": a verse of the chapter cited before ("ib. viii." is a chapter)
      const ibVerse = m?.[1] !== undefined && m[4] === undefined && /^\d+$/.test(m[3]!) && before !== null;
      const chapter = ibVerse ? before!.chapter : m ? chapterNumber(m[3]!) : null;
      const verses  = ibVerse ? [Number(m![3])] : m?.[4] === undefined ? [] : verseList(m[4]);
      // without its book (or "ib."), a place needs its verses: "(14)" is no place
      const sure = named !== undefined || m?.[1] !== undefined || verses.length > 0;

      return [
        ...out,
        !m || !sure || book === null || name === null || chapter === null || verses.some((v) => Number.isNaN(v))
          ? null
          : { book, bookName: name, chapter, verses, raw },
      ];
    }, []);
    return places.length > 0 && places.every((p) => p !== null) ? places as BiblePlace[] : [];
  }
};

// A place as a citation of the Bible, pointing at whichever copy of it the collection holds
export const bibleCitation = (source: Citation['source'], place: BiblePlace): Citation => ({
  source,
  referenceBookId: BIBLE_BOOK_ID,
  author:          'Bible',
  title:           bookName(place.book),
  location:        place.raw.startsWith(place.bookName) ? place.raw.slice(place.bookName.length).trim() : place.raw,
  raw:             place.raw,
  locationsCited:  [[
    { rawLabel: place.bookName, type: 'book', values: [place.book] },
    { rawLabel: String(place.chapter), type: 'chapter', values: [place.chapter] },
    ...(place.verses.length > 0
      ? [{ rawLabel: place.verses.join(', '), type: 'verse' as const, values: place.verses }]
      : []),
  ]],
});
