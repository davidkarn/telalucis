// The English works of Aquinas in the Aquinas Opera Omnia mirror of
// dhspriory.org (../AquinasOperaOmnia, see core/aquinas_works.ts) as book
// pages, with the citations in them. Pure functions;
// cli/commands/import_aquinas.command.ts reads the files and saves the books.
//
// The files were edited by many hands, so little can be assumed of their
// markup:
// - Most open with a contents list linking to anchors in the text (<a
//   href="#11">1-1: 1 Cor 1:1-9</a> ... <a name="11">): a page starts at the
//   first of each anchor the contents link to, and at each heading naming one
//   of the work's divisions ("LECTURE 3", "ARTICLE II", "Question One"), as
//   some files have anchors for only part of their text. Long stretches are
//   cut into pages of about MAX_PAGE_CHARS.
// - A page is cited by the divisions it is in (book, question, article ...):
//   those of its file (Physics3.htm is book 3), then those named in its
//   contents entry or heading, then the number of its anchor.
// - Files with the Latin beside the English (a table row each) keep the English
//   column.
// - Translators' footnotes are JavaScript strings (var WPFootnote12 = '...'),
//   shown by links in the text; they become footnotes.
// - Citations are found by pattern (see blockCitations): scripture in
//   parentheses, works in parentheses or brackets ("(De Trin. IV)", "[Cf. S.T.,
//   I. 16, aa. 1, 3; I Sent., 19, 5, 1]"), and those in footnotes, each given a
//   footnote, as the ThML importers do (core/thml.ts). Works naming their
//   sources in prose are read by an LLM as well (see PROSE_CITATIONS_FORMAT).
import { parse } from 'node-html-parser';
import type { Citation, CitationLocation, Page, PageBlock } from '../types.ts';
import type { ORResponseFormat } from '../lib/open_router.ts';
import { printedBookNumber } from './bible.ts';
import { LOCATION_TYPES } from './footnote_extraction.ts';
import type { CitationPart } from './summa_thml.ts';
import { scripRefCitations, workReferences, type WorkReference } from './thml.ts';

// A division a work's pages are cited by
export type DivisionType = 'book' | 'distinction' | 'question' | 'article' | 'chapter' | 'lecture';

export type AquinasFile = {
  // relative to the Opera Omnia folder ("english/Physics3.htm")
  path: string,
  // the divisions the whole file is in (Physics3.htm: book 3)
  parts?: CitationPart[],
  // Latin and English side by side, a table row each: the English is the second
  // cell
  bilingual?: boolean,
};

export type AquinasWork = {
  id: string,
  title: string,
  author?: string,
  translator: string | null,
  files: AquinasFile[],
  // the divisions its pages are cited by, the largest first
  levels: DivisionType[],
  // for a commentary on a book of the Bible: the book, as printed ("1 Cor"),
  // for its bare references ("(14:28)") to verses of it
  bible?: string,
  // whether its sources are mostly named in prose, so an LLM reads its pages
  // for them
  llm?: boolean,
  // the divisions an anchor's name stands for, when it isn't the number of the
  // smallest ("21L": book 2, lecture 1)
  anchorParts?: (name: string) => CitationPart[],
};

export type AquinasPage = Page & { citationParts: CitationPart[] };

// About how much text a page holds, when nothing divides a stretch of a work
// more finely
export const MAX_PAGE_CHARS = 12000;

const MAX_LABEL = 100;

// A private-use character, which the files don't use, around an anchor's name
// where it was
const ANCHOR = '\ue010';

const escapeHtml = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
const squash     = (s: string) => s.replace(/\s+/g, ' ').trim();

// An HTML fragment's text, entities decoded
export const textOf = (html: string): string => squash(parse(html.replace(/<br\s*\/?>/gi, ' ')).text);

const block = (label: PageBlock['label'], html: string, citations: Citation[] = []): PageBlock => (
  { bbox: [0, 0, 0, 0], label, html, citations }
);

// --- Numbers -----------------------------------------------------------------

const ROMAN_VALUES: Record<string, number> = { i: 1, v: 5, x: 10, l: 50, c: 100, d: 500, m: 1000 };

const WORD_NUMBERS: Record<string, number> = {
  one:       1,
  two:       2,
  three:     3,
  four:      4,
  five:      5,
  six:       6,
  seven:     7,
  eight:     8,
  nine:      9,
  ten:       10,
  eleven:    11,
  twelve:    12,
  thirteen:  13,
  fourteen:  14,
  fifteen:   15,
  sixteen:   16,
  seventeen: 17,
  eighteen:  18,
  nineteen:  19,
  twenty:    20,
  thirty:    30,
  forty:     40,
  fifty:     50,
};

const ORDINALS: Record<string, number> = {
  first:      1,
  second:     2,
  third:      3,
  fourth:     4,
  fifth:      5,
  sixth:      6,
  seventh:    7,
  eighth:     8,
  ninth:      9,
  tenth:      10,
  eleventh:   11,
  twelfth:    12,
  thirteenth: 13,
  fourteenth: 14,
  fifteenth:  15,
};

// "14", "XIV", "xiv", "Twenty-Nine" as a number; null for anything else
export const numberOf = (token: string): number | null => {
  const t = token.toLowerCase().replace(/[.,:]$/, '');

  if (/^\d+$/.test(t)) {
    return Number(t);
  }
  else if (/^[ivxlcdm]+$/.test(t)) {
    return [...t].reduce((n, ch, i) => {
      const v    = ROMAN_VALUES[ch]!;
      const next = ROMAN_VALUES[t[i + 1] ?? ''] ?? 0;
      return n + (v < next ? -v : v);
    }, 0);
  }
  else if (t.split('-').every((w) => WORD_NUMBERS[w] !== undefined)) {
    return t.split('-').reduce((n, w) => n + WORD_NUMBERS[w]!, 0);
  }
  else {
    return null;
  }
};

const ROMAN_DIGITS: [number, string][] = [
  [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I'],
];

const toRoman = (n: number): string => ROMAN_DIGITS.reduce(
  (out, [value, digits]) => ({
    rest: out.rest % value,
    text: out.text + digits.repeat(Math.floor(out.rest / value)),
  }),
  { rest: n, text: '' },
).text;

// --- Divisions ---------------------------------------------------------------

const DIVISION_WORDS: Record<string, DivisionType> = {
  book:        'book',
  liber:       'book',
  quodlibet:   'book',
  distinction: 'distinction',
  question:    'question',
  article:     'article',
  articulus:   'article',
  chapter:     'chapter',
  caput:       'chapter',
  psalm:       'chapter',
  lecture:     'lecture',
  lesson:      'lecture',
  lectio:      'lecture',
};

const WORDS    = Object.keys(WORD_NUMBERS).join('|');
const DIVIDED  = Object.keys(DIVISION_WORDS).join('|');
const NTH      = Object.keys(ORDINALS).join('|');
const NUMBER   = String.raw`\d+|[ivxlcIVXLC]+\b|(?:${ WORDS })(?:-(?:${ WORDS }))?`;
const DIVISION = new RegExp(String.raw`\b(${ DIVIDED })\s+(${ NUMBER })`, 'gi');
const ORDINAL  = new RegExp(String.raw`\b(${ NTH })\s+(${ DIVIDED })\b`, 'gi');
// a heading starting with a division: "LECTURE 3", "The First Lesson"
const DIVISION_START             = new RegExp(String.raw`^(?:the\s+)?(?:(?:${ NTH })\s+)?(?:${ DIVIDED })\b`, 'i');
const ALL_LEVELS: DivisionType[] = ['book', 'distinction', 'question', 'article', 'chapter', 'lecture'];
// "1-1: 1 Cor 1:1-9": chapter 1, lecture 1 of a commentary on scripture
const CHAPTER_LECTURE = /^\s*(\d+)\s*-\s*(\d+)\s*:/;

// The divisions a contents entry or heading names, of the types a work is cited
// by: "BOOK I LECTURE 3" -> book 1, lecture 3; "The First Lesson" -> lecture 1;
// "Question One: Truth" -> question 1
export const divisionsIn = (text: string, levels: DivisionType[]): CitationPart[] => {
  const found = new Map<string, number>();
  const pair  = text.match(CHAPTER_LECTURE);

  if (pair && levels.includes('chapter') && levels.includes('lecture')) {
    found.set('chapter', Number(pair[1]));
    found.set('lecture', Number(pair[2]));
  }
  for (const m of text.matchAll(DIVISION)) {
    const type = DIVISION_WORDS[m[1]!.toLowerCase()]!;
    const n    = numberOf(m[2]!);
    if (n !== null && n > 0 && !found.has(type)) {
      found.set(type, n);
    }
  }
  for (const m of text.matchAll(ORDINAL)) {
    const type = DIVISION_WORDS[m[2]!.toLowerCase()]!;
    if (!found.has(type)) {
      found.set(type, ORDINALS[m[1]!.toLowerCase()]!);
    }
  }

  return levels.flatMap((type) => (found.has(type) ? [{ type, value: found.get(type)! }] : []));
};

// Whether a block is a heading naming a division ("LECTURE 3", "ARTICLE II:
// Whether ...")
const isDivisionHeading = (text: string) => (
  text.length <= 120 && DIVISION_START.test(text) && divisionsIn(text, ALL_LEVELS).length > 0
);

const LABELS: Record<DivisionType, string> = {
  book:        'Book',
  distinction: 'Distinction',
  question:    'Question',
  article:     'Article',
  chapter:     'Chapter',
  lecture:     'Lecture',
};

// "Book III, Lecture 4"
export const labelOf = (parts: CitationPart[]): string => parts.map((p) => (
  `${ LABELS[p.type as DivisionType] ?? p.type } ${ p.type === 'book' ? toRoman(p.value) : p.value }`
)).join(', ');

// --- Reading a file ----------------------------------------------------------

// The translator's footnotes, by their variable names: var WPFootnote12 = '...\
// ...'; (JavaScript strings, continued over lines by a backslash)
export const wpFootnotes = (html: string): Map<string, string> => new Map(
  [...html.matchAll(/var\s+(WPFootnote\d+)\s*=\s*'((?:[^'\\]|\\[\s\S])*)'/g)].map((m) => [
    m[1]!,
    m[2]!.replace(/\\\r?\n/g, ' ').replace(/\\(['"/\\])/g, '$1'),
  ]),
);

// A bilingual file's English: each table row of two cells or more without its
// first cell (the Latin)
export const englishColumn = (html: string): string => html.replace(
  /(<tr\b[^>]*>)([\s\S]*?)(?=<tr\b|<\/table|$)/gi,
  (_, open: string, row: string) => {
    const cells = [...row.matchAll(/<t[dh]\b/gi)];
    return cells.length >= 2 ? open + row.slice(cells[1]!.index) : open + row;
  },
);

// The tags kept in a block's HTML: text markup, and <note> and <scripRef>, read
// by scripRefCitations
const INLINE = /^(?:i|em|b|strong|sup|sub|br|note|scripref|u)$/i;

const BLOCK_TAGS = /<\/?(?:p|h[1-6]|li|dd|dt|div|tr|td|th|blockquote|center|table|tbody|thead|ul|ol|dl|hr|pre|body|html)\b[^>]*>/gi;

// HTML with only inline markup kept
const inlineHtml = (html: string) => squash(html
  .replace(/<img\b[^>]*>/gi, '')
  .replace(/<\/?([a-zA-Z][\w-]*)\b[^>]*>/g, (tag, name: string) => {
    if (!INLINE.test(name)) {
      return '';
    }
    else if (/^br$/i.test(name)) {
      return '<br/>';
    }
    else {
      return tag.startsWith('</') ? `</${ name.toLowerCase() }>` : tag.replace(/\s+style="[^"]*"/i, '');
    }
  })
  .replace(/<(i|em|b|strong|u)>\s*<\/\1>/g, ' '))
  .replace(/^(?:<br\/>\s*)+|(?:\s*<br\/>)+$/g, '');

// Words common in Latin and in English, to tell them apart
const LATIN_WORDS   = new Set(('quod est et enim autem sed quia sicut non cum ad ex per vel etiam '
  + 'ergo igitur unde dicitur esse sunt ut eius ipsa ipsum huius secundum').split(' '));
const ENGLISH_WORDS = new Set(
  'the and of is that to which it as be not for by this are with from'.split(' ')
);

// Whether a block's text is Latin: some English files give the Latin as well,
// in paragraphs of its own
export const isLatin = (text: string): boolean => {
  const words   = text.toLowerCase().match(/[a-z]+/g) ?? [];
  const latin   = words.filter((w) => LATIN_WORDS.has(w)).length;
  const english = words.filter((w) => ENGLISH_WORDS.has(w)).length;
  return words.length >= 8 && latin >= 3 && latin > english * 2;
};

// A raw block of a file: an anchor the contents link to, or a stretch of text
// and its markup
type RawBlock =
  | { kind: 'anchor', name: string }
  | { kind: 'text', tag: string, html: string, text: string };

// The anchors a file's contents link to, with the entries' text: href="#12"
// or href="ThisFile.htm#12". Links to footnotes ("[3]", "(2)", a number in
// <sup>) aren't contents.
export const contentsTargets = (html: string, fileName: string): Map<string, string> => {
  const targets = new Map<string, string>();

  for (const m of html.matchAll(/<a\b[^>]*href="([^"#]*)#([^"]+)"[^>]*>([\s\S]*?)<\/a>/gi)) {
    const file     = decodeURIComponent(m[1]!.split('/').pop() ?? '');
    const text     = textOf(m[3]!);
    const footnote = /^\s*[[(]\s*\d{1,3}\s*[\])]\s*$/.test(text)
      || (/^\d{1,3}$/.test(text) && (/<sup/i.test(m[3]!) || /<sup>\s*$/i.test(html.slice(m.index - 10, m.index))));

    if ((file === '' || file === fileName) && !footnote && text.length > 0 && !targets.has(m[2]!)) {
      targets.set(m[2]!, text);
    }
  }

  return targets;
};

// The blocks of a file, in order: each paragraph, heading or table cell with
// text (contents lists, mostly links, left out), and the anchors the contents
// link to. Footnotes' links are replaced by their notes, as <note> elements
// (see scripRefCitations).
export const fileBlocks = (raw: string, fileName: string, bilingual = false): RawBlock[] => {
  const notes   = wpFootnotes(raw);
  const targets = contentsTargets(raw, fileName);
  const seen    = new Set<string>();
  const body    = (bilingual ? englishColumn : (h: string) => h)(raw
    .replace(/<!--[\s\S]*?-->/g, '')
    .replace(/<(script|style|head|title)\b[\s\S]*?<\/\1>/gi, ''))
    .replace(
      /<a\b[^>]*href="javascript:WPShow\('(WPFootnote\d+)'[^"]*"[^>]*>[\s\S]*?<\/a>/gi,
      (_, id: string) => (
        notes.has(id) ? `<note>${ inlineHtml(notes.get(id)!.replace(BLOCK_TAGS, ' ')) }</note>` : ''
      ),
    )
    // only an anchor's first place: some files repeat it on each paragraph
    .replace(/<a\b[^>]*\b(?:name|id)="([^"]+)"[^>]*>/gi, (tag, name: string) => {
      if (targets.has(name) && !seen.has(name)) {
        seen.add(name);
        return `<p>${ ANCHOR }${ name }${ ANCHOR }</p>`;
      }
      else {
        return tag;
      }
    });

  const blocks: RawBlock[] = [];
  const opens              = [...body.matchAll(BLOCK_TAGS)];

  opens.forEach((open, i) => {
    const inner = body.slice(open.index + open[0].length, opens[i + 1]?.index ?? body.length);
    const tag   = open[0].match(/^<\/?([a-z0-9]+)/i)![1]!.toLowerCase();
    const mark  = inner.match(new RegExp(`^\\s*${ ANCHOR }(.+?)${ ANCHOR }\\s*$`));

    if (mark) {
      blocks.push({ kind: 'anchor', name: mark[1]! });
    }
    else if (!open[0].startsWith('</') || inner.trim().length > 0) {
      // links' share of the text: a contents entry is all link
      const linked = [...inner.matchAll(/<a\b[^>]*href=[^>]*>([\s\S]*?)<\/a>/gi)]
        .reduce((n, m) => n + textOf(m[1]!).length, 0);
      const html   = inlineHtml(inner);
      const text   = textOf(html.replace(/<note>[\s\S]*?<\/note>/g, ''));

      if (text.length > 0 && linked < text.length * 0.6 && !isLatin(text)) {
        blocks.push({ kind: 'text', tag: /^h[1-6]$/.test(tag) ? tag : 'p', html, text });
      }
    }
  });

  return blocks;
};

// --- Citations ---------------------------------------------------------------

// One reference to scripture as printed: "Wis 7:27", "1 Cor. 15:3-4",
// "Exod, 3:14", "14:28", "Ps 18:1, 3"
const SCRIPTURE = /^\s*(?:cf\.\s*|see(?:\s+also)?\s+)?(?:((?:[1-4]\s*)?[A-Za-z]+)[.,-]?\s*\s)?(\d+)\s*:\s*(\d+[a-z]?(?:\s*[-–]\s*\d+[a-z]?)?(?:\s*,\s*\d+(?:\s*[-–]\s*\d+)?)*)\s*\.?\s*$/;

// One with its chapter in Roman numerals, and its verse after a comma or
// stop, if any: "2 Tim. iii.", "1 Tim. vi. 8", "Deut. xxxii, 4". Only with a
// book named, as "(Ethic. ii, 1)" looks the same.
const SCRIPTURE_ROMAN = /^\s*(?:cf\.\s*|see(?:\s+also)?\s+)?((?:[1-4]\s*)?[A-Za-z]+)\.?\s+([ivxlc]+)\.?(?:[,.]?\s*(\d+(?:\s*[-–]\s*\d+)?))?\s*\.?\s*$/i;

// A reference's book (or undefined), chapter and verses ("" for none), as
// printed
const scriptureReference = (part: string): [string, string | undefined, string, string] | null => {
  const m = part.match(SCRIPTURE);
  const r = m ? null : part.match(SCRIPTURE_ROMAN);

  if (m) {
    return [m[0], m[1], m[2]!, m[3]!];
  }
  else if (r && bibleBook(r[1]!) !== null) {
    return [r[0], r[1], String(numberOf(r[2]!)), r[3] ?? ''];
  }
  else {
    return null;
  }
};

// Names of books printedBookNumber doesn't know, as it knows them
const BOOK_ALIASES: Record<string, string> = {
  ez: 'Ezek', sir: 'Ecclus', sirach: 'Ecclus', revelation: 'Apoc', samuel: 'Kings', song: 'Cant',
};

// A book of the Bible as printed ("Sirach", "1 Samuel"), as printedBookNumber
// reads it, or null
const bibleBook = (printed: string): string | null => {
  const m    = printed.trim().match(/^([1-4]\s*)?([A-Za-z]+)$/);
  const name = m ? `${ m[1] ?? '' }${ BOOK_ALIASES[m[2]!.toLowerCase()] ?? m[2] }` : printed;
  return printedBookNumber(name) === null ? null : name;
};

// The book of the Bible named last in some text, when it's in its last
// dozen words: "as Isaiah says (40:18)", "it is written in 1 Corinthians"
const bookNamedIn = (text: string): string | null => {
  const words = text.split(/\s+/).slice(-12);
  for (let i = words.length - 1; i >= 0; i--) {
    const word   = words[i]!.replace(/[^A-Za-z]/g, '');
    const number = /^[1-4]$/.test(words[i - 1] ?? '') ? `${ words[i - 1] } ` : '';
    const book   = /^[A-Z]/.test(word) ? bibleBook(number + word) ?? bibleBook(word) : null;
    if (book) {
      return book;
    }
  }
  return null;
};

// Scripture cited in parentheses marked up as ThML's <scripRef>s, for
// scripRefCitations: each ";"-separated part of a parenthesis that is a
// reference. A part without a book is in the book of the part before it, or
// the book named just before the parenthesis, or, in a commentary on a book of
// the Bible (bible), in that book.
export const markScripture = (html: string, bible?: string): string => html.replace(
  /([([])([^()[\]<>]{3,200})([)\]])/g,
  (whole, open: string, inner: string, close: string, at: number) => {
    let book: string | null | undefined;
    let found   = 0;
    const named = () => bookNamedIn(textOf(html.slice(Math.max(0, at - 100), at))) ?? bible ?? null;
    const parts = inner.split(/(;\s*)/).map((part) => {
      const m = scriptureReference(part);
      const b = m?.[1] ? bibleBook(m[1]) : null;

      if ((open === '(') !== (close === ')')) {
        return part;
      }
      else if (!m || (m[1] && !b)) {
        return part;
      }
      else {
        book = b ?? book ?? named();
        if (!book) {
          return part;
        }
        else {
          found++;
          const verses = m[3].replace(/[a-z]/g, '');
          return `<scripRef passage="${ book } ${ m[2] }${ verses ? ':' + verses : '' }">${ part }</scripRef>`;
        }
      }
    });

    return found > 0 ? `${ open }${ parts.join('') }${ close }` : whole;
  },
);

// Capitalized words before a bracket that aren't a title ("First [y, aa]" is an
// outline's letter)
const NOT_TITLES = new Set([
  'First', 'Second', 'Third', 'Then', 'Here', 'Hence', 'Thus', 'Therefore', 'He', 'It', 'This',
  'God', 'Christ', 'Lord', 'But', 'And', 'For', 'Now', 'So', 'Again', 'Further', 'Also', 'Finally',
  'Secondly', 'Thirdly', 'Firstly', 'Lastly',
]);

// A bracketed reference ("[Cf. S.T., I. 16, aa. 1, 3; I Sent., 19, 5, 1]")
// rewritten for workReferences: without "Cf." or "Parallel readings:", and a
// book's number written before its title ("I Sent.", "VI Metaph.") moved after
// it ("Sent. I"). A bracket that is only a location ("[VIII, 5]") cites the
// title just before it ("the Physics [VIII, 5]"), if one is. before: the text
// before the bracket.
export const bracketReference = (inner: string, before: string): string => {
  const parts = squash(inner)
    .replace(/\s+,/g, ',')
    .replace(/^(?:parallel readings|cf\.?|see|compare)\s*:?\s*/i, '')
    .split(/\s*;\s*/)
    .map((part) => {
      const m = part.replace(/^(?:cf\.?|see)\s+/i, '').match(/^([IVX]+)\s+([A-Z][^,\d]*?)\s*(?:,\s*(.*))?$/);
      return m ? `${ m[2] } ${ m[1] }${ m[3] ? ', ' + m[3] : '' }` : part;
    });
  const title = before.match(/(?:\b(?:the|his)\s+)?([A-Z][\w’']+(?:\s+(?:de|on|of|the|[A-Z][\w’']+))*)\s*$/);

  if (/^[IVXLC\d]/.test(parts[0] ?? '') && title && !NOT_TITLES.has(title[1]!)) {
    parts[0] = `${ title[1] } ${ parts[0] }`;
  }

  return parts.join('; ');
};

// The works a bracketed reference cites (see bracketReference): as
// workReferences reads each ";"-separated part; for a part it rejects as all
// capitals ("C.G., I, 60"), its abbreviated title and the location after it;
// for one that is only a location ("ii, 5"), a place in the work before it
const ABBREVIATED = /^((?:[A-Z][A-Za-z]*\.\s*)+),?\s+([IVXLC\d].*)$/;

export const bracketWorks = (reference: string): WorkReference[] => reference
  .split(/\s*;\s*/)
  .filter((part) => part.length > 0)
  .reduce<WorkReference[]>((refs, part) => {
    const read        = workReferences(part);
    const abbreviated = part.match(ABBREVIATED);
    const previous    = refs[refs.length - 1];
    const location    = (s: string) => s.trim().replace(/[.,;]+$/, '');

    if (read.length > 0) {
      return [...refs, ...read];
    }
    else if (abbreviated) {
      return [...refs, { author: '', title: abbreviated[1]!.trim(), location: location(abbreviated[2]!) }];
    }
    else if (previous && /^[IVXLCivxlc\d]/.test(part)) {
      return [...refs, { ...previous, location: location(part) }];
    }
    else {
      return refs;
    }
  }, []);

// A placeholder note for a bracket's citations, its index between private-use
// characters: scripRefCitations numbers it with the other footnotes, then the
// footnote is given the bracket's citations (see blockCitations)
const BRACKET = '\ue012';

// Brackets citing works ("[Cf. <i>S.T.</i>, I. 16]", "the Physics [VIII,
// 5]"), each followed by a placeholder note, and what each cites. Brackets in
// notes are left as they are: a note can't hold another.
type BracketReference = { raw: string, works: WorkReference[] };

export const markBrackets = (html: string): { html: string, references: BracketReference[] } => {
  const references: BracketReference[] = [];

  const marked = html
    .split(/(<note>[\s\S]*?<\/note>)/)
    .map((piece, i) => (i % 2 === 1 ? piece : piece.replace(
      /\[((?:[^[\]<]|<\/?(?:i|em|b|strong|u)>){2,400})\]/g,
      (whole, inner: string, at: number) => {
        const raw   = bracketReference(textOf(inner), textOf(piece.slice(Math.max(0, at - 120), at)));
        const works = bracketWorks(raw);

        if (works.length === 0) {
          return whole;
        }
        else {
          references.push({ raw, works });
          return `${ whole }<note>${ BRACKET }${ references.length - 1 }${ BRACKET }</note>`;
        }
      },
    )))
    .join('');

  return { html: marked, references };
};

// Parentheses that look like citations but aren't: Bekker numbers of the text
// commented on ("(B. 1102 a 33)", "(1094 a 1)"), and an outline's letters ("(C
// 6)"). Hidden from scripRefCitations, then put back.
const NOT_CITATIONS = /\((?=\s*(?:B\.\s*)?\d{3,4}\s*[ab]\s*\d|\s*[A-Z]’?\s*,?\s*\d+\s*\))/g;
const HIDDEN_PAREN  = '\ue011';

// A block's citations: scripture and works cited in parentheses and brackets,
// and what its footnotes cite, each given a footnote numbered from `first` in
// the order they appear (see scripRefCitations). bible: the book of the Bible a
// commentary is on.
export const blockCitations = (
  html: string, source: { bookId: string, footnotePage: number }, first: number, bible?: string,
) => {
  const brackets = markBrackets(markScripture(html, bible));
  const result   = scripRefCitations(brackets.html.replace(NOT_CITATIONS, HIDDEN_PAREN), source, first);
  const placed   = new RegExp(`^${ BRACKET }(\\d+)${ BRACKET }$`);

  return {
    html:      result.html.replace(/<\/?scripRef\b[^>]*>/g, '').replace(new RegExp(HIDDEN_PAREN, 'g'), '('),
    footnotes: result.footnotes.map((f) => {
      const bracket = brackets.references[Number(f.raw.match(placed)?.[1] ?? NaN)];

      if (!bracket) {
        return f;
      }
      else {
        return {
          ...f,
          kind:      'work' as const,
          raw:       bracket.raw,
          citations: bracket.works.map((w): Citation => ({
            source:          { ...source, footnoteIdentifier: f.identifier },
            referenceBookId: null,
            author:          w.author,
            title:           w.title,
            location:        w.location,
            raw:             bracket.raw,
            locationsCited:  [],
          })),
        };
      }
    }),
  };
};

// --- Pages -------------------------------------------------------------------

// A stretch of a work starting at an anchor the contents link to or at a
// heading: its blocks, and what it says of its place (its contents entry, its
// first heading, its anchor's name)
type Section = {
  entry: string | null,
  anchor: string | null,
  heading: string | null,
  blocks: { tag: string, html: string, text: string }[],
};

// Whether a section has text beyond its headings: a heading after it
// starts the next section, one before it is its own
const hasText = (section: Section) => section.blocks.some((b) => (
  !/^h[1-6]$/.test(b.tag) && !isDivisionHeading(b.text)
));

// A file's sections: at each anchor its contents link to, and at each heading
// naming one of the work's divisions, unless it heads the section an anchor
// just began (most anchors are followed by the heading they link to)
const sectionsOf = (
  blocks: RawBlock[], targets: Map<string, string>, levels: DivisionType[],
): Section[] => {
  const sections: Section[] = [{ entry: null, anchor: null, heading: null, blocks: [] }];
  const current             = () => sections[sections.length - 1]!;

  for (const b of blocks) {
    if (b.kind === 'anchor') {
      sections.push({ entry: targets.get(b.name) ?? null, anchor: b.name, heading: null, blocks: [] });
    }
    else {
      const division = isDivisionHeading(b.text) && divisionsIn(b.text, levels).length > 0;
      const heading  = division || /^h[1-6]$/.test(b.tag);

      if (division && hasText(current())) {
        sections.push({ entry: null, anchor: null, heading: b.text, blocks: [b] });
      }
      else {
        if (heading && (current().heading === null || (division && !isDivisionHeading(current().heading!)))) {
          current().heading = b.text;
        }
        current().blocks.push(b);
      }
    }
  }

  return sections.filter((s) => s.blocks.length > 0);
};

// The divisions a section names: by its anchor's name, in a work that reads
// anchors its own way (anchorParts); else in its contents entry or heading;
// else by its anchor's name (the number of the smallest division, see
// anchorParts)
const sectionParts = (section: Section, work: AquinasWork): CitationPart[] => {
  const own   = section.anchor && work.anchorParts
    ? work.anchorParts(section.anchor).filter((p) => p.value > 0) : [];
  const named = [section.entry, section.heading]
    .flatMap((t) => (t ? [divisionsIn(t, work.levels)] : []))
    .find((parts) => parts.length > 0);

  if (own.length > 0) {
    return own;
  }
  else if (named) {
    return named;
  }
  else if (section.anchor) {
    return anchorParts(work.levels)(section.anchor).filter((p) => p.value > 0);
  }
  else {
    return [];
  }
};

// The divisions an anchor's name stands for: "12" the 12th of the smallest
// division, "1:2" or "1-2" the 2nd of the smallest in the 1st of the one above
// (De Potentia's question 1, article 2; De Caelo's book 1, lecture 2)
const anchorParts = (levels: DivisionType[]) => (name: string): CitationPart[] => {
  const one = name.match(/^(\d+)$/);
  const two = name.match(/^(\d+)[:.-](\d+)$/);

  if (one && levels.length > 0) {
    return [{ type: levels[levels.length - 1]!, value: Number(one[1]) }];
  }
  else if (two && levels.length > 1) {
    return [
      { type: levels[levels.length - 2]!, value: Number(two[1]) },
      { type: levels[levels.length - 1]!, value: Number(two[2]) },
    ];
  }
  else {
    return [];
  }
};

// The divisions a page is in: the work's place so far, with the divisions its
// section names replacing theirs and those below them
const placeAfter = (place: CitationPart[], named: CitationPart[], levels: DivisionType[]) => {
  if (named.length === 0) {
    return place;
  }
  else {
    const top = Math.min(...named.map((p) => levels.indexOf(p.type as DivisionType)));
    return [
      ...place.filter((p) => levels.indexOf(p.type as DivisionType) < top),
      ...named,
    ].sort((a, b) => levels.indexOf(a.type as DivisionType) - levels.indexOf(b.type as DivisionType));
  }
};

// A section's blocks in pages of about MAX_PAGE_CHARS, split between blocks
const pieces = <B extends { text: string }>(blocks: B[]): B[][] => blocks.reduce<B[][]>((out, b) => {
  const last = out[out.length - 1];
  const size = last?.reduce((n, x) => n + x.text.length, 0) ?? 0;

  if (!last || (size > 0 && size + b.text.length > MAX_PAGE_CHARS)) {
    out.push([b]);
  }
  else {
    last.push(b);
  }
  return out;
}, []);

// A work's pages, in reading order: a page per section of each file (see
// sectionsOf), cited by the divisions it's in; long sections over several
// pages, cited by the first. Each page has the work's title and its divisions
// as headers, its heading as a section header, its text, and a footnote for
// each citation found in it (see blockCitations).
type PagePlan = { label: string, heading: string, parts: CitationPart[], blocks: Section['blocks'] };

export const aquinasPages = (
  work: AquinasWork, files: { file: AquinasFile, html: string }[],
): AquinasPage[] => {
  const plans: PagePlan[] = [];
  const cited             = new Set<string>();

  for (const { file, html } of files) {
    const name    = file.path.split('/').pop()!;
    const targets = contentsTargets(html, name);
    let place     = file.parts ?? [];

    for (const section of sectionsOf(fileBlocks(html, name, file.bilingual), targets, work.levels)) {
      const named = sectionParts(section, work);
      place       = placeAfter(file.parts ?? [], placeAfter(place, named, work.levels), work.levels);
      // a page is cited by its divisions only when its section names its own
      // (a preface isn't the chapter before it), and only the first page with
      // them. A section of headings alone (a contents list's chapter titles,
      // a title page) only moves the place on.
      const key   = JSON.stringify(place);
      const shown = hasText(section);
      const parts = named.length > 0 && shown && !cited.has(key) ? place : [];
      const title = squash(section.entry ?? section.heading ?? '');
      if (shown) {
        cited.add(key);
      }

      pieces(shown ? section.blocks : []).forEach((blocks, i) => {
        const label = named.length > 0 ? labelOf(place) : title || work.title;
        plans.push({
          label:   (i === 0 ? label : `${ label } (${ i + 1 })`).slice(0, MAX_LABEL),
          heading: title,
          parts:   i === 0 ? parts : [],
          blocks,
        });
      });
    }
  }

  return plans.map((plan, n) => {
    const pageNumber             = n + 1;
    const text: PageBlock[]      = [];
    const footnotes: PageBlock[] = [];

    for (const b of plan.blocks) {
      const found = blockCitations(b.html, { bookId: work.id, footnotePage: pageNumber },
                                   footnotes.length + 1, work.bible);
      const label = /^h[1-6]$/.test(b.tag) || b.text === plan.heading || isDivisionHeading(b.text)
        ? 'SectionHeader' : 'Text';
      const tag   = label === 'Text' ? 'p' : 'h4';
      text.push(block(label, `<${ tag }>${ found.html }</${ tag }>`));
      footnotes.push(...found.footnotes.map((f) => block(
        'Footnote', `<p><sup>${ f.identifier }</sup> ${ escapeHtml(f.raw) }</p>`, f.citations,
      )));
    }

    return {
      pageNumber,
      printedPageNumber: plan.label,
      citationParts:     plan.parts,
      blocks:            [
        block('PageHeader', `<p>${ escapeHtml(work.title) }</p>`),
        ...(plan.parts.length > 1
          ? [block('PageHeader', `<p>${ escapeHtml(labelOf(plan.parts.slice(0, -1))) }</p>`)]
          : []),
        ...text,
        ...footnotes,
      ],
    };
  });
};

// --- Citations named in prose, read by an LLM --------------------------------

// What the model returns for a page: the works its text cites or names in prose
export type ProseCitations = {
  citations: {
    author: string,
    title: string,
    location: string,
    raw: string,
    locationsCited: CitationLocation[][],
  }[],
};

export const PROSE_CITATIONS_FORMAT: ORResponseFormat = {
  type:        'json_schema',
  json_schema: {
    name:   'prose_citations',
    strict: true,
    schema: {
      type:                 'object',
      additionalProperties: false,
      required:             ['citations'],
      properties:           {
        citations: {
          type:  'array',
          items: {
            type:                 'object',
            additionalProperties: false,
            required:             ['author', 'title', 'location', 'raw', 'locationsCited'],
            properties:           {
              author:         { type: 'string' },
              title:          { type: 'string' },
              location:       { type: 'string' },
              raw:            { type: 'string' },
              locationsCited: {
                type:  'array',
                items: {
                  type:  'array',
                  items: {
                    type:                 'object',
                    additionalProperties: false,
                    required:             ['rawLabel', 'type', 'values'],
                    properties:           {
                      rawLabel: { type: 'string' },
                      type:     { type: 'string', enum: LOCATION_TYPES },
                      values:   { type: 'array', items: { type: 'integer' } },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
};

// The system prompt for one page of a work: what to find, and what's found
// already
export const proseCitationsPrompt = (
  work: Pick<AquinasWork, 'title' | 'author'>, found: string[],
) => `You find the sources cited in one page of ${ work.author ?? 'Thomas Aquinas' }'s
"${ work.title }", in English translation.
The page's text follows, a paragraph a line. Return every place where the text cites or names a source
in its own words, without a reference in parentheses or brackets:
  "as the Philosopher says in Book VIII of the Physics" (Aristotle, Physics, Book VIII),
  "Augustine says in his book On the Trinity" (Augustine, De Trinitate),
  "according to Dionysius (Div. Nom. iv)" is already found by pattern: leave it out,
  "as Avicenna says" (Avicenna, no title: an author quoted or named as an authority counts).
These are illustrations of the forms, not text of the page: return only citations in the page's text.
- author: the author as usually named ("Aristotle" for "the Philosopher", "Averroes" for "the
  Commentator", "Paul" for "the Apostle", "Peter Lombard" for "the Master"). For scripture, "Bible".
- title: the work as the text names or plainly identifies it, in its usual form ("Physics",
  "De Trinitate", "Nicomachean Ethics"); for scripture, the book ("Psalms"). "" when the text names no
  work: never supply one the text doesn't name.
- location: the part of the work, as written ("Book VIII", "in the first chapter"); "" for none.
- raw: the words of the text that make the citation, copied character for character from the page
  (not reworded, nothing added), at most 200 characters.
- locationsCited: the location as groups of {rawLabel, type, values}; values are integers (convert
  Roman and spelled numbers). Types: ${ LOCATION_TYPES.join(', ') }. [] when there is no location.
Leave out: references to this work itself ("as was said above", "in the preceding lecture"); in a
commentary, the text being commented on when it is only quoted or paraphrased ("Then he says",
"he shows that"); and the citations already found, listed here:
${ found.length > 0 ? found.map((f) => '  - ' + f).join('\n') : '  (none)' }
`;

// A page's text as the model is given it: its text blocks, a paragraph a line
export const pageProse = (page: Page): string => page.blocks
  .filter((b) => b.label === 'Text' || b.label === 'SectionHeader')
  .map((b) => textOf(b.html))
  .join('\n');

// The raw text of a page's citations found by pattern, for the model to leave
// out
export const foundOnPage = (page: Page): string[] => [
  ...new Set(page.blocks.flatMap((b) => b.citations.map((c) => c.raw))),
];

const comparable = (s: string) => (
  s.normalize('NFC').toLowerCase().replace(/[^\p{L}\p{N}]+/gu, ' ').trim()
);

// The ways a citation's raw text may be found in the page: as the model gave
// it, or without a word it added or dropped at either end ("as Ibn-Sînâ says
// ..." for "Ibn-Sînâ says ..."); none shorter than 15 characters
const rawForms = (raw: string): string[] => {
  const words = comparable(raw).split(' ');
  return [words, words.slice(1), words.slice(0, -1), words.slice(1, -1)]
    .map((w) => w.join(' '))
    .filter((form) => form.length >= 15);
};

// A page with the citations the model found in its prose, each on the text
// block it quotes (or, when the model paraphrased it, the one naming the
// work), with no footnote identifier. Those neither quoted nor named on the
// page are left out: the model can make citations up (the prompt's examples,
// for one).
export const withProseCitations = (
  page: AquinasPage, bookId: string, result: ProseCitations,
): AquinasPage => {
  const texts  = page.blocks.map((b) => (b.label === 'Text' ? comparable(textOf(b.html)) : null));
  const placed = new Map<number, Citation[]>();

  for (const c of result.citations) {
    const forms  = rawForms(c.raw);
    const title  = comparable(c.title);
    const quoted = texts.findIndex((t) => t !== null && forms.some((form) => t.includes(form)));
    const named  = title.length >= 4 ? texts.findIndex((t) => t !== null && ` ${ t } `.includes(` ${ title } `)) : -1;
    const i      = quoted >= 0 ? quoted : named;
    if (i >= 0) {
      placed.set(i, [...placed.get(i) ?? [], {
        source:          { bookId, footnoteIdentifier: '', footnotePage: page.pageNumber },
        referenceBookId: null,
        author:          c.author,
        title:           c.title,
        location:        c.location,
        raw:             c.raw,
        locationsCited:  c.locationsCited,
      }]);
    }
  }

  return {
    ...page,
    blocks: page.blocks.map((b, i) => (
      placed.has(i) ? { ...b, citations: [...b.citations, ...placed.get(i)!] } : b
    )),
  };
};
