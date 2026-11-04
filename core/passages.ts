// A book's body text cut into passages of a few paragraphs, for searching the collection by meaning
// (each passage's embedding) and by words (see migrations/0030_create_passages.ts). Only the body:
// headings and text, not page headers, page footers or footnotes, with footnote markers left out.
// A passage starts at a heading when it has text enough already, and paragraphs are kept whole
// unless one alone is too long; a paragraph broken by a page (or a word by a line-end hyphen) is
// joined again. Pure functions; model/passages.ts reads the blocks and saves the passages.
import { squash, strip_tags } from './book_pages.ts';
import type { PageBlock } from '../types.ts';

// A passage's length in words: it's ended once it has TARGET_WORDS, before a paragraph taking it
// past MAX_WORDS, or at a heading once it has MIN_WORDS. A last passage shorter than MIN_WORDS
// joins the one before when together they're at most MAX_WORDS.
export const TARGET_WORDS = 350;
export const MAX_WORDS = 500;
export const MIN_WORDS = 80;

// How many numbers a passage's embedding has: the passages table's (migrations/0030_create_passages.ts)
export const EMBEDDING_DIMENSIONS = 1024;

// Passages with fewer words than this are left out: a title page's, a plate's caption
const LEAST_WORDS = 20;

export type PassageSourcePage = {
  pageNumber: number,
  blocks: { label: PageBlock['label'], html: string }[],
};

// position: its place among the book's passages, from 0. pageNumber, endPageNumber: the pages it
// starts and ends on.
export type Passage = {
  position: number,
  pageNumber: number,
  endPageNumber: number,
  text: string,
  wordCount: number,
};

// A paragraph or heading of the body, with the page it starts on
type Unit = { pageNumber: number, endPageNumber: number, text: string, heading: boolean };

const BODY_LABELS = new Set<PageBlock['label']>(['Text', 'SectionHeader']);

export const wordCount = (text: string) => (text.length === 0 ? 0 : text.split(' ').length);

// A block's text: its footnote markers (<sup>) left out, its lines run together
export const blockText = (html: string): string => squash(strip_tags(html.replace(/<sup>.*?<\/sup>/gis, ' ')));

// Whether the paragraph after `text` carries it on: text doesn't end a sentence ("…the power of" /
// "God exceeds…" across a page)
const continues = (text: string) => !/[.!?:;"”’)\]]$/.test(text);

// text and next joined: a word broken by a line-end hyphen ("contin-" "uation") made whole
const joined = (text: string, next: string) => (
  /\p{Ll}-$/u.test(text) && /^\p{Ll}/u.test(next) ? text.slice(0, -1) + next : `${ text } ${ next }`
);

// The body's paragraphs and headings in order, a paragraph broken by a page (or a block) one again
const bodyUnits = (pages: PassageSourcePage[]): Unit[] => pages
  .flatMap((page) => page.blocks
    .filter((b) => BODY_LABELS.has(b.label))
    .map((b) => ({
      pageNumber: page.pageNumber, endPageNumber: page.pageNumber, text: blockText(b.html), heading: b.label === 'SectionHeader',
    })))
  .filter((u) => u.text.length > 0)
  .reduce<Unit[]>((units, u) => {
    const last = units.at(-1);
    return last && !last.heading && !u.heading && continues(last.text)
      ? [...units.slice(0, -1), { ...last, endPageNumber: u.endPageNumber, text: joined(last.text, u.text) }]
      : [...units, u];
  }, []);

// A paragraph longer than MAX_WORDS in pieces of about TARGET_WORDS, cut between sentences (or,
// for a sentence longer than MAX_WORDS, between words)
const pieces = (unit: Unit): Unit[] => {
  if (wordCount(unit.text) <= MAX_WORDS) {
    return [unit];
  }
  else {
    const sentences = unit.text.split(/(?<=[.!?;:])\s+/).flatMap((s) => {
      const words = s.split(' ');
      return words.length <= MAX_WORDS
        ? [s]
        : Array.from({ length: Math.ceil(words.length / TARGET_WORDS) }, (_, i) => (
          words.slice(i * TARGET_WORDS, (i + 1) * TARGET_WORDS).join(' ')
        ));
    });

    return sentences.reduce<string[]>((out, s) => {
      const last = out.at(-1);
      return last !== undefined && wordCount(last) + wordCount(s) <= TARGET_WORDS
        ? [...out.slice(0, -1), `${ last } ${ s }`]
        : [...out, s];
    }, []).map((text) => ({ ...unit, text }));
  }
};

type Draft = { pageNumber: number, endPageNumber: number, texts: string[], words: number };

const draftOf = (u: Unit): Draft => ({
  pageNumber: u.pageNumber, endPageNumber: u.endPageNumber, texts: [u.text], words: wordCount(u.text),
});

const withUnit = (d: Draft, u: Unit): Draft => ({
  ...d, endPageNumber: u.endPageNumber, texts: [...d.texts, u.text], words: d.words + wordCount(u.text),
});

// The passages of a book's pages, in order (see TARGET_WORDS)
export const bookPassages = (pages: PassageSourcePage[]): Passage[] => {
  const drafts = bodyUnits(pages).flatMap(pieces).reduce<Draft[]>((out, u) => {
    const last  = out.at(-1);
    const words = wordCount(u.text);
    const ended = last === undefined
      || last.words >= TARGET_WORDS
      || last.words + words > MAX_WORDS
      || (u.heading && last.words >= MIN_WORDS);

    return ended ? [...out, draftOf(u)] : [...out.slice(0, -1), withUnit(last, u)];
  }, []);

  // a short last passage joins the one before
  const last   = drafts.at(-1);
  const before = drafts.at(-2);
  const merged = last && before && last.words < MIN_WORDS && before.words + last.words <= MAX_WORDS
    ? [...drafts.slice(0, -2), { ...before, endPageNumber: last.endPageNumber, texts: [...before.texts, ...last.texts], words: before.words + last.words }]
    : drafts;

  return merged
    .filter((d) => d.words >= LEAST_WORDS)
    .map((d, position) => ({
      position,
      pageNumber:    d.pageNumber,
      endPageNumber: d.endPageNumber,
      text:          d.texts.join('\n\n'),
      wordCount:     d.words,
    }));
};

// What's embedded for a passage: its book's title and author before its text, so a passage about
// "the will" in a commentary on Aristotle is found by a question naming him
export const embeddingInput = (passage: { text: string }, book: { title: string, author: string }) => (
  `${ book.title }${ book.author.length > 0 ? ` (${ book.author })` : '' }\n\n${ passage.text }`
);
