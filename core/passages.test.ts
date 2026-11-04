import { describe, expect, it } from '@jest/globals';
import { blockText, bookPassages, embeddingInput, MAX_WORDS, MIN_WORDS, type PassageSourcePage } from './passages.ts';

const words   = (n: number, word = 'word') => Array.from({ length: n }, () => word).join(' ');
const text    = (t: string) => ({ label: 'Text' as const, html: `<p>${ t }</p>` });
const heading = (t: string) => ({ label: 'SectionHeader' as const, html: `<h2>${ t }</h2>` });
const page    = (pageNumber: number, blocks: PassageSourcePage['blocks']): PassageSourcePage => ({ pageNumber, blocks });

describe('blockText', () => {
  it('leaves out footnote markers and tags, and runs lines together', () => {
    expect(blockText('<p>The will<sup>12</sup> is\n free &amp; <i>rational</i>.</p>')).toBe('The will is free & rational.');
  });
});

describe('bookPassages', () => {
  it("leaves out page headers, footers and footnotes", () => {
    const [passage, ...rest] = bookPassages([page(1, [
      { label: 'PageHeader', html: '<p>Commentary on Job</p>' },
      text(words(30, 'body')),
      { label: 'Footnote', html: '<p><sup>1</sup> 4:17</p>' },
      { label: 'PageFooter', html: '<p>12</p>' },
    ])]);
    expect(rest).toEqual([]);
    expect(passage!.text).toBe(words(30, 'body'));
  });

  it('joins a paragraph broken by a page, and a word broken by a hyphen', () => {
    const [passage] = bookPassages([
      page(4, [text(`${ words(20) } the power of`)]),
      page(5, [text(`God exceeds all contin-`), text(`uation of ${ words(10) }.`)]),
    ]);
    expect(passage!.text).toBe(`${ words(20) } the power of God exceeds all continuation of ${ words(10) }.`);
    expect([passage!.pageNumber, passage!.endPageNumber]).toEqual([4, 5]);
  });

  it('keeps paragraphs whole, ending a passage once it has its target length', () => {
    const paragraph = `${ words(150) }.`;
    const passages  = bookPassages([page(1, [text(paragraph), text(paragraph), text(paragraph), text(paragraph)])]);
    expect(passages.map((p) => p.wordCount)).toEqual([450, 150]);
    expect(passages.every((p) => p.wordCount <= MAX_WORDS)).toBe(true);
    expect(passages.map((p) => p.position)).toEqual([0, 1]);
  });

  it('starts a passage at a heading once it has text enough', () => {
    const passages = bookPassages([page(1, [
      heading('Chapter I'), text(`${ words(MIN_WORDS) }.`), heading('Chapter II'), text(`${ words(MIN_WORDS) }.`),
    ])]);
    expect(passages.map((p) => p.text.split('\n\n')[0])).toEqual(['Chapter I', 'Chapter II']);
  });

  it('cuts a paragraph too long for one passage between sentences', () => {
    const sentence = `${ words(99) } end.`;
    const passages = bookPassages([page(1, [text(Array.from({ length: 8 }, () => sentence).join(' '))])]);
    // pieces of 300, 300 and 200 words; the last two fit one passage
    expect(passages.map((p) => p.wordCount)).toEqual([300, 500]);
    expect(passages.every((p) => p.text.endsWith('end.'))).toBe(true);
  });

  it('joins a short last passage to the one before, and leaves out passages of a few words', () => {
    expect(bookPassages([page(1, [text(`${ words(400) }.`), text(`${ words(30) }.`)])]).map((p) => p.wordCount)).toEqual([430]);
    expect(bookPassages([page(1, [heading('PLATE IV')])])).toEqual([]);
  });
});

describe('embeddingInput', () => {
  it("puts the book's title and author before the text", () => {
    expect(embeddingInput({ text: 'The will is free.' }, { title: 'De Veritate', author: 'Thomas Aquinas' }))
      .toBe('De Veritate (Thomas Aquinas)\n\nThe will is free.');
  });
});
