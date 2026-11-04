import { describe, expect, it } from '@jest/globals';
import { bookText, type BookSource, type FootnoteCitation } from './book_text.ts';

type Block = BookSource['pages'][number]['blocks'][number];

const text     = (html: string): Block => ({ label: 'Text', html: `<p>${ html }</p>` });
const heading  = (t: string, tag = 'h2'): Block => ({ label: 'SectionHeader', html: `<${ tag }>${ t }</${ tag }>` });
const footnote = (html: string): Block => ({ label: 'Footnote', html: `<p>${ html }</p>` });
const header   = (t: string): Block => ({ label: 'PageHeader', html: `<p>${ t }</p>` });
const footer   = (t: string): Block => ({ label: 'PageFooter', html: `<p>${ t }</p>` });
const page     = (pageNumber: number, blocks: Block[], printedPageNumber = String(pageNumber + 10)) => (
  { pageNumber, printedPageNumber, blocks }
);
const book     = (pages: BookSource['pages'], more: Partial<BookSource> = {}): BookSource => (
  { id: 'b', title: 'On Truth', author: 'Thomas Aquinas', pages, ...more }
);

const texts = (source: BookSource) => bookText(source).sections.map((s) => [s.title, s.paragraphs.map((p) => p.text)]);

describe('bookText: the text', () => {
  it('leaves out running headers and footers, and page numbers OCR\'d as text', () => {
    expect(texts(book([
      page(1, [header('ON TRUTH'), text('First paragraph.'), footer('11')]),
      page(2, [text('12'), text('Second paragraph.'), text('xiv')]),
    ]))).toEqual([['', ['First paragraph.', 'Second paragraph.']]]);
  });

  it('joins a paragraph broken by a page, and a word broken by a hyphen', () => {
    const { sections } = bookText(book([
      page(1, [text('The power of God exceeds all contin-'), footer('11')]),
      page(2, [header('ON TRUTH'), text('uation of things, and'), text('so it is.'), text('A new paragraph.')]),
    ]));
    expect(sections[0]!.paragraphs.map((p) => [p.text, p.pageNumber, p.endPageNumber])).toEqual([
      ['The power of God exceeds all continuation of things, and so it is.', 1, 2],
      ['A new paragraph.', 2, 2],
    ]);
  });

  it("keeps a block's paragraphs apart", () => {
    expect(texts(book([page(1, [{ label: 'Text', html: '<p>One item</p><p>Another item</p>' }])])))
      .toEqual([['', ['One item', 'Another item']]]);
  });
});

describe('bookText: footnotes', () => {
  const citation: FootnoteCitation = {
    footnotePage: 1, footnoteIdentifier: '2', author: 'Augustine', title: 'De Trinitate', location: 'XV, 3', raw: 'Augustine, De Trin. XV, 3', referenceBookId: null,
  };
  const result                     = bookText(book([
    page(1, [
      text('Truth is in the intellect<sup>1</sup> and in things<sup>2</sup>, as'),
      footnote('<sup>1</sup> Aristotle, Metaph. VI, 4.'),
      footnote('<sup>2</sup> Augustine, De Trin. XV, 3, where he'),
    ]),
    page(2, [
      text('he says.<sup>1</sup> Then<sup>9</sup> more.'),
      footnote('argues the same.'),
      footnote('<sup>1</sup> Anselm, De Veritate 2.'),
      footnote('* A note with no marker in the text.'),
    ]),
  ], { citations: [citation] }));

  it('puts a reference to each footnote where its marker is, and leaves out markers with none', () => {
    expect(result.sections[0]!.paragraphs.map((p) => [p.text, p.footnoteIds])).toEqual([
      ['Truth is in the intellect[^1] and in things[^2], as he says.[^3] Then more.', [1, 2, 3, 4]],
    ]);
  });

  it("numbers the footnotes through the book, a footnote's run-on onto the next page its own", () => {
    expect(result.footnotes.map((f) => [f.id, f.marker, f.text, f.pageNumber, f.printedPageNumber, f.inline])).toEqual([
      [1, '1', 'Aristotle, Metaph. VI, 4.', 1, '11', true],
      [2, '2', 'Augustine, De Trin. XV, 3, where he argues the same.', 1, '11', true],
      [3, '1', 'Anselm, De Veritate 2.', 2, '12', true],
      [4, '*', 'A note with no marker in the text.', 2, '12', false],
    ]);
  });

  it("gives each footnote its citations", () => {
    expect(result.footnotes.map((f) => f.citations)).toEqual([[], [citation], [], []]);
  });
});

describe('bookText: symbol footnotes', () => {
  it('reads a symbol marker run into the footnote, and printed in the text without <sup>', () => {
    const result = bookText(book([
      page(1, [
        text('<i>God is omnipotent.*</i> And more.'),
        footnote('*Abelard had replaced the freedom of God by optimism.'),
        footnote('*A second footnote, with no marker in the text.'),
      ]),
      page(2, [text('Next page.'), footnote('†Of the next page.')]),
    ]));
    expect(result.footnotes.map((f) => [f.marker, f.text, f.inline])).toEqual([
      ['*', 'Abelard had replaced the freedom of God by optimism.', true],
      ['*', 'A second footnote, with no marker in the text.', false],
      ['†', 'Of the next page.', false],
    ]);
    expect(result.sections[0]!.paragraphs.map((p) => [p.text, p.footnoteIds])).toEqual([
      ['God is omnipotent.[^1] And more.', [1, 2]],
      ['Next page.', [3]],
    ]);
  });
});

describe('bookText: sections from headings', () => {
  const result = bookText(book([
    page(1, [text('A preface.')]),
    page(2, [heading('PART ONE', 'h1'), heading('CHAPTER I'), heading('Of Truth'), text('Chapter one.')]),
    page(3, [heading('ON TRUTH'), heading('CHAPTER II'), text('Chapter two.')]),
    page(4, [heading('ON TRUTH'), text('More of it.')]),
    page(5, [heading('ON TRUTH'), heading('PART TWO', 'h1'), text('Part two.')]),
  ]));

  it('nests headings by level, joins a heading to one of its level right before it, and leaves out running heads', () => {
    expect(result.sections.map((s) => [s.id, s.parentId, s.title, s.pageNumber, s.endPageNumber])).toEqual([
      [0, null, '', 1, 1],
      [1, null, 'PART ONE', 2, 4],
      [2, 1, 'CHAPTER I — Of Truth', 2, 2],
      [3, 1, 'CHAPTER II', 3, 4],
      [4, null, 'PART TWO', 5, 5],
    ]);
    expect(result.sections[3]!.paragraphs.map((p) => p.text)).toEqual(['Chapter two.', 'More of it.']);
  });

  it('makes the table of contents of the sections', () => {
    const titles = (nodes: typeof result.contents): unknown[] => nodes.map((n) => (
      n.children.length > 0 ? [n.title, n.printedPageNumber, titles(n.children)] : [n.title, n.printedPageNumber]
    ));
    expect(titles(result.contents)).toEqual([
      ['', '11'],
      ['PART ONE', '12', [['CHAPTER I — Of Truth', '12'], ['CHAPTER II', '13']]],
      ['PART TWO', '15'],
    ]);
  });
});

describe('bookText: sections from how the pages are cited', () => {
  const q      = (question: number, article?: number) => [
    { type: 'question', value: question }, ...(article === undefined ? [] : [{ type: 'article', value: article }]),
  ];
  const result = bookText(book([
    page(1, [heading('QUESTION 1'), text('Of truth.')]),
    page(2, [heading('Article 1'), text('Whether truth is in the intellect.'), heading('Article 2'), text('Whether in things')]),
    page(3, [text('as well.')]),
  ], {
    pageParts: [
      { pageNumber: 1, parts: q(1) },
      { pageNumber: 2, parts: q(1, 1) }, { pageNumber: 2, parts: q(1, 2) },
      { pageNumber: 3, parts: q(1, 2) },
    ],
    partNames: [{ parts: q(1), name: 'Truth' }],
  }));

  it('makes a section of each part, named or titled by its type and number, the next part starting at a heading', () => {
    expect(result.sections.map((s) => [s.parentId, s.title, s.pageNumber, s.endPageNumber, s.paragraphs.map((p) => p.text)])).toEqual([
      [null, 'Truth', 1, 3, ['QUESTION 1', 'Of truth.']],
      [0, 'Article 1', 2, 2, ['Article 1', 'Whether truth is in the intellect.']],
      [0, 'Article 2', 2, 3, ['Article 2', 'Whether in things as well.']],
    ]);
    expect(result.sections[1]!.parts).toEqual(q(1, 1));
    expect(result.contents[0]!.children.map((n) => n.heading)).toEqual(['Article 1', 'Article 2']);
    expect(result.sections[1]!.paragraphs[0]!.kind).toBe('heading');
  });
});
