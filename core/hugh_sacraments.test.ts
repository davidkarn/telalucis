import { describe, expect, it } from '@jest/globals';
import {
  bookHeading, chapterHeading, divisionCheck, divisionParts, HUGH_BOOK_ID, inlineCitations,
  MISPRINTED, numberWord, pageDivisions, partHeading,
} from './hugh_sacraments.ts';
import { biblePlaces } from './inline_bible_citations.ts';
import { HUGH_WORKS } from './medieval_citations.ts';
import type { Page, PageBlock } from '../types.ts';

const block   = (label: PageBlock['label'], html: string): PageBlock => (
  { label, html, bbox: [0, 0, 0, 0], citations: [] } as unknown as PageBlock
);
const page    = (pageNumber: number, blocks: PageBlock[]): Page => ({ pageNumber, printedPageNumber: '', blocks });
const text    = (t: string) => block('Text', `<p>${ t }</p>`);
const heading = (t: string) => block('SectionHeader', `<h4>${ t }</h4>`);

describe('numberWord', () => {
  it('reads cardinals, ordinals, Roman and Arabic numbers', () => {
    expect(['four', 'Sixth', 'eighteenth', 'XII', '12', 'FOURTEENTH.'].map(numberWord)).toEqual([4, 6, 18, 12, 12, 14]);
    expect(numberWord('foo')).toBeNull();
  });
});

describe('headings', () => {
  it('finds the beginnings of books', () => {
    expect(bookHeading('Prologue of the First Book on the Sacraments')).toBe(1);
    expect(bookHeading('HERE BEGINNETH THE FIRST BOOK')).toBe(1);
    expect(bookHeading('Prologue of theSecond Book on the Sacraments')).toBe(2);
    expect(bookHeading('The Body of Book I')).toBeNull();
  });

  it('finds parts, however they are worded', () => {
    expect(['PART FOUR', 'SIXTH PART', 'Sixteenth Part', 'PART XII', 'FOURTEENTH PART.'].map(partHeading))
      .toEqual([4, 6, 16, 12, 14]);
    expect(partHeading('The parts of the soul')).toBeNull();
  });

  it('finds chapters in section headers and in blocks wholly in italics or bold', () => {
    expect(chapterHeading(heading('XIV. <i>That the will of God is always fulfilled.</i>'))).toBe(14);
    expect(chapterHeading(text('<i>XV. How all things in God were from eternity.</i>'))).toBe(15);
    expect(chapterHeading(text('<b>XII. <i>That both sound harsh.</i></b>'))).toBe(12);
    expect(chapterHeading(heading("1. On the Cause of Man's Creation."))).toBe(1);
  });

  it('does not take text beginning with a number for a chapter', () => {
    expect(chapterHeading(text('II. On the primordial causes, as was said, and other things.'))).toBeNull();
    expect(chapterHeading(text('<i>1. On the Incarnation.</i> 2. Why the Son was sent.'))).toBeNull();
  });

  it('marks a misprinted numeral', () => {
    expect(chapterHeading(heading('<i>XVIIII. According to nature.</i>'))).toBe(MISPRINTED);
  });
});

describe('pageDivisions', () => {
  const pages                   = [
    page(1, [heading('Preface'), text('The late Archbishop...')]),
    page(2, [heading('Prologue of the First Book on the Sacraments'), heading('I. What must be learned.'), text('a')]),
    page(3, [text('continued'), heading('PART ONE'), heading('I. That there is one principle.'), text('b')]),
    page(4, [heading('II. Whether matter was made before form.'), text('c'),
             heading('<i>XVIIII. Misprinted.</i>'), text('d')]),
    // part two's heading missed: its chapter I begins it
    page(5, [text('e'), heading('I. On the cause of creation.'), text('f')]),
    page(6, [text('g'), heading('GENERAL INDEX'), text('Abelard, 4')]),
  ];
  const { divisions, warnings } = pageDivisions(pages);

  it('gives each page the divisions on it, none before the first book or after the index', () => {
    expect(divisions.has(1)).toBe(false);
    expect(divisions.get(2)).toEqual([{ book: 1, part: 0, chapter: 0 }, { book: 1, part: 0, chapter: 1 }]);
    expect(divisions.get(3)).toEqual([
      { book: 1, part: 0, chapter: 1 }, { book: 1, part: 1, chapter: 0 }, { book: 1, part: 1, chapter: 1 },
    ]);
    expect(divisions.get(4)).toEqual([{ book: 1, part: 1, chapter: 2 }, { book: 1, part: 1, chapter: 3 }]);
    expect(divisions.get(6)).toEqual([{ book: 1, part: 2, chapter: 1 }]);
  });

  it('begins a part at a chapter I without its heading, and says so', () => {
    expect(divisions.get(5)).toEqual([{ book: 1, part: 1, chapter: 3 }, { book: 1, part: 2, chapter: 1 }]);
    expect(warnings.some((w) => w.includes('part 2 taken to begin'))).toBe(true);
    expect(warnings.some((w) => w.includes('misprinted, taken to be 3'))).toBe(true);
  });

  it('gives a division as citation parts', () => {
    expect(divisionParts({ book: 2, part: 3, chapter: 12 })).toEqual([
      { type: 'book', value: 2 }, { type: 'part', value: 3 }, { type: 'chapter', value: 12 },
    ]);
    expect(divisionParts({ book: 1, part: 0, chapter: 0 })).toEqual([{ type: 'book', value: 1 }, { type: 'part', value: 0 }]);
  });

  it('checks the parts read against the running heads', () => {
    const headed = (n: number, head: string) => page(n, [block('PageHeader', head), text('x')]);
    const check  = new Map([[7, [{ book: 1, part: 2, chapter: 4 }]], [8, [{ book: 1, part: 2, chapter: 5 }]]]);
    expect(divisionCheck([headed(7, 'Book One, Part Two'), headed(8, 'Book One, Part Three')], check)).toEqual([
      'scan 8: running head "Book One, Part Three", parts read 1.2',
    ]);
  });
});

describe('biblePlaces', () => {
  const read = (inner: string) => biblePlaces(inner).map((p) => [p.book, p.chapter, p.verses]);

  it('reads a book, chapter and verses as printed', () => {
    expect(read('Cf. Rom. 8, 28')).toEqual([[52, 8, [28]]]);
    expect(read('Psal. 132, 1')).toEqual([[21, 132, [1]]]);
    expect(read('Gen. I, 4 and 6')).toEqual([[1, 1, [4, 6]]]);
    expect(read('Eccli. 18. 1.')).toEqual([[26, 18, [1]]]);
    expect(read('John III, 21')).toEqual([[50, 3, [21]]]);
    expect(read('Gen. 3, 1-4')).toEqual([[1, 3, [1, 2, 3, 4]]]);
    expect(read('Acts 8')).toEqual([[51, 8, []]]);
  });

  it('reads numbered books, Roman and Arabic, and not the I of "Isa."', () => {
    expect(read('Cf. I. John 4, 20')).toEqual([[69, 4, [20]]]);
    expect(read('2 Paralip. 15, 2')).toEqual([[14, 15, [2]]]);
    expect(read('Isa. 5, 20')).toEqual([[27, 5, [20]]]);
    expect(read('Isaias 9, 6')).toEqual([[27, 9, [6]]]);
  });

  it('reads a comma after the book, and "ff." or "etc." after the place', () => {
    expect(read('Rom., 6, 23')).toEqual([[52, 6, [23]]]);
    expect(read('Wisdom, 11, 21')).toEqual([[25, 11, [21]]]);
    expect(read('Cf. Gen. 2, 20 ff')).toEqual([[1, 2, [20]]]);
    expect(read('Cf. Exod. 14, 19, 20, 24, etc.')).toEqual([[2, 14, [19, 20, 24]]]);
  });

  it('reads several places, the book carried over to a place without one', () => {
    expect(read('Cf. Matt. 6, 25; Luke 12, 23')).toEqual([[47, 6, [25]], [49, 12, [23]]]);
    expect(read('Matt. 26, 41, Mark 14, 38')).toEqual([[47, 26, [41]], [48, 14, [38]]]);
    expect(read('Isa. 56, 7: Matt. 21, 13')).toEqual([[27, 56, [7]], [47, 21, [13]]]);
    expect(read('Cf. Deut. 12, 16 and Levit. 7, 15 and 26')).toEqual([[5, 12, [16]], [3, 7, [15, 26]]]);
    expect(read('Cf. Matt. 10, 22; 24, 13')).toEqual([[47, 10, [22]], [47, 24, [13]]]);
    expect(read('Ezech. 5, 1; cf. Acts 18, 18')).toEqual([[31, 5, [1]], [51, 18, [18]]]);
  });

  it('reads nothing from an aside, a word, or a place it cannot be sure of', () => {
    expect(read('since at that time I had no plan for a future work')).toEqual([]);
    expect(read('Deus')).toEqual([]);
    expect(read('14')).toEqual([]);
    expect(read('Cor. 12, 31')).toEqual([]);
    expect(read('2 Cor., 2, 16, not an exact quotation')).toEqual([]);
  });
});

describe('inlineCitations', () => {
  it('cites the Bible as the collection does, a citation per place, on its page', () => {
    const [first, second] = inlineCitations(HUGH_BOOK_ID, 41, '<p>as it is written (Cf. Matt. 6, 25; Luke 12, 23).</p>');
    expect(first).toEqual({
      source:          { bookId: HUGH_BOOK_ID, footnoteIdentifier: '', footnotePage: 41 },
      referenceBookId: 'bible',
      author:          'Bible',
      title:           'Matthew',
      location:        '6, 25',
      raw:             'Matt. 6, 25',
      locationsCited:  [[
        { rawLabel: 'Matt.', type: 'book', values: [47] },
        { rawLabel: '6', type: 'chapter', values: [6] },
        { rawLabel: '25', type: 'verse', values: [25] },
      ]],
    });
    expect(second!.title).toBe('Luke');
  });

  it('cites another work by its author, title, book and lines', () => {
    const [vergil] = inlineCitations(HUGH_BOOK_ID, 400, '<p>(Vergil, Aeneid, 6, 740-743)</p>');
    expect([vergil!.author, vergil!.title, vergil!.locationsCited]).toEqual(['Vergil', 'Aeneid', [[
      { rawLabel: '6', type: 'book', values: [6] },
      { rawLabel: '740-743', type: 'line', values: [740, 741, 742, 743] },
    ]]]);
  });

  it('finds none in asides', () => {
    expect(inlineCitations(HUGH_BOOK_ID, 1, '<p>Hugh (since at that time I had no plan) wrote</p>')).toEqual([]);
  });
});

describe('HUGH_WORKS', () => {
  const [work] = HUGH_WORKS;

  it("matches Hugh's De Sacramentis, not Hugon's", () => {
    for (const author of ['Hugh', 'Hugo', 'Hugh of St. Victor', 'Hugo de S. Victore']) {
      expect(work!.author.test(author)).toBe(true);
    }
    expect(work!.author.test('Hugon')).toBe(false);
    expect(work!.title.test('De Sacram.')).toBe(true);
    expect(work!.title.test('On the Sacraments')).toBe(true);
  });
});
