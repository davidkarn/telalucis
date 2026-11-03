import { describe, expect, it } from '@jest/globals';
import { anselmWorks, classifyHeading, placeLabel, placeParts, withBibleCitations } from './anselm_works.ts';
import { biblePlaces } from './inline_bible_citations.ts';
import { ANSELM_ALIAS_WORKS } from './medieval_citations.ts';
import type { Page, PageBlock } from '../types.ts';

const block   = (label: PageBlock['label'], html: string): PageBlock => (
  { label, html, bbox: [0, 0, 0, 0], citations: [] } as unknown as PageBlock
);
const heading = (t: string) => block('SectionHeader', `<h2>${ t }</h2>`);
const text    = (t: string) => block('Text', `<p>${ t }</p>`);
const page    = (pageNumber: number, blocks: PageBlock[]): Page => ({ pageNumber, printedPageNumber: '', blocks });

describe('classifyHeading', () => {
  it('names the works and collections', () => {
    expect(classifyHeading('Monologium - On The Being Of God', null, 0)).toEqual({ kind: 'name', context: 'monologium' });
    expect(classifyHeading('The Devotions of Saint Anselm', 'prayers', 0)).toEqual({ kind: 'name', context: null });
    expect(classifyHeading('Meditation I', null, 0)).toEqual({ kind: 'name', context: 'meditations' });
  });

  it('places chapters in the work named before them', () => {
    expect(classifyHeading('CHAPTER XII', 'monologium', 0)).toEqual(
      { kind: 'place', place: { work: 'monologium', chapter: 12 } },
    );
    expect(classifyHeading('PREFACE', 'proslogium', 0)).toEqual({ kind: 'place', place: { work: 'proslogium', chapter: 0 } });
    expect(classifyHeading('TWENTY-FIRST MEDITATION', null, 0)).toEqual(
      { kind: 'place', place: { work: 'meditationsAndPrayers', chapter: 21 } },
    );
    expect(classifyHeading('MEDITATION III', null, 0)).toEqual({ kind: 'place', place: { work: 'meditations', chapter: 3 } });
    expect(classifyHeading('XIV', 'meditations', 0)).toEqual(
      { kind: 'place', place: { work: 'meditations', chapter: 1, section: 14 } },
    );
    expect(classifyHeading('IV', 'prayers', 0)).toEqual({ kind: 'place', place: { work: 'prayers', chapter: 4 } });
  });

  it("places Cur Deus Homo's chapters in its books", () => {
    expect(classifyHeading('BOOK SECOND: CHAPTER I', 'curDeusHomo', 1)).toEqual(
      { kind: 'place', place: { work: 'curDeusHomo', book: 2, chapter: 1 } },
    );
    expect(classifyHeading('CHAPTER XVIII (A)', 'curDeusHomo', 2)).toEqual(
      { kind: 'place', place: { work: 'curDeusHomo', book: 2, chapter: 18 } },
    );
  });

  it("leaves out the introductions, notes, the Bible's chapters and Gaunilon", () => {
    for (const t of ['INTRODUCTION', 'FOOTNOTES', 'Note on the Argument of the Proslogion.', 'Psalms 119',
                     'Song of Solomon 1', '1 Kings 2', 'IN BEHALF OF THE FOOL - AN ANSWER', 'TABLE OF CONTENTS']) {
      expect(classifyHeading(t, 'monologium', 0)).toEqual({ kind: 'leave out' });
    }
  });

  it("keeps a chapter's title as text", () => {
    expect(classifyHeading('HE IS GREATER THAN CAN BE CONCEIVED.', 'proslogium', 0)).toEqual({ kind: 'text' });
  });
});

describe('placeParts and placeLabel', () => {
  it('cites and labels a page by its divisions', () => {
    expect(placeParts({ work: 'curDeusHomo', book: 2, chapter: 6 })).toEqual([
      { type: 'book', value: 2 }, { type: 'chapter', value: 6 },
    ]);
    expect(placeParts({ work: 'meditations', chapter: 1, section: 4 })).toEqual([
      { type: 'chapter', value: 1 }, { type: 'position', value: 4 },
    ]);
    expect(placeLabel({ work: 'curDeusHomo', book: 2, chapter: 6 })).toBe('Book II, Chapter VI');
    expect(placeLabel({ work: 'meditations', chapter: 1, section: 4 })).toBe('Meditation I, Section IV');
    expect(placeLabel({ work: 'monologium', chapter: 0 })).toBe('Preface');
    expect(placeLabel({ work: 'letters', chapter: 5 })).toBe('Letter V');
  });
});

describe('anselmWorks', () => {
  const pages = [
    page(1, [heading('Proslogium; Monologium; An Appendix in Behalf of the Fool by'), heading('INTRODUCTION'),
             text('Modern introduction.')]),
    page(2, [heading('Monologium - On The Being Of God')]),
    page(3, [heading('CHAPTER I'), heading('THERE IS A BEING WHICH IS BEST.'), text('Of all things')]),
    page(4, [block('PageHeader', 'Monologium'), text('that exist[12], one is best.')]),
    page(5, [heading('Monologium - On The Being Of God')]),
    page(6, [heading('CHAPTER II'), text('The same subject.')]),
    page(7, [heading('Psalms 15'), text('1 Yahweh, who shall dwell')]),
  ];
  const works = anselmWorks(pages);

  it('makes a page of each chapter, its heading first, without the names heading its pages', () => {
    const monologium = works.get('monologium')!;
    expect(monologium.map((p) => [p.pageNumber, p.printedPageNumber])).toEqual([[1, 'Chapter I'], [2, 'Chapter II']]);
    expect(monologium[0]!.blocks.map((b) => b.html)).toEqual([
      '<h2>CHAPTER I</h2>', '<h2>THERE IS A BEING WHICH IS BEST.</h2>', '<p>Of all things</p>', '<p>that exist, one is best.</p>',
    ]);
    expect(monologium[0]!.citationParts).toEqual([{ type: 'chapter', value: 1 }]);
  });

  it('leaves out the introduction and the Bible chapters', () => {
    expect([...works.keys()]).toEqual(['monologium']);
    expect(works.get('monologium')!.flatMap((p) => p.blocks).some((b) => b.html.includes('Yahweh'))).toBe(false);
  });
});

describe('withBibleCitations', () => {
  it('cites the Bible in parentheses, "ib." referring to the place before', () => {
    const [cited] = withBibleCitations('anselm-prayers-webb', [{
      pageNumber:        1,
      printedPageNumber: 'Prayer I',
      citationParts:     [],
      blocks:            [text('my soul (Ps. lxxxiii. 3), and (ib. 4); (Isa. Iv. 7)')],
    }]);
    expect(cited!.blocks[0]!.citations.map((c) => [c.title, ...c.locationsCited[0]!.slice(1).map((l) => l.values)]))
      .toEqual([['Psalms', [83], [3]], ['Psalms', [83], [4]], ['Isaias', [55], [7]]]);
  });
});

describe('biblePlaces in the translations of Anselm', () => {
  const read = (inner: string, previous: Parameters<typeof biblePlaces>[1] = null) => (
    biblePlaces(inner, previous).map((p) => [p.book, p.chapter, p.verses])
  );

  it('reads lower-case Roman chapters, "St." before a book, and the OCR\'s slips', () => {
    expect(read('Rom. viii. 32.')).toEqual([[52, 8, [32]]]);
    expect(read('1 St. Pet. iv. 8.')).toEqual([[67, 4, [8]]]);
    expect(read('St. John xvii. 21')).toEqual([[50, 17, [21]]]);
    expect(read('Ps. 1xviii. 5')).toEqual([[21, 68, [5]]]);
    expect(read('Zeph. i. 15, 16')).toEqual([[41, 1, [15, 16]]]);
  });

  it('reads "ib." as the place before: a chapter and verse, a verse, or the same place', () => {
    const before = { book: 50, bookName: 'St. John', chapter: 17, verses: [21], raw: 'St. John xvii. 21' };
    expect(read('ib. viii. 10.', before)).toEqual([[50, 8, [10]]]);
    expect(read('ib. 24', before)).toEqual([[50, 17, [24]]]);
    expect(read('ib.', before)).toEqual([[50, 17, [21]]]);
    expect(read('ib. 24')).toEqual([]);
  });
});

describe('ANSELM_ALIAS_WORKS', () => {
  it("matches citations of Anselm's treatises", () => {
    const [proslogium, monologium, curDeusHomo] = ANSELM_ALIAS_WORKS;
    expect(['Anselm', 'St. Anselm', 'S. Anselmus'].every((a) => monologium!.author.test(a))).toBe(true);
    expect(['Monolog.', 'Monol.', 'Monologium', 'Monologion'].every((t) => monologium!.title.test(t))).toBe(true);
    expect(proslogium!.title.test('Proslogio')).toBe(true);
    expect(curDeusHomo!.title.test('Cur Deus Homo?')).toBe(true);
  });
});
