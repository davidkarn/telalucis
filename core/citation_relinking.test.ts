import { describe, expect, it } from '@jest/globals';
import { relinkPlan, type RelinkCitation, type WorkPages } from './citation_relinking.ts';
import { AQUINAS_AUTHOR } from './citation_authors.ts';

let nextId = 1;
const cite = (source: string, author: string, title: string, raw: string, extra: Partial<RelinkCitation> = {}): RelinkCitation => ({
  id:                String(nextId++),
  source_book_id:    source,
  author,
  title,
  location:          '',
  raw,
  reference_book_id: null,
  ...extra,
});

const BOOKS = [
  { id: 'summa-theologiae', title: 'Summa Theologiae', author: 'Thomas Aquinas' },
  { id: 'npnf102-city-of-god', title: 'The City of God', author: 'Augustine of Hippo' },
  { id: 'denzinger', title: 'Enchiridion symbolorum et definitionum', author: 'Denzinger, Heinrich, 1819-1883' },
  { id: 'cg', title: 'Summa Contra Gentiles', author: 'Thomas Aquinas' },
];

// the City of God's pages: book 14, chapters 1 to 3
const PAGES: WorkPages = new Map([
  ['npnf102-city-of-god', [1, 2, 3].map((n) => ({ pageId: n, parts: [{ type: 'book', value: 14 }, { type: 'chapter', value: n }] }))],
]);

describe('relinkPlan', () => {
  it('points citations of the Summa at it with their places, an "Ibid." from the one before', () => {
    const plan = relinkPlan([
      cite('a', 'St. Thomas', 'Summa Theologica', 'Summa Theol., I, q. 2, a. 3.', { reference_book_id: 'summa-theologiae' }),
      cite('a', 'St. Thomas', 'Summa Theologica', 'Ibid., a. 4.'),
    ], BOOKS, PAGES);

    expect(plan.located).toEqual([{
      bookId:    'summa-theologiae',
      citations: [{ id: expect.any(String), groups: [{ raw: 'Ibid., a. 4.', parts: expect.arrayContaining([{ type: 'question', value: 2 }]) }] }],
    }]);
    expect(plan.links).toEqual([]);
  });

  it('reads an alias work\'s places on its pages, leaving those on no page unlinked', () => {
    const plan = relinkPlan([
      cite('a', 'Augustine', 'De Civ. Dei', 'De Civ. Dei, xiv, 2', { location: 'xiv, 2' }),
      cite('a', 'Augustine', 'De Civ. Dei', 'De Civ. Dei, xxii, 9', { location: 'xxii, 9' }),
    ], BOOKS, PAGES);

    expect(plan.located.map((l) => [l.bookId, l.citations.map((c) => c.groups.map((g) => g.parts))])).toEqual([
      ['npnf102-city-of-god', [[[{ type: 'book', value: 14 }, { type: 'chapter', value: 2 }]]]],
    ]);
    expect(plan.unplaced).toBe(1);
  });

  it('matches the others by author and title, never to their own book', () => {
    const plan = relinkPlan([
      cite('a', 'Denzinger', 'Enchiridion Symbolorum', 'Denzinger, Enchiridion Symbolorum, n. 3'),
      cite('denzinger', 'Denzinger', 'Enchiridion Symbolorum', 'Enchiridion, n. 4'),
      cite('a', 'Denzinger', 'Enchiridion Symbolorum', 'Denz., n. 5', { reference_book_id: 'other' }),
    ], BOOKS, PAGES);

    expect(plan.links.map((l) => l.bookId)).toEqual(['denzinger']);
  });

  it('fills in the authors the extraction left out, and then links them', () => {
    const plan = relinkPlan([cite('a', '', 'Summa Contra Gentiles', 'Summa Contra Gentiles, III, 69')], BOOKS, PAGES);

    expect(plan.names).toEqual([{ id: expect.any(String), author: AQUINAS_AUTHOR, title: 'Summa Contra Gentiles' }]);
    expect(plan.links.map((l) => l.bookId)).toEqual(['cg']);
  });

  it('links only to the books given', () => {
    const plan = relinkPlan([cite('a', 'Denzinger', 'Enchiridion Symbolorum', 'Denz.')], [BOOKS[1]!], PAGES);
    expect(plan.links).toEqual([]);
  });
});
