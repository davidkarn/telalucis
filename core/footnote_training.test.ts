import { describe, expect, it } from '@jest/globals';
import { placesOf } from './citation_groups.ts';
import {
  batchAnswer, citedShare, footnoteMarker, locationsFromPlaces, splitKey, splitOf, trainingChat,
  type SavedCitation,
} from './footnote_training.ts';
import type { FootnotePage } from './footnote_extraction.ts';
import type { CitationLocation } from '../types.ts';

const saved = (locations: CitationLocation[][]) => locations.flatMap((g) => placesOf(g).places);

describe('locationsFromPlaces', () => {
  it('undoes placesOf, splitting the joined labels when they split as many', () => {
    const locations: CitationLocation[][] = [
      [{ rawLabel: 'ch. 1', type: 'chapter', values: [1] }, { rawLabel: 'vv. 2-4', type: 'verse', values: [2, 3, 4] }],
      [{ rawLabel: 'p. 33', type: 'page', values: [33] }],
    ];

    expect(locationsFromPlaces(saved(locations))).toEqual(locations);
  });

  it('gives each type the whole label when the labels can\'t be told apart', () => {
    const locations: CitationLocation[][] = [[
      { rawLabel: 'Bk II', type: 'book', values: [2] },
      { rawLabel: 'ch 5, 8-9', type: 'chapter', values: [5, 8, 9] },
    ]];

    expect(locationsFromPlaces(saved(locations))).toEqual([[
      { rawLabel: 'Bk II, ch 5, 8-9', type: 'book', values: [2] },
      { rawLabel: 'Bk II, ch 5, 8-9', type: 'chapter', values: [5, 8, 9] },
    ]]);
  });

  it('makes a type the prompt doesn\'t name a position', () => {
    expect(locationsFromPlaces([{ raw: '§ 4', parts: [{ type: 'section', value: 4 }] }]))
      .toEqual([[{ rawLabel: '§ 4', type: 'position', values: [4] }]]);
  });
});

describe('footnoteMarker', () => {
  it('reads a <sup>, a number or a symbol at the start', () => {
    expect(footnoteMarker('<p><sup>12</sup> Ibid., p. 4.</p>')).toBe('12');
    expect(footnoteMarker('3 St. Thomas, Sum. Theol.')).toBe('3');
    expect(footnoteMarker('* Cf. Aristotle')).toBe('*');
    expect(footnoteMarker('continued from the page before')).toBe('');
  });
});

const page = (n: number, ...html: string[]): FootnotePage => ({
  page: n, blocks: html.map((h, i) => ({ label: 'Footnote', html: h, reading_order: i })),
});

const citation = (pageNumber: number, position: number, identifier: string, title: string): SavedCitation => ({
  pageNumber,
  position,
  identifier,
  author:   'St. Thomas',
  title,
  location: 'q. 1',
  raw:      `${ title }, q. 1`,
  groups:   [{ raw: 'q. 1', parts: [{ type: 'question', value: 1 }] }],
});

describe('batchAnswer', () => {
  it('gives each footnote its citations in reading order, and a page citing nothing its first footnote', () => {
    const batch     = [page(10, '<sup>1</sup> a', '<sup>2</sup> b'), page(11, '<sup>3</sup> nothing cited')];
    const citations = [citation(10, 1, '2', 'B'), citation(10, 0, '1', 'A1'), citation(10, 0, '1', 'A2'), citation(12, 0, '9', 'X')];

    expect(batchAnswer(batch, citations).map((f) => [f.page, f.identifier, f.citations.map((c) => c.title)]))
      .toEqual([[10, '1', ['A1', 'A2']], [10, '2', ['B']], [11, '3', []]]);
  });

  it('is the assistant\'s answer in the chat', () => {
    const chat = trainingChat('book', [page(10, '<sup>1</sup> a')], batchAnswer([page(10, '<sup>1</sup> a')], [citation(10, 0, '1', 'A')]));

    expect(chat.id).toBe('book:10');
    expect(chat.messages.map((m) => m.role)).toEqual(['system', 'user', 'assistant']);
    expect(chat.messages[1]!.content).toContain('<page number="10">');
    expect(JSON.parse(chat.messages[2]!.content).footnotes[0].citations[0].locationsCited)
      .toEqual([[{ rawLabel: 'q. 1', type: 'question', values: [1] }]]);
  });
});

describe('splitOf', () => {
  it('keeps a journal issue\'s articles together', () => {
    expect(splitKey('thomist_1961-07_35_3_p12')).toBe('thomist_1961-07_35_3');
    expect(splitOf('thomist_1961-07_35_3_p12')).toBe(splitOf('thomist_1961-07_35_3_p40'));
  });

  it('holds out about a tenth of the books', () => {
    const splits = Array.from({ length: 2000 }, (_, i) => splitOf(`book${ i }`));
    const held   = splits.filter((s) => s !== 'train').length;

    expect(held).toBeGreaterThan(140);
    expect(held).toBeLessThan(260);
  });
});

describe('citedShare', () => {
  it('is the share of the pages read with citations', () => {
    const answers = [batchAnswer([page(1, 'a'), page(2, 'b')], [citation(1, 0, '1', 'A')]), batchAnswer([page(3, 'c')], [])];

    expect(citedShare(answers)).toBeCloseTo(1 / 3);
    expect(citedShare([])).toBe(0);
  });
});
