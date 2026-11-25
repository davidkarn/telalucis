import { describe, expect, it } from '@jest/globals';
import {
  addScores, comparable, evalSetJson, evalSetProblems, isArticle, sampleEvalBatches, matchCitations, NO_SCORE, placeKeys, sampleBatches, scoreBatch, scoreSummary,
  similarity,
} from './footnote_eval.ts';
import type { BatchFootnote } from './footnote_training.ts';

const cite = (raw: string, title = 'Summa Theologica', values = [1]) => ({
  author:         'St. Thomas',
  title,
  location:       'q. 1',
  raw,
  locationsCited: [[{ rawLabel: 'q. 1', type: 'question' as const, values }]],
});

const note = (page: number, identifier: string, ...citations: ReturnType<typeof cite>[]): BatchFootnote => ({
  page, identifier, citations,
});

describe('comparable and similarity', () => {
  it('ignore case, quotes, spacing and ending punctuation', () => {
    expect(comparable('  “Summa”  Theol. ')).toBe('"summa" theol');
    expect(similarity('Sum. Theol., I, q. 2', 'Sum.Theol. I, q. 2.')).toBeGreaterThan(0.85);
    expect(similarity('Sum. Theol., I, q. 2', 'Aristotle, Metaph.')).toBeLessThan(0.3);
  });
});

describe('placeKeys', () => {
  it('names each place cited', () => {
    expect(placeKeys([[
      { rawLabel: 'ch. 1', type: 'chapter', values: [1] }, { rawLabel: 'vv. 2-3', type: 'verse', values: [2, 3] },
    ]])).toEqual(['chapter=1/verse=2', 'chapter=1/verse=3']);
  });
});

describe('matchCitations', () => {
  it('matches the most alike on the same page, each once', () => {
    const expected                 = [note(1, '1', cite('Sum. Theol., I, q. 2'), cite('Metaph. XII, 7'))];
    const answered                 = [
      note(1, '1', cite('Metaph. XII, 7.')),
      note(2, '1', cite('Sum. Theol., I, q. 2')),
      note(1, '2', cite('Sum. Theol., I, q. 2, a. 1')),
    ];
    const { pairs, missed, extra } = matchCitations(expected, answered);

    expect(pairs.map(([w, g]) => [w.citation.raw, g.identifier])).toEqual([
      ['Metaph. XII, 7', '1'], ['Sum. Theol., I, q. 2', '2'],
    ]);
    expect(missed).toEqual([]);
    expect(extra.map((e) => e.page)).toEqual([2]);
  });
});

describe('scoreBatch', () => {
  it('counts the citations found, the fields and places right', () => {
    const expected = [note(1, '1', cite('Sum. Theol., I, q. 2', 'Summa Theologica', [2])), note(1, '2', cite('Gilson, Le thomisme'))];
    const answered = [note(1, '1', cite('Sum. Theol., I, q. 2', 'Summa theologica.', [3]))];
    const score    = scoreBatch(expected, answered);

    expect(score).toEqual({
      expected: 2,
      answered: 1,
      matched:  1,
      labelled: { identifier: 1, author: 1, title: 1, collection: 0, issue: 0, location: 1 },
      fields:   { identifier: 1, author: 1, title: 1, collection: 0, issue: 0, location: 1 },
      exact:    0,
      places:   { expected: 2, answered: 1, matched: 0 },
    });

    const summary = scoreSummary(addScores(NO_SCORE, score));
    expect(summary.citations).toEqual({ precision: 1, recall: 0.5, f1: 2 / 3 });
  });

  it('is exact when every field and place is right', () => {
    const expected = [note(1, '1', cite('Sum. Theol., I, q. 2'))];
    expect(scoreBatch(expected, expected).exact).toBe(1);
    expect(scoreBatch(expected, []).matched).toBe(0);
  });

  it('compares collection and issue only when expected has them', () => {
    const article  = { ...cite('Acta S. Sedis, Vol. XXXVI, p. 339'), collection: 'Acta S. Sedis', issue: 'Vol. XXXVI' };
    const unknown  = cite('Theol. Studies, 3 (1942), 45');
    const expected = [note(1, '1', article, unknown)];
    const answered = [note(1, '1', { ...article, issue: null }, { ...unknown, collection: 'Theological Studies' })];
    const score    = scoreBatch(expected, answered);

    expect(score.labelled).toMatchObject({ collection: 1, issue: 1, title: 2 });
    expect(score.fields).toMatchObject({ collection: 1, issue: 0, title: 2 });
    expect(score.exact).toBe(1);
    expect(scoreSummary(score).fields.issue).toBe(0);
  });
});

describe('evalSetProblems', () => {
  const set = (locationsCited: unknown) => ({ about: '', batches: [{
    id: 'b:1', bookId: 'b', checked: true, pages: [],
    expected: [note(1, '2', { ...cite('Prologue'), locationsCited } as ReturnType<typeof cite>)],
  }] });

  it('names a citation whose locations are not groups, or of a type not asked for', () => {
    expect(evalSetProblems(set([[{ rawLabel: 'q. 1', type: 'question', values: [1] }]]))).toEqual([]);
    expect(evalSetProblems(set([{ rawLabel: 'Prologue', type: 'part', values: [1] }]))[0])
      .toMatch(/^b:1 p1 fn 2: locationsCited must be a list of groups/);
    expect(evalSetProblems(set([[{ rawLabel: 'N. 403', type: 'number', values: [403] }]]))[0])
      .toMatch(/bad location/);
  });
});

describe('sampleBatches', () => {
  it('takes at most perBook a book, spread over its batches, the same every time', () => {
    const batches = ['a', 'b'].flatMap((bookId) => Array.from({ length: 10 }, (_, i) => ({ bookId, i })));
    const sampled = sampleBatches(batches, { count: 10, perBook: 2 });

    expect(sampled.map((b) => [b.bookId, b.i]).sort()).toEqual([['a', 0], ['a', 5], ['b', 0], ['b', 5]]);
    expect(sampleBatches(batches, { count: 10, perBook: 2 })).toEqual(sampled);
  });

  it('takes a batch of every book before a second of any', () => {
    const batches = ['a', 'b', 'c'].flatMap((bookId) => Array.from({ length: 5 }, (_, i) => ({ bookId, i })));

    expect(new Set(sampleBatches(batches, { count: 3, perBook: 3 }).map((b) => b.bookId)).size).toBe(3);
  });
});

describe('sampleEvalBatches', () => {
  it('takes half of journal articles and half of other books', () => {
    const books    = ['a', 'b'].flatMap((bookId) => Array.from({ length: 20 }, (_, i) => ({ bookId, i })));
    const articles = Array.from({ length: 30 }, (_, n) => ({ bookId: `thomist_1951-10_14_4_p${ n }`, i: 0 }));
    const sampled  = sampleEvalBatches([...books, ...articles], { count: 10, perBook: 3 });

    expect(isArticle('thomist_1951-10_14_4_p442')).toBe(true);
    expect(sampled.filter((b) => isArticle(b.bookId))).toHaveLength(5);
    expect(sampled.filter((b) => !isArticle(b.bookId)).map((b) => b.bookId).sort()).toEqual(['a', 'a', 'a', 'b', 'b']);
  });

  it('makes up from articles what the other books lack', () => {
    const articles = Array.from({ length: 30 }, (_, n) => ({ bookId: `j_2000-01_1_1_p${ n }` }));
    expect(sampleEvalBatches([{ bookId: 'a' }, ...articles], { count: 10, perBook: 3 })).toHaveLength(10);
  });
});

describe('evalSetJson', () => {
  it('is the same JSON, short parts on one line', () => {
    const value = { batches: [{ id: 'x'.repeat(120), locations: [[{ rawLabel: 'q. 1', type: 'question', values: [1] }]] }] };
    const json  = evalSetJson(value);

    expect(JSON.parse(json)).toEqual(value);
    expect(json).toContain('[[{"rawLabel":"q. 1","type":"question","values":[1]}]]');
    expect(json.split('\n').length).toBeGreaterThan(3);
  });
});
