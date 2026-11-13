import { describe, expect, it } from '@jest/globals';
import {
  answerScore, areCopies, authorKeys, fused, RRF_K, searchResults, shingles, withoutCopies, withVoices, type Candidate,
} from './passage_search.ts';

const passage = (id: string, more: Partial<Candidate> = {}): Candidate => ({
  passageId:         id,
  bookId:            `book-${ id }`,
  title:             `Title ${ id }`,
  author:            `Author ${ id }`,
  authorIds:         [`a${ id }`],
  pageNumber:        1,
  endPageNumber:     1,
  printedPageNumber: '1',
  text:              `text of ${ id }`,
  ...more,
});

const SUMMA = 'Whether the existence of God is self evident. It seems that the existence of God is self evident, '
  + 'for those things are said to be self evident to us the knowledge of which is naturally implanted in us';

describe('fused', () => {
  it('scores each candidate by its places in the lists, a citing one at half weight', () => {
    const ranked = fused({
      meaning: [passage('1'), passage('2')],
      words:   [passage('2'), passage('3')],
      cites:   [passage('4', { cites: { bookId: 'book-1', pageNumber: 1 } })],
    });
    expect(ranked.map((r) => r.passageId)).toEqual(['2', '1', '3', '4']);
    expect(ranked[0]!.ranks).toEqual({ meaning: 1, words: 0 });
    expect(ranked[0]!.fusedScore).toBeCloseTo(1 / (RRF_K + 1) + 1 / RRF_K);
    expect(ranked[3]!.fusedScore).toBeCloseTo(0.5 / RRF_K);
    expect(ranked[3]!.cites).toEqual({ bookId: 'book-1', pageNumber: 1 });
  });
});

describe('copies', () => {
  it("tells another printing of a text from a passage sharing a phrase", () => {
    const reprint = `${ SUMMA.replace('naturally', 'naturaly') } as the first principles`;
    const quoting = 'Saint Thomas asks whether the existence of God is self evident, and answers that it is not, to us';
    expect(areCopies(shingles(SUMMA), shingles(reprint))).toBe(true);
    expect(areCopies(shingles(SUMMA), shingles(quoting))).toBe(false);
  });

  it('makes the copies of a text one, the first kept, the others listed', () => {
    const kept = withoutCopies([
      passage('1', { text: SUMMA }),
      passage('2', { text: 'Something else entirely, about the will and its freedom of choice.' }),
      passage('3', { text: `${ SUMMA }.`, title: 'Of God and His Creatures', printedPageNumber: '9' }),
    ]);
    expect(kept.map((k) => k.passageId)).toEqual(['1', '2']);
    expect(kept[0]!.copies).toEqual([{ bookId: 'book-3', title: 'Of God and His Creatures', pageNumber: 1, printedPageNumber: '9' }]);
  });

  it("doesn't make one book's passages copies of each other", () => {
    expect(withoutCopies([passage('1', { text: SUMMA }), passage('2', { text: SUMMA, bookId: 'book-1' })])).toHaveLength(2);
  });
});

describe('voices', () => {
  it("knows an author by their ids, else by their name as printed", () => {
    expect(authorKeys({ authorIds: ['7', '9'], author: 'X and Y' })).toEqual(['id:7', 'id:9']);
    expect(authorKeys({ authorIds: [], author: 'Pohle, Joseph, 1852-1922.' })).toEqual(['name:pohle joseph']);
  });

  it('gives no author or book more than their places, then fills the rest in order', () => {
    const aquinas = (id: string, bookId: string) => passage(id, { authorIds: ['aquinas'], bookId });
    const results = [
      aquinas('1', 'summa'), aquinas('2', 'summa'), aquinas('3', 'summa'), aquinas('4', 'scg'),
      passage('5'), aquinas('6', 'veritate'), passage('7'),
    ];
    expect(withVoices(results, { limit: 5, perAuthor: 3, perBook: 2 }).map((r) => r.passageId)).toEqual(['1', '2', '4', '5', '7']);
    expect(withVoices(results.slice(0, 3), { limit: 3, perAuthor: 1, perBook: 1 }).map((r) => r.passageId)).toEqual(['1', '2', '3']);
  });
});

describe('searchResults', () => {
  it('orders by the reranker\'s scores when given, leaving out what it didn\'t score', () => {
    const candidates = fused({ meaning: [passage('1'), passage('2'), passage('3')] });
    const results    = searchResults(candidates, new Map([['1', 0.1], ['2', 0.9]]));
    expect(results.map((r) => [r.passageId, r.score, r.foundBy])).toEqual([['2', 0.9, ['meaning']], ['1', 0.1, ['meaning']], ['3', -Infinity, ['meaning']]]);
    expect(Object.keys(results[0]!)).not.toContain('ranks');
  });

  it('orders by the fused score without a reranker', () => {
    const results = searchResults(fused({ meaning: [passage('1'), passage('2')], words: [passage('2')] }), null);
    expect(results.map((r) => r.passageId)).toEqual(['2', '1']);
  });
});

describe('answerScore', () => {
  it('finds the first result on an expected page, how many answers are found, and how many authors', () => {
    const results = [
      passage('1', { bookId: 'pohle' }),
      passage('2', { bookId: 'summa', pageNumber: 12, endPageNumber: 13, authorIds: ['aquinas'] }),
      passage('3', { bookId: 'summa', pageNumber: 20, endPageNumber: 20, authorIds: ['aquinas'] }),
    ];
    expect(answerScore(results, [{ bookId: 'summa', pages: [13] }, { bookId: 'scg', pages: [9] }]))
      .toEqual({ firstAnswer: 2, answersFound: 1, authors: 2 });
    expect(answerScore(results, [{ bookId: 'scg', pages: [9] }]).firstAnswer).toBeNull();
  });
});
