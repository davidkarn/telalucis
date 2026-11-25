import { describe, expect, it } from '@jest/globals';
import {
  batchBookId, judgeAnswer, relabelledChat, trainingBatch, writtenBackCitations, type TrainingBatch,
} from './footnote_relabel.ts';
import type { PlacedCitation } from './footnote_extraction.ts';
import { trainingPrompt, type BatchFootnote } from './footnote_training.ts';

const cite = (raw: string) => ({ author: 'St. Thomas', title: 'Summa', location: '', raw, locationsCited: [] });

const note = (page: number, ...raws: string[]): BatchFootnote => ({ page, identifier: '1', citations: raws.map(cite) });

const RAWS = ['Sum. Theol., I, q. 2', 'Metaph. XII, 7', 'De anima III, 5', 'Phys. II, 3', 'Eth. Nic. X, 7'];

const batch = (saved: BatchFootnote[]): TrainingBatch => ({ id: 'b:1,2', message: '', pages: [1, 2], saved });

describe('trainingBatch', () => {
  it('reads the id, the pages and the answer saved from a chat', () => {
    const saved = [note(12, 'Sum. Theol., I, q. 2')];

    expect(trainingBatch({
      id:       'b:12,13',
      messages: [
        { role: 'system', content: 'prompt' },
        { role: 'user', content: '2 pages: 12, 13.\n<page number="12">\nx\n</page>\n<page number="13">\ny\n</page>' },
        { role: 'assistant', content: JSON.stringify({ footnotes: saved }) },
      ],
    })).toMatchObject({ id: 'b:12,13', pages: [12, 13], saved });
  });
});

describe('judgeAnswer', () => {
  it('keeps an answer finding about as many as were saved', () => {
    expect(judgeAnswer(batch([note(1, ...RAWS)]), [note(1, ...RAWS), note(2)]).verdict).toBe('keep');
  });

  it('finds "more" when the answer has many more than were saved', () => {
    expect(judgeAnswer(batch([note(1, RAWS[0]!)]), [note(1, ...RAWS)])).toMatchObject({
      verdict: 'more', reason: 'found 5, 1 saved',
    });
  });

  it('drops a failed request, a skipped page, a page not given, or far fewer found', () => {
    expect(judgeAnswer(batch([note(1, ...RAWS)]), null).reason).toBe('failed');
    expect(judgeAnswer(batch([note(1, RAWS[0]!), note(2, RAWS[1]!)]), [note(1, RAWS[0]!)]).reason).toBe('skipped page 2');
    expect(judgeAnswer(batch([]), [note(3, RAWS[0]!)]).reason).toBe('1 footnotes on pages not given');
    expect(judgeAnswer(batch([note(1, ...RAWS)]), [note(1, RAWS[0]!, RAWS[1]!)])).toMatchObject({
      verdict: 'drop', reason: 'found 2 of 5 saved',
    });
  });

  it('keeps a page whose footnotes the teacher says cite nothing, when none were saved', () => {
    expect(judgeAnswer(batch([note(1, RAWS[0]!), note(2)]), [note(1, RAWS[0]!)]).verdict).toBe('keep');
  });
});

describe('relabelledChat', () => {
  it('gives the teacher\'s answer with the prompt without insights', () => {
    const chat = relabelledChat({ ...batch([]), message: 'the footnotes' }, [note(1, RAWS[0]!)]);

    expect(chat.messages[0]!.content).toBe(trainingPrompt());
    expect(chat.messages[1]!.content).toBe('the footnotes');
    expect(JSON.parse(chat.messages[2]!.content).footnotes[0].citations[0].raw).toBe(RAWS[0]);
  });
});

describe('batchBookId', () => {
  it("is the id's book, which may hold a colon", () => {
    expect(batchBookId('thomist_1939-04_1_1_p45:45,46')).toBe('thomist_1939-04_1_1_p45');
    expect(batchBookId('a:b:12,13')).toBe('a:b');
  });
});

describe('writtenBackCitations', () => {
  const batch = (pages: number[]): TrainingBatch => ({ id: 'b:' + pages.join(','), message: '', pages, saved: [] });
  const cite  = (raw: string) => ({ author: 'A', title: 'T', collection: null, issue: null, location: '', raw, locationsCited: [] });
  const saved = (pageNumber: number, raw: string): PlacedCitation => ({
    pageNumber, position: 3,
    citation: {
      source: { bookId: 'b', footnoteIdentifier: '1', footnotePage: pageNumber }, referenceBookId: 'linked',
      author: 'Old', title: '', location: '', raw, locationsCited: [],
    },
  });
  const blocks = new Map([
    [1, [{ position: 3, html: '<p><sup>1</sup> x</p>' }, { position: 4, html: '<p><sup>2</sup> y</p>' }]],
    [2, [{ position: 5, html: '<p><sup>1</sup> z</p>' }]],
  ]);

  it("replaces the citations on a kept batch's pages, and keeps the rest as they were", () => {
    const out = writtenBackCitations('b', [
      { batch: batch([1]), verdict: 'keep', footnotes: [
        { page: 1, identifier: '1', citations: [cite('new 1')] },
        { page: 1, identifier: '2', citations: [cite('new 2')] },
      ] },
      { batch: batch([2]), verdict: 'drop', footnotes: [{ page: 2, identifier: '1', citations: [cite('dropped')] }] },
    ], [saved(1, 'old 1'), saved(2, 'old 2')], blocks);

    expect(out.replacedPages).toEqual([1]);
    expect(out.citations.map((c) => [c.pageNumber, c.position, c.citation.raw, c.citation.referenceBookId]))
      .toEqual([[1, 3, 'new 1', null], [1, 4, 'new 2', null], [2, 3, 'old 2', 'linked']]);
    expect(out.unplaced).toEqual([]);
  });

  it('leaves out a citation whose marker is in no block of its page', () => {
    const out = writtenBackCitations('b', [
      { batch: batch([2]), verdict: 'more', footnotes: [{ page: 2, identifier: '7', citations: [cite('lost')] }] },
    ], [], blocks);

    expect(out.citations).toEqual([]);
    expect(out.unplaced.map((c) => c.raw)).toEqual(['lost']);
  });
});
