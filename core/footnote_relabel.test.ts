import { describe, expect, it } from '@jest/globals';
import { judgeAnswer, relabelledChat, trainingBatch, type TrainingBatch } from './footnote_relabel.ts';
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
