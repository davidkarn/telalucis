import { describe, expect, it } from '@jest/globals';
import {
  chatLimits, chatRequestMessages, chatUsage, conversationTitle, dayStart, limitReached,
  messageContent, numberedPassages, passagePlace, searchQuery, type ChatPassage,
} from './chats.ts';
import type { Candidate } from './passage_search.ts';

const candidate = (passageId: string): Candidate => ({
  passageId,
  bookId:            `book-${ passageId }`,
  title:             'A Title',
  author:            'An Author',
  authorIds:         [],
  pageNumber:        10,
  endPageNumber:     11,
  printedPageNumber: '4',
  text:              `text of ${ passageId }`,
});

const passage = (passageId: string, n: number): ChatPassage => ({ ...numberedPassages([candidate(passageId)], [])[0]!, n });

describe('chatLimits', () => {
  it('reads the limits from the environment, else the defaults', () => {
    expect(chatLimits({})).toEqual({ perUser: 20, perApp: 300 });
    expect(chatLimits({ CHAT_USER_DAILY_LIMIT: '5', CHAT_APP_DAILY_LIMIT: '0' })).toEqual({ perUser: 5, perApp: 0 });
    expect(chatLimits({ CHAT_USER_DAILY_LIMIT: 'lots', CHAT_APP_DAILY_LIMIT: ' ' })).toEqual({ perUser: 20, perApp: 300 });
  });
});

describe('limitReached', () => {
  const now    = new Date('2026-11-25T15:30:00Z');
  const limits = { perUser: 20, perApp: 300 };

  it('lets a message be sent under both limits', () => {
    expect(limitReached(chatUsage({ user: 19, app: 299 }, limits, now))).toBeNull();
  });

  it("stops one at the reader's limit, then at the app's", () => {
    expect(limitReached(chatUsage({ user: 20, app: 20 }, limits, now))).toMatch(/the 20 messages you can send today.*00:00 UTC/);
    expect(limitReached(chatUsage({ user: 3, app: 300 }, limits, now))).toMatch(/as many messages as it can today/);
  });

  it('counts the UTC day', () => {
    expect(dayStart(now).toISOString()).toBe('2026-11-25T00:00:00.000Z');
    expect(chatUsage({ user: 0, app: 0 }, limits, now).resetsAt).toBe('2026-11-26T00:00:00.000Z');
  });
});

describe('messageContent', () => {
  it('trims a message, and refuses an empty or too long one', () => {
    expect(messageContent('  Is grace a quality?  ')).toEqual({ content: 'Is grace a quality?' });
    expect(messageContent('   ')).toHaveProperty('error');
    expect(messageContent(42)).toHaveProperty('error');
    expect(messageContent('x'.repeat(2001))).toHaveProperty('error');
  });
});

describe('conversationTitle', () => {
  it('is the first message on one line, cut at a word', () => {
    expect(conversationTitle('What is\n  natural law?')).toBe('What is natural law?');
    const title = conversationTitle('How does Thomas Aquinas distinguish the natural law from the eternal law and from human law?');
    expect(title).toBe('How does Thomas Aquinas distinguish the natural law from the eternal law and…');
    expect(title.length).toBeLessThanOrEqual(80);
  });
});

describe('searchQuery', () => {
  const earlier = [
    { role: 'user' as const, content: 'What does Thomas say about the unity of the substantial form?' },
    { role: 'assistant' as const, content: 'He holds there is one [ST I q76](1).' },
  ];

  it('searches for a short follow-up with the message before it', () => {
    expect(searchQuery('And Scotus?', earlier)).toBe(`${ earlier[0]!.content }\nAnd Scotus?`);
  });

  it('searches for a full question alone, and for the first message alone', () => {
    const full = 'How did the Franciscan school answer the question of the plurality of forms in man?';
    expect(searchQuery(full, earlier)).toBe(full);
    expect(searchQuery('And Scotus?', [])).toBe('And Scotus?');
  });
});

describe('numberedPassages', () => {
  it('keeps the numbers of passages given before, and numbers the others after the highest', () => {
    const numbered = numberedPassages([candidate('7'), candidate('8'), candidate('9')], [passage('8', 2), passage('3', 5)]);
    expect(numbered.map((p) => [p.passageId, p.n])).toEqual([['7', 6], ['8', 2], ['9', 7]]);
    expect(numbered[0]).not.toHaveProperty('authorIds');
  });

  it('numbers from 1 in a new conversation', () => {
    expect(numberedPassages([candidate('7'), candidate('8')], []).map((p) => p.n)).toEqual([1, 2]);
  });
});

describe('passagePlace', () => {
  it('cites the printed page, else the scan page', () => {
    expect(passagePlace({ pageNumber: 10, printedPageNumber: '4' })).toBe('p. 4');
    expect(passagePlace({ pageNumber: 10, printedPageNumber: 'xii' })).toBe('xii');
    expect(passagePlace({ pageNumber: 10, printedPageNumber: '' })).toBe('scan page 10');
    expect(passagePlace({ pageNumber: 10, printedPageNumber: 'Book II, Chapter 65' })).toBe('Book II, Chapter 65');
    expect(passagePlace({ pageNumber: 10, printedPageNumber: 'QUAESTIONES DISPUTATAE DE ANIMA' })).toBe('scan page 10');
  });
});

describe('chatRequestMessages', () => {
  it('gives the instructions, the conversation from a reader message, and the passages with the message', () => {
    const messages = chatRequestMessages(
      [
        { role: 'assistant', content: 'left over' },
        { role: 'user', content: 'first' },
        { role: 'assistant', content: '' },
        { role: 'user', content: 'second' },
        { role: 'assistant', content: 'an answer' },
      ],
      [passage('8', 2)],
      'third',
    );

    expect(messages.map((m) => m.role)).toEqual(['system', 'user', 'user', 'assistant', 'user']);
    expect(messages.slice(1, 4).map((m) => m.content)).toEqual(['first', 'second', 'an answer']);
    expect(messages.at(-1)!.content).toContain('<passage number="2">\nAn Author, A Title, p. 4\ntext of 8\n</passage>');
    expect(messages.at(-1)!.content).toMatch(/My message:\n\nthird$/);
  });

  it('says when nothing was found', () => {
    expect(chatRequestMessages([], [], 'q').at(-1)!.content).toContain('No passages in the collection were found');
  });
});
