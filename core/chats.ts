// Readers' chats with an LLM about the collection (see api/chats.service.ts):
// each message they send is searched for in the collection's passages (see
// lib/passage_search.ts), and the model is given the passages found, numbered,
// and asked to answer from them, citing each it draws on as a markdown link to
// its number ("[Aquinas on the soul](3)"). A passage keeps its number for the
// whole conversation, so an answer may also cite one given for an earlier
// message. Each signed-in reader may send so many messages a day, and the app
// as a whole so many (UTC days). Pure functions.
import type { Candidate } from './passage_search.ts';

// The model answering, unless CHAT_MODEL names another (an OpenRouter one)
export const DEFAULT_CHAT_MODEL = 'anthropic/claude-haiku-5.5';

// An answer's limit, in tokens
export const CHAT_MAX_TOKENS = 2000;

export const MAX_MESSAGE_LENGTH = 2000;

const MAX_TITLE_LENGTH = 80;

// How many of the conversation's earlier messages the model is given
const HISTORY_MESSAGES = 10;

// A message shorter than this many words is searched for with the reader's
// message before it, as a follow-up ("what about Scotus?") says little alone
const FOLLOW_UP_WORDS = 12;

// A passage given to the model for an answer, as the answer keeps it: its
// number in the conversation, its book and pages, and its text
export type ChatPassage = {
  n: number,
  passageId: string,
  bookId: string,
  title: string,
  author: string,
  pageNumber: number,
  endPageNumber: number,
  printedPageNumber: string,
  text: string,
};

export type ChatTurn = { role: 'user' | 'assistant', content: string };

// How many messages may be sent in a day: by each reader, and by all of them
export type ChatLimits = { perUser: number, perApp: number };

export const DEFAULT_CHAT_LIMITS: ChatLimits = { perUser: 20, perApp: 300 };

// The messages sent today, by this reader and by all, against the limits
export type ChatUsage = {
  used: number,
  limit: number,
  appUsed: number,
  appLimit: number,
  resetsAt: string,
};

const limitFrom = (raw: string | undefined, fallback: number): number => {
  const n = Number(raw);
  return raw !== undefined && raw.trim().length > 0 && Number.isInteger(n) && n >= 0 ? n : fallback;
};

// The limits, CHAT_USER_DAILY_LIMIT and CHAT_APP_DAILY_LIMIT when they're set
// to whole numbers, else the defaults
export const chatLimits = (env: Record<string, string | undefined>): ChatLimits => ({
  perUser: limitFrom(env['CHAT_USER_DAILY_LIMIT'], DEFAULT_CHAT_LIMITS.perUser),
  perApp:  limitFrom(env['CHAT_APP_DAILY_LIMIT'], DEFAULT_CHAT_LIMITS.perApp),
});

// The start of the UTC day `now` is in, and of the next
export const dayStart = (now: Date): Date => (
  new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate()))
);

export const nextDayStart = (now: Date): Date => (
  new Date(dayStart(now).getTime() + 24 * 60 * 60 * 1000)
);

export const chatUsage = (
  counts: { user: number, app: number }, limits: ChatLimits, now: Date,
): ChatUsage => ({
  used:     counts.user,
  limit:    limits.perUser,
  appUsed:  counts.app,
  appLimit: limits.perApp,
  resetsAt: nextDayStart(now).toISOString(),
});

// Why another message can't be sent today; null when it can
export const limitReached = (usage: ChatUsage): string | null => {
  if (usage.used >= usage.limit) {
    return `You've sent the ${ usage.limit } messages you can send today. You can send more after `
      + `${ usage.resetsAt.slice(11, 16) } UTC.`;
  }
  else if (usage.appUsed >= usage.appLimit) {
    return 'Chat has answered as many messages as it can today, while it is being tried out. '
      + `It will answer more after ${ usage.resetsAt.slice(11, 16) } UTC.`;
  }
  else {
    return null;
  }
};

// A message as sent: trimmed, and not empty nor too long
export const messageContent = (raw: unknown): { content: string } | { error: string } => {
  const content = typeof raw === 'string' ? raw.trim() : '';

  if (content.length === 0) {
    return { error: 'Write a message first' };
  }
  else if (content.length > MAX_MESSAGE_LENGTH) {
    return { error: `A message can be at most ${ MAX_MESSAGE_LENGTH } characters` };
  }
  else {
    return { content };
  }
};

// A conversation's title: its first message, on one line, cut at a word
// before MAX_TITLE_LENGTH characters
export const conversationTitle = (content: string): string => {
  const line = content.replace(/\s+/g, ' ').trim();

  if (line.length <= MAX_TITLE_LENGTH) {
    return line;
  }
  else {
    const cut   = line.slice(0, MAX_TITLE_LENGTH - 1);
    const space = cut.lastIndexOf(' ');
    return (space > MAX_TITLE_LENGTH / 2 ? cut.slice(0, space) : cut).trimEnd() + '…';
  }
};

// What to search the collection for: the message, after the reader's message
// before it when it's a short follow-up
export const searchQuery = (content: string, earlier: ChatTurn[]): string => {
  const previous = earlier.filter((t) => t.role === 'user').at(-1);
  const words    = content.split(/\s+/).filter((w) => w.length > 0).length;

  return previous !== undefined && words < FOLLOW_UP_WORDS ? `${ previous.content }\n${ content }` : content;
};

// The passages found for a message, numbered: one given before in the
// conversation keeps its number, the others are numbered after the highest
export const numberedPassages = (found: Candidate[], earlier: ChatPassage[]): ChatPassage[] => {
  const known = new Map(earlier.map((p) => [p.passageId, p.n]));
  let next    = Math.max(0, ...earlier.map((p) => p.n)) + 1;

  return found.map((c) => ({
    n:                 known.get(c.passageId) ?? next++,
    passageId:         c.passageId,
    bookId:            c.bookId,
    title:             c.title,
    author:            c.author,
    pageNumber:        c.pageNumber,
    endPageNumber:     c.endPageNumber,
    printedPageNumber: c.printedPageNumber,
    text:              c.text,
  }));
};

// Where a passage starts, as a reader would cite it: "p. 12", the printed
// number as it is when it isn't one ("xii", "Book II, Chapter 65"), or the
// scan's page when none is printed (or a running head was taken for one:
// "QUAESTIONES DISPUTATAE DE ANIMA")
export const passagePlace = (p: Pick<ChatPassage, 'pageNumber' | 'printedPageNumber'>): string => {
  const printed = p.printedPageNumber.trim();

  if (/^\d+$/.test(printed)) {
    return `p. ${ printed }`;
  }
  else if (printed.length === 0 || printed.length > 24 || /^[^a-z]*[A-Z]{4}[^a-z]*$/.test(printed)) {
    return `scan page ${ p.pageNumber }`;
  }
  else {
    return printed;
  }
};

export const CHAT_SYSTEM_PROMPT = `You are a research assistant for Tela Lucis, a library of \
philosophy and theology (mostly scholastic and Catholic works, and the journals about them). A \
reader is asking you about the collection. With each of their messages you are given passages \
from the collection found for it, each numbered.

Answer from the passages. When you draw on one, cite it with a markdown link whose target is the \
passage's number, and whose text says briefly what it is: "Thomas holds that the soul is the form \
of the body [Summa Theologiae I, q. 76, a. 1](3)." Cite only the numbers you were given, in this \
message or earlier in the conversation; never make up a citation. Cite several passages when \
several bear on a point, and say where they disagree. The reader sees only the links' text, not \
the numbers, so never write "passage 8" or "the third passage" in your prose: name the author or \
the work instead.

The passages are what a search found, not the whole collection. When they don't answer the \
question, say so plainly, and say what they do cover; don't say the collection lacks something, \
only that it wasn't found. You may add what is commonly known, but say that it isn't from the \
passages, and don't cite it. Quote \
sparingly and exactly. Write in the reader's language, in plain prose with short paragraphs \
(markdown: **bold**, *italics*, and lists when they help), and keep to what was asked.`;

// Said again with each message, next to its passages: models drift from the
// instructions as a conversation goes on, and copy their earlier answers
export const CITATION_REMINDER = '(Answer from these passages and those given before. Cite each '
  + 'passage you draw on as a markdown link to its number, like [Cronin, The Science of Ethics](15), '
  + 'at the point you draw on it. Never write "passage 15" in your prose.)';

// The passages, as the model is given them
export const passagesPart = (passages: ChatPassage[]): string => (
  passages.length === 0
    ? 'No passages in the collection were found for this message.'
    : passages.map((p) => [
      `<passage number="${ p.n }">`,
      `${ p.author.length > 0 ? p.author : 'Anonymous' }, ${ p.title }, ${ passagePlace(p) }`,
      p.text.trim(),
      '</passage>',
    ].join('\n')).join('\n\n')
);

// The end of the conversation, as the model is given it: the last
// HISTORY_MESSAGES messages with something in them, from a reader's
const recentTurns = (earlier: ChatTurn[]): ChatTurn[] => {
  const recent = earlier.filter((t) => t.content.trim().length > 0).slice(-HISTORY_MESSAGES);
  const first  = recent.findIndex((t) => t.role === 'user');
  return first < 0 ? [] : recent.slice(first);
};

// The model's messages for an answer: the instructions, the end of the
// conversation so far, and the reader's message with the passages found for it
export const chatRequestMessages = (
  earlier: ChatTurn[], passages: ChatPassage[], content: string,
): { role: 'system' | 'user' | 'assistant', content: string }[] => [
  { role: 'system', content: CHAT_SYSTEM_PROMPT },
  ...recentTurns(earlier),
  {
    role:    'user',
    content: `Passages found in the collection for this message:\n\n${ passagesPart(passages) }\n\n`
      + `${ CITATION_REMINDER }\n\nMy message:\n\n${ content }`,
  },
];
