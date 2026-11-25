// Training a model of our own to extract footnotes' citations (see
// core/footnote_extraction.ts) from the citations extracted already: each
// batch of an OCR'd book's pages, as footnoteBatches makes them, with the
// answer rebuilt from the citations saved, as a chat to fine-tune on (the
// "messages" format of mlx-lm and most fine-tuning tools). The prompt is the
// one without insights (footnotesPrompt(null)): a model trained on the answers
// needn't be told them, and a shorter prompt is read faster.
//
// What's saved is not quite what the model answered: the footnotes citing
// nothing aren't saved (a page with none is given its first footnote, citing
// nothing, so that the answer has every page), and each location group was
// saved as its places with the locators' labels joined (see
// core/citation_groups.ts), so that a group's labels are split again only when
// they can be told apart. Books are split into train, valid and test by their
// id (a journal issue's articles together), so the test books, from which the
// eval set is drawn (see core/footnote_eval.ts), are never trained on.
// Pure functions; cli/commands/export_footnote_training.command.ts reads the
// books and writes the files.
import type { PlaceGroup } from './citation_groups.ts';
import { batchHtml, type BatchFootnotes } from './footnote_batches.ts';
import {
  footnotesPrompt, LOCATION_TYPES, type FootnotePage,
} from './footnote_extraction.ts';
import { strip_tags } from './book_pages.ts';
import type { CitationLocation } from '../types.ts';

// A footnote of a batch's answer, without the insights asked for with them
export type BatchFootnote = BatchFootnotes['footnotes'][number];

// A citation as saved: the footnote block it's in (its page and position
// among the page's blocks), its marker, and its citation_groups rows in order
export type SavedCitation = {
  pageNumber: number,
  position: number,
  identifier: string,
  author: string,
  title: string,
  collection: string | null,
  issue: string | null,
  location: string,
  raw: string,
  groups: PlaceGroup[],
};

// A group's locators' labels, as placesOf joined them: one a type when the
// joined label splits into as many, else the whole label for each
const rawLabels = (raw: string, count: number): string[] => {
  const pieces = raw.split(', ');

  if (count === 1) {
    return [raw];
  }
  else if (pieces.length === count) {
    return pieces;
  }
  else {
    return Array.from({ length: count }, () => raw);
  }
};

const isLocationType = (type: string): type is CitationLocation['type'] => (
  (LOCATION_TYPES as string[]).includes(type)
);

// A citation's locations from its citation_groups rows: a group of the rows
// with the same label, each of its types with its values in the order first
// given (placesOf undone: "ch. 1, vv. 2-4", saved as three places, is chapter
// [1] and verse [2, 3, 4] again). A type the prompt doesn't name is
// "position", as the prompt asks.
export const locationsFromPlaces = (groups: PlaceGroup[]): CitationLocation[][] => {
  const byLabel = groups.reduce(
    (m, g) => m.set(g.raw, [...m.get(g.raw) ?? [], g]),
    new Map<string, PlaceGroup[]>(),
  );

  return [...byLabel].map(([raw, places]) => {
    const values = places.flatMap((p) => p.parts).reduce((m, { type, value }) => {
      const seen = m.get(type) ?? [];
      return m.set(type, seen.includes(value) ? seen : [...seen, value]);
    }, new Map<string, number[]>());
    const labels = rawLabels(raw, values.size);

    return [...values].map(([type, vals], i) => ({
      rawLabel: labels[i]!,
      type:     isLocationType(type) ? type : 'position',
      values:   vals,
    }));
  });
};

// A footnote's marker as printed at its start: a <sup>'s, or a number or
// symbol starting its text; "" when there's none (it continues from the page
// before)
export const footnoteMarker = (html: string): string => {
  const sup  = /^\s*(?:<[^>]+>\s*)*<sup>\s*([^<\s]+)\s*<\/sup>/i.exec(html);
  const text = /^\s*(\d{1,3}|[*†‡§¶]+)(?=[\s.)])/u.exec(strip_tags(html));

  if (sup !== null) {
    return sup[1]!;
  }
  else if (text !== null) {
    return text[1]!;
  }
  else {
    return '';
  }
};

// The answer for a batch from the citations saved of its pages: the footnotes
// citing something, page by page in reading order, each with its citations in
// the order saved; a page with none gets its first footnote citing nothing.
// citations: the book's, in the order saved (by id).
export const batchAnswer = (batch: FootnotePage[], citations: SavedCitation[]): BatchFootnote[] => (
  batch.flatMap((page) => {
    const onPage    = citations
      .map((c, order) => ({ c, order }))
      .filter(({ c }) => c.pageNumber === page.page)
      .sort((a, b) => a.c.position - b.c.position || a.order - b.order)
      .map(({ c }) => c);
    const footnotes = onPage.reduce((m, c) => m.set(c.identifier, [...m.get(c.identifier) ?? [], c]),
                                    new Map<string, SavedCitation[]>());
    const first     = [...page.blocks]
      .sort((a, b) => a.reading_order - b.reading_order)
      .find((b) => b.label === 'Footnote');

    if (footnotes.size === 0) {
      return [{ page: page.page, identifier: footnoteMarker(first?.html ?? ''), citations: [] }];
    }
    else {
      return [...footnotes].map(([identifier, cited]) => ({
        page:      page.page,
        identifier,
        citations: cited.map((c) => ({
          author:         c.author,
          title:          c.title,
          collection:     c.collection,
          issue:          c.issue,
          location:       c.location,
          raw:            c.raw,
          locationsCited: locationsFromPlaces(c.groups),
        })),
      }));
    }
  })
);

// A book is trained on (and tested with) only when at least this share of
// the pages whose footnotes are read have citations saved: some books' were
// extracted poorly (early imports, requests that failed), and their pages
// saved without citations would teach a model to leave citations out
export const MIN_COVERAGE = 0.7;

// The share of a book's batches' pages that have citations saved (see
// MIN_COVERAGE); 0 with no pages
export const citedShare = (answers: BatchFootnote[][]): number => {
  const pages = new Set(answers.flat().map((f) => f.page));
  const cited = new Set(answers.flat().filter((f) => f.citations.length > 0).map((f) => f.page));

  return pages.size === 0 ? 0 : cited.size / pages.size;
};

export type ChatMessage = { role: 'system' | 'user' | 'assistant', content: string };

// The system prompt a trained model is given: the one without insights
export const trainingPrompt = (): string => footnotesPrompt(null, true);

// A batch's id: its book's, with its pages ("<book>:12,13,15")
export const batchId = (bookId: string, batch: FootnotePage[]): string => (
  `${ bookId }:${ batch.map((p) => p.page).join(',') }`
);

// A chat to train on, with its batch's id (see batchId; mlx-lm reads only the messages)
export type TrainingChat = { id: string, messages: ChatMessage[] };

// A batch and its answer, as a chat to train on
export const trainingChat = (
  bookId: string, batch: FootnotePage[], answer: BatchFootnote[],
): TrainingChat => ({
  id:       batchId(bookId, batch),
  messages: [
    { role: 'system', content: trainingPrompt() },
    { role: 'user', content: batchHtml(batch) },
    { role: 'assistant', content: JSON.stringify({ footnotes: answer }) },
  ],
});

export type Split = 'train' | 'valid' | 'test';

// The percent of books held out for testing, and for validating while training
export const TEST_PERCENT = 5;
export const VALID_PERCENT = 5;

// What a book is split by: its id, a journal article's its issue's (the
// article's id without its "_p<page>")
export const splitKey = (bookId: string): string => bookId.replace(/_p\d+$/, '');

// FNV-1a: a hash that's the same on every run, to split books by
const fnv1a = (text: string): number => [...text].reduce(
  (h, ch) => Math.imul(h ^ ch.codePointAt(0)!, 0x01000193) >>> 0,
  0x811c9dc5,
);

// The split a book is in, the same every time
export const splitOf = (bookId: string): Split => {
  const bucket = fnv1a(splitKey(bookId)) % 100;

  if (bucket < TEST_PERCENT) {
    return 'test';
  }
  else if (bucket < TEST_PERCENT + VALID_PERCENT) {
    return 'valid';
  }
  else {
    return 'train';
  }
};

// The same order every time, unlike the books' (by id): to sample by
export const stableHash = fnv1a;
