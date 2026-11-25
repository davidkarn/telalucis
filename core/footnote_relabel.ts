// Relabelling the chats to train a footnote extraction model on (see
// core/footnote_training.ts): each batch's citations extracted again by a
// stronger model, the teacher, in place of those saved, which were extracted
// by a weaker one, under older rules, with pages missed. The teacher may be
// given a longer prompt (with insights) than the model trained, which learns
// to do without it. Each answer is compared with the citations saved (as
// core/footnote_eval.ts compares a model's with those expected) to judge it:
//
// - keep: it answers every page with citations saved and finds about as many;
// - more: it finds many more than were saved, mostly those missed before:
//   kept, and listed to look over a sample of;
// - drop: it skips a page with citations saved, or finds far fewer, or the
//   request failed: likely the teacher's mistake, and not trained on.
//
// The answers kept can be written back to the collection in place of the
// citations saved (see writtenBackCitations).
//
// Pure functions; cli/commands/relabel_footnote_training.command.ts asks the
// teacher and writes the files, and apply_relabelled_citations.command.ts
// writes the answers back.
import { scoreBatch, type BatchScore } from './footnote_eval.ts';
import { trainingPrompt, type BatchFootnote, type TrainingChat } from './footnote_training.ts';
import { footnoteBlockFor, type PlacedCitation } from './footnote_extraction.ts';
import type { Citation } from '../types.ts';

// A training chat's parts: the batch's id, what the model is given, its
// pages, and the answer saved
export type TrainingBatch = { id: string, message: string, pages: number[], saved: BatchFootnote[] };

export const trainingBatch = (chat: TrainingChat): TrainingBatch => {
  const message = chat.messages.find((m) => m.role === 'user')?.content ?? '';
  const answer  = chat.messages.find((m) => m.role === 'assistant')?.content ?? '{"footnotes":[]}';

  return {
    id:    chat.id,
    message,
    pages: [...message.matchAll(/<page number="(\d+)">/g)].map((m) => Number(m[1])),
    saved: (JSON.parse(answer) as { footnotes: BatchFootnote[] }).footnotes,
  };
};

export type Verdict = 'keep' | 'more' | 'drop';

// An answer finding fewer than this share of the citations saved is dropped
export const MIN_SAVED_FOUND = 0.5;

// An answer finding more than this many times the citations saved, and
// MORE_MARGIN more, finds "more"
export const MORE_RATIO = 1.25;
export const MORE_MARGIN = 2;

export type Judgement = { verdict: Verdict, reason: string, score: BatchScore };

// What's made of the teacher's answer for a batch (see the verdicts above);
// null footnotes for a request that failed
export const judgeAnswer = (batch: TrainingBatch, footnotes: BatchFootnote[] | null): Judgement => {
  const score    = scoreBatch(batch.saved, footnotes ?? []);
  const answered = new Set((footnotes ?? []).map((f) => f.page));
  const skipped  = [...new Set(batch.saved.filter((f) => f.citations.length > 0).map((f) => f.page))]
    .filter((p) => !answered.has(p));
  const stray    = (footnotes ?? []).filter((f) => !batch.pages.includes(f.page)).length;

  if (footnotes === null) {
    return { verdict: 'drop', reason: 'failed', score };
  }
  else if (skipped.length > 0) {
    return { verdict: 'drop', reason: `skipped page${ skipped.length > 1 ? 's' : '' } ${ skipped.join(', ') }`, score };
  }
  else if (stray > 0) {
    return { verdict: 'drop', reason: `${ stray } footnotes on pages not given`, score };
  }
  else if (score.expected > 0 && score.answered < MIN_SAVED_FOUND * score.expected) {
    return { verdict: 'drop', reason: `found ${ score.answered } of ${ score.expected } saved`, score };
  }
  else if (score.answered > MORE_RATIO * score.expected + MORE_MARGIN) {
    return { verdict: 'more', reason: `found ${ score.answered }, ${ score.expected } saved`, score };
  }
  else {
    return { verdict: 'keep', reason: '', score };
  }
};

// The chat with the teacher's answer, given the prompt the model trained is
// given (without insights, whatever the teacher was given)
export const relabelledChat = (batch: TrainingBatch, footnotes: BatchFootnote[]): TrainingChat => ({
  id:       batch.id,
  messages: [
    { role: 'system', content: trainingPrompt() },
    { role: 'user', content: batch.message },
    { role: 'assistant', content: JSON.stringify({ footnotes }) },
  ],
});

// A batch's book, from its id ("<book>:12,13,15", see batchId)
export const batchBookId = (id: string): string => id.slice(0, id.lastIndexOf(':'));

// A book's footnote blocks on each page, each with its position among the page's blocks (as
// citations are placed, see PlacedCitation)
export type FootnoteBlocks = Map<number, { position: number, html: string }[]>;

// A book's citations with the teacher's answers written back (apply-relabelled-citations): on
// each page of a batch whose answer is kept (keep or more, see judgeAnswer), the answer's
// citations, each in its footnote's block (see footnoteBlockFor), unlinked, in place of those
// saved; everywhere else, those saved, as they are (linked as they were). The answer's
// citations whose page has no block with their marker are left out, in unplaced.
// answered: the book's batches with their answers and verdicts; saved: its citations placed.
export const writtenBackCitations = (
  bookId: string,
  answered: { batch: TrainingBatch, footnotes: BatchFootnote[] | null, verdict: Verdict }[],
  saved: PlacedCitation[],
  blocks: FootnoteBlocks,
): { citations: PlacedCitation[], unplaced: Citation[], replacedPages: number[] } => {
  const kept     = answered.filter((a) => a.verdict !== 'drop' && a.footnotes !== null);
  const replaced = new Set(kept.flatMap((a) => a.batch.pages));
  const fresh    = kept.flatMap((a) => (a.footnotes ?? []).filter((f) => replaced.has(f.page)).flatMap((f) => (
    f.citations.map((c): Citation => ({
      source:          { bookId, footnoteIdentifier: f.identifier, footnotePage: f.page },
      referenceBookId: null,
      author:          c.author,
      title:           c.title,
      collection:      c.collection ?? null,
      issue:           c.issue ?? null,
      location:        c.location,
      raw:             c.raw,
      locationsCited:  c.locationsCited,
    }))
  )));
  const placed   = fresh.map((citation) => ({
    citation,
    block: footnoteBlockFor(blocks.get(citation.source.footnotePage) ?? [], citation.source.footnoteIdentifier),
  }));

  return {
    citations: [
      ...saved.filter((s) => !replaced.has(s.pageNumber)),
      ...placed.flatMap(({ citation, block }) => (
        block === undefined ? [] : [{ pageNumber: citation.source.footnotePage, position: block.position, citation }]
      )),
    ].sort((a, b) => a.pageNumber - b.pageNumber || a.position - b.position),
    unplaced:      placed.filter((p) => p.block === undefined).map((p) => p.citation),
    replacedPages: [...replaced].sort((a, b) => a - b),
  };
};
