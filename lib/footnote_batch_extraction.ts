// Extracting the citations in many books' footnotes through OpenRouter's Batch
// API (see lib/open_router_batches.ts), for about half the price of asking a
// request at a time, and without waiting between requests: a request for each
// batch of pages of each book (see footnoteBatches), sent in API batches of
// batchSize, each waited for (most take minutes, at most a day). Each answer
// is kept in <dir>/answers.jsonl by a hash of its request, and the API batches
// submitted and not yet collected in <dir>/pending.json: a run stopped while
// waiting collects them when run again, and a request answered is never sent
// again (a change to the prompt, model or examples makes another request). The
// pages the model missed, and those of requests that failed, are then asked a
// request at a time (see askFootnoteBatches). Needs OPENROUTER_KEY.
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { answeredBatch, footnoteBatches, type BatchFootnotes } from '../core/footnote_batches.ts';
import type { ExtractedCitations } from '../core/ocred_book_import.ts';
import {
  askFootnoteBatches, DEFAULT_FOOTNOTES_MODEL, defaultReasoning, footnoteBatchRequest, type ExtractionOptions,
} from './footnote_citations.ts';
import { chatRequestBody, isLocalModel, parseJsonResponse } from './open_router.ts';
import {
  awaitBatch, batchResults, submitBatch, type BatchRequest, type BatchResult,
} from './open_router_batches.ts';
import type { SuryaPage } from '../types.ts';

export const DEFAULT_BATCH_SIZE = 500;

// How often an API batch is asked about, and each request's answer limit
// (thinking included): answers are about 5,000 tokens, and OpenRouter
// estimates a batch's cost, to check the balance, from the limit
const POLL_MS    = 30_000;
const MAX_TOKENS = 16_000;

export type BookPages = { bookId: string, pages: SuryaPage[] };

export type BatchExtractionOptions = ExtractionOptions & { dir: string, batchSize?: number };

// A request's answer, kept by its key (see requestKey): the model's footnotes, or why there are none
type SavedAnswer = { key: string, result: BatchFootnotes | null, error: string | null };

// An API batch submitted and not yet collected: its id, and its requests' keys (their custom_ids)
type Pending = { id: string, keys: string[] };

// A request's key: a hash of its body, so the same request is known again
const requestKey = (body: object): string => (
  createHash('sha256').update(JSON.stringify(body)).digest('hex').slice(0, 32)
);

const readLines = <T>(file: string): T[] => (
  fs.existsSync(file)
    ? fs.readFileSync(file, 'utf8').split('\n').filter((l) => l.trim().length > 0).map((l) => JSON.parse(l) as T)
    : []
);

// A request's answer in its result
const answerOf = (key: string, result: BatchResult | undefined, failure: string | null): SavedAnswer => {
  if (failure !== null) {
    return { key, result: null, error: failure };
  }
  else if (result === undefined) {
    return { key, result: null, error: 'no result for this request' };
  }
  else if (result.response === null || result.response.status_code !== 200) {
    return { key, result: null, error: result.error?.message ?? `status ${ result.response?.status_code ?? 'unknown' }` };
  }
  else {
    try {
      return { key, result: parseJsonResponse<BatchFootnotes>(result.response.body), error: null };
    }
    catch (e) {
      return { key, result: null, error: (e as Error).message };
    }
  }
};

// Ask for the citations in the books' footnotes through the Batch API (see
// the top), waiting for every API batch; then each book's citations, its
// misses and failures asked a request at a time when they're wanted
export const extractInBatches = async(
  books: BookPages[], options: BatchExtractionOptions,
): Promise<{ citationsOf: (bookId: string) => Promise<ExtractedCitations> }> => {
  const model = options.model ?? DEFAULT_FOOTNOTES_MODEL;

  if (isLocalModel(model)) {
    throw new Error(`a local model (${ model }) has no Batch API: extract without --batch`);
  }

  const reasoning   = options.reasoning ?? defaultReasoning(model);
  const answersFile = path.join(options.dir, 'answers.jsonl');
  const pendingFile = path.join(options.dir, 'pending.json');
  const answers     = new Map(readLines<SavedAnswer>(answersFile).map((a) => [a.key, a]));
  let pending       = fs.existsSync(pendingFile) ? JSON.parse(fs.readFileSync(pendingFile, 'utf8')) as Pending[] : [];

  fs.mkdirSync(options.dir, { recursive: true });

  const savePending = () => fs.writeFileSync(pendingFile, JSON.stringify(pending, null, 2) + '\n');

  const asks = books.flatMap(({ bookId, pages }) => footnoteBatches(pages).map((batch) => {
    const { messages, format } = footnoteBatchRequest(bookId, batch, options.examples);
    const body                 = chatRequestBody(messages, format, model, reasoning, MAX_TOKENS);
    return { bookId, batch, body, key: requestKey(body) };
  }));

  // an API batch's answers, kept once it's done, and it taken off the pending
  const collect = async(p: Pending) => {
    const batch   = await awaitBatch(p.id, POLL_MS, (b) => console.log(
      `  batch ${ b.id }: ${ b.status }, ${ b.request_counts.completed }/${ b.request_counts.total } done`
        + (b.request_counts.failed > 0 ? `, ${ b.request_counts.failed } failed` : ''),
    ));
    const results = batch.status === 'completed' ? await batchResults(p.id) : [];
    const byKey   = new Map(results.map((r) => [r.custom_id, r]));
    const failure = batch.status === 'completed' ? null : `batch ${ batch.status }${ batch.error?.message ? ': ' + batch.error.message : '' }`;
    const got     = p.keys.map((key) => answerOf(key, byKey.get(key), failure));

    fs.appendFileSync(answersFile, got.map((a) => JSON.stringify(a) + '\n').join(''));
    got.forEach((a) => answers.set(a.key, a));
    pending = pending.filter((q) => q.id !== p.id);
    savePending();
    console.log(`  batch ${ p.id }: ${ got.filter((a) => a.result !== null).length }/${ got.length } answered`);
  };

  for (const p of [...pending]) {
    console.log(`collecting batch ${ p.id }, submitted before`);
    await collect(p);
  }

  const todo   = [...new Map(asks.filter((a) => !answers.get(a.key)?.result).map((a) => [a.key, a])).values()];
  const size   = options.batchSize ?? DEFAULT_BATCH_SIZE;
  const chunks = Array.from({ length: Math.ceil(todo.length / size) }, (_, i) => todo.slice(i * size, (i + 1) * size));

  console.log(`${ books.length } books, ${ asks.length } requests: ${ asks.length - todo.length } answered before, `
    + `${ todo.length } to send in ${ chunks.length } batches to ${ model }`);

  const sent: Pending[] = [];
  for (const chunk of chunks) {
    const requests: BatchRequest[] = chunk.map((a) => ({ custom_id: a.key, body: a.body }));
    const batch                    = await submitBatch(model, requests);
    const p                        = { id: batch.id, keys: chunk.map((a) => a.key) };

    pending.push(p);
    sent.push(p);
    savePending();
    console.log(`submitted batch ${ batch.id }: ${ chunk.length } requests (${ batch.status })`);
  }
  for (const p of sent) {
    await collect(p);
  }

  return {
    citationsOf: (bookId) => {
      const answered = asks.filter((a) => a.bookId === bookId).map((a) => {
        const saved = answers.get(a.key);
        return saved?.result
          ? answeredBatch(bookId, a.batch, saved.result)
          : { citations: [], strays: 0, missed: a.batch, retry: [a.batch] };
      });
      const retry    = answered.flatMap((a) => a.retry);

      if (retry.length > 0) {
        console.log(`  ${ bookId }: asking again for ${ retry.flat().length } pages missed or failed, a request at a time`);
      }

      return askFootnoteBatches(bookId, retry, options, answered.flatMap((a) => a.citations));
    },
  };
};
