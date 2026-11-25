// OpenRouter's Batch API (https://openrouter.ai/docs/guides/features/batch-api): many chat
// completion requests sent at once, for about half the price, answered within 24 hours (most
// batches in minutes). A batch is submitted, then asked about until it's done, then its results
// are read a page at a time, matched to the requests by custom_id (they aren't in order). Each
// request's body is a chat completion request's (see chatRequestBody in lib/open_router.ts),
// and each result's body the response it would have had. Needs OPENROUTER_KEY.
import { setTimeout } from 'node:timers/promises';
import type { ORResponse } from './open_router.ts';

const API = 'https://openrouter.ai/api/v1/batches';

// A batch's statuses, those after which it changes no more last
export type BatchStatus =
  'validating' | 'in_progress' | 'finalizing' | 'completed' | 'failed' | 'expired' | 'cancelled';

export const FINAL_STATUSES: BatchStatus[] = ['completed', 'failed', 'expired', 'cancelled'];

// The parts of a batch this code reads. request_counts are an estimate until it's done.
export type Batch = {
  id: string,
  status: BatchStatus,
  request_counts: { total: number, completed: number, failed: number },
  error: { message?: string } | null,
};

export type BatchRequest = { custom_id: string, body: object };

// One request's result: its response, or its error
export type BatchResult = {
  custom_id: string,
  response: { status_code: number, body: ORResponse } | null,
  error: { message?: string, type?: string } | null,
};

const headers = () => ({
  Authorization:  'Bearer ' + process.env.OPENROUTER_KEY,
  'Content-Type': 'application/json',
});

// An API error, and whether asking again later may succeed: a batch just submitted isn't found
// for a while (404), its results aren't ready while it runs (409), and the rate limit and the
// server's own errors pass
export class BatchApiError extends Error {
  constructor(message: string, readonly status: number) {
    super(message);
  }

  get passing(): boolean {
    return [404, 409, 429].includes(this.status) || this.status >= 500;
  }
}

// The body of an API response, or an error with its message
const read = async <T>(response: Response, what: string): Promise<T> => {
  const body = await response.json().catch(() => null) as (T & { error?: { message?: string } }) | null;

  if (!response.ok || body === null) {
    throw new BatchApiError(`${ what }: ${ response.status } ${ body?.error?.message ?? response.statusText }`, response.status);
  }
  else {
    return body;
  }
};

// Errors in a row that may pass (see BatchApiError) before one is taken to be lasting
const PASSING_ERRORS = 10;

// `ask`'s answer, asking again every `everyMs` while its errors may pass, at most PASSING_ERRORS
// in a row
const patiently = async <T>(ask: () => Promise<T>, everyMs: number): Promise<T> => {
  for (let errors = 1; ; errors++) {
    try {
      return await ask();
    }
    catch (e) {
      if (!(e instanceof BatchApiError && e.passing) || errors >= PASSING_ERRORS) {
        throw e;
      }
      else {
        await setTimeout(everyMs);
      }
    }
  }
};

// Submit a batch of chat completion requests to `model` (its normal id, not ":batch")
export const submitBatch = async(model: string, requests: BatchRequest[]): Promise<Batch> => (
  read<Batch>(await fetch(API, {
    method:  'POST',
    headers: headers(),
    body:    JSON.stringify({ endpoint: '/v1/chat/completions', model, completion_window: '24h', requests }),
  }), 'submitting a batch')
);

export const getBatch = async(id: string): Promise<Batch> => (
  read<Batch>(await fetch(`${ API }/${ id }`, { headers: headers() }), `batch ${ id }`)
);

// The batch once it's done, asking every `everyMs`, telling `progress` each time it's asked
export const awaitBatch = async(
  id: string, everyMs: number, progress: (batch: Batch) => void,
): Promise<Batch> => {
  for (;;) {
    const batch = await patiently(() => getBatch(id), everyMs);
    progress(batch);

    if (FINAL_STATUSES.includes(batch.status)) {
      return batch;
    }
    else {
      await setTimeout(everyMs);
    }
  }
};

// Results are given at most this many a page
const RESULTS_PAGE = 1000;

// How long to wait before asking again for a page of results that may come (see patiently)
const RESULTS_RETRY_MS = 10_000;

// Every result of a batch that's done, a page at a time
export const batchResults = async(id: string): Promise<BatchResult[]> => {
  const results: BatchResult[] = [];
  let cursor: string | null    = null;

  for (;;) {
    const query: string = new URLSearchParams({
      limit: String(RESULTS_PAGE), ...(cursor === null ? {} : { cursor }),
    }).toString();
    const page          = await patiently(async() => read<{
      data: BatchResult[], has_more: boolean, next_cursor: string | null,
    }>(await fetch(`${ API }/${ id }/results?${ query }`, { headers: headers() }), `batch ${ id }'s results`),
    RESULTS_RETRY_MS);

    results.push(...page.data);
    if (!page.has_more || page.next_cursor === null) {
      return results;
    }
    else {
      cursor = page.next_cursor;
    }
  }
};
