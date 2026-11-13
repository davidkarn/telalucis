// Searching the collection's passages for a question (see core/passage_search.ts): its embedding's
// nearest passages and those with its words, merged; the passages citing the pages of the best of
// these (their commentators); the best few dozen of all scored again by the reranker (when it can
// be reached; else they keep their merged order); and the places shared out among the voices.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { DEFAULT_LIMITS, fused, searchResults, type SearchResult, type VoiceLimits } from '../core/passage_search.ts';
import { embeddingInput } from '../core/passages.ts';
import { PassageSearchQueries } from '../model/passage_search.ts';
import { embed } from './embeddings.ts';
import { rerank } from './reranker.ts';

export type SearchOptions = {
  // which lists to search: by meaning, by words, and citing the best found
  words: boolean,
  cites: boolean,
  // score the best found again with the reranker: how many of them (0: none)
  rerank: number,
  limits: VoiceLimits,
};

export const DEFAULT_SEARCH: SearchOptions = { words: true, cites: true, rerank: 60, limits: DEFAULT_LIMITS };

// Candidates each search finds
const POOL = 100;
// The pages whose citing passages are looked for: those of the best found by meaning and words
const CITED_PAGES = 5;
// Citing passages looked for of each such page
const CITING_PER_PAGE = 15;

export type SearchReport = {
  results: SearchResult[],
  // what each step took, in milliseconds; whether the reranker was used, and if not why
  timings: Record<string, number>,
  reranked: boolean | string,
};

const timed = async<T>(timings: Record<string, number>, step: string, work: () => Promise<T>): Promise<T> => {
  const started = Date.now();
  const result  = await work();
  timings[step] = Date.now() - started;
  return result;
};

export const searchPassages = async(
  db: Kysely<Database>, question: string, options: SearchOptions = DEFAULT_SEARCH,
): Promise<SearchReport> => {
  const timings: Record<string, number> = {};

  const [vector]         = await timed(timings, 'embed', () => embed([question]));
  const [meaning, words] = await timed(timings, 'search', () => Promise.all([
    PassageSearchQueries.findNearest(db, vector!, POOL),
    options.words ? PassageSearchQueries.findWithWords(db, question, POOL) : Promise.resolve([]),
  ]));

  // the commentators on the pages the best found are on
  const found  = fused({ meaning, words });
  const pages  = [...new Map(found.map((c) => [`${ c.bookId }\u0000${ c.pageNumber }`, c])).values()].slice(0, CITED_PAGES);
  const citing = options.cites
    ? await timed(timings, 'cites', async() => (
      (await Promise.all(pages.map((p) => PassageSearchQueries.findCiting(db, p, CITING_PER_PAGE)))).flat()
    ))
    : [];

  const candidates = fused({ meaning, words, cites: citing });
  const pool       = candidates.slice(0, Math.max(options.rerank, options.limits.limit));

  let scores: Map<string, number> | null = null;
  let reranked: boolean | string         = false;
  if (options.rerank > 0) {
    try {
      const toScore = pool.slice(0, options.rerank);
      const s       = await timed(timings, 'rerank', () => rerank(question, toScore.map((c) => embeddingInput(c, c))));
      scores        = new Map(toScore.map((c, i) => [c.passageId, s[i]!]));
      reranked      = true;
    }
    catch (e) {
      reranked = `not reranked: ${ e instanceof Error ? e.message : String(e) }`;
    }
  }

  return { results: searchResults(pool, scores, options.limits), timings, reranked };
};
