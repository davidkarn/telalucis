// Searching the blocks of text for a search term (see core/block_search.ts): the
// blocks with its words and the blocks of the passages nearest it in meaning,
// at once, merged, a page of them with what's shown of each. Without the
// embeddings server (lib/embeddings.ts), only by its words, saying so.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import {
  blockSearchResult, fusedBlocks, passageBlock, type BlockSearchResult,
} from '../core/block_search.ts';
import { BlockSearchQueries } from '../model/block_search.ts';
import { PassageSearchQueries } from '../model/passage_search.ts';
import { embed } from './embeddings.ts';

// Blocks found by their words, and passages found by meaning (a block each), before merging: the
// most the results run to
export const WORDS_POOL = 200;
export const MEANING_POOL = 100;

export type BlockSearchReport = {
  meta: {
    // how many were found, of at most WORDS_POOL + MEANING_POOL
    count: number,
    // whether each way of searching was used: by meaning needs the embeddings server
    searchedBy: { words: boolean, meaning: boolean },
    // why it wasn't searched by meaning, when it wasn't
    meaningUnavailable?: string,
  },
  items: BlockSearchResult[],
};

// How long the embeddings server is waited for before searching by words alone
const EMBED_TIMEOUT_MS = 10_000;

const embedTerm = (term: string): Promise<number[]> => Promise.race([
  embed([term]).then(([vector]) => vector!),
  new Promise<never>((_, reject) => setTimeout(
    () => reject(new Error(`no answer in ${ EMBED_TIMEOUT_MS / 1000 }s`)), EMBED_TIMEOUT_MS,
  ).unref()),
]);

// The blocks of the passages nearest the term in meaning, one a passage, the nearest first; or,
// when the embeddings server can't be reached, why
const findByMeaning = async(db: Kysely<Database>, term: string): Promise<string[] | Error> => {
  let vector: number[];

  try {
    vector = await embedTerm(term);
  }
  catch (e) {
    return new Error(`the embeddings server (EMBEDDINGS_URL) couldn't be reached: ${
      e instanceof Error ? e.message : String(e) }`);
  }

  const nearest = await PassageSearchQueries.findNearest(db, vector, MEANING_POOL);
  const blocks  = await BlockSearchQueries.findPassageBlocks(db, nearest.map((p) => ({
    passageId: p.passageId, bookId: p.bookId, pageNumber: p.pageNumber, endPageNumber: p.endPageNumber,
  })), term);

  return nearest
    .map((p) => passageBlock(p.text, blocks.get(p.passageId) ?? []))
    .filter((b) => b !== null)
    .map((b) => b.blockId);
};

export const searchBlocks = async(
  db: Kysely<Database>, term: string, { offset, length }: { offset: number, length: number },
): Promise<BlockSearchReport> => {
  const [words, meaning] = await Promise.all([
    BlockSearchQueries.findWithWords(db, term, WORDS_POOL),
    findByMeaning(db, term),
  ]);
  const fused            = fusedBlocks({ words, meaning: meaning instanceof Error ? [] : meaning });
  const page             = fused.slice(offset, offset + length);
  const details          = new Map((await BlockSearchQueries.findBlockDetails(db, page.map((f) => f.blockId), term))
    .map((d) => [d.id, d]));

  return {
    meta: {
      count:      fused.length,
      searchedBy: { words: true, meaning: !(meaning instanceof Error) },
      ...(meaning instanceof Error ? { meaningUnavailable: meaning.message } : {}),
    },
    items: page.flatMap((f) => {
      const row = details.get(f.blockId);
      return row === undefined ? [] : [blockSearchResult(row, f)];
    }),
  };
};
