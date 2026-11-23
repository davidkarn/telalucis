// Searching the blocks of text for a search term (see lib/block_search.ts), from
// two lists: the blocks with its words (their tsvector, page_blocks.search) and
// the blocks of the passages nearest it in meaning (the passages' embeddings),
// one block of each passage: the one with the most of its words, else its
// first. The lists are merged by reciprocal rank fusion, so a block found both
// ways comes before one found only one way. Pure functions.
import { norm } from './book_pages.ts';
import { issueName } from './journal_issues.ts';
import { blockText } from './passages.ts';
import { RRF_K } from './passage_search.ts';

export type BlockSource = 'words' | 'meaning';

export type FusedBlock = { blockId: string, score: number, foundBy: BlockSource[] };

// The blocks of the lists (ids, the best first), each once, by their fused score, the best first;
// ties by the order they were first found in
export const fusedBlocks = (lists: Record<BlockSource, string[]>): FusedBlock[] => {
  const byId = new Map<string, FusedBlock>();

  for (const [source, ids] of Object.entries(lists) as [BlockSource, string[]][]) {
    ids.forEach((blockId, rank) => {
      const known = byId.get(blockId) ?? { blockId, score: 0, foundBy: [] };
      byId.set(blockId, known.foundBy.includes(source) ? known : {
        blockId,
        score:   known.score + 1 / (RRF_K + rank),
        foundBy: [...known.foundBy, source],
      });
    });
  }

  return [...byId.values()].sort((a, b) => b.score - a.score);
};

// A block on a passage's pages, with how well it matches the search term's words (ts_rank_cd; 0
// with none of them)
export type PassageBlock = {
  blockId: string,
  pageNumber: number,
  position: number,
  html: string,
  wordScore: number,
};

// Enough of a block's text to tell whether a passage holds it, normalized (letters and digits,
// lower-cased); a passage joins a paragraph broken by a page or a line-end hyphen, so a block's
// start or end may be all of it that's found whole
const PROBE = 40;

// Whether the passage's text (normalized) holds the block: its start or its end
export const passageHoldsBlock = (passageText: string, html: string): boolean => {
  const text = norm(blockText(html));

  return text.length > 0 && (
    passageText.includes(text.slice(0, PROBE)) || passageText.includes(text.slice(-PROBE))
  );
};

// The block best standing for a passage among the blocks of its pages: of those it holds, the
// one with the most of the search term's words, else its first; null when it holds none of them
export const passageBlock = (passageText: string, blocks: PassageBlock[]): PassageBlock | null => {
  const text = norm(passageText);
  const held = blocks
    .filter((b) => passageHoldsBlock(text, b.html))
    .sort((a, b) => b.wordScore - a.wordScore || a.pageNumber - b.pageNumber || a.position - b.position);

  return held[0] ?? null;
};

// The longest search term taken
export const MAX_QUERY_LENGTH = 500;

// The search term, its runs of spaces made one; an error instead when it's empty or too long
export const searchTerm = (q: unknown): { term: string } | { error: string } => {
  const term = typeof q === 'string' ? q.trim().replace(/\s+/g, ' ') : '';

  if (term.length === 0) {
    return { error: 'q must be a search term' };
  }
  else if (term.length > MAX_QUERY_LENGTH) {
    return { error: `q can't be longer than ${ MAX_QUERY_LENGTH } characters` };
  }
  else {
    return { term };
  }
};

// A block found, as GET /search gives it (the BlockSearchResult schema in api.yaml)
export type BlockSearchResult = {
  blockId: string,
  label: string,
  html: string,
  // its text with the search term's words in <mark> (HTML: its entities kept)
  snippet: string,
  score: number,
  foundBy: BlockSource[],
  page: { pageNumber: number, printedPageNumber: string },
  book: { id: string, title: string, author: string },
  authors: { id: string, name: string }[],
  journalIssue: {
    id: string, journalId: string, journalName: string, startDate: string, endDate: string,
    volume: string | null, number: string | null,
  } | null,
};

// What model/block_search.ts's findBlockDetails reads of a block
export type BlockDetails = {
  id: string, label: string, html: string, snippet: string,
  book_id: string, page_number: number, printed_page_number: string, title: string, author: string,
  authors: { id: string, name: string }[] | null,
  journal_issue_id: string | null, journal_id: string | null, journal_name: string | null,
  issue_start_date: string | null, issue_end_date: string | null,
};

// A block found, with what's shown of it. Its journal issue's volume and number are read from the
// article's id ("<journal>_<date>_<volume>_<number>_p<page>"; see core/journal_issues.ts).
export const blockSearchResult = (row: BlockDetails, found: FusedBlock): BlockSearchResult => {
  const issue = issueName(row.book_id.replace(/_p\d+$/, ''));

  return {
    blockId:      row.id,
    label:        row.label,
    html:         row.html,
    snippet:      row.snippet,
    score:        found.score,
    foundBy:      found.foundBy,
    page:         { pageNumber: row.page_number, printedPageNumber: row.printed_page_number },
    book:         { id: row.book_id, title: row.title, author: row.author },
    authors:      row.authors ?? [],
    journalIssue: row.journal_issue_id === null ? null : {
      id:          row.journal_issue_id,
      journalId:   row.journal_id ?? '',
      journalName: row.journal_name ?? '',
      startDate:   row.issue_start_date ?? '',
      endDate:     row.issue_end_date ?? '',
      volume:      issue.volume,
      number:      issue.part,
    },
  };
};
