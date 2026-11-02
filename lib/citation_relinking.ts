// Linking the citations that cite a book in the collection but aren't pointed
// at it (see core/citation_relinking.ts): run by relink-citations over every
// book, and by the importers for the books they import, so the citations made
// of a book before it was imported find it.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { ALIAS_WORKS, relinkPlan, type RelinkPlan, type WorkPages } from '../core/citation_relinking.ts';
import { SUMMA_BOOK_ID } from '../core/summa_thml.ts';
import { findCitedPages } from '../model/book_pages_to_citations.ts';
import { BookQueries } from '../model/books.ts';
import {
  findAllCitations, linkCitations, relinkCitations, saveCitationNames,
} from '../model/incoming_citations.ts';

export type RelinkReport = {
  names: number,
  located: { bookId: string, citations: number }[],
  linked: { bookId: string, citations: number }[],
  unplaced: number,
};

const countByBook = (bookIds: string[]) => [...bookIds.reduce(
  (counts, id) => counts.set(id, (counts.get(id) ?? 0) + 1), new Map<string, number>(),
)].map(([bookId, citations]) => ({ bookId, citations })).sort((a, b) => b.citations - a.citations);

// Link the unlinked citations of these books (every book when none are
// given), and save the authors filled in; with dryRun, only plan it. The
// counts, by book.
export const linkUnlinkedCitations = async(
  db: Kysely<Database>, { bookIds, dryRun = false }: { bookIds?: string[], dryRun?: boolean } = {},
): Promise<RelinkReport> => {
  const books   = await BookQueries.findBookNames(db, bookIds);
  const held    = new Set(books.map((b) => b.id));
  const workIds = [SUMMA_BOOK_ID, ...new Set(ALIAS_WORKS.map((w) => w.bookId))].filter((id) => held.has(id));
  const pages   = new Map(await Promise.all(workIds.map(async(id) => (
    [id, await findCitedPages(db, id)] as const
  )))) as WorkPages;
  const plan    = relinkPlan(await findAllCitations(db), books, pages);

  if (!dryRun) {
    await save(db, plan);
  }

  return {
    names:    plan.names.length,
    located:  plan.located.map((l) => ({ bookId: l.bookId, citations: l.citations.length }))
      .sort((a, b) => b.citations - a.citations),
    linked:   countByBook(plan.links.map((l) => l.bookId)),
    unplaced: plan.unplaced,
  };
};

const save = async(db: Kysely<Database>, plan: RelinkPlan) => {
  await saveCitationNames(db, plan.names);
  for (const { bookId, citations } of plan.located) {
    await relinkCitations(db, bookId, citations);
  }
  await linkCitations(db, plan.links);
};

// The report in a line, for the importers' output
export const relinkSummary = (report: RelinkReport): string => {
  const located = report.located.reduce((n, l) => n + l.citations, 0);
  const linked  = report.linked.reduce((n, l) => n + l.citations, 0);
  return `linked ${ located + linked } earlier citations of it (${ located } with their places read)`;
};
