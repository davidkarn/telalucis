// Importing an article of a journal issue OCR'd with surya (see
// core/journal_articles.ts and cli/commands/import_journal_issues.command.ts)
// as actions: its journal and journal issue, found or added, then the article
// as a book in it, its pages, and the citations extracted from its footnotes
// (each in its footnote block, see placeCitations), with the insights learned.
// Each step waits on the one before it succeeding. Pure functions;
// actions/app_actions.ts carries the actions out.
import type { AppAction, AppActionStep, ActionResultsTbl } from '../actions/app_actions.ts';
import type { Page } from '../types.ts';
import { placeCitations } from './footnote_extraction.ts';
import type { ExtractedCitations } from './ocred_book_import.ts';
import type { Period } from './journal_issues.ts';

// The issue an article is in
export type JournalIssue = {
  journal: string,
  period: Period,
};

// An article as a book: its id (see articleBookId), title, author, and its
// first page on archive.org
export type JournalArticle = {
  bookId: string,
  title: string,
  author: string,
  url: string | null,
};

const log = (...data: unknown[]): AppAction => ({ cmd: 'log', data });

const succeeded = (results: ActionResultsTbl, id: string) => results[id]?.success === true;

// The id an action returned
const idFrom = (results: ActionResultsTbl, id: string) => results[id]!.result as string;

export const importJournalArticleActions = (
  issue: JournalIssue, article: JournalArticle, pages: Page[], extracted: ExtractedCitations,
): AppActionStep[] => {
  const { placed, unplaced } = placeCitations(pages, extracted.citations);

  return [
    ...extracted.failedPages.map((f) => log(`${ article.bookId }: page ${ f.page } failed: ${ f.error }`)),
    {
      id:   'journal',
      cmd:  'modelAction',
      data: { model: 'Journals', act: 'findOrCreateJournal', params: [issue.journal] },
    },
    (results) => (
      succeeded(results, 'journal')
        ? {
            id:   'journalIssue',
            cmd:  'modelAction',
            data: {
              model:  'Journals',
              act:    'findOrCreateJournalIssue',
              params: [idFrom(results, 'journal'), issue.period.start, issue.period.end],
            },
          }
        : null
    ),
    (results) => (
      succeeded(results, 'journalIssue')
        ? {
            id:   'saveBook',
            cmd:  'modelAction',
            data: {
              model:  'Books',
              act:    'saveBook',
              params: [{
                id:               article.bookId,
                title:            article.title,
                author:           article.author,
                url:              article.url,
                journal_issue_id: idFrom(results, 'journalIssue'),
              }, pages],
            },
          }
        : null
    ),
    (results) => (
      succeeded(results, 'saveBook')
        ? {
            id:   'saveCitations',
            cmd:  'modelAction',
            data: {
              model:  'ExtractedCitations',
              act:    'replaceExtractedCitations',
              params: [article.bookId, placed, extracted.insights],
            },
          }
        : null
    ),
    ...unplaced.map((c) => log(
      `${ article.bookId }: page ${ c.source.footnotePage } has no Footnote block for "${ c.raw.slice(0, 60) }"`
    )),
    (results) => log({
      bookId:      article.bookId,
      period:      `${ issue.period.start } to ${ issue.period.end }`,
      saved:       results['saveCitations']?.result ?? results['saveBook']?.errorMessage ?? null,
      unplaced:    unplaced.length,
      failedPages: extracted.failedPages.length,
    }),
  ];
};
