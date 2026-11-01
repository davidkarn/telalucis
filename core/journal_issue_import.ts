// Importing a journal issue OCR'd with surya (see
// cli/commands/import_journal_issues.command.ts) as actions: its journal and
// journal issue, found or added, then the issue as a book in it, its pages,
// and the citations extracted from its footnotes (each in its footnote block,
// see placeCitations), with the insights learned. Each step waits on the one
// before it succeeding. Pure functions; actions/app_actions.ts carries the
// actions out.
import type { AppAction, AppActionStep, ActionResultsTbl } from '../actions/app_actions.ts';
import type { Page } from '../types.ts';
import { placeCitations } from './footnote_extraction.ts';
import type { ExtractedCitations } from './ocred_book_import.ts';
import type { Period } from './journal_issues.ts';

// The author given an issue: its articles' authors are many
export const ISSUE_AUTHOR = 'Various';

export type JournalIssue = {
  bookId: string,
  journal: string,
  title: string,
  url: string | null,
  period: Period,
};

const log = (...data: unknown[]): AppAction => ({ cmd: 'log', data });

const succeeded = (results: ActionResultsTbl, id: string) => results[id]?.success === true;

// The id an action returned
const idFrom = (results: ActionResultsTbl, id: string) => results[id]!.result as string;

export const importJournalIssueActions = (
  issue: JournalIssue, pages: Page[], extracted: ExtractedCitations,
): AppActionStep[] => {
  const { placed, unplaced } = placeCitations(pages, extracted.citations);

  return [
    ...extracted.failedPages.map((f) => log(`${ issue.bookId }: page ${ f.page } failed: ${ f.error }`)),
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
                id:               issue.bookId,
                title:            issue.title,
                author:           ISSUE_AUTHOR,
                url:              issue.url,
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
              params: [issue.bookId, placed, extracted.insights],
            },
          }
        : null
    ),
    ...unplaced.map((c) => log(
      `${ issue.bookId }: page ${ c.source.footnotePage } has no Footnote block for "${ c.raw.slice(0, 60) }"`
    )),
    (results) => log({
      bookId:      issue.bookId,
      period:      `${ issue.period.start } to ${ issue.period.end }`,
      saved:       results['saveCitations']?.result ?? results['saveBook']?.errorMessage ?? null,
      unplaced:    unplaced.length,
      failedPages: extracted.failedPages.length,
    }),
  ];
};
