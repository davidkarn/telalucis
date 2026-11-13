// What the pages served to search engines and the sitemaps list (see core/seo.ts): only books
// shown in the library's searches (not withheld as likely under copyright), and the authors,
// journals and issues with such books
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

// The ids of the books shown, by id
const findShownBookIds = async(db: Kysely<Database>): Promise<string[]> => (
  (await db.selectFrom('books')
    .select('books.id')
    .where(CopyrightStatusCheckSql.isShownInSearches('books.id'))
    .orderBy('books.id')
    .execute()).map((r) => r.id)
);

// How many pages the books shown have
const countShownPages = async(db: Kysely<Database>): Promise<number> => (
  Number((await db.selectFrom('pages')
    .select((eb) => eb.fn.countAll<string>().as('count'))
    .where(CopyrightStatusCheckSql.isShownInSearches('pages.book_id'))
    .executeTakeFirstOrThrow()).count)
);

// A run of the pages of the books shown, by book and page
const findShownPages = async(
  db: Kysely<Database>, offset: number, limit: number,
): Promise<{ bookId: string, pageNumber: number }[]> => (
  (await db.selectFrom('pages')
    .select(['pages.book_id', 'pages.page_number'])
    .where(CopyrightStatusCheckSql.isShownInSearches('pages.book_id'))
    .orderBy('pages.book_id').orderBy('pages.page_number')
    .offset(offset).limit(limit)
    .execute()).map((r) => ({ bookId: r.book_id, pageNumber: r.page_number }))
);

// The authors with a book shown, by name
const findAuthorsWithBooks = (db: Kysely<Database>): Promise<{ id: string, name: string }[]> => (
  db.selectFrom('authors')
    .select(['authors.id', 'authors.name'])
    .where(({ exists, selectFrom }) => exists(
      selectFrom('book_authors').select('book_authors.book_id')
        .whereRef('book_authors.author_id', '=', 'authors.id')
        .where(CopyrightStatusCheckSql.isShownInSearches('book_authors.book_id'))
    ))
    .orderBy('authors.name').orderBy('authors.id')
    .execute()
);

// Every journal issue, as journal and issue ids
const findJournalIssueIds = (
  db: Kysely<Database>,
): Promise<{ journalId: string, issueId: string }[]> => (
  db.selectFrom('journal_issues')
    .select(['journal_issues.journal_id as journalId', 'journal_issues.id as issueId'])
    .orderBy('journal_issues.journal_id').orderBy('journal_issues.start_date').orderBy('journal_issues.id')
    .execute()
);

// A book's authors, in the order they're named; and the journal issue it's in (null for a book)
const findBookLinks = async(db: Kysely<Database>, bookId: string): Promise<{
  authors: { id: string, name: string }[],
  issue: { journalId: string, issueId: string } | null,
}> => {
  const authors = await db.selectFrom('book_authors')
    .innerJoin('authors', 'authors.id', 'book_authors.author_id')
    .select(['authors.id', 'authors.name'])
    .where('book_authors.book_id', '=', bookId)
    .orderBy('book_authors.position')
    .execute();
  const issue   = await db.selectFrom('books')
    .innerJoin('journal_issues', 'journal_issues.id', 'books.journal_issue_id')
    .select(['journal_issues.journal_id as journalId', 'journal_issues.id as issueId'])
    .where('books.id', '=', bookId)
    .executeTakeFirst();

  return { authors, issue: issue ?? null };
};

export const SeoQueries = {
  findShownBookIds,
  countShownPages,
  findShownPages,
  findAuthorsWithBooks,
  findJournalIssueIds,
  findBookLinks,
};
