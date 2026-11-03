// Journals, and their issues (journal_issues), whose articles are books
// (books.journal_issue_id; see migrations/0027_create_journals.ts).
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

// The id of the journal named `name`, added when there's none yet
const findOrCreateJournal = async(db: Kysely<Database>, name: string): Promise<string> => {
  const found = await db.selectFrom('journals')
    .select('id')
    .where('name', '=', name)
    .executeTakeFirst();

  return found?.id ?? (await db.insertInto('journals')
    .values({ name })
    .returning('id')
    .executeTakeFirstOrThrow()).id;
};

// The id of the journal's issue covering the period from startDate to endDate
// ("1951-07-01"), added when there's none yet. A volume and its table of
// contents, covering the same year, share one.
const findOrCreateJournalIssue = async(
  db: Kysely<Database>, journalId: string, startDate: string, endDate: string,
): Promise<string> => {
  const found = await db.selectFrom('journal_issues')
    .select('id')
    .where('journal_id', '=', journalId)
    .where(sql<boolean>`start_date = ${ startDate }::date and end_date = ${ endDate }::date`)
    .executeTakeFirst();

  return found?.id ?? (await db.insertInto('journal_issues')
    .values({ journal_id: journalId, start_date: startDate, end_date: endDate })
    .returning('id')
    .executeTakeFirstOrThrow()).id;
};

export const JournalActions = { findOrCreateJournal, findOrCreateJournalIssue };

// SQL: the articles of the issue `issue` ("i") shown in searches (not
// withheld, see shownInSearches)
const shownArticles = (issue: string) => sql`(
  select b.id from books b
  where b.journal_issue_id = ${ sql.ref(`${ issue }.id`) }
    and ${ CopyrightStatusCheckSql.isShownInSearches('b.id') })`;

export type JournalRow = {
  id: string, name: string, issue_count: number, article_count: number,
  first_year: number | null, last_year: number | null,
};

// The journals, by name, with how many issues and articles each has (those
// shown), and the years of their first and last issues; with journalId, that
// one only
const findJournals = (db: Kysely<Database>, journalId?: string): Promise<JournalRow[]> => (
  sql<JournalRow>`
    select j.id::text, j.name,
      count(i.id)::int as issue_count,
      coalesce(sum((select count(*) from ${ shownArticles('i') } a)), 0)::int as article_count,
      extract(year from min(i.start_date))::int as first_year,
      extract(year from max(i.end_date))::int as last_year
    from journals j
    left join journal_issues i on i.journal_id = j.id
    where ${ journalId === undefined ? sql`true` : sql`j.id = ${ journalId }` }
    group by j.id, j.name
    order by j.name, j.id
  `.execute(db).then((r) => r.rows)
);

export type JournalIssueRow = {
  id: string, journal_id: string, journal_name: string, start_date: string, end_date: string,
  // an article's book id without its page ("..._1939-04_1_1"), what the
  // issue's file was named (see issueName); null for an issue without one
  issue_name: string | null,
  article_count: number,
};

const issueColumns = sql`
  i.id::text, i.journal_id::text, j.name as journal_name,
  to_char(i.start_date, 'YYYY-MM-DD') as start_date, to_char(i.end_date, 'YYYY-MM-DD') as end_date,
  (select min(regexp_replace(b.id, '_p[0-9]+$', '')) from books b where b.journal_issue_id = i.id)
    as issue_name,
  (select count(*) from ${ shownArticles('i') } a)::int as article_count`;

// A page of a journal's issues, in date order, and how many there are
const findJournalIssues = async(
  db: Kysely<Database>, journalId: string, { offset, length }: { offset: number, length: number },
): Promise<{ rows: JournalIssueRow[], count: number }> => {
  const rows  = await sql<JournalIssueRow>`
    select ${ issueColumns }
    from journal_issues i join journals j on j.id = i.journal_id
    where i.journal_id = ${ journalId }
    order by i.start_date, i.end_date, i.id
    offset ${ offset } limit ${ length }
  `.execute(db);
  const total = await db.selectFrom('journal_issues')
    .select((eb) => eb.fn.countAll<string>().as('count'))
    .where('journal_id', '=', journalId)
    .executeTakeFirstOrThrow();

  return { rows: rows.rows, count: Number(total.count) };
};

// One of a journal's issues; undefined when it has no such issue
const findJournalIssue = async(
  db: Kysely<Database>, journalId: string, issueId: string,
): Promise<JournalIssueRow | undefined> => (
  (await sql<JournalIssueRow>`
    select ${ issueColumns }
    from journal_issues i join journals j on j.id = i.journal_id
    where i.journal_id = ${ journalId } and i.id = ${ issueId }
  `.execute(db)).rows[0]
);

export const JournalQueries = { findJournals, findJournalIssues, findJournalIssue };
