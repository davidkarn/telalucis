// Journals, and their issues (journal_issues), whose articles are books
// (books.journal_issue_id; see migrations/0027_create_journals.ts).
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';

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
