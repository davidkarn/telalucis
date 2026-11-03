import { Inject, Injectable } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { DB } from './database.module';
import type { Database } from './database';
import { issueName } from '../core/journal_issues';
import { JournalQueries, type JournalIssueRow, type JournalRow } from '../model/journals';

// The JournalSummary schema in ../api.yaml: a journal, how many issues and
// articles it has (those shown), and the years its issues span (null without
// issues)
export type JournalSummary = {
  id: string, name: string, issueCount: number, articleCount: number,
  firstYear: number | null, lastYear: number | null,
};

// The JournalIssueSummary schema in ../api.yaml: an issue of a journal, the
// period it covers ("1939-04-01" to "1939-05-31"), its volume and number as
// its file names them (null when they don't; number may be "5-6",
// "contents", "index", "supplement"), and how many articles it has
export type JournalIssueSummary = {
  id: string, journalId: string, journalName: string, startDate: string, endDate: string,
  volume: string | null, number: string | null, articleCount: number,
};

const toJournal = (r: JournalRow): JournalSummary => ({
  id:           r.id,
  name:         r.name,
  issueCount:   r.issue_count,
  articleCount: r.article_count,
  firstYear:    r.first_year,
  lastYear:     r.last_year,
});

const toIssue = (r: JournalIssueRow): JournalIssueSummary => {
  const name = r.issue_name === null ? null : issueName(r.issue_name);
  return {
    id:           r.id,
    journalId:    r.journal_id,
    journalName:  r.journal_name,
    startDate:    r.start_date,
    endDate:      r.end_date,
    volume:       name?.volume ?? null,
    number:       name?.part ?? null,
    articleCount: r.article_count,
  };
};

@Injectable()
export class JournalsService {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {}

  // Every journal, by name
  async list(): Promise<{ items: JournalSummary[], count: number }> {
    const rows = await JournalQueries.findJournals(this.db);
    return { items: rows.map(toJournal), count: rows.length };
  }

  // A journal; null when there's none with that id
  async get(journalId: string): Promise<JournalSummary | null> {
    const [row] = await JournalQueries.findJournals(this.db, journalId);
    return row === undefined ? null : toJournal(row);
  }

  // A page of a journal's issues, in date order; null when there's no such
  // journal. count: all of them.
  async issues(
    journalId: string, opts: { offset: number, length: number },
  ): Promise<{ items: JournalIssueSummary[], count: number } | null> {
    if (await this.get(journalId) === null) {
      return null;
    }
    else {
      const { rows, count } = await JournalQueries.findJournalIssues(this.db, journalId, opts);
      return { items: rows.map(toIssue), count };
    }
  }

  // One of a journal's issues; null when it has no such issue
  async issue(journalId: string, issueId: string): Promise<JournalIssueSummary | null> {
    const row = await JournalQueries.findJournalIssue(this.db, journalId, issueId);
    return row === undefined ? null : toIssue(row);
  }
}
