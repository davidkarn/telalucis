import { type JournalIssueSummary, type JournalSummary } from '../api';

export const journalPath = (journalId: string) => '/journals/' + encodeURIComponent(journalId);

export const journalIssuePath = (journalId: string, issueId: string) => (
  journalPath(journalId) + '/issues/' + encodeURIComponent(issueId)
);

const MONTHS = [
  'January', 'February', 'March', 'April', 'May', 'June',
  'July', 'August', 'September', 'October', 'November', 'December',
];

// The period an issue covers, as dates ("1939-04-01"), in words: "April 1939", "April–May 1939",
// "December 1939–January 1940", or a whole year, "1939"
export const issuePeriod = (startDate: string, endDate: string): string => {
  const [startYear, startMonth] = startDate.split('-').map(Number) as [number, number];
  const [endYear, endMonth]     = endDate.split('-').map(Number) as [number, number];
  const month                   = (m: number) => MONTHS[m - 1] ?? '';

  if (startYear === endYear && startMonth === 1 && endMonth === 12) {
    return String(startYear);
  }
  else if (startYear === endYear && startMonth === endMonth) {
    return month(startMonth) + ' ' + startYear;
  }
  else if (startYear === endYear) {
    return month(startMonth) + '–' + month(endMonth) + ' ' + startYear;
  }
  else {
    return month(startMonth) + ' ' + startYear + '–' + month(endMonth) + ' ' + endYear;
  }
};

const PARTS: Record<string, string> = {
  contents: 'Table of Contents', index: 'Index', supplement: 'Supplement',
};

// An issue's volume and number: "Vol. 1, No. 3", "Vol. 25, Index"; '' when neither is known
export const issueNumbering = (issue: Pick<JournalIssueSummary, 'volume' | 'number'>): string => (
  [
    issue.volume === null ? '' : 'Vol. ' + issue.volume,
    issue.number === null ? '' : PARTS[issue.number] ?? 'No. ' + issue.number,
  ].filter((s) => s.length > 0).join(', ')
);

// An issue's title: its numbering and period ("Vol. 1, No. 1 · April–May 1939")
export const issueTitle = (issue: JournalIssueSummary): string => (
  [issueNumbering(issue), issuePeriod(issue.startDate, issue.endDate)]
    .filter((s) => s.length > 0).join(' · ')
);

// The years a journal's issues span: "1939–1961", "1939"; '' when it has none
export const journalYears = (journal: JournalSummary): string => (
  journal.firstYear === null || journal.lastYear === null
    ? ''
    : journal.firstYear === journal.lastYear
      ? String(journal.firstYear)
      : journal.firstYear + '–' + journal.lastYear
);
