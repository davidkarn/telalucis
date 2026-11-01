// Journal issues downloaded from archive.org (../thomist, ../the_new_scholasticism,
// ../the_modern_schoolman, ../franciscan_studies), each a PDF OCR'd with surya
// into <folder>/results/surya/<name>/results.json: which journal each is
// from, its title, and the period it covers. Pure functions;
// cli/commands/import_journal_issues.command.ts reads the files and saves
// them, each issue a book in a journal issue (journal_issues).
//
// An issue's file is named by its archive.org identifier, without "sim_" in
// some: "sim_american-catholic-philosophical-quarterly_1951-07_25_3" (July
// 1951, volume 25, number 3), "res-philosophica_february-march-1926_3_5-6",
// "thomist-a-speculative-quarterly-review_1947_10_contents" (a volume's
// contents), "franciscanstudie0013unse" (a volume, its date only in its
// archive.org title: "Franciscan Studies 1953: Vol 13").

// The journals, by the folder their issues are in
export const JOURNALS: { folder: string, name: string }[] = [
  { folder: 'thomist', name: 'The Thomist' },
  { folder: 'the_new_scholasticism', name: 'The New Scholasticism' },
  { folder: 'the_modern_schoolman', name: 'The Modern Schoolman' },
  { folder: 'franciscan_studies', name: 'Franciscan Studies' },
];

// What a file's name says of its issue: the date as written ("1951-07",
// "february-march 1926"), the volume, and the number or part ("3", "5-6",
// "contents", "index", "supplement"); null for what it doesn't say
export type IssueName = { date: string | null, volume: string | null, part: string | null };

export const issueName = (name: string): IssueName => {
  const segments = name.replace(/^sim_/, '').split('_');
  const at       = segments.findIndex((s, i) => i > 0 && /\d{4}/.test(s));

  if (at < 0) {
    return { date: null, volume: null, part: null };
  }
  else {
    // "november-1958-may-1959" -> "november 1958-may 1959"
    const date = segments[at]!.replace(/-(\d{4})/g, ' $1');
    // a trailing "_0" marks a second scan of the same issue
    const rest = segments.slice(at + 1).filter((s, i, all) => !(s === '0' && i === all.length - 1));
    return { date, volume: rest[0] ?? null, part: rest[1] ?? null };
  }
};

// A period, as dates ("1951-07-01"): precision 'month' for an issue dated by
// its month alone, whose end is worked out from the issues after it (see
// issuePeriods)
export type Period = { start: string, end: string, precision: 'month' | 'range' | 'year' };

const MONTHS = [
  'january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september',
  'october', 'november', 'december',
];
const MONTH  = `(${ MONTHS.join('|') }|${ MONTHS.map((m) => m.slice(0, 3)).join('|') })\\.?`;

const pad = (n: number) => String(n).padStart(2, '0');
const ymd = (d: Date) => `${ d.getUTCFullYear() }-${ pad(d.getUTCMonth() + 1) }-${ pad(d.getUTCDate()) }`;
// the first and last days of a month (month from 1)
const firstDay = (year: number, month: number) => ymd(new Date(Date.UTC(year, month - 1, 1)));
const lastDay  = (year: number, month: number) => ymd(new Date(Date.UTC(year, month, 0)));
const monthOf  = (name: string) => MONTHS.findIndex((m) => m.startsWith(name.toLowerCase().slice(0, 3))) + 1;

// The period a date as written covers: "1951-07" (a month), "July 1951",
// "February-March 1926", "November 1958-May 1959", "1939-1986", "1953" (a
// year); null when it has no year
export const periodOf = (text: string): Period | null => {
  const t      = text.toLowerCase();
  const month  = t.match(/\b(\d{4})-(\d{2})\b(?!-\d{2}\b)/);
  const months = t.match(new RegExp(`\\b${ MONTH }(?:\\s+(\\d{4}))?\\s*[-–]\\s*${ MONTH }\\s+(\\d{4})`));
  const named  = t.match(new RegExp(`\\b${ MONTH }\\s+(\\d{4})`));
  const years  = t.match(/\b(\d{4})\s*[-–]\s*(\d{4})\b/);
  const year   = t.match(/\b(1[89]\d\d|20\d\d)\b/);

  if (month && Number(month[2]) >= 1 && Number(month[2]) <= 12) {
    const [y, m] = [Number(month[1]), Number(month[2])];
    return { start: firstDay(y, m), end: lastDay(y, m), precision: 'month' };
  }
  else if (months) {
    const endYear = Number(months[4]);
    return {
      start:     firstDay(months[2] ? Number(months[2]) : endYear, monthOf(months[1]!)),
      end:       lastDay(endYear, monthOf(months[3]!)),
      precision: 'range',
    };
  }
  else if (named) {
    const [y, m] = [Number(named[2]), monthOf(named[1]!)];
    return { start: firstDay(y, m), end: lastDay(y, m), precision: 'month' };
  }
  else if (years && Number(years[2]) >= Number(years[1])) {
    return { start: firstDay(Number(years[1]), 1), end: lastDay(Number(years[2]), 12), precision: 'range' };
  }
  else if (year) {
    return { start: firstDay(Number(year[1]), 1), end: lastDay(Number(year[1]), 12), precision: 'year' };
  }
  else {
    return null;
  }
};

const monthsBetween = (from: string, to: string) => (
  (Number(to.slice(0, 4)) - Number(from.slice(0, 4))) * 12 + Number(to.slice(5, 7)) - Number(from.slice(5, 7))
);

const dayBefore = (date: string) => {
  const d = new Date(`${ date }T00:00:00Z`);
  d.setUTCDate(d.getUTCDate() - 1);
  return ymd(d);
};

// Each of a journal's issue periods, those dated by a month alone running
// until the next issue (a quarterly's July issue: July to September); the
// last, or one with no issue in the six months after it, for as long as the
// journal's issues usually are apart. Other periods are as they are.
export const issuePeriods = (periods: (Period | null)[]): (Period | null)[] => {
  const starts = [...new Set(periods.flatMap((p) => (p?.precision === 'month' ? [p.start] : [])))].sort();
  const gaps   = starts.slice(1).map((s, i) => monthsBetween(starts[i]!, s)).filter((g) => g > 0 && g <= 6).sort();
  const usual  = gaps[Math.floor(gaps.length / 2)] ?? 1;

  return periods.map((p) => {
    if (p?.precision !== 'month') {
      return p;
    }
    else {
      const next = starts.find((s) => s > p.start);
      const gap  = next ? monthsBetween(p.start, next) : Infinity;
      const end  = gap <= 6 ? dayBefore(next!) : dayBefore(firstDay(
        Number(p.start.slice(0, 4)), Number(p.start.slice(5, 7)) + usual,
      ));
      return { ...p, end };
    }
  });
};

const PARTS: Record<string, string> = { contents: 'Table of Contents', index: 'Index', supplement: 'Supplement' };

// An issue's title: archive.org's when there is one ("The New Scholasticism
// 1951-07: Vol 25 Iss 3"), else made from its file's name the same way
export const issueTitle = (journal: string, name: IssueName, archiveTitle: string | null): string => {
  if (archiveTitle && archiveTitle.trim().length > 0) {
    return archiveTitle.replace(/\s+/g, ' ').trim();
  }
  else {
    const part = name.part === null ? '' : ` ${ PARTS[name.part] ?? `Iss ${ name.part }` }`;
    const date = name.date === null ? '' : ` ${ name.date }`;
    return `${ journal }${ date }${ name.volume === null ? '' : `: Vol ${ name.volume }` }${ part }`;
  }
};
