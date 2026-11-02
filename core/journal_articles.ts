// A journal issue's articles: each article in an issue OCR'd with surya
// becomes a book of its own, by its author and with its title, its pages
// numbered as the journal prints them. The departments (book reviews, brief
// notices, books received, news) are a book each, by "Various"; the cover,
// masthead, contents, advertisements and indexes are left out.
//
// The journals mark their articles differently: The Thomist's running heads
// give the author on the left page and the title on the right; The Modern
// Schoolman's (1950) a short title and "by Henry C. Wolz"; its monthly issues
// (1928) only the journal's name, the articles a page or two each with a
// "By ..." under their titles. So an LLM is given an outline of the issue (see
// issueOutline) and names its articles (see ARTICLES_PROMPT); here are the
// outline, the prompt, and the checks on what it gives. Pure functions;
// cli/commands/import_journal_issues.command.ts makes the requests.
import type { ORResponseFormat } from '../lib/open_router.ts';
import type { Citation, Page } from '../types.ts';
import { strip_tags } from './book_pages.ts';

// The author given a department of an issue ("Book Reviews"): its authors are
// many
export const DEPARTMENT_AUTHOR = 'Various';

// An article (or department) as the model gives it: its first and last scan
// pages (Page.pageNumber, SuryaPage.page), inclusive
export type IssueArticle = {
  title: string,
  author: string,
  firstScan: number,
  lastScan: number,
  department: boolean,
};

export const ARTICLES_FORMAT: ORResponseFormat = {
  type:        'json_schema',
  json_schema: {
    name:   'issue_articles',
    strict: true,
    schema: {
      type:                 'object',
      additionalProperties: false,
      required:             ['articles'],
      properties:           {
        articles: {
          type:  'array',
          items: {
            type:                 'object',
            additionalProperties: false,
            required:             ['title', 'author', 'firstScan', 'lastScan', 'department'],
            properties:           {
              title:      { type: 'string' },
              author:     { type: 'string' },
              firstScan:  { type: 'integer' },
              lastScan:   { type: 'integer' },
              department: { type: 'boolean' },
            },
          },
        },
      },
    },
  },
};

export const ARTICLES_PROMPT = `You are given an outline of one scanned issue of a philosophy journal: a line per
scanned page, starting with its scan number ([scan 12]), then the page number printed on it (printed 6,
or "printed ?" when none was read): the two differ, and you return scan numbers. Then its running heads (heads:), its headings (headings:), and the start of its text
(text:). A table of contents is given in full.

Return every article in the issue, in order, each with:
- title: the article's title as printed at its start or in the table of contents, in normal
  capitalization ("St. Thomas and Christian Perfection", not "ST. THOMAS AND CHRISTIAN PERFECTION"),
  with its subtitle, without "(Continued)" or a part number's heading.
- author: its author's name as printed in its byline or the table of contents, in normal
  capitalization ("David L. Greenstock, O.P."); several authors joined by " and ". "" if unsigned.
- firstScan, lastScan: the scan numbers ([scan N]) of its first and last pages, never the printed numbers.
  An article runs until the page before the next article or department begins.
- department: false.

Running heads tell articles apart: an article's pages share its running heads (often its author on
one side and its title on the other), and its first page usually has none, but its title as a
heading. Return each of the issue's departments (book reviews, brief notices, books received, news,
correspondence, chronicle) as one entry, department: true, titled as the journal heads it ("Book
Reviews") with author "". Leave out the cover, title page, masthead, table of contents, editorial
notices, advertisements and indexes. Entries are in page order and do not overlap: an article
continued among later pages ("Continued on page 64") ends where its first run of pages ends.`;

const MAX_LINE  = 90;
const MAX_HEADS = 4;
const MAX_TEXTS = 2;
const MAX_TOC   = 2500;
const CONTENTS  = /\bcontents\b/i;

const text = (html: string) => strip_tags(html).replace(/\s+/g, ' ').trim();
const cut  = (s: string, n = MAX_LINE) => (s.length > n ? `${ s.slice(0, n) }…` : s);

// The outline of an issue the model is given: a line per page (see
// ARTICLES_PROMPT), a table of contents' text in full
export const issueOutline = (pages: Page[]): string => pages.map((page) => {
  const of     = (labels: string[]) => page.blocks.filter((b) => labels.includes(b.label)).map((b) => text(b.html))
    .filter((t) => t.length > 0);
  const heads  = of(['PageHeader', 'PageFooter']).slice(0, MAX_HEADS);
  const titles = of(['SectionHeader']);
  const texts  = of(['Text']);
  const toc    = [...titles, ...texts.slice(0, 3)].some((t) => CONTENTS.test(t));
  const parts  = [
    `[scan ${ page.pageNumber }] printed ${ page.printedPageNumber || '?' }`,
    heads.length > 0 ? `heads: ${ heads.map((h) => cut(h, 50)).join(' / ') }` : '',
    titles.length > 0 ? `headings: ${ titles.slice(0, MAX_HEADS).map((h) => cut(h)).join(' / ') }` : '',
    toc ? `contents: ${ cut(texts.join(' / '), MAX_TOC) }`
      : texts.length > 0 ? `text: ${ texts.slice(0, MAX_TEXTS).map((t) => cut(t, 60)).join(' / ') }` : '',
  ];
  return parts.filter((p) => p.length > 0).join(' | ');
}).join('\n');

// A title without the "(Continued)" of an article's second part
const CONTINUED = /\s*(?:[—–-]+\s*continued|\(continued\))\.?\s*$/i;

// The model's articles checked: on pages of the issue, in page order, titled
// (without "Continued"), the departments' author "Various". Each runs to the
// page before the next starts: the model ends some a page early, and a page
// between two articles (an advertisement, a blank) is better in one than
// lost; an overlap ends the earlier one. The last keeps the end it was given.
export const checkedArticles = (articles: IssueArticle[], pages: Page[]): IssueArticle[] => {
  const scans = pages.map((p) => p.pageNumber);
  const first = Math.min(...scans);
  const last  = Math.max(...scans);

  const kept = articles
    .map((a) => ({
      ...a,
      title:     a.title.replace(/\s+/g, ' ').replace(CONTINUED, '').trim(),
      author:    a.department ? DEPARTMENT_AUTHOR : a.author.replace(/\s+/g, ' ').trim(),
      firstScan: Math.max(first, a.firstScan),
      lastScan:  Math.min(last, a.lastScan),
    }))
    .filter((a) => a.title.length > 0 && a.firstScan <= a.lastScan)
    .sort((a, b) => a.firstScan - b.firstScan)
    // one article to a first page
    .filter((a, i, all) => i === 0 || a.firstScan > all[i - 1]!.firstScan);

  return kept.map((a, i) => ({
    ...a,
    lastScan: i + 1 < kept.length ? kept[i + 1]!.firstScan - 1 : a.lastScan,
  }));
};

// How much to add to an article's scan numbers for its printed ones: the
// most common difference among its pages printed with a number, else among
// the issue's; 0 when no page has one
export const pageOffset = (article: Page[], issue: Page[]): number => {
  const commonest = (pages: Page[]) => {
    const counts = new Map<number, number>();
    for (const p of pages) {
      if (/^\d+$/.test(p.printedPageNumber)) {
        const d = Number(p.printedPageNumber) - p.pageNumber;
        counts.set(d, (counts.get(d) ?? 0) + 1);
      }
    }
    return [...counts].sort((a, b) => b[1] - a[1])[0]?.[0];
  };
  return commonest(article) ?? commonest(issue) ?? 0;
};

// An article's pages, numbered as the journal prints them: each its scan
// number plus the article's offset (see pageOffset), so that a page whose
// number was misread or not printed (an article's first) is numbered all the
// same, and no two are numbered alike
export const articlePages = (article: IssueArticle, issue: Page[]): Page[] => {
  const pages  = issue.filter((p) => p.pageNumber >= article.firstScan && p.pageNumber <= article.lastScan);
  const offset = pageOffset(pages, issue);

  return pages.map((p) => ({ ...p, pageNumber: p.pageNumber + offset, printedPageNumber: String(p.pageNumber + offset) }));
};

// The citations found on an article's scan pages, on its pages as numbered
// (see articlePages)
export const renumberCitations = (citations: Citation[], offset: number): Citation[] => (
  citations.map((c) => ({ ...c, source: { ...c.source, footnotePage: c.source.footnotePage + offset } }))
);

// An article's book id: its issue's, and the page it starts on
// ("thomist-a-speculative-quarterly-review_1950-01_13_1_p16")
export const articleBookId = (issueName: string, firstPrinted: number): string => `${ issueName }_p${ firstPrinted }`;

// Whether a book is an article of an issue (see articleBookId)
export const isArticleOf = (bookId: string, issueName: string): boolean => (
  bookId.startsWith(`${ issueName }_p`) && /^\d+$/.test(bookId.slice(issueName.length + 2))
);
