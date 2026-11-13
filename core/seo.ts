// What search engines and link previews are given: the web app's pages as HTML with their own
// title, description, canonical URL, robots directives and JSON-LD, and their content in #root
// (which the app replaces when it starts), and the robots.txt and sitemaps that list them. Pure
// functions: api/seo.service.ts loads the data and serves them.
import { blockText } from './passages.ts';

export const SITE_NAME = 'Tela Lucis';

// At most this many URLs in a sitemap (the protocol allows 50,000)
export const SITEMAP_SIZE = 40_000;

// The web app's routes (web/src/main.tsx) that have content of their own
export type SeoRoute =
  | { kind: 'home' }
  | { kind: 'authors' }
  | { kind: 'author', authorId: string }
  | { kind: 'journals' }
  | { kind: 'journal', journalId: string }
  | { kind: 'journalIssue', journalId: string, issueId: string }
  | { kind: 'book', bookId: string, volume?: number, pageId?: number }
  // several books open side by side: not indexed, each book's own URL is
  | { kind: 'books', bookIds: string[] }
  | { kind: 'other' };

const segmentsOf = (pathname: string): string[] | null => {
  try {
    return pathname.split('/').filter((s) => s.length > 0).map(decodeURIComponent);
  }
  catch {
    return null;
  }
};

const positiveInt = (s: string | undefined): number | null => (
  s !== undefined && /^[1-9]\d{0,8}$/.test(s) ? Number(s) : null
);

// The books of /books/<id>[/volumes/<n>][/pages/<n>]/books/<id>..., as web/src/core/open_books.ts
// reads them; null when the path isn't one
const openBooksOf = (parts: string[]): { bookId: string, volume?: number, pageId?: number }[] | null => {
  const books: { bookId: string, volume?: number, pageId?: number }[] = [];
  let i                                                               = 0;

  while (i < parts.length) {
    const bookId = parts[i + 1];

    if (parts[i] !== 'books' || bookId === undefined || bookId.length === 0) {
      return null;
    }
    else {
      i           += 2;
      const volume = parts[i] === 'volumes' ? positiveInt(parts[i + 1]) : null;
      i += volume === null ? 0 : 2;
      const pageId = parts[i] === 'pages' ? positiveInt(parts[i + 1]) : null;
      i += pageId === null ? 0 : 2;

      books.push({
        bookId,
        ...(volume === null ? {} : { volume }),
        ...(pageId === null ? {} : { pageId }),
      });
    }
  }

  return books.length > 0 ? books : null;
};

export const seoRouteOf = (pathname: string): SeoRoute => {
  const parts = segmentsOf(pathname);
  const books = parts !== null && parts[0] === 'books' ? openBooksOf(parts) : null;

  if (parts === null) {
    return { kind: 'other' };
  }
  else if (parts.length === 0) {
    return { kind: 'home' };
  }
  else if (books !== null && books.length === 1) {
    return { kind: 'book', ...books[0]! };
  }
  else if (books !== null) {
    return { kind: 'books', bookIds: books.map((b) => b.bookId) };
  }
  else if (parts[0] === 'authors' && parts.length === 1) {
    return { kind: 'authors' };
  }
  else if (parts[0] === 'authors' && parts.length === 2) {
    return { kind: 'author', authorId: parts[1]! };
  }
  else if (parts[0] === 'journals' && parts.length === 1) {
    return { kind: 'journals' };
  }
  else if (parts[0] === 'journals' && parts.length === 2) {
    return { kind: 'journal', journalId: parts[1]! };
  }
  else if (parts[0] === 'journals' && parts.length === 4 && parts[2] === 'issues') {
    return { kind: 'journalIssue', journalId: parts[1]!, issueId: parts[3]! };
  }
  else {
    return { kind: 'other' };
  }
};

// Paths of the app's pages
const enc = encodeURIComponent;
export const bookPath = (bookId: string, pageId?: number) => (
  '/books/' + enc(bookId) + (pageId === undefined ? '' : '/pages/' + pageId)
);
export const authorPath = (authorId: string) => '/authors/' + enc(authorId);
export const journalPath = (journalId: string) => '/journals/' + enc(journalId);
export const journalIssuePath = (journalId: string, issueId: string) => (
  journalPath(journalId) + '/issues/' + enc(issueId)
);

export const escapeHtml = (s: string): string => (
  s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;').replace(/'/g, '&#39;')
);

// JSON inside a <script>: nothing in it can close the element
const scriptJson = (value: unknown): string => (
  JSON.stringify(value).replace(/</g, '\\u003c').replace(/>/g, '\\u003e').replace(/&/g, '\\u0026')
);

// At most `max` characters, cut between words with an ellipsis
export const truncate = (text: string, max: number): string => {
  if (text.length <= max) {
    return text;
  }
  else {
    const cut  = text.slice(0, max - 1);
    const word = cut.lastIndexOf(' ');
    return (word > max / 2 ? cut.slice(0, word) : cut).replace(/[\s,;:.]+$/, '') + '…';
  }
};

// A page as served: its status, what goes in <head>, and its content (HTML, for #root)
export type SeoDocument = {
  status: number,
  title: string,
  description: string,
  // absolute; null for a page not to be indexed under any URL
  canonicalUrl: string | null,
  index: boolean,
  imageUrl?: string,
  jsonLd?: Record<string, unknown>,
  body: string,
};

// The robots directive of a page: links are followed even on pages not indexed
export const robotsOf = (doc: Pick<SeoDocument, 'index'>): string => (
  doc.index ? 'index,follow' : 'noindex,follow'
);

const headTags = (doc: SeoDocument): string => [
  `<title>${ escapeHtml(doc.title) }</title>`,
  `<meta name="description" content="${ escapeHtml(doc.description) }">`,
  `<meta name="robots" content="${ robotsOf(doc) }">`,
  ...(doc.canonicalUrl === null ? [] : [
    `<link rel="canonical" href="${ escapeHtml(doc.canonicalUrl) }">`,
    `<meta property="og:url" content="${ escapeHtml(doc.canonicalUrl) }">`,
  ]),
  `<meta property="og:site_name" content="${ SITE_NAME }">`,
  `<meta property="og:type" content="${ doc.jsonLd?.['@type'] === 'Book' ? 'book' : 'website' }">`,
  `<meta property="og:title" content="${ escapeHtml(doc.title) }">`,
  `<meta property="og:description" content="${ escapeHtml(doc.description) }">`,
  ...(doc.imageUrl === undefined ? [] : [
    `<meta property="og:image" content="${ escapeHtml(doc.imageUrl) }">`,
  ]),
  `<meta name="twitter:card" content="${ doc.imageUrl === undefined ? 'summary' : 'summary_large_image' }">`,
  ...(doc.jsonLd === undefined ? [] : [
    `<script type="application/ld+json">${ scriptJson(doc.jsonLd) }</script>`,
  ]),
].join('\n    ');

// The web app's index.html with the page's <head> tags in place of its <title>, and its content
// in #root
export const renderDocument = (template: string, doc: SeoDocument): string => (
  template
    .replace(/<title>[\s\S]*?<\/title>/, () => headTags(doc))
    .replace(/<div id="root"><\/div>/, () => `<div id="root">${ doc.body }</div>`)
);

// --- the pages ---

const link = (href: string, text: string) => `<a href="${ escapeHtml(href) }">${ escapeHtml(text) }</a>`;

const list = (items: string[]) => (
  items.length === 0 ? '' : `<ul>${ items.map((i) => `<li>${ i }</li>`).join('') }</ul>`
);

const nav = `<nav>${ link('/', 'Library') } · ${ link('/authors', 'Authors') } · ${
  link('/journals', 'Journals') }</nav>`;

const main = (...parts: string[]) => `<main>${ nav }${ parts.join('') }</main>`;

export type BookSummaryForSeo = { id: string, title: string, author: string };

const bookItem = (b: BookSummaryForSeo) => (
  link(bookPath(b.id), b.title) + (b.author.length > 0 ? ' — ' + escapeHtml(b.author) : '')
);

export const homeDocument = (origin: string): SeoDocument => ({
  status:       200,
  title:        `${ SITE_NAME } - Library`,
  description:  'A library of scanned books and journals of theology and philosophy, with the '
    + 'citations between them linked page to page.',
  canonicalUrl: origin + '/',
  index:        true,
  jsonLd:       { '@context': 'https://schema.org', '@type': 'WebSite', name: SITE_NAME, url: origin + '/' },
  body:         main(`<h1>${ SITE_NAME }</h1>`),
});

export const authorsDocument = (
  origin: string, authors: { id: string, name: string }[],
): SeoDocument => ({
  status:       200,
  title:        `${ SITE_NAME } - By author`,
  description:  `The authors of the books in ${ SITE_NAME }.`,
  canonicalUrl: origin + '/authors',
  index:        true,
  body:         main('<h1>Authors</h1>', list(authors.map((a) => link(authorPath(a.id), a.name)))),
});

export const authorDocument = (
  origin: string,
  author: { id: string, name: string, bookCount: number },
  books: BookSummaryForSeo[],
): SeoDocument => ({
  status:       200,
  title:        `${ SITE_NAME } - ${ author.name }`,
  description:  truncate(`${ author.bookCount } ${ author.bookCount === 1 ? 'work' : 'works' } by `
    + `${ author.name }: ${ books.map((b) => b.title).join('; ') }`, 160),
  canonicalUrl: origin + authorPath(author.id),
  index:        true,
  jsonLd:       {
    '@context': 'https://schema.org',
    '@type':    'Person',
    name:       author.name,
    url:        origin + authorPath(author.id),
  },
  body:         main(`<h1>${ escapeHtml(author.name) }</h1>`, list(books.map(bookItem))),
});

export const journalsDocument = (
  origin: string, journals: { id: string, name: string }[],
): SeoDocument => ({
  status:       200,
  title:        `${ SITE_NAME } - Journals`,
  description:  `The journals in ${ SITE_NAME }: ${ journals.map((j) => j.name).join(', ') }`,
  canonicalUrl: origin + '/journals',
  index:        true,
  body:         main('<h1>Journals</h1>', list(journals.map((j) => link(journalPath(j.id), j.name)))),
});

export type IssueForSeo = {
  id: string, journalId: string, journalName: string, startDate: string, endDate: string,
  volume: string | null, number: string | null,
};

const MONTHS = [
  'January', 'February', 'March', 'April', 'May', 'June',
  'July', 'August', 'September', 'October', 'November', 'December',
];

// The period an issue covers in words, as the web app writes it (web/src/core/journals.ts):
// "April 1939", "April–May 1939", "December 1939–January 1940", "1939"
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

// An issue's title, as the web app writes it: "Vol. 1, No. 3 · April–May 1939"
export const issueTitle = (issue: IssueForSeo): string => [
  [
    issue.volume === null ? '' : 'Vol. ' + issue.volume,
    issue.number === null ? '' : PARTS[issue.number] ?? 'No. ' + issue.number,
  ].filter((s) => s.length > 0).join(', '),
  issuePeriod(issue.startDate, issue.endDate),
].filter((s) => s.length > 0).join(' · ');

export const journalDocument = (
  origin: string, journal: { id: string, name: string }, issues: IssueForSeo[],
): SeoDocument => ({
  status:       200,
  title:        `${ SITE_NAME } - ${ journal.name }`,
  description:  truncate(`${ journal.name }: ${ issues.length } ${
    issues.length === 1 ? 'issue' : 'issues' }, ${ issues.map(issueTitle).join('; ') }`, 160),
  canonicalUrl: origin + journalPath(journal.id),
  index:        true,
  jsonLd:       {
    '@context': 'https://schema.org',
    '@type':    'Periodical',
    name:       journal.name,
    url:        origin + journalPath(journal.id),
  },
  body:         main(
    `<h1>${ escapeHtml(journal.name) }</h1>`,
    list(issues.map((i) => link(journalIssuePath(i.journalId, i.id), issueTitle(i)))),
  ),
});

export const journalIssueDocument = (
  origin: string, issue: IssueForSeo, articles: BookSummaryForSeo[],
): SeoDocument => {
  const name = `${ issue.journalName }, ${ issueTitle(issue) }`;
  const url  = origin + journalIssuePath(issue.journalId, issue.id);

  return {
    status:       200,
    title:        `${ SITE_NAME } - ${ name }`,
    description:  truncate(`${ name }: ${ articles.map((a) => a.title).join('; ') }`, 160),
    canonicalUrl: url,
    index:        true,
    jsonLd:       {
      '@context': 'https://schema.org',
      '@type':    'PublicationIssue',
      name,
      url,
      isPartOf:   {
        '@type': 'Periodical',
        name:    issue.journalName,
        url:     origin + journalPath(issue.journalId),
      },
    },
    body:         main(
      `<h1>${ escapeHtml(issue.journalName) }</h1>`,
      `<p>${ link(journalPath(issue.journalId), issue.journalName) } · ${
        escapeHtml(issueTitle(issue)) }</p>`,
      list(articles.map(bookItem)),
    ),
  };
};

export type BookForSeo = {
  id: string,
  title: string,
  author: string,
  authors: { id: string, name: string }[],
  // the journal issue it's an article in
  issue: IssueForSeo | null,
  coverPhotoPath?: string,
  pageCount: number,
  contents: { pageId: number, label: string }[],
};

const PART_LABELS: Record<string, string> = {
  book:     'Book',
  chapter:  'Chapter',
  question: 'Question',
  article:  'Article',
  part:     'Part',
  lecture:  'Lecture',
  volume:   'Volume',
  verse:    'Verse',
  page:     'Page',
  appendix: 'Appendix',
  prose:    'Prose',
  metre:    'Metre',
};

// At most this many entries of a book's table of contents are listed
export const MAX_CONTENTS = 2000;

type ContentsEntryForSeo = {
  partType: string, partValue: string, pageId: number, label?: string,
  childEntries: ContentsEntryForSeo[],
};

// A book's table of contents (core/contents.ts) as a list, each entry named as the web app names
// it (web/src/core/contents.ts: its proper name, else "Question 2") after the entries it's in
// ("Book 1, Chapter 2")
export const contentsList = (
  entries: ContentsEntryForSeo[], within = '',
): { pageId: number, label: string }[] => entries
  .flatMap((e) => {
    const own   = e.label ?? (
      (PART_LABELS[e.partType] ?? e.partType.slice(0, 1).toUpperCase() + e.partType.slice(1))
        + ' ' + e.partValue
    );
    const label = within.length > 0 ? `${ within }, ${ own }` : own;
    return [{ pageId: e.pageId, label }, ...contentsList(e.childEntries, label)];
  })
  .slice(0, MAX_CONTENTS);

export type PageForSeo = {
  pageNumber: number,
  printedPageNumber: string,
  blocks: { label: string, html: string }[],
  previousPageId: number | null,
  nextPageId: number | null,
};

// The text of a page's blocks, without its running headers and footers: headings, paragraphs and
// footnotes, in order
export const pageTexts = (page: Pick<PageForSeo, 'blocks'>): { label: string, text: string }[] => (
  page.blocks
    .filter((b) => b.label !== 'PageHeader' && b.label !== 'PageFooter')
    .map((b) => ({ label: b.label, text: blockText(b.html) }))
    .filter((b) => b.text.length > 0)
);

const pageBody = (texts: { label: string, text: string }[]): string => texts
  .map((t) => (t.label === 'SectionHeader'
    ? `<h2>${ escapeHtml(t.text) }</h2>`
    : t.label === 'Footnote'
      ? `<p><small>${ escapeHtml(t.text) }</small></p>`
      : `<p>${ escapeHtml(t.text) }</p>`))
  .join('');

const byLine = (book: BookForSeo): string => (
  book.authors.length > 0
    ? 'by ' + book.authors.map((a) => link(authorPath(a.id), a.name)).join(', ')
    : book.author.length > 0 ? 'by ' + escapeHtml(book.author) : ''
);

// "p. 12", "p. xiv"; a page named by its part as it is ("Book II, Chapter 6"); "page 3" for one
// without a printed number
export const pageLabel = (page: Pick<PageForSeo, 'printedPageNumber' | 'pageNumber'>): string => {
  const printed = page.printedPageNumber.trim();

  if (printed.length === 0) {
    return `page ${ page.pageNumber }`;
  }
  else if (/^(\d+|[ivxlcdm]+)$/i.test(printed)) {
    return `p. ${ printed }`;
  }
  else {
    return printed;
  }
};

// A book (page undefined: its first page, or its table of contents) or one of its pages
export const bookDocument = (origin: string, book: BookForSeo, page?: PageForSeo): SeoDocument => {
  const url      = origin + bookPath(book.id, page?.pageNumber);
  const texts    = page === undefined ? [] : pageTexts(page);
  const prose    = texts.filter((t) => t.label !== 'Footnote').map((t) => t.text).join(' ');
  const where    = book.issue === null ? '' : `${ book.issue.journalName }, ${ issueTitle(book.issue) }`;
  const heading  = book.author.length > 0 ? `${ book.title } - ${ book.author }` : book.title;
  const imageUrl = book.coverPhotoPath === undefined
    ? undefined
    : /^https?:\/\//.test(book.coverPhotoPath)
      ? book.coverPhotoPath
      : origin + '/' + book.coverPhotoPath.replace(/^\/+/, '');
  const work     = {
    '@type': book.issue === null ? 'Book' : 'ScholarlyArticle',
    name:    book.title,
    url:     origin + bookPath(book.id),
    ...(book.authors.length > 0 || book.author.length > 0 ? {
      author: book.authors.length > 0
        ? book.authors.map((a) => ({ '@type': 'Person', name: a.name, url: origin + authorPath(a.id) }))
        : { '@type': 'Person', name: book.author },
    } : {}),
    ...(book.issue === null ? { numberOfPages: book.pageCount } : {
      isPartOf: {
        '@type':  'PublicationIssue',
        name:     where,
        url:      origin + journalIssuePath(book.issue.journalId, book.issue.id),
        isPartOf: { '@type': 'Periodical', name: book.issue.journalName },
      },
    }),
    ...(imageUrl === undefined ? {} : { image: imageUrl }),
  };
  const pageNav  = page === undefined ? '' : `<nav>${ [
    page.previousPageId === null ? '' : link(bookPath(book.id, page.previousPageId), 'Previous page'),
    link(bookPath(book.id), 'Contents'),
    page.nextPageId === null ? '' : link(bookPath(book.id, page.nextPageId), 'Next page'),
  ].filter((s) => s.length > 0).join(' · ') }</nav>`;
  const contents = page !== undefined || book.contents.length === 0 ? '' : '<h2>Contents</h2>' + list(
    book.contents.map((c) => link(bookPath(book.id, c.pageId), c.label)),
  );

  return {
    status:       200,
    title:        page === undefined ? heading : `${ heading }, ${ pageLabel(page) }`,
    description:  truncate(
      prose.length > 0
        ? prose
        : [
            book.title,
            book.author.length > 0 ? `by ${ book.author }` : '',
            where.length > 0 ? `in ${ where }` : `${ book.pageCount } pages`,
          ].filter((s) => s.length > 0).join(', ') + '.',
      160,
    ),
    canonicalUrl: url,
    index:        true,
    ...(imageUrl === undefined ? {} : { imageUrl }),
    jsonLd:       page === undefined
      ? { '@context': 'https://schema.org', ...work }
      : {
          '@context': 'https://schema.org',
          '@type':    'WebPage',
          name:       `${ heading }, ${ pageLabel(page) }`,
          url,
          isPartOf:   work,
        },
    body:         main(
      `<article><h1>${ escapeHtml(book.title) }</h1>`,
      `<p>${ [
        byLine(book),
        book.issue === null
          ? ''
          : 'in ' + link(journalIssuePath(book.issue.journalId, book.issue.id), where),
      ].filter((s) => s.length > 0).join(' ') }</p>`,
      page === undefined
        ? ''
        : `<p>${ escapeHtml(pageLabel(page)) }</p>${ pageNav }${ pageBody(texts) }${ pageNav }`,
      contents,
      '</article>',
    ),
  };
};

// Several books open side by side: links to each, not indexed (each book is under its own URL)
export const openBooksDocument = (books: BookSummaryForSeo[]): SeoDocument => ({
  status:       200,
  title:        books.map((b) => b.title).join(' · '),
  description:  truncate('Reading ' + books.map((b) => b.title).join(', ') + '.', 160),
  canonicalUrl: null,
  index:        false,
  body:         main(list(books.map(bookItem))),
});

export const notFoundDocument = (): SeoDocument => ({
  status:       404,
  title:        `${ SITE_NAME } - Not found`,
  description:  'There is no such page.',
  canonicalUrl: null,
  index:        false,
  body:         '',
});

// A book withheld as likely still under copyright (see core/copyright_status.ts)
export const withheldDocument = (message: string): SeoDocument => ({
  status:       451,
  title:        SITE_NAME,
  description:  message,
  canonicalUrl: null,
  index:        false,
  body:         '',
});

// Pages the app has that aren't for search engines (the admin panel): the app as it is
export const unindexedDocument = (): SeoDocument => ({
  status:       200,
  title:        SITE_NAME,
  description:  '',
  canonicalUrl: null,
  index:        false,
  body:         '',
});

// --- robots.txt and the sitemaps ---

export const robotsTxt = (origin: string): string => [
  'User-agent: *',
  'Disallow: /tl-admin',
  'Disallow: /api/',
  '',
  `Sitemap: ${ origin }/sitemap.xml`,
  '',
].join('\n');

const escapeXml = (s: string) => escapeHtml(s);

export const urlsetXml = (urls: string[]): string => [
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
  ...urls.map((u) => `<url><loc>${ escapeXml(u) }</loc></url>`),
  '</urlset>',
  '',
].join('\n');

export const sitemapIndexXml = (urls: string[]): string => [
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
  ...urls.map((u) => `<sitemap><loc>${ escapeXml(u) }</loc></sitemap>`),
  '</sitemapindex>',
  '',
].join('\n');

// The sitemaps (/sitemaps/<name>), given how many books' pages there are to list: the books, the
// authors, the journals and their issues, and the pages, SITEMAP_SIZE to a file
export const sitemapNames = (pageCount: number): string[] => [
  'books.xml', 'authors.xml', 'journals.xml',
  ...Array.from({ length: Math.ceil(pageCount / SITEMAP_SIZE) }, (_, i) => `pages-${ i + 1 }.xml`),
];

export type SitemapName =
  | { kind: 'books' | 'authors' | 'journals' }
  | { kind: 'pages', offset: number, limit: number };

// Which sitemap a name is; null for none
export const sitemapOf = (name: string): SitemapName | null => {
  const pages = /^pages-([1-9]\d{0,4})\.xml$/.exec(name);

  if (name === 'books.xml' || name === 'authors.xml' || name === 'journals.xml') {
    return { kind: name.replace(/\.xml$/, '') as 'books' | 'authors' | 'journals' };
  }
  else if (pages !== null) {
    return { kind: 'pages', offset: (Number(pages[1]) - 1) * SITEMAP_SIZE, limit: SITEMAP_SIZE };
  }
  else {
    return null;
  }
};
