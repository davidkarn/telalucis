import { describe, expect, it } from '@jest/globals';
import {
  bookDocument, contentsList, escapeHtml, issueTitle, openBooksDocument, pageLabel, pageTexts,
  renderDocument, robotsTxt, seoRouteOf, sitemapNames, sitemapOf, truncate, urlsetXml,
  SITEMAP_SIZE, type BookForSeo, type PageForSeo,
} from './seo.ts';

const ORIGIN = 'https://example.org';

const TEMPLATE = '<html><head><meta charset="UTF-8" />\n<title>Books</title></head>'
  + '<body><div id="root"></div></body></html>';

const book = (over: Partial<BookForSeo> = {}): BookForSeo => ({
  id:        'aristotle-politics',
  title:     'Politics',
  author:    'Aristotle',
  authors:   [{ id: '56', name: 'Aristotle' }],
  issue:     null,
  pageCount: 104,
  contents:  [{ pageId: 3, label: 'Book I' }],
  ...over,
});

const page = (over: Partial<PageForSeo> = {}): PageForSeo => ({
  pageNumber:        20,
  printedPageNumber: '12',
  blocks:            [
    { label: 'PageHeader', html: '<p>Politics</p>' },
    { label: 'SectionHeader', html: '<h2>CHAPTER VI</h2>' },
    { label: 'Text', html: '<p>It is also <i>nearly</i> the same<sup>1</sup> in the Laws.</p>' },
    { label: 'Footnote', html: '<p><sup>1</sup> Plato, Laws 739.</p>' },
    { label: 'PageFooter', html: '<p>12</p>' },
  ],
  previousPageId: 19,
  nextPageId:     21,
  ...over,
});

describe('seoRouteOf', () => {
  it('reads the app\'s public routes', () => {
    expect(seoRouteOf('/')).toEqual({ kind: 'home' });
    expect(seoRouteOf('/authors')).toEqual({ kind: 'authors' });
    expect(seoRouteOf('/authors/56')).toEqual({ kind: 'author', authorId: '56' });
    expect(seoRouteOf('/journals/2/issues/102')).toEqual({ kind: 'journalIssue', journalId: '2', issueId: '102' });
  });

  it('reads one book, with its volume and page, its id decoded', () => {
    expect(seoRouteOf('/books/A%2520Manual/volumes/2/pages/14')).toEqual({
      kind: 'book', bookId: 'A%20Manual', volume: 2, pageId: 14,
    });
    expect(seoRouteOf('/books/x')).toEqual({ kind: 'book', bookId: 'x' });
  });

  it('reads several books open side by side', () => {
    expect(seoRouteOf('/books/a/pages/3/books/b')).toEqual({ kind: 'books', bookIds: ['a', 'b'] });
  });

  it('leaves other paths, and malformed ones, to the app', () => {
    expect(seoRouteOf('/tl-admin/books')).toEqual({ kind: 'other' });
    expect(seoRouteOf('/books/a/chapters/3')).toEqual({ kind: 'other' });
    expect(seoRouteOf('/books/%E0%A4%A')).toEqual({ kind: 'other' });
  });
});

describe('escapeHtml and truncate', () => {
  it('escapes markup and quotes', () => {
    expect(escapeHtml('<a href="x">O\'Neil & co</a>')).toBe('&lt;a href=&quot;x&quot;&gt;O&#39;Neil &amp; co&lt;/a&gt;');
  });

  it('cuts between words', () => {
    expect(truncate('one two three four', 12)).toBe('one two…');
    expect(truncate('short', 12)).toBe('short');
  });
});

describe('pageLabel', () => {
  it('numbers pages printed with a number, and names those printed with a part', () => {
    expect(pageLabel({ printedPageNumber: '12', pageNumber: 20 })).toBe('p. 12');
    expect(pageLabel({ printedPageNumber: 'xiv', pageNumber: 2 })).toBe('p. xiv');
    expect(pageLabel({ printedPageNumber: 'Book II, Chapter 6', pageNumber: 20 })).toBe('Book II, Chapter 6');
    expect(pageLabel({ printedPageNumber: '', pageNumber: 3 })).toBe('page 3');
  });
});

describe('pageTexts', () => {
  it('leaves out running headers and footers, and footnote markers', () => {
    expect(pageTexts(page())).toEqual([
      { label: 'SectionHeader', text: 'CHAPTER VI' },
      { label: 'Text', text: 'It is also nearly the same in the Laws.' },
      { label: 'Footnote', text: 'Plato, Laws 739.' },
    ]);
  });
});

describe('contentsList', () => {
  it('names each entry after those it is in', () => {
    expect(contentsList([{
      partType:     'book',
      partValue:    '1',
      pageId:       3,
      childEntries: [
        { partType: 'chapter', partValue: '2', pageId: 4, childEntries: [] },
        { partType: 'chapter', partValue: '3', pageId: 6, label: 'On Slavery', childEntries: [] },
      ],
    }])).toEqual([
      { pageId: 3, label: 'Book 1' },
      { pageId: 4, label: 'Book 1, Chapter 2' },
      { pageId: 6, label: 'Book 1, On Slavery' },
    ]);
  });
});

describe('issueTitle', () => {
  it('names an issue as the web app does', () => {
    expect(issueTitle({
      id:          '102',
      journalId:   '2',
      journalName: 'The Modern Schoolman',
      startDate:   '1927-11-01',
      endDate:     '1927-11-30',
      volume:      '4',
      number:      '2',
    })).toBe('Vol. 4, No. 2 · November 1927');
  });
});

describe('bookDocument', () => {
  it('gives a page its own title, description, canonical URL and text', () => {
    const doc = bookDocument(ORIGIN, book(), page());

    expect(doc.title).toBe('Politics - Aristotle, p. 12');
    expect(doc.description).toBe('CHAPTER VI It is also nearly the same in the Laws.');
    expect(doc.canonicalUrl).toBe(`${ ORIGIN }/books/aristotle-politics/pages/20`);
    expect(doc.index).toBe(true);
    expect(doc.body).toContain('<h2>CHAPTER VI</h2>');
    expect(doc.body).toContain('<a href="/books/aristotle-politics/pages/21">Next page</a>');
    expect(doc.body).toContain('<a href="/authors/56">Aristotle</a>');
    expect(doc.body).not.toContain('Politics</p><h2>');
    expect(doc.jsonLd).toMatchObject({ '@type': 'WebPage', isPartOf: { '@type': 'Book', numberOfPages: 104 } });
  });

  it('lists a book\'s contents, and describes it when it has no page open', () => {
    const doc = bookDocument(ORIGIN, book({ coverPhotoPath: 'covers/p.jpg' }));

    expect(doc.title).toBe('Politics - Aristotle');
    expect(doc.description).toBe('Politics, by Aristotle, 104 pages.');
    expect(doc.canonicalUrl).toBe(`${ ORIGIN }/books/aristotle-politics`);
    expect(doc.imageUrl).toBe(`${ ORIGIN }/covers/p.jpg`);
    expect(doc.body).toContain('<a href="/books/aristotle-politics/pages/3">Book I</a>');
  });

  it('makes a journal article a ScholarlyArticle in its issue', () => {
    const doc = bookDocument(ORIGIN, book({
      id:    'res-philosophica_1927-11_4_2_p24',
      issue: {
        id:          '102',
        journalId:   '2',
        journalName: 'The Modern Schoolman',
        startDate:   '1927-11-01',
        endDate:     '1927-11-30',
        volume:      '4',
        number:      '2',
      },
    }));

    expect(doc.jsonLd).toMatchObject({
      '@type':  'ScholarlyArticle',
      isPartOf: { '@type': 'PublicationIssue', url: `${ ORIGIN }/journals/2/issues/102` },
    });
    expect(doc.description).toBe('Politics, by Aristotle, in The Modern Schoolman, Vol. 4, No. 2 · November 1927.');
  });

  it('escapes what it is given', () => {
    const doc = bookDocument(ORIGIN, book({ title: '<script>alert(1)</script>' }));
    expect(doc.body).not.toContain('<script>');
  });
});

describe('openBooksDocument', () => {
  it('is not indexed, has no canonical URL, and links each book', () => {
    const doc = openBooksDocument([
      { id: 'a', title: 'A', author: 'X' }, { id: 'b c', title: 'B', author: '' },
    ]);

    expect(doc.index).toBe(false);
    expect(doc.canonicalUrl).toBe(null);
    expect(doc.body).toContain('<a href="/books/b%20c">B</a>');
  });
});

describe('renderDocument', () => {
  it('puts the head tags in place of the title, and the content in #root', () => {
    const html = renderDocument(TEMPLATE, openBooksDocument([{ id: 'a', title: 'A $& B', author: 'X' }]));

    expect(html).not.toContain('<title>Books</title>');
    expect(html).toContain('<title>A $&amp; B</title>');
    expect(html).toContain('<meta name="robots" content="noindex,follow">');
    expect(html).not.toContain('rel="canonical"');
    expect(html).toContain('<div id="root"><main>');
  });

  it('keeps JSON-LD from closing its script', () => {
    const html = renderDocument(TEMPLATE, bookDocument(ORIGIN, book({ title: '</script><b>' })));
    expect(html).toMatch(/<script type="application\/ld\+json">[^<]*\\u003c\/script\\u003e/);
  });
});

describe('robots.txt and the sitemaps', () => {
  it('disallows the admin panel and the API, and names the sitemap', () => {
    expect(robotsTxt(ORIGIN)).toContain('Disallow: /tl-admin\n');
    expect(robotsTxt(ORIGIN)).toContain(`Sitemap: ${ ORIGIN }/sitemap.xml`);
  });

  it('splits the pages into sitemaps of SITEMAP_SIZE', () => {
    expect(sitemapNames(SITEMAP_SIZE + 1)).toEqual(['books.xml', 'authors.xml', 'journals.xml', 'pages-1.xml', 'pages-2.xml']);
    expect(sitemapOf('pages-2.xml')).toEqual({ kind: 'pages', offset: SITEMAP_SIZE, limit: SITEMAP_SIZE });
    expect(sitemapOf('authors.xml')).toEqual({ kind: 'authors' });
    expect(sitemapOf('pages-0.xml')).toBe(null);
    expect(sitemapOf('other.xml')).toBe(null);
  });

  it('escapes URLs', () => {
    expect(urlsetXml(['https://e.org/a?b=1&c=2'])).toContain('<loc>https://e.org/a?b=1&amp;c=2</loc>');
  });
});
