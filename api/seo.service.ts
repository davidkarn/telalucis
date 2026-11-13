// The web app's pages as served to search engines and link previews (see core/seo.ts): the
// built index.html with each page's head tags and content, and the robots.txt and sitemaps.
// nginx (docker/nginx.conf) passes the app's public paths here.
import { HttpException, Inject, Injectable } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { readFile, stat } from 'node:fs/promises';
import path from 'node:path';
import {
  authorDocument, authorPath, authorsDocument, bookDocument, bookPath, contentsList,
  homeDocument, journalDocument, journalIssueDocument, journalIssuePath, journalPath,
  journalsDocument, notFoundDocument, openBooksDocument, renderDocument, robotsTxt, seoRouteOf,
  sitemapIndexXml, sitemapNames, sitemapOf, unindexedDocument, urlsetXml, withheldDocument,
  type BookForSeo, type BookSummaryForSeo, type PageForSeo, type SeoDocument,
} from '../core/seo';
import { WITHHELD_MESSAGE } from '../core/copyright_status';
import { SeoQueries } from '../model/seo';
import { AuthorsService } from './authors.service';
import { BooksService } from './books.service';
import { DB } from './database.module';
import type { Database } from './database';
import { JournalsService } from './journals.service';

// The most books or issues an author's, journal's or issue's page lists
const MAX_LISTED = 1000;

// The built web app's index.html (WEB_INDEX_HTML; web/dist/index.html when it's been built,
// else the app's source index.html, in development)
const templatePaths = () => (
  process.env.WEB_INDEX_HTML !== undefined
    ? [process.env.WEB_INDEX_HTML]
    : [path.join('web', 'dist', 'index.html'), path.join('web', 'index.html')]
);

const isWithheld = (e: unknown) => e instanceof HttpException && e.getStatus() === 451;

@Injectable()
export class SeoService {
  private template: { path: string, mtimeMs: number, html: string } | null = null;

  constructor(
    @Inject(DB) private readonly db: Kysely<Database>,
    @Inject(BooksService) private readonly books: BooksService,
    @Inject(AuthorsService) private readonly authors: AuthorsService,
    @Inject(JournalsService) private readonly journals: JournalsService,
  ) {}

  // index.html, read again when it changes (a new build)
  private async indexHtml(): Promise<string> {
    for (const file of templatePaths()) {
      const info = await stat(file).catch(() => null);

      if (info === null) {
        continue;
      }
      else if (this.template?.path === file && this.template.mtimeMs === info.mtimeMs) {
        return this.template.html;
      }
      else {
        this.template = { path: file, mtimeMs: info.mtimeMs, html: await readFile(file, 'utf8') };
        return this.template.html;
      }
    }
    throw new Error(`The web app's index.html isn't at ${ templatePaths().join(' or ') }`);
  }

  // The page at `pathname`, as HTML, with its status
  async page(origin: string, pathname: string): Promise<{ status: number, robots: string, html: string }> {
    const doc  = await this.document(origin, pathname);
    const html = renderDocument(await this.indexHtml(), doc);
    return { status: doc.status, robots: doc.index ? 'index, follow' : 'noindex, follow', html };
  }

  private async document(origin: string, pathname: string): Promise<SeoDocument> {
    const route = seoRouteOf(pathname);

    try {
      switch (route.kind) {
      case 'home':
        return homeDocument(origin);
      case 'authors':
        return authorsDocument(origin, await SeoQueries.findAuthorsWithBooks(this.db));
      case 'author':
        return await this.authorDocument(origin, route.authorId);
      case 'journals':
        return journalsDocument(origin, (await this.journals.list()).items);
      case 'journal':
        return await this.journalDocument(origin, route.journalId);
      case 'journalIssue':
        return await this.journalIssueDocument(origin, route.journalId, route.issueId);
      case 'book':
        return await this.bookDocument(origin, route.bookId, route.volume, route.pageId);
      case 'books':
        return await this.openBooksDocument(route.bookIds);
      case 'other':
        return unindexedDocument();
      }
    }
    catch (e) {
      if (isWithheld(e)) {
        return withheldDocument(WITHHELD_MESSAGE);
      }
      else {
        throw e;
      }
    }
  }

  private async authorDocument(origin: string, authorId: string): Promise<SeoDocument> {
    const author = /^[1-9]\d{0,17}$/.test(authorId) ? await this.authors.get(authorId) : null;

    if (author === null) {
      return notFoundDocument();
    }
    else {
      const { items } = await this.books.search({ offset: 0, length: MAX_LISTED, authorId });
      return authorDocument(origin, author, items);
    }
  }

  private async journalDocument(origin: string, journalId: string): Promise<SeoDocument> {
    const journal = /^[1-9]\d{0,17}$/.test(journalId) ? await this.journals.get(journalId) : null;
    const issues  = journal === null
      ? null
      : await this.journals.issues(journalId, { offset: 0, length: MAX_LISTED });

    return journal === null || issues === null
      ? notFoundDocument()
      : journalDocument(origin, journal, issues.items);
  }

  private async journalIssueDocument(
    origin: string, journalId: string, issueId: string,
  ): Promise<SeoDocument> {
    const ids   = /^[1-9]\d{0,17}$/.test(journalId) && /^[1-9]\d{0,17}$/.test(issueId);
    const issue = ids ? await this.journals.issue(journalId, issueId) : null;

    if (issue === null) {
      return notFoundDocument();
    }
    else {
      const { items } = await this.books.search({ offset: 0, length: 100, journalIssueId: issueId });
      return journalIssueDocument(origin, issue, items);
    }
  }

  private async bookDocument(
    origin: string, bookId: string, volume: number | undefined, pageId: number | undefined,
  ): Promise<SeoDocument> {
    const book = await this.books.get(bookId, { volume, pageId });
    const page = book === null || pageId === undefined ? null : await this.books.getPage(bookId, pageId);

    if (book === null || (pageId !== undefined && page === null)) {
      return notFoundDocument();
    }
    else {
      const links                              = await SeoQueries.findBookLinks(this.db, bookId);
      const issue                              = links.issue === null
        ? null
        : await this.journals.issue(links.issue.journalId, links.issue.issueId);
      const forSeo: BookForSeo                 = {
        id:        book.id,
        title:     book.title,
        author:    book.author,
        authors:   links.authors,
        issue,
        pageCount: book.pageCount,
        contents:  contentsList(book.contents),
        ...(book.coverPhotoPath === undefined ? {} : { coverPhotoPath: book.coverPhotoPath }),
      };
      const pageIds                            = book.pageOrder
        .filter((p) => p.isVolume !== true).map((p) => p.pageId);
      const at                                 = pageId === undefined ? -1 : pageIds.indexOf(pageId);
      const pageForSeo: PageForSeo | undefined = page === null ? undefined : {
        pageNumber:        page.pageNumber,
        printedPageNumber: page.printedPageNumber,
        blocks:            page.blocks,
        previousPageId:    at > 0 ? pageIds[at - 1]! : null,
        nextPageId:        at >= 0 && at + 1 < pageIds.length ? pageIds[at + 1]! : null,
      };

      return bookDocument(origin, forSeo, pageForSeo);
    }
  }

  // Several books open: their titles, those that can be shown
  private async openBooksDocument(bookIds: string[]): Promise<SeoDocument> {
    const books = await Promise.all(bookIds.map(async(id): Promise<BookSummaryForSeo | null> => {
      const book = await this.books.get(id).catch((e: unknown) => {
        if (isWithheld(e)) {
          return null;
        }
        else {
          throw e;
        }
      });
      return book === null ? null : { id: book.id, title: book.title, author: book.author };
    }));

    return openBooksDocument(books.filter((b) => b !== null));
  }

  robotsTxt(origin: string): string {
    return robotsTxt(origin);
  }

  async sitemapIndex(origin: string): Promise<string> {
    const pages = await SeoQueries.countShownPages(this.db);
    return sitemapIndexXml(sitemapNames(pages).map((n) => `${ origin }/sitemaps/${ n }`));
  }

  // The sitemap /sitemaps/<name>; null when there's no such sitemap
  async sitemap(origin: string, name: string): Promise<string | null> {
    const which = sitemapOf(name);

    if (which === null) {
      return null;
    }
    else if (which.kind === 'books') {
      const ids = await SeoQueries.findShownBookIds(this.db);
      return urlsetXml([
        origin + '/', origin + '/authors', origin + '/journals',
        ...ids.map((id) => origin + bookPath(id)),
      ]);
    }
    else if (which.kind === 'authors') {
      const authors = await SeoQueries.findAuthorsWithBooks(this.db);
      return urlsetXml(authors.map((a) => origin + authorPath(a.id)));
    }
    else if (which.kind === 'journals') {
      const journals = (await this.journals.list()).items;
      const issues   = await SeoQueries.findJournalIssueIds(this.db);
      return urlsetXml([
        ...journals.map((j) => origin + journalPath(j.id)),
        ...issues.map((i) => origin + journalIssuePath(i.journalId, i.issueId)),
      ]);
    }
    else {
      const pages = await SeoQueries.findShownPages(this.db, which.offset, which.limit);
      return pages.length === 0
        ? null
        : urlsetXml(pages.map((p) => origin + bookPath(p.bookId, p.pageNumber)));
    }
  }
}
