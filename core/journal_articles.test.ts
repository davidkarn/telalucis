import { describe, expect, it } from '@jest/globals';
import {
  articleBookId, articlePages, checkedArticles, DEPARTMENT_AUTHOR, isArticleOf, issueOutline, pageOffset,
  renumberCitations, type IssueArticle,
} from './journal_articles.ts';
import type { Page, PageBlock } from '../types.ts';

const block = (label: PageBlock['label'], text: string): PageBlock => ({ bbox: [0, 0, 0, 0], label, html: `<p>${ text }</p>`, citations: [] });
const page  = (scan: number, printed: string, ...blocks: PageBlock[]): Page => ({ pageNumber: scan, printedPageNumber: printed, blocks });
const issue = Array.from({ length: 12 }, (_, i) => page(i + 1, i + 1 > 2 ? String(i - 1) : ''));
const art   = (title: string, firstScan: number, lastScan: number, extra: Partial<IssueArticle> = {}): IssueArticle => ({
  title, author: 'A. Writer', firstScan, lastScan, department: false, ...extra,
});

describe('issueOutline', () => {
  it('gives each page\'s scan and printed numbers, running heads, headings and first text', () => {
    expect(issueOutline([
      page(8, '2', block('PageHeader', '2'), block('PageHeader', 'DAVID L. GREENSTOCK'), block('Text', 'been altogether happy')),
      page(22, '', block('SectionHeader', 'IDEALISM'), block('Text', 'WHEN Kant concludes')),
    ])).toBe(
      '[scan 8] printed 2 | heads: 2 / DAVID L. GREENSTOCK | text: been altogether happy\n'
      + '[scan 22] printed ? | headings: IDEALISM | text: WHEN Kant concludes'
    );
  });

  it('gives a table of contents in full', () => {
    const toc = page(3, 'iii', block('SectionHeader', 'TABLE OF CONTENTS'), block('Text', 'St. Thomas and Christian Perfection DAVID L. GREENSTOCK 1 '.repeat(3)));
    expect(issueOutline([toc])).toContain('contents: St. Thomas and Christian Perfection DAVID L. GREENSTOCK 1 St. Thomas');
  });
});

describe('checkedArticles', () => {
  it('orders the articles, runs each to the page before the next, and cleans titles', () => {
    expect(checkedArticles([
      art('Second', 7, 8), art('First — Continued', 3, 5), art('Book Reviews', 10, 40, { department: true, author: '' }),
    ], issue)).toEqual([
      art('First', 3, 6), art('Second', 7, 9), art('Book Reviews', 10, 12, { department: true, author: DEPARTMENT_AUTHOR }),
    ]);
  });

  it('drops untitled articles, those off the issue, and a second one on the same first page', () => {
    expect(checkedArticles([art('', 3, 4), art('Off', 30, 40), art('A', 5, 6), art('B', 5, 9)], issue))
      .toEqual([art('A', 5, 6)]);
  });
});

describe('articlePages', () => {
  it('numbers an article\'s pages as printed, a page misread or unnumbered as its neighbours say', () => {
    const pages = [page(7, ''), page(8, '6'), page(9, '9'), page(10, '8')];
    expect(pageOffset(pages, pages)).toBe(-2);
    expect(articlePages(art('A', 7, 10), pages).map((p) => [p.pageNumber, p.printedPageNumber]))
      .toEqual([[5, '5'], [6, '6'], [7, '7'], [8, '8']]);
  });

  it('takes the issue\'s offset when the article prints no number', () => {
    expect(pageOffset([page(5, ''), page(6, '')], issue)).toBe(-2);
  });
});

describe('renumberCitations and article ids', () => {
  it('moves citations to the pages as numbered, and names an article by its issue and first page', () => {
    const citation = { source:          { bookId: 'b', footnoteIdentifier: '1', footnotePage: 9 },
                       referenceBookId: null,
                       author:          '',
                       title:           '',
                       location:        '',
                       raw:             '',
                       locationsCited:  [] };
    expect(renumberCitations([citation], -6)[0]!.source.footnotePage).toBe(3);
    expect(articleBookId('thomist_1950-01_13_1', 16)).toBe('thomist_1950-01_13_1_p16');
    expect([isArticleOf('thomist_1950-01_13_1_p16', 'thomist_1950-01_13_1'), isArticleOf('thomist_1950-01_13_1', 'thomist_1950-01_13_1')])
      .toEqual([true, false]);
  });
});
