import { describe, expect, it } from '@jest/globals';
import {
  citedArticle, citedCollectionVolume, collectionVolume, firstCitedPage, issueNumbers,
  type JournalArticleRef,
} from './collection_links.ts';

describe('issueNumbers', () => {
  it('reads the volume and year as written', () => {
    expect(issueNumbers('XXIX (April, 1955)')).toEqual({ volume: 29, year: 1955 });
    expect(issueNumbers('2 (1941)')).toEqual({ volume: 2, year: 1941 });
    expect(issueNumbers('t. iii.')).toEqual({ volume: 3, year: null });
    expect(issueNumbers('Vol. 23, no. 4, 1931')).toEqual({ volume: 23, year: 1931 });
    expect(issueNumbers('n. s. III—VI (1916—1920)')).toEqual({ volume: 3, year: 1916 });
    expect(issueNumbers('1892')).toEqual({ volume: null, year: 1892 });
  });

  it('takes no volume from an issue named by its date', () => {
    expect(issueNumbers('October 16, 1906')).toEqual({ volume: null, year: 1906 });
    expect(issueNumbers('12th and 19th November 1872')).toEqual({ volume: null, year: 1872 });
    expect(issueNumbers('Vol. XXXVI')).toEqual({ volume: 36, year: null });
  });
});

describe('firstCitedPage', () => {
  it('is the first page in arabic numerals', () => {
    expect(firstCitedPage('pp. 37—53')).toBe(37);
    expect(firstCitedPage('451-480')).toBe(451);
    expect(firstCitedPage('p. xlii')).toBeNull();
  });
});

describe('citedArticle', () => {
  const article = (id: string, firstPage: number, lastPage: number, extra: Partial<JournalArticleRef> = {}) => ({
    id, title: 'An Article ' + id, journal: 'Theological Studies', volume: 2, year: 1941, firstPage, lastPage, ...extra,
  });
  const articles = [article('a', 307, 324), article('b', 325, 360), article('c', 1, 40, { volume: 3, year: 1942 })];

  it("is the article of the journal's issue whose pages take in the page cited", () => {
    expect(citedArticle({ title: '', collection: 'Theological Studies', issue: '2 (1941)', location: '330-334' }, articles)?.id)
      .toBe('b');
    expect(citedArticle({ title: '', collection: 'Theological Studies', issue: '3 (1942)', location: '12' }, articles)?.id)
      .toBe('c');
  });

  it('goes by the year when the issue names no volume', () => {
    expect(citedArticle({ title: '', collection: 'Theological Studies', issue: 'March 1942', location: 'p. 5' }, articles)?.id)
      .toBe('c');
  });

  it('goes by the title without a page, and finds none in another journal or issue', () => {
    expect(citedArticle({ title: 'An Article a', collection: 'Theological Studies', issue: '2 (1941)', location: '' }, articles)?.id)
      .toBe('a');
    expect(citedArticle({ title: '', collection: 'Theological Studies', issue: '2 (1941)', location: '' }, articles)).toBeUndefined();
    expect(citedArticle({ title: '', collection: 'The Thomist', issue: '2 (1941)', location: '330' }, articles)).toBeUndefined();
    expect(citedArticle({ title: '', collection: 'Theological Studies', issue: '9 (1948)', location: '330' }, articles)).toBeUndefined();
  });
});

describe('collectionVolume and citedCollectionVolume', () => {
  const books = [
    { id: 'pg100', title: 'Patrologia Graeca 100' },
    { id: 'pg101', title: 'Patrologia Graeca 101' },
    { id: 'other', title: 'The Enchiridion' },
  ];

  it('reads a volume of a collection from its title', () => {
    expect(collectionVolume('Patrologia Graeca 100')).toEqual({ name: 'Patrologia Graeca', volume: 100 });
    expect(collectionVolume('Acta Apostolicae Sedis, vol. XVII')).toEqual({ name: 'Acta Apostolicae Sedis', volume: 17 });
    expect(collectionVolume('The Enchiridion')).toBeNull();
  });

  it('is the volume holding the issue cited', () => {
    expect(citedCollectionVolume({ title: '', collection: 'Patrologia Graeca', issue: '100', location: '1023' }, books)?.id)
      .toBe('pg100');
    expect(citedCollectionVolume({ title: '', collection: 'Patrologia Latina', issue: '100', location: '1' }, books))
      .toBeUndefined();
    expect(citedCollectionVolume({ title: '', collection: null, issue: '100', location: '1' }, books)).toBeUndefined();
  });
});
