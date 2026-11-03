import { describe, expect, it } from '@jest/globals';
import { authorNames, namesAnAuthor } from './authors.ts';

describe('namesAnAuthor', () => {
  it('is true of a name', () => {
    expect(namesAnAuthor('Augustine of Hippo')).toBe(true);
  });

  it("is false of no author, an anonymous one or the Bible's, in any case", () => {
    expect(namesAnAuthor('')).toBe(false);
    expect(namesAnAuthor('  ')).toBe(false);
    expect(namesAnAuthor('Anonymous')).toBe(false);
    expect(namesAnAuthor('bible')).toBe(false);
  });
});

describe('authorNames', () => {
  it('is the one name of a book by one author', () => {
    expect(authorNames('Augustine of Hippo')).toEqual(['Augustine of Hippo']);
    expect(authorNames('  Walter  Farrell, O. P. ')).toEqual(['Walter Farrell, O. P.']);
  });

  it('splits names joined by "and" or "&", in order', () => {
    expect(authorNames('Mortimer J. Adler and Walter Farrell, O. P.')).toEqual(
      ['Mortimer J. Adler', 'Walter Farrell, O. P.'],
    );
    expect(authorNames('Yves de la Brière, S. J. AND P. M. Colbach, S. J.')).toEqual(
      ['Yves de la Brière, S. J.', 'P. M. Colbach, S. J.'],
    );
    expect(authorNames('Lewis & Langford')).toEqual(['Lewis', 'Langford']);
  });

  it('does not split a name at a comma, or within a word', () => {
    expect(authorNames('Humphrey, William, 1839-1910')).toEqual(['Humphrey, William, 1839-1910']);
    expect(authorNames('Alexander Anderson')).toEqual(['Alexander Anderson']);
  });

  it('names each author once, and none for no author', () => {
    expect(authorNames('Lewis and lewis')).toEqual(['Lewis']);
    expect(authorNames('Anonymous')).toEqual([]);
    expect(authorNames('Various')).toEqual([]);
    expect(authorNames('')).toEqual([]);
  });
});
