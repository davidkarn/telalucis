import { describe, expect, it } from '@jest/globals';
import { citablePageNumber } from './page_citations.ts';

describe('citablePageNumber', () => {
  it('is the printed number when it is an integer', () => {
    expect(citablePageNumber('227')).toBe(227);
  });

  it('is null for Roman numerals, spreads and unnumbered pages', () => {
    expect(citablePageNumber('xii')).toBeNull();
    expect(citablePageNumber('8-9')).toBeNull();
    expect(citablePageNumber('')).toBeNull();
  });
});
