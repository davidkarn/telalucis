import { describe, expect, it } from '@jest/globals';
import { citation_page_html, CONTEXT_WORDS, footnoteContext, type SourceBlock } from './citation_pages.ts';

const block = (id: string, label: string, html: string): SourceBlock => ({ id, label, html });

describe('citation_page_html', () => {
  const blocks = [
    block('1', 'PageHeader', '<p>Running head</p>'),
    block('2', 'Text', '<p>Body text.</p>'),
    block('3', 'Footnote', '<p><sup>1</sup> First note.</p><p><sup>2</sup> Second note.</p>'),
    block('4', 'Footnote', '<p>continued second note.</p><p><sup>3</sup> Third note.</p>'),
    block('5', 'PageFooter', '<p>12</p>'),
  ];

  it("keeps the body and only the citation's footnote, without headers and footers", () => {
    expect(citation_page_html(blocks, '3', '1')).toBe(
      '<p>Body text.</p>\n<p><sup>1</sup> First note.</p>'
    );
  });

  it('follows a footnote into the next block, up to the next marked one', () => {
    expect(citation_page_html(blocks, '3', '2)')).toBe(
      '<p>Body text.</p>\n<p><sup>2</sup> Second note.</p>\n<p>continued second note.</p>'
    );
  });

  it("keeps the citation's whole block when no paragraph has its marker", () => {
    expect(citation_page_html(blocks, '4', '9')).toBe(
      '<p>Body text.</p>\n<p>continued second note.</p><p><sup>3</sup> Third note.</p>'
    );
  });
});

describe('footnoteContext', () => {
  const words = (from: number, to: number) => Array.from({ length: to - from + 1 }, (_, i) => `w${ from + i }`).join(' ');

  it("gives the words of the paragraph around the footnote's marker, without other markers", () => {
    expect(footnoteContext([
      block('1', 'Text', '<p>Unrelated paragraph.</p><p>Hence the Philosopher says (Ethic. i)<sup>2</sup>: "Goodness is what all desire."<sup>3</sup></p>'),
      block('2', 'Footnote', '<p><sup>2</sup> Eth. Nic. I. i. 1.</p>'),
    ], '2')).toBe('Hence the Philosopher says (Ethic. i): "Goodness is what all desire."');
  });

  it(`gives at most ${ CONTEXT_WORDS } words, 15 before the marker, with "…" where it's cut`, () => {
    const context = footnoteContext([block('1', 'Text', `<p>${ words(1, 40) }<sup>8</sup> ${ words(41, 80) }</p>`)], '8');
    expect(context).toBe(`…${ words(26, 50) }…`);
  });

  it("starts at the paragraph's start when the marker is near it", () => {
    expect(footnoteContext([block('1', 'Text', `<p>${ words(1, 3) }<sup>1</sup> ${ words(4, 60) }</p>`)], '1'))
      .toBe(`${ words(1, 25) }…`);
  });

  it('matches a marker printed with a closing bracket or point', () => {
    expect(footnoteContext([block('1', 'Text', '<p>As he says<sup>4)</sup> here.</p>')], '4')).toBe('As he says here.');
  });

  it('is null when no body paragraph has the marker, or the footnote has none', () => {
    const page = [block('1', 'Text', '<p>No markers here.</p>'), block('2', 'Footnote', '<p><sup>5</sup> Note.</p>')];
    expect(footnoteContext(page, '5')).toBeNull();
    expect(footnoteContext(page, '')).toBeNull();
  });
});
