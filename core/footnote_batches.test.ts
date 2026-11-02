import { describe, expect, it } from '@jest/globals';
import {
  batchCitations, batchHtml, FOOTNOTE_BATCH_FORMAT, footnoteBatches, hasCitableFootnotes,
  MAX_BATCH_CHARS, MAX_BATCH_PAGES, missedPages, retryBatches,
} from './footnote_batches.ts';
import { footnotesPrompt, withoutInsights } from './footnote_extraction.ts';
import type { SuryaPage } from '../types.ts';

const page = (n: number, ...footnotes: string[]): SuryaPage => ({
  page:       n,
  image_bbox: [0, 0, 0, 0],
  blocks:     [
    { label: 'Text', html: '<p>Body text.</p>' },
    ...footnotes.map((html) => ({ label: 'Footnote', html })),
  ].map((b, i) => ({
    ...b,
    reading_order: i,
    bbox:          [0, 0, 0, 0],
    polygon:       [],
    confidence:    1,
    raw_label:     b.label,
    skipped:       false,
    error:         false,
  })) as SuryaPage['blocks'],
});

describe('hasCitableFootnotes', () => {
  it('keeps pages whose footnotes may cite something', () => {
    expect(hasCitableFootnotes(page(1, '<p>2 Summa Theol., III, q. 62, a. 5.</p>'))).toBe(true);
    expect(hasCitableFootnotes(page(1, '<p>76 Ibid., a. 2, ad 1.</p>'))).toBe(true);
    expect(hasCitableFootnotes(page(1, '<p>121 Gal. 4: 9.</p>'))).toBe(true);
    expect(hasCitableFootnotes(page(1, '<p>22 Cf. above, n. 88.</p>', '<p>23 Pohle, Christology.</p>'))).toBe(true);
  });

  it('leaves out pages with no footnotes, stray numbers, or only cross-references', () => {
    expect(hasCitableFootnotes(page(1))).toBe(false);
    expect(hasCitableFootnotes(page(1, '<p>81</p>'))).toBe(false);
    expect(hasCitableFootnotes(page(1, '<p>18 Cf. supra, note 10.</p>', '<p>27 See above, p. 101.</p>'))).toBe(false);
  });
});

describe('footnoteBatches', () => {
  it('groups consecutive pages with footnotes, skipping the others', () => {
    const pages = [page(1, '<p>1 Pohle.</p>'), page(2), page(3, '<p>2 Ibid.</p>'), page(4, '<p>81</p>')];
    expect(footnoteBatches(pages).map((b) => b.map((p) => p.page))).toEqual([[1, 3]]);
  });

  it('starts a new batch at MAX_BATCH_PAGES pages or MAX_BATCH_CHARS characters', () => {
    const small = Array.from({ length: MAX_BATCH_PAGES + 1 }, (_, i) => page(i + 1, '<p>1 Pohle.</p>'));
    expect(footnoteBatches(small).map((b) => b.length)).toEqual([MAX_BATCH_PAGES, 1]);

    const long = `<p>1 ${ 'Pohle, Christology. '.repeat(MAX_BATCH_CHARS / 30) }</p>`;
    expect(footnoteBatches([page(1, long), page(2, long), page(3, long)]).map((b) => b.length)).toEqual([1, 1, 1]);
  });
});

describe('batchHtml', () => {
  it('gives each page\'s footnotes in a <page>', () => {
    expect(batchHtml([page(4, '<p>1 A.</p>'), page(5, '<p>2 B.</p>')])).toBe(
      '2 pages: 4, 5. Return the footnotes of each.\n'
      + '<page number="4">\n<p>1 A.</p>\n</page>\n<page number="5">\n<p>2 B.</p>\n</page>'
    );
  });
});

describe('FOOTNOTE_BATCH_FORMAT and the prompt', () => {
  it('asks for each footnote\'s page', () => {
    const item = (FOOTNOTE_BATCH_FORMAT.json_schema.schema as {
      properties: { footnotes: { items: { required: string[] } } },
    }).properties.footnotes.items;
    expect(item.required).toEqual(['page', 'identifier', 'citations']);
    expect(footnotesPrompt([], true)).toContain('- page: the number of the <page> the footnote is on.');
    expect(footnotesPrompt([])).not.toContain('<page');
  });
});

describe('leaving insights out', () => {
  it('neither gives nor asks for insights, in the prompt or the format', () => {
    const schema = withoutInsights(FOOTNOTE_BATCH_FORMAT).json_schema.schema as {
      required: string[], properties: Record<string, unknown>,
    };
    expect(schema.required).toEqual(['footnotes']);
    expect(Object.keys(schema.properties)).toEqual(['footnotes']);
    expect(footnotesPrompt(null, true)).not.toContain('insight');
    expect(footnotesPrompt(['First insight'], true)).toContain('    - First insight');
    expect((FOOTNOTE_BATCH_FORMAT.json_schema.schema as { required: string[] }).required)
      .toContain('additionalInsights');
  });
});

describe('missedPages and retryBatches', () => {
  it('asks the pages the model missed again in batches half the size, then alone', () => {
    const batch  = [1, 2, 3, 4, 5, 6].map((n) => page(n, '<p>1 Pohle.</p>'));
    const missed = missedPages(batch, {
      additionalInsights: [],
      footnotes:          [{ page: 1, identifier: '1', citations: [] }, { page: 2, identifier: '2', citations: [] }],
    });

    expect(missed.map((p) => p.page)).toEqual([3, 4, 5, 6]);
    expect(retryBatches(batch, missed).map((b) => b.map((p) => p.page))).toEqual([[3, 4, 5], [6]]);
    expect(retryBatches([page(3)], [page(3)])).toEqual([]);
  });
});

describe('batchCitations', () => {
  it('puts each citation on the page the model gave, leaving out pages not in the batch', () => {
    const citation = { author: 'Pohle', title: 'Christology', location: 'p. 3', raw: 'Pohle, p. 3', locationsCited: [] };
    const found    = batchCitations('book', [page(4), page(5)], {
      additionalInsights: [],
      footnotes:          [
        { page: 5, identifier: '2', citations: [citation] },
        { page: 9, identifier: '3', citations: [citation] },
      ],
    });

    expect(found.citations.map((c) => c.source)).toEqual([
      { bookId: 'book', footnoteIdentifier: '2', footnotePage: 5 },
    ]);
    expect(found.strays).toBe(1);
  });
});
