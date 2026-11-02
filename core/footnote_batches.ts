// Extracting footnotes' citations several pages at a time: most pages have
// only a few short footnotes, so the prompt (its instructions and insights,
// see footnotesPrompt) is most of each request; given the footnotes of
// several pages at once, the model reads them for a fraction of the cost, and
// can resolve an "Ibid." on one page from the page before. Pages whose
// footnotes can cite nothing are left out. Pure functions;
// lib/footnote_citations.ts makes the requests.
import type { ORResponseFormat } from '../lib/open_router.ts';
import type { Citation, SuryaPage } from '../types.ts';
import { strip_tags } from './book_pages.ts';
import { FOOTNOTES_FORMAT, footnoteHtml, type PageFootnotes } from './footnote_extraction.ts';

// A batch holds pages until their footnotes come to about this many
// characters, and at most MAX_BATCH_PAGES pages: small enough for the model to
// keep track of each page's footnotes, and for a failed request to lose little
export const MAX_BATCH_CHARS = 5000;
export const MAX_BATCH_PAGES = 6;

// What the model returns for a batch: each footnote with the page it's on
export type BatchFootnotes = Omit<PageFootnotes, 'footnotes'> & {
  footnotes: (PageFootnotes['footnotes'][number] & { page: number })[],
};

type Schema = {
  properties: { footnotes: { items: { required: string[], properties: Record<string, unknown> } } },
};

// FOOTNOTES_FORMAT with each footnote's page
export const FOOTNOTE_BATCH_FORMAT: ORResponseFormat = (() => {
  const format = structuredClone(FOOTNOTES_FORMAT);
  const item   = (format.json_schema.schema as unknown as Schema).properties.footnotes.items;

  item.required           = ['page', ...item.required];
  item.properties         = { page: { type: 'integer' }, ...item.properties };
  format.json_schema.name = 'pages_footnotes';
  return format;
})();

// A footnote that only points elsewhere in the book: "Cf. supra, note 10.",
// "See above, p. 101."
const CROSS_REFERENCE = /^\W*\d*\W*(?:cf\.?|see|vide)?\s*,?\s*(?:above|below|supra|infra)\b[^A-Za-z]*(?:(?:p|pp|n|nn|note|notes)\b\.?[^A-Za-z]*)*$/i;

// Whether a page's footnotes may cite something: not when each of its
// Footnote blocks has no letters (a stray page number) or is only a cross-
// reference. Kept whenever in doubt: one citation outweighs a request.
export const hasCitableFootnotes = (page: SuryaPage): boolean => page.blocks
  .filter((b) => b.label === 'Footnote')
  .map((b) => strip_tags(b.html).replace(/\s+/g, ' ').trim())
  .some((text) => /[A-Za-z]/.test(text) && !CROSS_REFERENCE.test(text));

// The pages with footnotes worth reading (see hasCitableFootnotes), in order,
// in batches of consecutive pages (see MAX_BATCH_CHARS and MAX_BATCH_PAGES); a
// page with more footnotes than a batch holds is a batch of its own
export const footnoteBatches = (pages: SuryaPage[]): SuryaPage[][] => pages
  .filter(hasCitableFootnotes)
  .reduce<{ batches: SuryaPage[][], chars: number }>((out, page) => {
    const last  = out.batches[out.batches.length - 1];
    const chars = footnoteHtml(page).length;

    if (!last || last.length >= MAX_BATCH_PAGES || out.chars + chars > MAX_BATCH_CHARS) {
      return { batches: [...out.batches, [page]], chars };
    }
    else {
      last.push(page);
      return { batches: out.batches, chars: out.chars + chars };
    }
  }, { batches: [], chars: 0 })
  .batches;

// What the model is given for a batch: which pages there are (told the
// count, the model is less apt to stop after the first few), then each page's
// footnotes in a <page>
export const batchHtml = (batch: SuryaPage[]): string => [
  `${ batch.length } pages: ${ batch.map((p) => p.page).join(', ') }. Return the footnotes of each.`,
  ...batch.map((page) => `<page number="${ page.page }">\n${ footnoteHtml(page) }\n</page>`),
].join('\n');

// A batch's footnotes' text, what insights' keywords are looked for in (see
// insightsForPage)
export const batchText = (batch: SuryaPage[]): string => (
  strip_tags(batch.map((page) => footnoteHtml(page)).join('\n'))
);

// The pages of a batch the model returned no footnotes for: it can stop
// partway through a batch. They are asked again (see retryBatches).
export const missedPages = (batch: SuryaPage[], result: BatchFootnotes): SuryaPage[] => {
  const answered = new Set(result.footnotes.map((f) => f.page));
  return batch.filter((p) => !answered.has(p.page));
};

// The batches to ask again for the pages missed in a batch: in batches half
// its size, so that a page missed again ends up asked on its own; none for a
// page asked on its own already (its footnotes cite nothing, as the model
// read them)
export const retryBatches = (batch: SuryaPage[], missed: SuryaPage[]): SuryaPage[][] => {
  const size = Math.max(1, Math.floor(batch.length / 2));
  return batch.length === 1
    ? []
    : Array.from({ length: Math.ceil(missed.length / size) }, (_, i) => missed.slice(i * size, (i + 1) * size));
};

// The citations the model returned for a batch, each on the page it gave
// (source.footnotePage), as pageCitations gives them; and how many footnotes
// it gave a page not in the batch, whose citations are left out
export const batchCitations = (
  bookId: string, batch: SuryaPage[], result: BatchFootnotes,
): { citations: Citation[], strays: number } => {
  const pages = new Set(batch.map((p) => p.page));
  const kept  = result.footnotes.filter((f) => pages.has(f.page));

  return {
    citations: kept.flatMap((footnote) => footnote.citations.map((c) => ({
      source: {
        bookId,
        footnoteIdentifier: footnote.identifier,
        footnotePage:       footnote.page,
      },
      referenceBookId: null,
      author:          c.author,
      title:           c.title,
      location:        c.location,
      raw:             c.raw,
      locationsCited:  c.locationsCited,
    }))),
    strays: result.footnotes.length - kept.length,
  };
};
