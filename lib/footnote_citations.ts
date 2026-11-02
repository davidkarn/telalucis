// The citations in an OCR'd book's footnotes, extracted by an LLM several
// pages at a time (see core/footnote_extraction.ts and
// core/footnote_batches.ts), with the insights the model learned doing it:
// shared by process-ocred-books and import-journal-issues. Needs
// OPENROUTER_KEY.
import { setTimeout } from 'node:timers/promises';
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { fillCitationAuthors } from '../core/citation_authors.ts';
import { batchExamples, examplesPart, type ExampleIndex } from '../core/citation_examples.ts';
import {
  batchCitations, batchFootnotes, batchHtml, batchText, FOOTNOTE_BATCH_FORMAT, footnoteBatches, missedPages,
  retryBatches, type BatchFootnotes,
} from '../core/footnote_batches.ts';
import {
  footnotesPrompt, insightsForPage, learnedInsights, removeDuplicateInsights, withoutInsights,
  type ScoredInsight,
} from '../core/footnote_extraction.ts';
import type { ExtractedCitations } from '../core/ocred_book_import.ts';
import { log } from './lib.ts';
import { isLocalModel, makeOpenRouterRequest, parseJsonResponse } from './open_router.ts';
import { FootnoteExtractionInsightQueries } from '../model/footnote_extraction_insights.ts';
import type { Citation, SuryaPage } from '../types.ts';

// OpenRouter allows 20 requests a minute
const REQUEST_INTERVAL_MS = 3200;

export const DEFAULT_FOOTNOTES_MODEL = 'openai/gpt-4o-mini';

// Every citation in the book's footnotes, a request per batch of pages with
// footnotes worth reading (see core/footnote_batches.ts), each request given
// the insights learned so far: the widely applying ones (operatingInsights),
// and any with a keyword found in the batch's footnotes (see insightsForPage).
// Pages the model returns nothing for are asked again in smaller batches, then
// alone (see retryBatches). The pages of a batch whose request fails are
// returned in failedPages rather than failing the whole book. Authors the
// model left out are filled in from the titles where they can be (see
// fillCitationAuthors).
// skipInsights: neither give the model insights nor ask it for new ones, for
// shorter requests; none are returned, so none are saved.
// model: an OpenRouter model, or a local one ("local/<name>", see
// lib/open_router.ts), asked without waiting between requests.
// examples: citations to show the model those like each batch's (see
// core/citation_examples.ts), none from this book.
export const extractFootnoteCitations = async(
  db: Kysely<Database>, bookId: string, pages: SuryaPage[],
  { skipInsights = false, model = DEFAULT_FOOTNOTES_MODEL, examples }: {
    skipInsights?: boolean, model?: string, examples?: ExampleIndex,
  } = {},
): Promise<ExtractedCitations> => {
  const batches                                        = footnoteBatches(pages);
  const citations: Citation[]                          = [];
  const failedPages: { page: number, error: string }[] = [];
  const format                                         = skipInsights ? withoutInsights(FOOTNOTE_BATCH_FORMAT) : FOOTNOTE_BATCH_FORMAT;

  let insights: ScoredInsight[] = skipInsights
    ? []
    : await FootnoteExtractionInsightQueries.findInsights(db);

  let operatingInsights = insights.filter((i) => !i.score || i.score <= 1);

  // the batches still to ask, those of pages the model missed put next
  const queue = [...batches];
  let asked   = 0;

  while (queue.length > 0) {
    const batch = queue.shift()!;
    asked++;

    try {
      const given    = skipInsights
        ? null
        : insightsForPage(operatingInsights, insights, batchText(batch));
      const shown    = examples
        ? examplesPart(batchExamples(examples, batchFootnotes(batch), { excludeBookId: bookId }))
        : '';
      const response = await makeOpenRouterRequest([
        { role: 'system', content: footnotesPrompt(given, true) + shown },
        { role: 'user', content: batchHtml(batch) },
      ], format, model);

      const result      = parseJsonResponse<BatchFootnotes>(response);
      const learned     = learnedInsights(result);
      const found       = batchCitations(bookId, batch, result);
      const missed      = missedPages(batch, result);
      insights          = removeDuplicateInsights(insights.concat(learned));
      operatingInsights = removeDuplicateInsights(
        operatingInsights.concat(learned.filter((s) => s.score !== null && s.score <= 2))
      );
      citations.push(...found.citations);
      queue.unshift(...retryBatches(batch, missed));
      log(response, `request ${ asked } (${ queue.length } to go): pages ${ batch.map((p) => p.page).join(', ') }`
        + (missed.length > 0 ? `; missed ${ missed.map((p) => p.page).join(', ') }` : '')
        + (found.strays > 0 ? `; ${ found.strays } footnotes on pages not given, left out` : ''));
      if (!isLocalModel(model)) {
        await setTimeout(REQUEST_INTERVAL_MS);
      }
    }
    catch (e) {
      const error = (e as Error).message ?? String(e);
      failedPages.push(...batch.map((page) => ({ page: page.page, error })));
    }
  }

  return { citations: fillCitationAuthors(citations), failedPages, insights };
};
