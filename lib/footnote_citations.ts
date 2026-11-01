// The citations in an OCR'd book's footnotes, extracted by an LLM a page at a
// time (see core/footnote_extraction.ts), with the insights the model learned
// doing it: shared by process-ocred-books and import-journal-issues. Needs
// OPENROUTER_KEY.
import { setTimeout } from 'node:timers/promises';
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import {
  FOOTNOTES_FORMAT, footnoteHtml, footnotesPrompt, footnoteText, insightsForPage,
  learnedInsights, pageCitations, removeDuplicateInsights,
  type PageFootnotes, type ScoredInsight,
} from '../core/footnote_extraction.ts';
import type { ExtractedCitations } from '../core/ocred_book_import.ts';
import { log, mapLimited } from './lib.ts';
import { makeOpenRouterRequest, parseJsonResponse } from './open_router.ts';
import { FootnoteExtractionInsightQueries } from '../model/footnote_extraction_insights.ts';
import type { Citation, SuryaPage } from '../types.ts';

// OpenRouter allows 20 requests a minute
const REQUEST_INTERVAL_MS = 3200;

// Every citation in the book's footnotes, a request per page with Footnote blocks, each page's
// request given the insights learned so far: the widely applying ones (operatingInsights), and
// any with a keyword found in the page's footnotes (see insightsForPage). Pages whose request
// fails are returned in failedPages rather than failing the whole book.
export const extractFootnoteCitations = async(
  db: Kysely<Database>, bookId: string, pages: SuryaPage[]
): Promise<ExtractedCitations> => {
  const footnotePages                                  = pages.filter((p) => p.blocks.some((b) => b.label === 'Footnote'));
  const citations: Citation[]                          = [];
  const failedPages: { page: number, error: string }[] = [];

  let insights: ScoredInsight[] = await FootnoteExtractionInsightQueries.findInsights(db);

  let operatingInsights = insights.filter((i) => !i.score || i.score <= 1);

  await mapLimited(footnotePages, 1, async(page) => {
    try {
      const given    = insightsForPage(operatingInsights, insights, footnoteText(page));
      const response = await makeOpenRouterRequest([
        { role: 'system', content: footnotesPrompt(given) },
        { role: 'user', content: footnoteHtml(page) },
      ], FOOTNOTES_FORMAT, 'openai/gpt-4o-mini');

      const result      = parseJsonResponse<PageFootnotes>(response);
      const learned     = learnedInsights(result);
      insights          = removeDuplicateInsights(insights.concat(learned));
      operatingInsights = removeDuplicateInsights(
        operatingInsights.concat(learned.filter((s) => s.score !== null && s.score <= 2))
      );
      citations.push(...pageCitations(bookId, page.page, result));
      log(response, 'page ' + page.page + ' of ' + pages.length);
      await setTimeout(REQUEST_INTERVAL_MS);
    }
    catch (e) {
      failedPages.push({ page: page.page, error: (e as Error).message ?? String(e) });
    }
  });

  return { citations, failedPages, insights };
};
