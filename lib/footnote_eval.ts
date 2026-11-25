// Asking a model for an eval batch's citations (see core/footnote_eval.ts),
// as lib/footnote_citations.ts asks it when importing a book: with the prompt
// a trained model is given (no insights, see core/footnote_training.ts), or
// with the insights saved that apply to the batch, as an import gives them
// (none learned). Needs OPENROUTER_KEY for an OpenRouter model.
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import {
  batchMessage, FOOTNOTE_BATCH_FORMAT, type BatchFootnotes,
} from '../core/footnote_batches.ts';
import { strip_tags } from '../core/book_pages.ts';
import type { EvalBatch } from '../core/footnote_eval.ts';
import {
  footnotesPrompt, insightsForPage, withoutInsights, type ScoredInsight,
} from '../core/footnote_extraction.ts';
import type { BatchFootnote } from '../core/footnote_training.ts';
import { FootnoteExtractionInsightQueries } from '../model/footnote_extraction_insights.ts';
import { makeOpenRouterRequest, parseJsonResponse } from './open_router.ts';

// The insights given with each batch: none, or the saved ones (see
// insightsForPage)
export type EvalInsights = ScoredInsight[] | null;

export const findEvalInsights = (db: Kysely<Database>): Promise<ScoredInsight[]> => (
  FootnoteExtractionInsightQueries.findInsights(db)
);

export type EvalAnswer = {
  footnotes: BatchFootnote[] | null,
  error: string | null,
  ms: number,
  // the tokens of the request and the answer, when the server says
  promptTokens: number | null,
  completionTokens: number | null,
};

// The citations a model reads in a batch's footnotes (`message`, as batchMessage
// makes it), with the insights that apply to them, or none
export const askFootnotes = async(
  message: string, model: string, insights: EvalInsights,
): Promise<EvalAnswer> => {
  const given   = insights === null
    ? null
    : insightsForPage(insights.filter((i) => !i.score || i.score <= 1), insights, strip_tags(message));
  const format  = given === null ? withoutInsights(FOOTNOTE_BATCH_FORMAT) : FOOTNOTE_BATCH_FORMAT;
  const started = Date.now();

  try {
    const response = await makeOpenRouterRequest([
      { role: 'system', content: footnotesPrompt(given, true) },
      { role: 'user', content: message },
    ], format, model);
    const result   = parseJsonResponse<BatchFootnotes>(response);

    return {
      footnotes:        result.footnotes,
      error:            null,
      ms:               Date.now() - started,
      promptTokens:     response.usage?.prompt_tokens ?? null,
      completionTokens: response.usage?.completion_tokens ?? null,
    };
  }
  catch (e) {
    return {
      footnotes:        null,
      error:            e instanceof Error ? e.message : String(e),
      ms:               Date.now() - started,
      promptTokens:     null,
      completionTokens: null,
    };
  }
};

export const askEvalBatch = (batch: EvalBatch, model: string, insights: EvalInsights): Promise<EvalAnswer> => (
  askFootnotes(batchMessage(batch.pages), model, insights)
);
