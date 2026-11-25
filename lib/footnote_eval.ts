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
import {
  makeOpenRouterRequest, parseJsonResponse, type ORMessage, type ORReasoning, type ORResponse,
  type ORResponseFormat,
} from './open_router.ts';

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
  // of the answer's tokens, those spent thinking
  reasoningTokens: number | null,
};

// The request for the citations in a batch's footnotes (`message`, as batchMessage makes it),
// with the insights that apply to them, or none: its messages and response format
export const footnotesRequest = (
  message: string, insights: EvalInsights,
): { messages: ORMessage[], format: ORResponseFormat } => {
  const given = insights === null
    ? null
    : insightsForPage(insights.filter((i) => !i.score || i.score <= 1), insights, strip_tags(message));

  return {
    messages: [
      { role: 'system', content: footnotesPrompt(given, true) },
      { role: 'user', content: message },
    ],
    format: given === null ? withoutInsights(FOOTNOTE_BATCH_FORMAT) : FOOTNOTE_BATCH_FORMAT,
  };
};

const failedAnswer = (error: unknown, ms: number): EvalAnswer => ({
  footnotes:        null,
  error:            error instanceof Error ? error.message : String(error),
  ms,
  promptTokens:     null,
  completionTokens: null,
  reasoningTokens:  null,
});

// The answer in a chat completion response, taking `ms`; failed when it's an error or its
// content isn't the footnotes
export const footnotesAnswer = (response: ORResponse, ms: number): EvalAnswer => {
  try {
    const result = parseJsonResponse<BatchFootnotes>(response);

    return {
      footnotes:        result.footnotes,
      error:            null,
      ms,
      promptTokens:     response.usage?.prompt_tokens ?? null,
      completionTokens: response.usage?.completion_tokens ?? null,
      reasoningTokens:  response.usage?.completion_tokens_details?.reasoning_tokens ?? null,
    };
  }
  catch (e) {
    return failedAnswer(e, ms);
  }
};

// The citations a model reads in a batch's footnotes (see footnotesRequest)
export const askFootnotes = async(
  message: string, model: string, insights: EvalInsights, reasoning?: ORReasoning,
): Promise<EvalAnswer> => {
  const { messages, format } = footnotesRequest(message, insights);
  const started              = Date.now();

  try {
    return footnotesAnswer(await makeOpenRouterRequest(messages, format, model, reasoning), Date.now() - started);
  }
  catch (e) {
    return failedAnswer(e, Date.now() - started);
  }
};

export const askEvalBatch = (
  batch: EvalBatch, model: string, insights: EvalInsights, reasoning?: ORReasoning,
): Promise<EvalAnswer> => (
  askFootnotes(batchMessage(batch.pages), model, insights, reasoning)
);
