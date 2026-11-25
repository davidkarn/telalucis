// The citations in an OCR'd book's footnotes, extracted by an LLM several
// pages at a time (see core/footnote_extraction.ts and
// core/footnote_batches.ts): shared by process-ocred-books and
// import-journal-issues. A request at a time (extractFootnoteCitations), or
// many books' at once through OpenRouter's Batch API (see
// lib/footnote_batch_extraction.ts), which asks those missed here. Needs
// OPENROUTER_KEY.
import { setTimeout } from 'node:timers/promises';
import { fillCitationAuthors } from '../core/citation_authors.ts';
import { batchExamples, examplesPart, type ExampleIndex } from '../core/citation_examples.ts';
import {
  answeredBatch, batchFootnotes, batchHtml, FOOTNOTE_BATCH_FORMAT, footnoteBatches, type BatchFootnotes,
} from '../core/footnote_batches.ts';
import { footnotesPrompt, withoutInsights } from '../core/footnote_extraction.ts';
import type { ExtractedCitations } from '../core/ocred_book_import.ts';
import { log } from './lib.ts';
import {
  isLocalModel, makeOpenRouterRequest, parseJsonResponse, type ORMessage, type ORReasoning,
  type ORResponseFormat,
} from './open_router.ts';
import type { Citation, SuryaPage } from '../types.ts';

// OpenRouter allows 20 requests a minute
const REQUEST_INTERVAL_MS = 3200;

// The answer limit asked of a local model: some servers stop answers short without one
// (mlx_lm.server at 512 tokens, a few footnotes' worth), and answers are about 5,000 tokens
export const LOCAL_MAX_TOKENS = 8192;

// measured on eval/footnotes.json against openai/gpt-4o-mini (see eval-footnotes)
export const DEFAULT_FOOTNOTES_MODEL = 'anthropic/claude-haiku-5.5';

// How much the default model thinks: a budget of 1024 tokens (it spends more) did best on the
// eval, better than its default and than not thinking, no slower and no dearer than its default
export const DEFAULT_FOOTNOTES_REASONING: ORReasoning = { max_tokens: 1024 };

// The reasoning a model is asked with when none is given: the default model's, else the
// model's own (a model without reasoning is refused one, see require_parameters)
export const defaultReasoning = (model: string): ORReasoning | undefined => (
  model === DEFAULT_FOOTNOTES_MODEL ? DEFAULT_FOOTNOTES_REASONING : undefined
);

// How the citations are extracted. model: an OpenRouter model, or a local one
// ("local/<name>", see lib/open_router.ts), asked without waiting between
// requests: a model distilled for it is served by scripts/distill_footnotes.sh
// serve, as "local/default_model". examples: citations to show the model those like each batch's
// (see core/citation_examples.ts), none from the book itself. reasoning: how
// much the model thinks (see ORReasoning), defaultReasoning when not given.
export type ExtractionOptions = { model?: string, examples?: ExampleIndex, reasoning?: ORReasoning };

// The request for the citations in a batch of a book's pages: its messages and response format
export const footnoteBatchRequest = (
  bookId: string, batch: SuryaPage[], examples?: ExampleIndex,
): { messages: ORMessage[], format: ORResponseFormat } => {
  const shown = examples
    ? examplesPart(batchExamples(examples, batchFootnotes(batch), { excludeBookId: bookId }))
    : '';

  return {
    messages: [
      { role: 'system', content: footnotesPrompt(null, true) + shown },
      { role: 'user', content: batchHtml(batch) },
    ],
    format: withoutInsights(FOOTNOTE_BATCH_FORMAT),
  };
};

// The book's citations, with the batches given asked a request at a time
// after those answered already (`found`). Pages the model returns nothing for
// are asked again in smaller batches, then alone (see retryBatches). The pages
// of a batch whose request fails are returned in failedPages rather than
// failing the whole book. Authors the model left out are filled in from the
// titles where they can be (see fillCitationAuthors).
export const askFootnoteBatches = async(
  bookId: string, batches: SuryaPage[][], options: ExtractionOptions = {}, found: Citation[] = [],
): Promise<ExtractedCitations> => {
  const model                                          = options.model ?? DEFAULT_FOOTNOTES_MODEL;
  const reasoning                                      = options.reasoning ?? defaultReasoning(model);
  const citations: Citation[]                          = [...found];
  const failedPages: { page: number, error: string }[] = [];

  // the batches still to ask, those of pages the model missed put next
  const queue = [...batches];
  let asked   = 0;

  while (queue.length > 0) {
    const batch = queue.shift()!;
    asked++;

    try {
      const { messages, format } = footnoteBatchRequest(bookId, batch, options.examples);
      const response             = await makeOpenRouterRequest(
        messages, format, model, reasoning, isLocalModel(model) ? LOCAL_MAX_TOKENS : undefined,
      );
      const answer               = answeredBatch(bookId, batch, parseJsonResponse<BatchFootnotes>(response));

      citations.push(...answer.citations);
      queue.unshift(...answer.retry);
      log(response, `${ bookId }: request ${ asked } (${ queue.length } to go): pages ${ batch.map((p) => p.page).join(', ') }`
        + (answer.missed.length > 0 ? `; missed ${ answer.missed.map((p) => p.page).join(', ') }` : '')
        + (answer.strays > 0 ? `; ${ answer.strays } footnotes on pages not given, left out` : ''));
      if (!isLocalModel(model)) {
        await setTimeout(REQUEST_INTERVAL_MS);
      }
    }
    catch (e) {
      const error = (e as Error).message ?? String(e);
      failedPages.push(...batch.map((page) => ({ page: page.page, error })));
    }
  }

  return { citations: fillCitationAuthors(citations), failedPages };
};

// Every citation in the book's footnotes, a request per batch of pages with
// footnotes worth reading (see core/footnote_batches.ts), asked one after
// another (see askFootnoteBatches)
export const extractFootnoteCitations = (
  bookId: string, pages: SuryaPage[], options: ExtractionOptions = {},
): Promise<ExtractedCitations> => (
  askFootnoteBatches(bookId, footnoteBatches(pages), options)
);
