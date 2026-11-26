// The record kept of each request the web app makes of an LLM (llm_requests;
// see model/llm_requests.ts): what asked for it, what was sent, what came
// back, and the tokens and cost OpenRouter reports. Pure functions.

// What in the web app asks: chat answers (api/chats.service.ts). Earlier rows
// may say 'page_insights', for the page insights there used to be.
export type LlmSource = 'chat';

// OpenRouter's usage of a request, as it reports it in the response (or a
// stream's last chunk) when asked to (USAGE_ACCOUNTING in lib/open_router.ts).
// completion_tokens includes the reasoning tokens; cost is in US dollars.
export type LlmUsage = {
  prompt_tokens?: number,
  completion_tokens?: number,
  total_tokens?: number,
  cost?: number,
  prompt_tokens_details?: { cached_tokens?: number },
  completion_tokens_details?: { reasoning_tokens?: number },
};

export type LlmRequestStatus = 'ok' | 'error' | 'stopped';

export type LlmRequestRecord = {
  source: LlmSource,
  userId: string | null,
  // what it was about: { conversationId, messageId }, { bookId, pageNumber }
  context: Record<string, unknown>,
  model: string,
  messages: { role: string, content: string }[],
  // the request's other parameters: reasoning, max_tokens, response_format
  parameters: Record<string, unknown>,
  // the answer's text, as far as it got; null when nothing came back
  response: string | null,
  status: LlmRequestStatus,
  error: string | null,
  usage: LlmUsage | null,
  generationId: string | null,
  durationMs: number,
};

const count = (n: unknown): number | null => (
  typeof n === 'number' && Number.isFinite(n) && n >= 0 ? Math.round(n) : null
);

// A request's usage as llm_requests keeps it; nulls for what wasn't reported
export const usageColumns = (usage: LlmUsage | null) => ({
  prompt_tokens:     count(usage?.prompt_tokens),
  completion_tokens: count(usage?.completion_tokens),
  reasoning_tokens:  count(usage?.completion_tokens_details?.reasoning_tokens),
  cached_tokens:     count(usage?.prompt_tokens_details?.cached_tokens),
  cost:              typeof usage?.cost === 'number' && Number.isFinite(usage.cost) && usage.cost >= 0 ? usage.cost : null,
});
