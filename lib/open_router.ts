// OpenRouter chat completions (for process-ocred-books and
// consolidate-footnote-insights, see cli/), or a local server's (see
// LOCAL_MODEL_PREFIX). Needs OPENROUTER_KEY for OpenRouter's.
import type { JSONSchema7 } from 'json-schema';

export type ORMessage = {
  role: 'system' | 'user' | 'assistant',
  content: string
};

export type ORResponseFormat = {
  type: 'json_schema',
  json_schema: { name: string, strict: boolean, schema: JSONSchema7 }
};

// The parts of an OpenRouter chat completion response this code reads
export type ORResponse = {
  error?: { message?: string },
  choices?: { message?: { content?: unknown } }[],
  // completion_tokens includes the reasoning tokens
  usage?: {
    prompt_tokens?: number,
    completion_tokens?: number,
    completion_tokens_details?: { reasoning_tokens?: number },
  },
};

// How much a reasoning model thinks (OpenRouter's reasoning parameter): not at all, at most
// max_tokens (1024 at least), or at an effort. For Claude on chat completions an effort is
// a share of the request's max_tokens, not Claude's own effort, so a budget is the surer.
// Left out, the model's default.
export type ORReasoning = { enabled: false } | { max_tokens: number } | { effort: ReasoningEffort };

export const REASONING_EFFORTS = ['minimal', 'low', 'medium', 'high', 'xhigh', 'max'] as const;
type ReasoningEffort = typeof REASONING_EFFORTS[number];

// --reasoning's value: "off", a budget of tokens ("2000"), or an effort ("low")
export const parseReasoning = (value: string): ORReasoning => {
  if (value === 'off') {
    return { enabled: false };
  }
  else if (/^\d+$/.test(value)) {
    return { max_tokens: Math.max(1024, Number(value)) };
  }
  else if ((REASONING_EFFORTS as readonly string[]).includes(value)) {
    return { effort: value as ReasoningEffort };
  }
  else {
    throw new Error(`--reasoning: "off", a number of tokens, or one of ${ REASONING_EFFORTS.join(', ') }`);
  }
};

// A reasoning setting as named in a file name ("off", "2000", "low")
export const reasoningName = (reasoning: ORReasoning): string => (
  'enabled' in reasoning ? 'off' : 'max_tokens' in reasoning ? String(reasoning.max_tokens) : reasoning.effort
);

// A model on a local server with an OpenAI-style API (llama-server, LM
// Studio, Ollama) is named "local/<its name>": asked at LOCAL_LLM_URL
// (http://localhost:8080/v1 when not set), without a key
export const LOCAL_MODEL_PREFIX = 'local/';

export const isLocalModel = (model: string): boolean => model.startsWith(LOCAL_MODEL_PREFIX);

const endpoint = (model: string) => (
  isLocalModel(model)
    ? {
        url:     `${ process.env.LOCAL_LLM_URL ?? 'http://localhost:8080/v1' }/chat/completions`,
        model:   model.slice(LOCAL_MODEL_PREFIX.length),
        headers: {},
      }
    : {
        url:     'https://openrouter.ai/api/v1/chat/completions',
        model,
        headers: {
          Authorization:  'Bearer ' + process.env.OPENROUTER_KEY,
          'HTTP-Referer': 'https://webdever.net',
          'X-Title':      'Webdever',
        },
      }
);

// A chat completion request's body, as sent alone (makeOpenRouterRequest) or in a batch (see
// lib/open_router_batches.ts). maxTokens: the answer's limit, thinking included; the
// provider's when not given.
export const chatRequestBody = (
  msgs: ORMessage[], responseFormat: ORResponseFormat | undefined, model: string,
  reasoning?: ORReasoning, maxTokens?: number,
) => ({
  model:    endpoint(model).model,
  messages: msgs,
  ...(responseFormat ? { response_format: responseFormat } : {}),
  ...(reasoning ? { reasoning } : {}),
  ...(maxTokens === undefined ? {} : { max_tokens: maxTokens }),
  // only OpenRouter's providers that follow the response format
  ...(responseFormat && !isLocalModel(model) ? { provider: { require_parameters: true } } : {}),
});

export const makeOpenRouterRequest = (
  msgs: ORMessage[], responseFormat?: ORResponseFormat, model: string = 'openai/gpt-4o',
  reasoning?: ORReasoning, maxTokens?: number,
) => {
  const to = endpoint(model);

  return fetch(to.url, {
    method:  'POST',
    headers: { ...to.headers, 'Content-Type': 'application/json' },
    body:    JSON.stringify(chatRequestBody(msgs, responseFormat, model, reasoning, maxTokens)),
  })
    .then((result) => result.json() as Promise<ORResponse>);
};

// The message content of a structured-output response, parsed; throws on an error response
export const parseJsonResponse = <T>(response: ORResponse): T => {
  if (response.error) {
    throw new Error(response.error.message ?? JSON.stringify(response.error));
  }
  else {
    const content = response.choices?.[0]?.message?.content;
    if (typeof content !== 'string') {
      throw new Error('no content in response: ' + JSON.stringify(response));
    }
    else {
      return JSON.parse(content);
    }
  }
};

// What a streamed answer came to: its text, and the tokens counted when the
// stream says (in its last chunk)
export type StreamedAnswer = { content: string, promptTokens: number | null, completionTokens: number | null };

// A streamed chunk of a chat completion (server-sent events, "data: {...}")
type StreamChunk = {
  error?: { message?: string },
  choices?: { delta?: { content?: string | null } }[],
  usage?: { prompt_tokens?: number, completion_tokens?: number },
};

// The lines of server-sent events' data, as they come
async function* eventData(body: ReadableStream<Uint8Array>): AsyncGenerator<string> {
  const decoder = new TextDecoder();
  let buffered  = '';

  for await (const bytes of body) {
    buffered   += decoder.decode(bytes, { stream: true });
    const lines = buffered.split('\n');
    buffered    = lines.pop() ?? '';
    for (const line of lines) {
      // comments (": OPENROUTER PROCESSING") and other fields are passed over
      if (line.startsWith('data:')) {
        yield line.slice(5).trim();
      }
    }
  }
}

// Ask for a chat completion streamed, calling onText with each piece of its
// text as it comes; its whole text once it's done. Throws when the request is
// refused, or the stream reports an error (err.partial: the text before it).
export const streamChat = async(
  msgs: ORMessage[], model: string, onText: (text: string) => void,
  options: { reasoning?: ORReasoning, maxTokens?: number, signal?: AbortSignal } = {},
): Promise<StreamedAnswer> => {
  const to       = endpoint(model);
  const response = await fetch(to.url, {
    method:  'POST',
    headers: { ...to.headers, 'Content-Type': 'application/json' },
    body:    JSON.stringify({
      ...chatRequestBody(msgs, undefined, model, options.reasoning, options.maxTokens),
      stream: true,
    }),
    ...(options.signal ? { signal: options.signal } : {}),
  });

  if (!response.ok || response.body === null) {
    const message = await response.json().then((b: ORResponse) => b.error?.message, () => undefined);
    throw new Error(message ?? `the model's server answered ${ response.status } ${ response.statusText }`);
  }
  else {
    const answer: StreamedAnswer = { content: '', promptTokens: null, completionTokens: null };

    for await (const data of eventData(response.body)) {
      if (data === '[DONE]') {
        break;
      }
      else if (data.length > 0) {
        const chunk = JSON.parse(data) as StreamChunk;
        const text  = chunk.choices?.[0]?.delta?.content ?? '';

        if (chunk.error) {
          throw Object.assign(new Error(chunk.error.message ?? 'the model stopped with an error'), { partial: answer.content });
        }
        else if (text.length > 0) {
          answer.content += text;
          onText(text);
        }
        if (chunk.usage) {
          answer.promptTokens     = chunk.usage.prompt_tokens ?? null;
          answer.completionTokens = chunk.usage.completion_tokens ?? null;
        }
      }
    }

    return answer;
  }
};
