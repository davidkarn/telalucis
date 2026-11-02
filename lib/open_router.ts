// OpenRouter chat completions (for process-ocred-books and
// consolidate-footnote-insights, see cli/), or a local server's (see
// LOCAL_MODEL_PREFIX). Needs OPENROUTER_KEY for OpenRouter's.
import type { JSONSchema7 } from 'json-schema';

export type ORMessage = {
  role: 'system' | 'user',
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
};

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

export const makeOpenRouterRequest = (
  msgs: ORMessage[], responseFormat?: ORResponseFormat, model: string = 'openai/gpt-4o'
) => {
  const to = endpoint(model);

  return fetch(to.url, {
    method:  'POST',
    headers: { ...to.headers, 'Content-Type': 'application/json' },
    body:    JSON.stringify({
      model:    to.model,
      messages: msgs,
      ...(responseFormat ? { response_format: responseFormat } : {}),
      // only OpenRouter's providers that follow the response format
      ...(responseFormat && !isLocalModel(model) ? { provider: { require_parameters: true } } : {}),
    }),
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
