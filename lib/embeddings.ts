// Embeddings of passages and questions, from a server with an OpenAI-style /embeddings endpoint:
// llama-server running bge-m3 (multilingual, for the collection's English, Latin, French and
// German) by default, at EMBEDDINGS_URL (http://localhost:8081/v1 when not set):
//   llama-server -hf gpustack/bge-m3-GGUF:Q8_0 --embedding --pooling cls -ub 2048 -b 8192 --port 8081
// EMBEDDINGS_MODEL names the model (bge-m3 when not set); passages embedded with another are
// embedded again (see the index-passages command). Its embeddings must have EMBEDDING_DIMENSIONS
// numbers (core/passages.ts). EMBEDDINGS_KEY: a key, for a server needing one.
import { EMBEDDING_DIMENSIONS } from '../core/passages.ts';

export const embeddingsModel = () => process.env.EMBEDDINGS_MODEL ?? 'bge-m3';

const embeddingsUrl = () => `${ process.env.EMBEDDINGS_URL ?? 'http://localhost:8081/v1' }/embeddings`;

type EmbeddingsResponse = {
  data?: { index: number, embedding: number[] }[],
  error?: { message?: string } | string,
};

// The embeddings of these texts, in their order
export const embed = async(texts: string[]): Promise<number[][]> => {
  const res  = await fetch(embeddingsUrl(), {
    method:  'POST',
    headers: {
      'Content-Type': 'application/json',
      ...(process.env.EMBEDDINGS_KEY ? { Authorization: 'Bearer ' + process.env.EMBEDDINGS_KEY } : {}),
    },
    body: JSON.stringify({ model: embeddingsModel(), input: texts }),
  });
  const body = await res.json().catch(() => ({})) as EmbeddingsResponse;

  if (!res.ok || !body.data) {
    const message = typeof body.error === 'string' ? body.error : body.error?.message;
    throw new Error(`embeddings request failed (${ res.status }): ${ message ?? res.statusText }`);
  }
  else {
    const embeddings = [...body.data].sort((a, b) => a.index - b.index).map((d) => d.embedding);
    const wrong      = embeddings.find((e) => e.length !== EMBEDDING_DIMENSIONS);

    if (embeddings.length !== texts.length) {
      throw new Error(`asked for ${ texts.length } embeddings, got ${ embeddings.length }`);
    }
    else if (wrong) {
      throw new Error(`embeddings have ${ wrong.length } numbers, the passages table ${ EMBEDDING_DIMENSIONS }`);
    }
    else {
      return embeddings;
    }
  }
};
