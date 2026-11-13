// Scoring passages against a question with a reranking model, which reads the two together (more
// exact than comparing their embeddings, and slower: it's given only the best few dozen found).
// From a server with a /rerank endpoint (llama-server, Jina and Cohere style): llama-server
// running bge-reranker-v2-m3 (multilingual) by default, at RERANK_URL (http://localhost:8082/v1
// when not set):
//   llama-server -hf gpustack/bge-reranker-v2-m3-GGUF:Q8_0 --reranking -ub 2048 -b 8192 -c 8192 --port 8082
// RERANK_MODEL names the model (bge-reranker-v2-m3 when not set).

type RerankResponse = {
  results?: { index: number, relevance_score: number }[],
  error?: { message?: string } | string,
};

const rerankUrl = () => `${ process.env.RERANK_URL ?? 'http://localhost:8082/v1' }/rerank`;

// Each document's score against the query, in the documents' order: the higher, the more it
// answers it (a model's raw scores: below 0 for most that don't)
export const rerank = async(query: string, documents: string[]): Promise<number[]> => {
  if (documents.length === 0) {
    return [];
  }
  else {
    const res  = await fetch(rerankUrl(), {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({
        model: process.env.RERANK_MODEL ?? 'bge-reranker-v2-m3', query, documents, top_n: documents.length,
      }),
    });
    const body = await res.json().catch(() => ({})) as RerankResponse;

    if (!res.ok || !body.results) {
      const message = typeof body.error === 'string' ? body.error : body.error?.message;
      throw new Error(`rerank request failed (${ res.status }): ${ message ?? res.statusText }`);
    }
    else {
      const scores = new Array<number>(documents.length).fill(-Infinity);
      for (const r of body.results) {
        scores[r.index] = r.relevance_score;
      }
      return scores;
    }
  }
};
