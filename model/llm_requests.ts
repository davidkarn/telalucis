// The record of every request the web app makes of an LLM (llm_requests; see
// core/llm_requests.ts and migrations/0036_create_llm_requests.ts).
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { usageColumns, type LlmRequestRecord } from '../core/llm_requests.ts';

const saveLlmRequest = async(db: Kysely<Database>, record: LlmRequestRecord): Promise<string> => {
  const row = await db.insertInto('llm_requests')
    .values({
      source:        record.source,
      user_id:       record.userId,
      context:       JSON.stringify(record.context),
      model:         record.model,
      messages:      JSON.stringify(record.messages),
      parameters:    JSON.stringify(record.parameters),
      response:      record.response,
      status:        record.status,
      error:         record.error,
      ...usageColumns(record.usage),
      generation_id: record.generationId,
      duration_ms:   Math.round(record.durationMs),
    })
    .returning('id')
    .executeTakeFirstOrThrow();

  return row.id;
};

export const LlmRequestActions = { saveLlmRequest };
