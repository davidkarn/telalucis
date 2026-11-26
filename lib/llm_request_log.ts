// Keeping the record of a request the web app made of an LLM (see
// model/llm_requests.ts), without letting a failure to keep it fail what the
// request was for: it's logged instead.
import { Logger } from '@nestjs/common';
import type { Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { LlmRequestRecord } from '../core/llm_requests.ts';
import { LlmRequestActions } from '../model/llm_requests.ts';

const logger = new Logger('LlmRequests');

export const recordLlmRequest = async(db: Kysely<Database>, record: LlmRequestRecord): Promise<void> => {
  try {
    await LlmRequestActions.saveLlmRequest(db, record);
  }
  catch (e) {
    logger.error(`couldn't record a ${ record.source } request to ${ record.model }: ${ e instanceof Error ? e.message : String(e) }`);
  }
};
