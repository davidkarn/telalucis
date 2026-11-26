// Readers' chats with an LLM about the collection (see core/chats.ts): their
// conversations, and answering a message. An answer is streamed as it's
// written, as events (ChatEvent): the conversation the message is in, the
// passages found for it, the answer's text a piece at a time, and the answer
// as saved. A message whose search fails, or that the model refuses outright,
// is taken back, so it doesn't count toward the day's limits; once the model
// has started answering it counts, and an answer cut short is saved with why.
// Every request of the model is recorded in llm_requests, with what it cost.
import { Inject, Injectable } from '@nestjs/common';
import type { Kysely } from 'kysely';
import {
  CHAT_MAX_TOKENS, chatLimits, chatRequestMessages, conversationTitle, DEFAULT_CHAT_MODEL,
  numberedPassages, searchQuery, type ChatPassage, type ChatTurn, type ChatUsage,
} from '../core/chats.ts';
import type { LlmUsage } from '../core/llm_requests.ts';
import { recordLlmRequest } from '../lib/llm_request_log.ts';
import { isLocalModel, streamChat, USAGE_ACCOUNTING, type ORReasoning } from '../lib/open_router.ts';
import { DEFAULT_SEARCH, searchPassages } from '../lib/passage_search.ts';
import { ChatActions, ChatQueries, type ConversationRow, type MessageRow } from '../model/chats.ts';
import { DB } from './database.module.ts';
import type { Database } from './database.ts';

// The ConversationSummary and ChatMessage schemas in api.yaml
export type ConversationSummary = { id: string, title: string, createdAt: string, updatedAt: string };

export type ChatMessage = {
  id: string,
  role: 'user' | 'assistant',
  content: string,
  passages: ChatPassage[],
  error: string | null,
  createdAt: string,
};

// The events of an answer (the ChatEvent schema in api.yaml)
export type ChatEvent =
  | { type: 'conversation', conversation: ConversationSummary, message: ChatMessage, usage: ChatUsage }
  | { type: 'passages', passages: ChatPassage[], searchedByMeaning: boolean }
  | { type: 'text', text: string }
  | { type: 'done', message: ChatMessage }
  | { type: 'error', message: string };

const summaryOf = (row: ConversationRow): ConversationSummary => ({
  id:        row.id,
  title:     row.title,
  createdAt: row.created_at.toISOString(),
  updatedAt: row.updated_at.toISOString(),
});

const messageOf = (row: MessageRow): ChatMessage => ({
  id:        row.id,
  role:      row.role,
  content:   row.content,
  passages:  row.passages,
  error:     row.error,
  createdAt: row.created_at.toISOString(),
});

const messageText = (e: unknown) => (e instanceof Error ? e.message : String(e));

// Why a message couldn't be sent: a limit reached (429), or the model not set up (503)
export class ChatRefused extends Error {
  constructor(message: string, readonly status: 429 | 503, readonly usage?: ChatUsage) {
    super(message);
  }
}

@Injectable()
export class ChatsService {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {}

  private model() {
    return process.env.CHAT_MODEL ?? DEFAULT_CHAT_MODEL;
  }

  usage(userId: string): Promise<ChatUsage> {
    return ChatQueries.findUsage(this.db, userId, chatLimits(process.env), new Date());
  }

  async conversations(userId: string) {
    const [rows, usage] = await Promise.all([ChatQueries.findConversations(this.db, userId), this.usage(userId)]);
    return { items: rows.map(summaryOf), usage };
  }

  // One of the reader's conversations and its messages; undefined when it isn't theirs
  async conversation(userId: string, id: string) {
    const row = await ChatQueries.findConversation(this.db, userId, id);

    if (row === undefined) {
      return undefined;
    }
    else {
      return { conversation: summaryOf(row), messages: (await ChatQueries.findMessages(this.db, id)).map(messageOf) };
    }
  }

  deleteConversation(userId: string, id: string): Promise<boolean> {
    return ChatActions.deleteConversation(this.db, userId, id);
  }

  // The messages before this one in the conversation; null when there's no
  // such conversation of the reader's
  async earlierMessages(userId: string, conversationId: string | null): Promise<MessageRow[] | null> {
    if (conversationId === null) {
      return [];
    }
    else if (await ChatQueries.findConversation(this.db, userId, conversationId) === undefined) {
      return null;
    }
    else {
      return ChatQueries.findMessages(this.db, conversationId);
    }
  }

  // Save the reader's message, within the day's limits (else ChatRefused);
  // what's needed to answer it
  async send(userId: string, conversationId: string | null, content: string) {
    const model = this.model();

    if (!isLocalModel(model) && (process.env.OPENROUTER_KEY ?? '').length === 0) {
      throw new ChatRefused('Chat is unavailable: no OPENROUTER_KEY is set', 503);
    }

    const sent = await ChatActions.sendMessage(this.db, {
      userId,
      conversationId,
      title:  conversationTitle(content),
      content,
      limits: chatLimits(process.env),
      now:    new Date(),
    });

    if (!sent.sent) {
      throw new ChatRefused(sent.refused, 429, sent.usage);
    }
    else {
      return sent;
    }
  }

  // Answer a reader's message just sent (see send), emitting its events;
  // `started`: whether it started the conversation, which is taken back with it
  async answer(
    userId: string,
    sent: { conversation: ConversationRow, messageId: string, usage: ChatUsage },
    earlier: MessageRow[], content: string, started: boolean,
    emit: (event: ChatEvent) => void, signal: AbortSignal,
  ): Promise<void> {
    const model  = this.model();
    const turns  = earlier.map((m): ChatTurn => ({ role: m.role, content: m.content }));
    const given  = earlier.flatMap((m) => m.passages);
    const unsend = () => ChatActions.unsendMessage(this.db, sent.messageId, started ? sent.conversation.id : null);

    emit({
      type:         'conversation',
      conversation: summaryOf(sent.conversation),
      message:      {
        id: sent.messageId, role: 'user', content, passages: [], error: null, createdAt: new Date().toISOString(),
      },
      usage:        sent.usage,
    });

    let passages: ChatPassage[];
    try {
      const report = await searchPassages(this.db, searchQuery(content, turns), { ...DEFAULT_SEARCH, meaningOptional: true });
      passages     = numberedPassages(report.results, given);
      emit({ type: 'passages', passages, searchedByMeaning: report.meaningUnavailable === undefined });
    }
    catch (e) {
      await unsend();
      emit({ type: 'error', message: `Couldn't search the collection: ${ messageText(e) }` });
      return;
    }

    const messages                  = chatRequestMessages(turns, passages, content);
    const parameters                = { reasoning: { enabled: false } as ORReasoning, max_tokens: CHAT_MAX_TOKENS };
    const startedAt                 = Date.now();
    let written                     = '';
    let usage: LlmUsage | null      = null;
    let generationId: string | null = null;
    let error: string | null        = null;
    try {
      await streamChat(messages, model, (text) => {
        written += text;
        emit({ type: 'text', text });
      }, {
        reasoning: parameters.reasoning,
        maxTokens: CHAT_MAX_TOKENS,
        signal,
        extra:     USAGE_ACCOUNTING,
        onChunk:   (about) => {
          usage        = about.usage ?? usage;
          generationId = about.generationId ?? generationId;
        },
      });
    }
    catch (e) {
      error = signal.aborted ? 'Stopped before the answer was finished' : messageText(e);
    }

    const record = (answerId: string | null) => recordLlmRequest(this.db, {
      source:     'chat',
      userId,
      context:    { conversationId: sent.conversation.id, messageId: sent.messageId, answerId },
      model,
      messages,
      parameters,
      response:   written.length > 0 ? written : null,
      status:     signal.aborted ? 'stopped' : error === null ? 'ok' : 'error',
      error,
      usage,
      generationId,
      durationMs: Date.now() - startedAt,
    });

    if (error !== null && written.length === 0 && !signal.aborted) {
      // refused before answering: nothing to keep, nor to count
      await record(null);
      await unsend();
      emit({ type: 'error', message: `The model couldn't answer: ${ error }` });
    }
    else {
      const tokens = usage as LlmUsage | null;
      const saved  = await ChatActions.saveAnswer(this.db, {
        conversationId:   sent.conversation.id,
        content:          written,
        passages,
        model,
        error,
        promptTokens:     tokens?.prompt_tokens ?? null,
        completionTokens: tokens?.completion_tokens ?? null,
      });
      await record(saved.id);
      emit({ type: 'done', message: messageOf(saved) });
    }
  }
}
