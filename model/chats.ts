// Readers' chats and their messages (conversations, conversation_messages; see
// core/chats.ts and migrations/0035_create_conversations.ts): a reader's
// conversations and one's messages, the messages sent today, and sending one
// within the day's limits.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import {
  chatUsage, dayStart, limitReached, type ChatLimits, type ChatPassage, type ChatUsage,
} from '../core/chats.ts';

export type ConversationRow = { id: string, title: string, created_at: Date, updated_at: Date };

export type MessageRow = {
  id: string,
  role: 'user' | 'assistant',
  content: string,
  passages: ChatPassage[],
  error: string | null,
  created_at: Date,
};

// How many conversations a reader's list shows, the latest first
const CONVERSATIONS_LISTED = 200;

// A reader's conversations, not deleted, the one written in last first
const findConversations = (db: Kysely<Database>, userId: string): Promise<ConversationRow[]> => (
  db.selectFrom('conversations')
    .select(['id', 'title', 'created_at', 'updated_at'])
    .where('user_id', '=', userId)
    .where('deleted_at', 'is', null)
    .orderBy('updated_at', 'desc')
    .limit(CONVERSATIONS_LISTED)
    .execute()
);

// One of a reader's conversations, not deleted; undefined when it isn't theirs
const findConversation = (
  db: Kysely<Database>, userId: string, id: string,
): Promise<ConversationRow | undefined> => (
  db.selectFrom('conversations')
    .select(['id', 'title', 'created_at', 'updated_at'])
    .where('id', '=', id)
    .where('user_id', '=', userId)
    .where('deleted_at', 'is', null)
    .executeTakeFirst()
);

// A conversation's messages, in the order they were written
const findMessages = async(db: Kysely<Database>, conversationId: string): Promise<MessageRow[]> => {
  const rows = await db.selectFrom('conversation_messages')
    .select(['id', 'role', 'content', 'passages', 'error', 'created_at'])
    .where('conversation_id', '=', conversationId)
    .orderBy('id')
    .execute();

  return rows.map((r) => ({ ...r, passages: r.passages as ChatPassage[] }));
};

// The messages sent since `since`: by the reader, and by everyone
const countSent = async(
  db: Kysely<Database>, userId: string, since: Date,
): Promise<{ user: number, app: number }> => {
  const row = await db.selectFrom('conversation_messages as m')
    .innerJoin('conversations as c', 'c.id', 'm.conversation_id')
    .select([
      sql<number>`count(*) filter (where c.user_id = ${ userId })::int`.as('user'),
      sql<number>`count(*)::int`.as('app'),
    ])
    .where('m.role', '=', 'user')
    .where('m.created_at', '>=', since)
    .executeTakeFirstOrThrow();

  return { user: row.user, app: row.app };
};

const findUsage = async(
  db: Kysely<Database>, userId: string, limits: ChatLimits, now: Date,
): Promise<ChatUsage> => (
  chatUsage(await countSent(db, userId, dayStart(now)), limits, now)
);

export type SendRequest = {
  userId: string,
  // the conversation it's in; null to start one, with this title
  conversationId: string | null,
  title: string,
  content: string,
  limits: ChatLimits,
  now: Date,
};

export type Sent =
  | { sent: true, conversation: ConversationRow, messageId: string, usage: ChatUsage }
  | { sent: false, refused: string, usage: ChatUsage };

// Save a reader's message, and the conversation it starts, unless a limit
// has been reached today. The counting and saving are done holding a lock, so
// messages sent at once can't both take the last place.
const sendMessage = (db: Kysely<Database>, request: SendRequest): Promise<Sent> => (
  db.transaction().execute(async(trx) => {
    await sql`select pg_advisory_xact_lock(hashtext('conversation_messages_limits'))`.execute(trx);
    const usage   = await findUsage(trx, request.userId, request.limits, request.now);
    const refused = limitReached(usage);

    if (refused !== null) {
      return { sent: false, refused, usage };
    }
    else {
      const conversation = request.conversationId === null
        ? await trx.insertInto('conversations')
          .values({ user_id: request.userId, title: request.title })
          .returning(['id', 'title', 'created_at', 'updated_at'])
          .executeTakeFirstOrThrow()
        : await trx.updateTable('conversations')
          .set({ updated_at: new Date() })
          .where('id', '=', request.conversationId)
          .where('user_id', '=', request.userId)
          .where('deleted_at', 'is', null)
          .returning(['id', 'title', 'created_at', 'updated_at'])
          .executeTakeFirstOrThrow();
      const message      = await trx.insertInto('conversation_messages')
        .values({ conversation_id: conversation.id, role: 'user', content: request.content })
        .returning('id')
        .executeTakeFirstOrThrow();

      return { sent: true, conversation, messageId: message.id, usage: { ...usage, used: usage.used + 1, appUsed: usage.appUsed + 1 } };
    }
  })
);

// Take back a reader's message that wasn't answered at all (the search or the
// model failed before any answer), so it doesn't count toward the limits; and
// the conversation it started, when it started one
const unsendMessage = (db: Kysely<Database>, messageId: string, startedConversation: string | null) => (
  db.transaction().execute(async(trx) => {
    await trx.deleteFrom('conversation_messages').where('id', '=', messageId).execute();
    if (startedConversation !== null) {
      await trx.deleteFrom('conversations').where('id', '=', startedConversation).execute();
    }
  })
);

export type Answer = {
  conversationId: string,
  content: string,
  passages: ChatPassage[],
  model: string,
  promptTokens: number | null,
  completionTokens: number | null,
  error: string | null,
};

// Save the model's answer to a message; the answer's row
const saveAnswer = (db: Kysely<Database>, answer: Answer): Promise<MessageRow> => (
  db.transaction().execute(async(trx) => {
    const row = await trx.insertInto('conversation_messages')
      .values({
        conversation_id:   answer.conversationId,
        role:              'assistant',
        content:           answer.content,
        passages:          JSON.stringify(answer.passages),
        model:             answer.model,
        prompt_tokens:     answer.promptTokens,
        completion_tokens: answer.completionTokens,
        error:             answer.error,
      })
      .returning(['id', 'role', 'content', 'passages', 'error', 'created_at'])
      .executeTakeFirstOrThrow();
    await trx.updateTable('conversations').set({ updated_at: new Date() })
      .where('id', '=', answer.conversationId).execute();

    return { ...row, passages: row.passages as ChatPassage[] };
  })
);

// Mark one of a reader's conversations deleted; whether there was one to delete
const deleteConversation = async(db: Kysely<Database>, userId: string, id: string): Promise<boolean> => {
  const result = await db.updateTable('conversations')
    .set({ deleted_at: new Date() })
    .where('id', '=', id)
    .where('user_id', '=', userId)
    .where('deleted_at', 'is', null)
    .executeTakeFirst();

  return result.numUpdatedRows > 0n;
};

export const ChatQueries = { findConversations, findConversation, findMessages, findUsage };

export const ChatActions = { sendMessage, unsendMessage, saveAnswer, deleteConversation };
