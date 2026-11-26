// A signed-in reader's chats with an LLM about the collection (see
// api/chats.service.ts): their conversations, one with its messages, deleting
// one, and sending a message, whose answer is streamed back as server-sent
// events. Signed out: 401.
import {
  BadRequestException, Body, Controller, Delete, Get, HttpCode, HttpException, Inject,
  NotFoundException, Param, Post, Req, Res, UnauthorizedException,
} from '@nestjs/common';
import type { Request, Response } from 'express';
import { messageContent } from '../core/chats.ts';
import type { SessionUser } from '../model/users.ts';
import { ChatRefused, ChatsService, type ChatEvent } from './chats.service.ts';

// The signed-in reader's id; 401 when signed out
const userIdOf = (req: Request): string => {
  // passport's req.user, as api/sessions.ts declares it
  const user = req.user as SessionUser | null | undefined;

  if (user === undefined || user === null) {
    throw new UnauthorizedException('Sign in to chat');
  }
  else {
    return user.id;
  }
};

// A conversation's id as given: a whole number; 404 for anything else
const conversationIdOf = (raw: unknown): string => {
  if (typeof raw === 'string' && /^\d{1,18}$/.test(raw)) {
    return raw;
  }
  else {
    throw new NotFoundException('No such conversation');
  }
};

@Controller('chats')
export class ChatsController {
  constructor(@Inject(ChatsService) private readonly chats: ChatsService) {}

  @Get()
  list(@Req() req: Request) {
    return this.chats.conversations(userIdOf(req));
  }

  @Get(':conversationId')
  async conversation(@Req() req: Request, @Param('conversationId') id: string) {
    const found = await this.chats.conversation(userIdOf(req), conversationIdOf(id));

    if (found === undefined) {
      throw new NotFoundException('No such conversation');
    }
    else {
      return found;
    }
  }

  @Delete(':conversationId')
  @HttpCode(204)
  async delete(@Req() req: Request, @Param('conversationId') id: string) {
    if (!await this.chats.deleteConversation(userIdOf(req), conversationIdOf(id))) {
      throw new NotFoundException('No such conversation');
    }
  }

  // Send a message, in a conversation or starting one (conversationId null or
  // left out). Refusals come before the stream starts, as JSON: 400 for a
  // message that won't do, 404 for a conversation not theirs, 429 at a limit
  // (with the usage), 503 when the model isn't set up. Then the answer's events,
  // one a "data:" line each; the model is stopped when the reader goes away.
  @Post('messages')
  async send(
    @Req() req: Request,
    @Res() res: Response,
    @Body() body: { conversationId?: unknown, content?: unknown } | undefined,
  ) {
    const userId         = userIdOf(req);
    const message        = messageContent(body?.content);
    const conversationId = body?.conversationId === undefined || body.conversationId === null
      ? null
      : conversationIdOf(body.conversationId);

    if ('error' in message) {
      throw new BadRequestException(message.error);
    }

    const earlier = await this.chats.earlierMessages(userId, conversationId);
    if (earlier === null) {
      throw new NotFoundException('No such conversation');
    }

    let sent;
    try {
      sent = await this.chats.send(userId, conversationId, message.content);
    }
    catch (e) {
      if (e instanceof ChatRefused) {
        throw new HttpException({ statusCode: e.status, message: e.message, usage: e.usage ?? null }, e.status);
      }
      else {
        throw e;
      }
    }

    res.status(200);
    res.setHeader('Content-Type', 'text/event-stream; charset=utf-8');
    res.setHeader('Cache-Control', 'no-cache, no-transform');
    // nginx passes each event on as it comes rather than holding the answer back
    res.setHeader('X-Accel-Buffering', 'no');
    res.flushHeaders();

    const stop = new AbortController();
    res.on('close', () => stop.abort());
    const emit = (event: ChatEvent) => {
      if (!res.writableEnded) {
        res.write(`data: ${ JSON.stringify(event) }\n\n`);
      }
    };

    try {
      await this.chats.answer(userId, sent, earlier, message.content, conversationId === null, emit, stop.signal);
    }
    catch (e) {
      emit({ type: 'error', message: `The answer failed: ${ e instanceof Error ? e.message : String(e) }` });
    }
    res.end();
  }
}
