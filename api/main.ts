// Start the API (from src/):  DATABASE_URL=postgres://... [PORT=3000] npx tsx api/main.ts
// In development, `npm run dev` restarts it whenever a file it imports (or .env) changes, and logs
// every query (LOG_QUERIES=1; failed queries are always logged).
// DATABASE_URL and OPENROUTER_KEY (for page insights) can also come from .env, and for signing in
// with Google, GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET, SESSION_SECRET and ADMIN_GOOGLE_EMAILS.
// Chat (/chats, see api/chats.service.ts) uses OPENROUTER_KEY, CHAT_MODEL (anthropic/claude-haiku-5.5),
// CHAT_USER_DAILY_LIMIT (20) and CHAT_APP_DAILY_LIMIT (300), and the embeddings and reranker
// servers (EMBEDDINGS_URL, RERANK_URL; it searches by words alone without the first).
import 'dotenv/config';
import 'reflect-metadata';
import { NestFactory } from '@nestjs/core';
import type { NestExpressApplication } from '@nestjs/platform-express';
import { AppModule } from './app.module';
import { configureSessions } from './sessions';

const bootstrap = async() => {
  const app = await NestFactory.create<NestExpressApplication>(AppModule);
  configureSessions(app);
  app.enableShutdownHooks();
  await app.listen(Number(process.env.PORT ?? 3000));
};

bootstrap();
