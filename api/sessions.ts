// Reader sessions: express-session, kept in Postgres (user_sessions, through
// connect-pg-simple), with passport reading the signed-in user from them into
// req.user (see SessionSerializer). The cookie (tl_session) is HttpOnly,
// SameSite=Lax (so it's sent on the redirect back from Google), Secure over
// https, and signed with SESSION_SECRET.
import { Logger } from '@nestjs/common';
import type { NestExpressApplication } from '@nestjs/platform-express';
import connectPgSimple from 'connect-pg-simple';
import session from 'express-session';
import passport from 'passport';
import { randomBytes } from 'node:crypto';
import { USER_SESSION_COOKIE, USER_SESSION_MS } from '../core/user_auth';
import type { SessionUser } from '../model/users';

declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace Express {
    // eslint-disable-next-line @typescript-eslint/no-empty-object-type
    interface User extends SessionUser {}
  }
}

declare module 'express-session' {
  interface SessionData {
    // where to go back to once signed in with Google
    returnTo?: string,
  }
}

const logger = new Logger('Sessions');

// SESSION_SECRET, or else a random one, which ends every session on restart
const sessionSecret = (): string => {
  const secret = process.env.SESSION_SECRET ?? '';

  if (secret.length > 0) {
    return secret;
  }
  else {
    logger.warn('SESSION_SECRET is not set: sessions end whenever the API restarts');
    return randomBytes(32).toString('hex');
  }
};

export const configureSessions = (app: NestExpressApplication) => {
  const PgStore = connectPgSimple(session);
  const store   = new PgStore({
    conString:            process.env.DATABASE_URL,
    tableName:            'user_sessions',
    createTableIfMissing: false,  // migrations/0031_create_users_and_sessions.ts
    pruneSessionInterval: 15 * 60,
  });

  // nginx's X-Forwarded-Proto says whether the request came over https
  app.set('trust proxy', 1);
  app.use(session({
    name:              USER_SESSION_COOKIE,
    store,
    secret:            sessionSecret(),
    resave:            false,
    saveUninitialized: false,
    cookie:            { httpOnly: true, sameSite: 'lax', secure: 'auto', maxAge: USER_SESSION_MS },
  }));
  app.use(passport.initialize());
  app.use(passport.session());
};
