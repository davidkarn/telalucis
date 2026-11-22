// What passport keeps of a signed-in user in their session: their id, read
// back into req.user on each request. A user who's been deleted is signed out.
import { Inject, Injectable } from '@nestjs/common';
import { PassportSerializer } from '@nestjs/passport';
import type { Kysely } from 'kysely';
import { UserQueries } from '../model/users';
import { DB } from './database.module';
import type { Database } from './database';

type Done<T> = (err: unknown, value?: T | false) => void;

@Injectable()
export class SessionSerializer extends PassportSerializer {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  serializeUser(user: Express.User, done: Done<string>): void {
    done(null, user.id);
  }

  deserializeUser(id: string, done: Done<Express.User>): void {
    UserQueries.findSessionUser(this.db, id).then(
      (user) => done(null, user ?? false),
      (e: unknown) => done(e),
    );
  }
}
