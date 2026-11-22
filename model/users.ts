// Readers signed in with Google (users; see core/user_auth.ts and
// migrations/0031_create_users_and_sessions.ts)
import type { Kysely, Selectable } from 'kysely';
import type { Database, UsersTable } from '../api/database.ts';
import type { GoogleUserFields } from '../core/user_auth.ts';

export type User = Selectable<UsersTable>;

const findUserById = (db: Kysely<Database>, id: string) => (
  db.selectFrom('users').selectAll().where('id', '=', id).executeTakeFirst()
);

// A user as the session holds them (req.user; see api/sessions.ts)
export type SessionUser = {
  id: string, email: string, emailVerified: boolean, name: string, pictureUrl: string | null,
};

const sessionUserOf = (user: User): SessionUser => ({
  id:            user.id,
  email:         user.email,
  emailVerified: user.email_verified,
  name:          user.name,
  pictureUrl:    user.picture_url,
});

const findSessionUser = async(db: Kysely<Database>, id: string): Promise<SessionUser | undefined> => {
  const user = await findUserById(db, id);
  return user === undefined ? undefined : sessionUserOf(user);
};

// The user for a Google account signing in: made the first time, its email and
// picture refreshed every time after (not its name, which they may have changed)
const upsertGoogleUser = (db: Kysely<Database>, fields: GoogleUserFields): Promise<User> => (
  db.insertInto('users')
    .values({
      google_id:      fields.googleId,
      email:          fields.email,
      email_verified: fields.emailVerified,
      name:           fields.name,
      picture_url:    fields.pictureUrl,
    })
    .onConflict((oc) => oc.column('google_id').doUpdateSet((eb) => ({
      email:             eb.ref('excluded.email'),
      email_verified:    eb.ref('excluded.email_verified'),
      picture_url:       eb.ref('excluded.picture_url'),
      last_signed_in_at: new Date(),
    })))
    .returningAll()
    .executeTakeFirstOrThrow()
);

// Changes the name a user goes by; their updated row, undefined when there's no such user
const updateUserName = (db: Kysely<Database>, id: string, name: string) => (
  db.updateTable('users').set({ name }).where('id', '=', id).returningAll().executeTakeFirst()
);

export const UserQueries = { findUserById, findSessionUser };

export const UserSelectors = { sessionUserOf };

export const UserActions = { upsertGoogleUser, updateUserName };
