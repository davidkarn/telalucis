// Signing in with Google through passport: the OAuth client (GOOGLE_CLIENT_ID,
// GOOGLE_CLIENT_SECRET; see core/user_auth.ts), its state and PKCE verifier
// kept in the session while the browser is at Google, and the user made or
// refreshed from the profile Google gives. Provided only when the client is
// set (see AppModule).
import { Inject, Injectable } from '@nestjs/common';
import { PassportStrategy } from '@nestjs/passport';
import type { Kysely } from 'kysely';
import { Strategy, type Profile } from 'passport-google-oauth20';
import { googleAuthConfig, googleUserFields } from '../core/user_auth';
import { UserActions, UserSelectors } from '../model/users';
import { DB } from './database.module';
import type { Database } from './database';

@Injectable()
export class GoogleStrategy extends PassportStrategy(Strategy, 'google') {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    const config = googleAuthConfig(process.env);

    if (config === undefined) {
      throw new Error('GoogleStrategy needs GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET');
    }
    super({
      clientID:     config.clientId,
      clientSecret: config.clientSecret,
      callbackURL:  config.callbackUrl,
      scope:        ['openid', 'email', 'profile'],
      state:        true,
      pkce:         true,
      // a callback given as a path is on the origin the request came to (through nginx)
      proxy:        true,
    });
  }

  // The signed-in user; false (signing in fails) when Google gave no email
  async validate(_accessToken: string, _refreshToken: string, profile: Profile): Promise<Express.User | false> {
    const fields = googleUserFields(profile);

    if (fields === null) {
      return false;
    }
    else {
      return UserSelectors.sessionUserOf(await UserActions.upsertGoogleUser(this.db, fields));
    }
  }
}
