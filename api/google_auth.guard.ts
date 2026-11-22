// The guards for signing in with Google: GoogleSignInGuard sends the browser
// to Google (remembering where to come back to), GoogleCallbackGuard takes it
// back, signing the user in. Both answer 503 when the client isn't set.
import {
  Injectable, Logger, ServiceUnavailableException, type ExecutionContext,
} from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import type { Request } from 'express';
import { googleAuthConfig, safeReturnTo } from '../core/user_auth';

// The callback's request: where to go back to, signed in or not
export type GoogleCallbackRequest = Request & { signInReturnTo: string };

const logger = new Logger('GoogleAuth');

const requireGoogleConfig = () => {
  if (googleAuthConfig(process.env) === undefined) {
    throw new ServiceUnavailableException(
      'Signing in with Google is off: GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET are not set'
    );
  }
};

// GET /auth/google?returnTo=<path>: redirects to Google's consent screen
@Injectable()
export class GoogleSignInGuard extends AuthGuard('google') {
  override async canActivate(context: ExecutionContext): Promise<boolean> {
    requireGoogleConfig();
    const req = context.switchToHttp().getRequest<Request>();

    req.session.returnTo = safeReturnTo(req.query['returnTo']);
    return (await super.canActivate(context)) as boolean;
  }
}

// GET /auth/google/callback: signs in the user Google sent back; without one
// (they declined, or the exchange failed), lets the request through unsigned
// in, for the controller to send them back saying so
@Injectable()
export class GoogleCallbackGuard extends AuthGuard('google') {
  override async canActivate(context: ExecutionContext): Promise<boolean> {
    requireGoogleConfig();
    const req = context.switchToHttp().getRequest<GoogleCallbackRequest>();

    req.signInReturnTo = safeReturnTo(req.session.returnTo);
    delete req.session.returnTo;
    await super.canActivate(context);

    if (req.user) {
      // passport starts a new session, so a session id from before signing in is no use
      await super.logIn(req);
    }
    return true;
  }

  override handleRequest<U>(err: unknown, user: U | false): U | undefined {
    if (err) {
      logger.warn(`signing in with Google failed: ${ String(err) }`);
    }
    return err || !user ? undefined : user;
  }
}
