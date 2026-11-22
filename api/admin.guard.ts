import {
  Injectable, ServiceUnavailableException, UnauthorizedException, type CanActivate,
  type ExecutionContext,
} from '@nestjs/common';
import {
  adminCredentials, adminSignInMethods, cookieValue, isValidSession, SESSION_COOKIE,
} from '../core/admin_auth';
import { isAdminUser } from '../core/user_auth';

// The parts of a request the admin endpoints read
export type AdminRequest = {
  headers: { cookie?: string },
  secure: boolean,
  user?: Express.User,
  logout?: (done: (err: unknown) => void) => void,
};

export const ADMIN_CLOSED_MESSAGE = 'The admin panel is closed: set ADMIN_USERNAME and ADMIN_PASSWORD, '
  + 'or GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET and ADMIN_GOOGLE_EMAILS';

// Whether a request carries a valid admin session cookie (see core/admin_auth.ts), or comes
// from a reader signed in with Google whose email is in ADMIN_GOOGLE_EMAILS. Throws 503 when
// neither way of signing in is set up.
export const isAdminRequest = (req: AdminRequest): boolean => {
  const methods = adminSignInMethods(process.env);
  const admin   = adminCredentials(process.env);

  if (!methods.password && !methods.google) {
    throw new ServiceUnavailableException(ADMIN_CLOSED_MESSAGE);
  }
  else {
    const byPassword = admin !== undefined
      && isValidSession(cookieValue(req.headers.cookie, SESSION_COOKIE), admin, Date.now());
    return byPassword || (methods.google && isAdminUser(req.user, process.env));
  }
};

// Lets through only requests from a signed-in admin: 401 otherwise
@Injectable()
export class AdminGuard implements CanActivate {
  canActivate(context: ExecutionContext): boolean {
    if (!isAdminRequest(context.switchToHttp().getRequest<AdminRequest>())) {
      throw new UnauthorizedException('Sign in to the admin panel first');
    }
    else {
      return true;
    }
  }
}
