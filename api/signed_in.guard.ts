// Lets through only requests from a reader signed in with Google (passport's
// req.user, see api/sessions.ts): 401 otherwise.
import {
  Injectable, UnauthorizedException, type CanActivate, type ExecutionContext,
} from '@nestjs/common';
import type { Request } from 'express';

@Injectable()
export class SignedInGuard implements CanActivate {
  canActivate(context: ExecutionContext): boolean {
    if (context.switchToHttp().getRequest<Request>().user === undefined) {
      throw new UnauthorizedException('Sign in first');
    }
    else {
      return true;
    }
  }
}
