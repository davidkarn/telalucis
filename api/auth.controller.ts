// Signing in with Google, and the signed-in reader (see api/sessions.ts and
// core/user_auth.ts). /auth/google starts it, Google sends the browser back to
// /auth/google/callback (at /api/... through nginx), which goes back to where
// it started.
import {
  BadRequestException, Body, Controller, Get, HttpCode, Inject, Patch, Post, Req, Res,
  UnauthorizedException, UseGuards,
} from '@nestjs/common';
import type { Request, Response } from 'express';
import type { Kysely } from 'kysely';
import { googleAuthConfig, isAdminUser, signInFailedPath, userNameEdit } from '../core/user_auth';
import { UserActions, UserSelectors } from '../model/users';
import { DB } from './database.module';
import type { Database } from './database';
import {
  GoogleCallbackGuard, GoogleSignInGuard, type GoogleCallbackRequest,
} from './google_auth.guard';

// The CurrentUser schema in api.yaml
const currentUser = (req: Request) => ({
  user:                  req.user ?? null,
  isAdmin:               isAdminUser(req.user, process.env),
  googleSignInAvailable: googleAuthConfig(process.env) !== undefined,
});

@Controller('auth')
export class AuthController {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {}

  // Redirects to Google (the guard does); returnTo: the path to come back to
  @Get('google')
  @UseGuards(GoogleSignInGuard)
  signIn() {
    return undefined;
  }

  @Get('google/callback')
  @UseGuards(GoogleCallbackGuard)
  callback(@Req() req: GoogleCallbackRequest, @Res() res: Response) {
    res.redirect(req.user ? req.signInReturnTo : signInFailedPath(req.signInReturnTo));
  }

  @Get('me')
  me(@Req() req: Request) {
    return currentUser(req);
  }

  // Changes the signed-in user's name: 401 when signed out, 400 for a name that won't do
  @Patch('me')
  async update(@Req() req: Request, @Body() body: { name?: unknown } | undefined) {
    const edit = userNameEdit(body?.name);

    if (req.user === undefined) {
      throw new UnauthorizedException('Sign in first');
    }
    else if ('error' in edit) {
      throw new BadRequestException(edit.error);
    }
    else {
      const updated = await UserActions.updateUserName(this.db, req.user.id, edit.name);

      if (updated === undefined) {
        throw new UnauthorizedException('Sign in first');
      }
      else {
        req.user = UserSelectors.sessionUserOf(updated);
        return currentUser(req);
      }
    }
  }

  // Signs out, ending the session
  @Post('logout')
  @HttpCode(200)
  async logout(@Req() req: Request) {
    await new Promise<void>((resolve, reject) => req.logout((e) => e ? reject(e) : resolve()));
    return currentUser(req);
  }
}
