// Signing in with Google, and the signed-in reader (see api/sessions.ts and
// core/user_auth.ts). /auth/google starts it, Google sends the browser back to
// /auth/google/callback (at /api/... through nginx), which goes back to where
// it started.
import { Controller, Get, HttpCode, Post, Req, Res, UseGuards } from '@nestjs/common';
import type { Request, Response } from 'express';
import { googleAuthConfig, isAdminUser, signInFailedPath } from '../core/user_auth';
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

  // Signs out, ending the session
  @Post('logout')
  @HttpCode(200)
  async logout(@Req() req: Request) {
    await new Promise<void>((resolve, reject) => req.logout((e) => e ? reject(e) : resolve()));
    return currentUser(req);
  }
}
