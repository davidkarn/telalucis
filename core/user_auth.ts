// Signing in with Google: the OAuth client's settings, the reader a Google
// profile describes, who among them is an admin (ADMIN_GOOGLE_EMAILS), and
// where to send them back to afterwards. Pure functions; api/google.strategy.ts,
// api/auth.controller.ts and api/sessions.ts use them.

export type GoogleAuthConfig = { clientId: string, clientSecret: string, callbackUrl: string };

// Where Google sends the browser back to, through nginx's /api/ (or the dev
// server's proxy), as a path on the site's origin
export const DEFAULT_GOOGLE_CALLBACK = '/api/auth/google/callback';

export const USER_SESSION_COOKIE = 'tl_session';
export const USER_SESSION_MS = 30 * 24 * 60 * 60 * 1000;

// The Google OAuth client from the environment (GOOGLE_CLIENT_ID,
// GOOGLE_CLIENT_SECRET, optionally GOOGLE_CALLBACK_URL); undefined when the id
// or secret is unset, which leaves signing in with Google off
export const googleAuthConfig = (
  env: Record<string, string | undefined>
): GoogleAuthConfig | undefined => {
  const clientId     = env['GOOGLE_CLIENT_ID'] ?? '';
  const clientSecret = env['GOOGLE_CLIENT_SECRET'] ?? '';
  const callbackUrl  = env['GOOGLE_CALLBACK_URL'] ?? '';

  return clientId.length > 0 && clientSecret.length > 0
    ? { clientId, clientSecret, callbackUrl: callbackUrl.length > 0 ? callbackUrl : DEFAULT_GOOGLE_CALLBACK }
    : undefined;
};

// The emails in ADMIN_GOOGLE_EMAILS (separated by commas or spaces), lowercased
export const adminGoogleEmails = (env: Record<string, string | undefined>): string[] => (
  (env['ADMIN_GOOGLE_EMAILS'] ?? '')
    .split(/[\s,]+/)
    .map((e) => e.trim().toLowerCase())
    .filter((e) => e.length > 0)
);

// Whether a signed-in reader is an admin: their Google email is verified and
// listed in ADMIN_GOOGLE_EMAILS
export const isAdminUser = (
  user: { email: string, emailVerified: boolean } | undefined,
  env: Record<string, string | undefined>,
): boolean => (
  user !== undefined && user.emailVerified
    && adminGoogleEmails(env).includes(user.email.trim().toLowerCase())
);

// The parts of a Google profile (passport-google-oauth20's) a user is made from
export type GoogleProfile = {
  id: string,
  displayName?: string,
  emails?: { value: string, verified?: boolean | string }[],
  photos?: { value: string }[],
};

export type GoogleUserFields = {
  googleId: string,
  email: string,
  emailVerified: boolean,
  name: string,
  pictureUrl: string | null,
};

// The user a Google profile describes; null when it has no email (the email
// scope wasn't granted)
export const googleUserFields = (profile: GoogleProfile): GoogleUserFields | null => {
  const email = profile.emails?.[0];

  if (email === undefined || email.value.length === 0) {
    return null;
  }
  else {
    const name = profile.displayName?.trim() ?? '';
    return {
      googleId:      profile.id,
      email:         email.value,
      emailVerified: email.verified === true || email.verified === 'true',
      name:          name.length > 0 ? name : email.value,
      pictureUrl:    profile.photos?.[0]?.value ?? null,
    };
  }
};

// A path on this site to go back to after signing in: a path starting with one
// "/" (not "//host", which the browser would take as another site, nor one with
// a backslash, which some take as "/"); "/" for anything else
export const safeReturnTo = (returnTo: unknown): string => (
  typeof returnTo === 'string' && /^\/(?![/\\])[^\\\s]*$/.test(returnTo) ? returnTo : '/'
);

// The path to go back to, telling the page signing in failed
export const signInFailedPath = (returnTo: string): string => (
  returnTo + (returnTo.includes('?') ? '&' : '?') + 'signIn=failed'
);
