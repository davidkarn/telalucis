// Where to go back to after signing in: a path on this site (starting with one
// "/", not "//host" nor with a backslash, which a browser may take as another
// site); "/" for anything else. The API checks it again (core/user_auth.ts).
export const safeReturnTo = (returnTo: string | null): string => (
  returnTo !== null && /^\/(?![/\\])[^\\\s]*$/.test(returnTo) ? returnTo : '/'
);

// The login page, coming back to returnTo once signed in
export const loginPath = (returnTo: string): string => (
  returnTo === '/' ? '/login' : `/login?returnTo=${encodeURIComponent(returnTo)}`
);

// Pages a link to sign in shouldn't come back to: signing in there leads on anyway
export const isAccountPage = (pathname: string): boolean => (
  pathname === '/login' || pathname === '/account'
);
