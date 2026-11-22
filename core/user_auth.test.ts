import { describe, expect, it } from '@jest/globals';
import {
  adminGoogleEmails, DEFAULT_GOOGLE_CALLBACK, googleAuthConfig, googleUserFields, isAdminUser,
  MAX_USER_NAME_LENGTH, safeReturnTo, signInFailedPath, userNameEdit,
} from './user_auth.ts';

describe('googleAuthConfig', () => {
  it('needs the client id and secret, defaulting the callback', () => {
    expect(googleAuthConfig({ GOOGLE_CLIENT_ID: 'id', GOOGLE_CLIENT_SECRET: 's' }))
      .toEqual({ clientId: 'id', clientSecret: 's', callbackUrl: DEFAULT_GOOGLE_CALLBACK });
    expect(googleAuthConfig({
      GOOGLE_CLIENT_ID: 'id', GOOGLE_CLIENT_SECRET: 's', GOOGLE_CALLBACK_URL: 'https://x/cb',
    })?.callbackUrl).toBe('https://x/cb');
    expect(googleAuthConfig({ GOOGLE_CLIENT_ID: 'id' })).toBeUndefined();
    expect(googleAuthConfig({ GOOGLE_CLIENT_ID: '', GOOGLE_CLIENT_SECRET: 's' })).toBeUndefined();
  });
});

describe('adminGoogleEmails and isAdminUser', () => {
  const env = { ADMIN_GOOGLE_EMAILS: ' Ann@Example.com, bob@example.com  carl@example.com' };

  it('reads the list, lowercased, split on commas and spaces', () => {
    expect(adminGoogleEmails(env)).toEqual(['ann@example.com', 'bob@example.com', 'carl@example.com']);
    expect(adminGoogleEmails({})).toEqual([]);
  });

  it('takes only a listed, verified email', () => {
    expect(isAdminUser({ email: 'ANN@example.com', emailVerified: true }, env)).toBe(true);
    expect(isAdminUser({ email: 'ann@example.com', emailVerified: false }, env)).toBe(false);
    expect(isAdminUser({ email: 'dan@example.com', emailVerified: true }, env)).toBe(false);
    expect(isAdminUser(undefined, env)).toBe(false);
    expect(isAdminUser(null, env)).toBe(false);
  });
});

describe('googleUserFields', () => {
  it("takes the profile's first email, name and photo", () => {
    expect(googleUserFields({
      id:          '123',
      displayName: 'Ann Lee',
      emails:      [{ value: 'ann@example.com', verified: true }],
      photos:      [{ value: 'https://p/1' }],
    })).toEqual({
      googleId:      '123',
      email:         'ann@example.com',
      emailVerified: true,
      name:          'Ann Lee',
      pictureUrl:    'https://p/1',
    });
  });

  it('names them by their email without a name, and needs an email', () => {
    expect(googleUserFields({ id: '1', emails: [{ value: 'a@b.c', verified: 'true' }] }))
      .toMatchObject({ name: 'a@b.c', emailVerified: true, pictureUrl: null });
    expect(googleUserFields({ id: '1', emails: [{ value: 'a@b.c' }] })?.emailVerified).toBe(false);
    expect(googleUserFields({ id: '1', displayName: 'X' })).toBeNull();
  });
});

describe('safeReturnTo', () => {
  it('keeps a path on this site', () => {
    expect(safeReturnTo('/tl-admin')).toBe('/tl-admin');
    expect(safeReturnTo('/books/1?page=2')).toBe('/books/1?page=2');
    expect(safeReturnTo('/')).toBe('/');
  });

  it('sends anything else home', () => {
    for (const bad of ['//evil.com', '/\\evil.com', 'https://evil.com', 'evil', '', '/a b', undefined, 3]) {
      expect(safeReturnTo(bad)).toBe('/');
    }
  });
});

describe('signInFailedPath', () => {
  it('adds the flag to the query', () => {
    expect(signInFailedPath('/tl-admin')).toBe('/tl-admin?signIn=failed');
    expect(signInFailedPath('/books?x=1')).toBe('/books?x=1&signIn=failed');
  });
});

describe('userNameEdit', () => {
  it('trims and collapses spaces', () => {
    expect(userNameEdit('  Ann   Lee \n')).toEqual({ name: 'Ann Lee' });
  });

  it('refuses an empty, overlong, non-string or control-character name', () => {
    expect(userNameEdit('   ')).toHaveProperty('error');
    expect(userNameEdit('x'.repeat(MAX_USER_NAME_LENGTH + 1))).toHaveProperty('error');
    expect(userNameEdit('x'.repeat(MAX_USER_NAME_LENGTH))).toEqual({ name: 'x'.repeat(MAX_USER_NAME_LENGTH) });
    expect(userNameEdit(3)).toHaveProperty('error');
    expect(userNameEdit(undefined)).toHaveProperty('error');
    expect(userNameEdit('Ann\u0000Lee')).toHaveProperty('error');
  });
});
