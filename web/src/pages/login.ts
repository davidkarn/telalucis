// The login page (/login?returnTo=<path>): signing in with Google, coming back
// here (so a failure can be shown: ?signIn=failed), then going on to returnTo.
// Already signed in, goes straight on.
import { createElement as __ } from 'react'
import { Navigate, useSearchParams } from 'react-router'
import { useQuery } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { googleSignInUrl } from '../api'
import { currentUserQuery } from '../queries'
import { loginPath, safeReturnTo } from '../core/auth'
import { Header } from '../components/header'
import { GoogleLogo } from '../components/google_logo'
import { LoadingSpinner } from '../components/loading_spinner'
import { useDocumentTitle } from '../lib'
import './login.scss'

export default function LoginPage() {
  const current        = useQuery(currentUserQuery());
  const [searchParams] = useSearchParams();
  const returnTo       = safeReturnTo(searchParams.get('returnTo'));
  const failed         = searchParams.get('signIn') === 'failed';
  useDocumentTitle('Tela Lucis - Sign in');

  return (
    __('main', {className: 'login'},
      __(Header, {}),
      __('section', {className: 'page-body'},
        match(current)
          .with({status: 'pending'}, () => __(LoadingSpinner, {}))
          .with({status: 'error'}, (c) => (
            __('p', {className: 'error'}, "Couldn't reach the API: ", c.error.message)
          ))
          .with({data: {user: P.nonNullable}}, () => __(Navigate, {to: returnTo, replace: true}))
          .with({data: {googleSignInAvailable: false}}, () => (
            __('p', {className: 'muted'}, "Signing in isn't available on this site yet.")
          ))
          .with({status: 'success'}, () => (
            __('div', {className: 'login-card'},
              __('h1', {}, 'Sign in'),
              __('p', {className: 'muted'}, 'Sign in to Tela Lucis with your Google account.'),
              failed && __('p', {className: 'error'}, "Couldn't sign in with Google. Try again?"),
              __('a', {className: 'google-sign-in', href: googleSignInUrl(loginPath(returnTo))},
                __(GoogleLogo, {}),
                'Sign in with Google'
              )
            )
          ))
          .exhaustive()
      )
    )
  );
}
