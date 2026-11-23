// The link at the top left of the header: signed in, their picture and name,
// going to their account (/account); signed out, "Sign in", going to the login
// page and coming back to this one. Nothing when signing in isn't set up.
import { createElement as __ } from 'react'
import { Link, useLocation } from 'react-router'
import { useQuery } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { currentUserQuery } from '../queries'
import { isAccountPage, loginPath } from '../core/auth'
import './account_link.scss'

export const AccountLink = () => {
  const current  = useQuery(currentUserQuery());
  const location = useLocation();
  const here     = location.pathname + location.search;

  return match(current.data)
    .with({user: P.nonNullable}, ({user}) => (
      __(Link, {className: 'account-link', to: '/account', title: 'Your account'},
        __('span', {className: 'account-link-name'}, user.name),
      )
    ))
    .with({googleSignInAvailable: true}, () => (
      __(Link, {className: 'account-link', to: loginPath(isAccountPage(location.pathname) ? '/' : here)},
        'Sign in'
      )
    ))
    .otherwise(() => null);
};
