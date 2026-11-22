// The signed-in reader in the header: their picture (or initial), opening a
// menu with their name and email, the admin panel for an admin, and signing
// out; or, signed out, a link to sign in with Google, coming back to this page.
// Nothing when signing in with Google isn't set up.
import { createElement as __, useEffect, useRef, useState } from 'react'
import { Link, useLocation } from 'react-router'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { googleSignInUrl, signOut, type User } from '../api'
import { currentUserQuery } from '../queries'
import './user_menu.scss'

export const UserMenu = () => {
  const current  = useQuery(currentUserQuery());
  const location = useLocation();
  const returnTo = location.pathname + location.search;

  return match(current.data)
    .with({user: P.nonNullable}, (c) => __(SignedIn, {user: c.user, isAdmin: c.isAdmin}))
    .with({googleSignInAvailable: true}, () => (
      __('a', {className: 'user-menu-sign-in', href: googleSignInUrl(returnTo)}, 'Sign in')
    ))
    .otherwise(() => null);
};

const Avatar = ({user}: {user: User}) => (
  user.pictureUrl !== null
    ? __('img', {className: 'user-avatar', src: user.pictureUrl, alt: '', referrerPolicy: 'no-referrer'})
    : __('span', {className: 'user-avatar'}, user.name.slice(0, 1).toUpperCase())
);

// Their picture, opening the menu; closed by a press outside it
const SignedIn = ({user, isAdmin}: {user: User, isAdmin: boolean}) => {
  const queryClient     = useQueryClient();
  const [open, setOpen] = useState(false);
  const menu            = useRef<HTMLDivElement>(null);

  const logout = useMutation({
    mutationFn: signOut,
    onSuccess:  () => {
      setOpen(false);
      return Promise.all([
        queryClient.invalidateQueries({queryKey: ['auth']}),
        queryClient.invalidateQueries({queryKey: ['admin']}),
      ]);
    },
  });

  useEffect(() => {
    if (!open) {
      return undefined;
    }
    else {
      const closeOutside = (e: PointerEvent) => {
        if (menu.current !== null && !menu.current.contains(e.target as Node)) {
          setOpen(false);
        }
      };
      document.addEventListener('pointerdown', closeOutside);
      return () => document.removeEventListener('pointerdown', closeOutside);
    }
  }, [open]);

  return (
    __('div', {className: 'user-menu', ref: menu},
      __('button', {
        type:            'button',
        className:       'user-menu-button',
        title:           user.name,
        'aria-label':    `Signed in as ${user.name}`,
        'aria-expanded': open,
        onClick:         () => setOpen(!open),
      }, __(Avatar, {user})),
      open && __('div', {className: 'user-menu-dropdown'},
        __('div', {className: 'user-menu-name'}, user.name),
        __('div', {className: 'user-menu-email'}, user.email),
        __('hr', {}),
        isAdmin && __(Link, {to: '/tl-admin', onClick: () => setOpen(false)}, 'Admin panel'),
        __('button', {
          type:     'button',
          onClick:  () => logout.mutate(),
          disabled: logout.isPending,
        }, logout.isPending ? 'Signing out' : 'Sign out'),
        logout.isError && __('p', {className: 'error'}, logout.error.message),
      )
    )
  );
};
