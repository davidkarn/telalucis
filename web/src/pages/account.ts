// The signed-in reader's account (/account): their picture and email, the name
// they go by (editable), the admin panel for an admin, and signing out.
// Signed out, sends them to the login page, coming back here.
import { createElement as __, useState } from 'react'
import { Link, Navigate, useNavigate } from 'react-router'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { signOut, updateMyName, type CurrentUser, type User } from '../api'
import { currentUserQuery } from '../queries'
import { loginPath } from '../core/auth'
import { Header } from '../components/header'
import { LoadingSpinner } from '../components/loading_spinner'
import { useDocumentTitle } from '../lib'
import './account.scss'

const MAX_NAME_LENGTH = 100;  // MAX_USER_NAME_LENGTH in the API's core/user_auth.ts

export default function AccountPage() {
  const current = useQuery(currentUserQuery());
  useDocumentTitle('Tela Lucis - Your account');

  return (
    __('main', {className: 'account'},
      __(Header, {}),
      __('section', {className: 'page-body'},
        match(current)
          .with({status: 'pending'}, () => __(LoadingSpinner, {}))
          .with({status: 'error'}, (c) => (
            __('p', {className: 'error'}, "Couldn't reach the API: ", c.error.message)
          ))
          .with({data: {user: P.nonNullable}}, (c) => (
            __(Account, {user: c.data.user, isAdmin: c.data.isAdmin})
          ))
          .with({status: 'success'}, () => __(Navigate, {to: loginPath('/account'), replace: true}))
          .exhaustive()
      )
    )
  );
}

function Account({user, isAdmin}: {user: User, isAdmin: boolean}) {
  const queryClient = useQueryClient();
  const navigate    = useNavigate();

  const logout = useMutation({
    mutationFn: signOut,
    onSuccess:  async(result: CurrentUser) => {
      queryClient.setQueryData(currentUserQuery().queryKey, result);
      await queryClient.invalidateQueries({queryKey: ['admin']});
      navigate('/');
    },
  });

  return (
    __('div', {className: 'account-card'},
      __('div', {className: 'account-who'},
        user.pictureUrl !== null
          ? __('img', {className: 'account-avatar', src: user.pictureUrl, alt: '', referrerPolicy: 'no-referrer'})
          : __('span', {className: 'account-avatar'}, user.name.slice(0, 1).toUpperCase()),
        __('div', {},
          __('h1', {}, user.name),
          __('div', {className: 'muted'}, user.email),
        )
      ),
      __(NameForm, {key: user.name, name: user.name}),
      __('hr', {}),
      __('div', {className: 'account-actions'},
        isAdmin && __(Link, {to: '/tl-admin'}, 'Admin panel'),
        __('button', {type: 'button', onClick: () => logout.mutate(), disabled: logout.isPending},
          logout.isPending ? 'Signing out' : 'Sign out'
        ),
      ),
      logout.isError && __('p', {className: 'error'}, logout.error.message),
    )
  );
}

// Editing the name they go by; remade (key) when it changes, starting from the saved one
function NameForm({name}: {name: string}) {
  const queryClient       = useQueryClient();
  const [edit, setEdit]   = useState(name);
  const [saved, setSaved] = useState(false);
  const changed           = edit.trim().replace(/\s+/g, ' ') !== name;

  const save = useMutation({
    mutationFn: () => updateMyName(edit),
    onSuccess:  (result: CurrentUser) => {
      queryClient.setQueryData(currentUserQuery().queryKey, result);
      setSaved(true);
    },
  });

  return (
    __('form', {
      className: 'account-name',
      onSubmit:  (e: React.FormEvent) => {
        e.preventDefault();
        save.mutate();
      },
    },
      __('label', {},
        'Name',
        __('input', {
          name:         'name',
          autoComplete: 'name',
          required:     true,
          maxLength:    MAX_NAME_LENGTH,
          value:        edit,
          onChange:     (e: React.ChangeEvent<HTMLInputElement>) => {
            setEdit(e.target.value);
            setSaved(false);
          },
        })
      ),
      __('div', {className: 'account-name-actions'},
        __('button', {type: 'submit', disabled: !changed || edit.trim().length === 0 || save.isPending},
          save.isPending ? 'Saving' : 'Save'
        ),
        saved && !changed && __('span', {className: 'muted'}, 'Saved'),
      ),
      save.isError && __('p', {className: 'error'}, save.error.message),
    )
  );
}
