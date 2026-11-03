import { StrictMode, createElement as __ } from 'react'
import { createRoot } from 'react-dom/client'
import { BrowserRouter, Route, Routes } from 'react-router'
import { QueryClientProvider } from '@tanstack/react-query'
import { queryClient } from './queries.ts'
import './index.css'
import App from './pages/home.ts'
import BookView from './pages/book_view.ts'
import AdminPage from './pages/admin.ts'
import AuthorsPage from './pages/authors.ts'
import AuthorPage from './pages/author.ts'
import JournalsPage from './pages/journals.ts'
import JournalPage from './pages/journal.ts'
import JournalIssuePage from './pages/journal_issue.ts'

createRoot(document.getElementById('root')!).render(
  __(StrictMode, {},
    __(QueryClientProvider, {client: queryClient},
      __(BrowserRouter, {},
        __(Routes, {},
          __(Route, {path: '/', element: __(App)}),
          __(Route, {path: '/authors', element: __(AuthorsPage)}),
          // one author's books
          __(Route, {path: '/authors/:authorId', element: __(AuthorPage)}),
          __(Route, {path: '/journals', element: __(JournalsPage)}),
          // a journal's issues, and an issue's articles
          __(Route, {path: '/journals/:journalId', element: __(JournalPage)}),
          __(Route, {path: '/journals/:journalId/issues/:issueId', element: __(JournalIssuePage)}),
          // one or more open books: /books/<id>[/pages/<n>]/books/<id>[/pages/<n>]... (see core/open_books.ts)
          __(Route, {path: '/books/*', element: __(BookView)}),
          // the admin panel and its tabs: /tl-admin[/<tab>]
          __(Route, {path: '/tl-admin/*', element: __(AdminPage)}),
        )
      )
    )
  ),
)
