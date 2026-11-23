// The search page (/search?q=<term>&page=<n>): the passages of the collection
// matching a search term, by its words and its meaning (GET /search), ten at a
// time, each with its book's title, its authors (and journal issue, for an
// article) and a link to its page in the book. The term is searched when it's
// sent (Enter or the button), not as it's typed: each search embeds it.
import { createElement as __, useEffect, useState } from 'react'
import { Link, useSearchParams } from 'react-router'
import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { Search } from 'lucide-react'
import type { BlockSearchResult, BlockSearchResults } from '../api'
import { searchQuery } from '../queries'
import { openBooksPath } from '../core/open_books'
import { issueLabel, pageOf, snippetParts } from '../core/search'
import { Header } from '../components/header'
import { LoadingSpinner } from '../components/loading_spinner'
import Pager from '../components/pager'
import { useDocumentTitle } from '../lib'
import './search.scss'

const PAGE_LENGTH = 10;

export default function SearchPage() {
  const [searchParams] = useSearchParams();
  const term           = (searchParams.get('q') ?? '').trim();
  const page           = pageOf(searchParams.get('page'));

  const results = useQuery({
    ...searchQuery({q: term, offset: (page - 1) * PAGE_LENGTH, length: PAGE_LENGTH}),
    enabled:         term.length > 0,
    placeholderData: keepPreviousData,
  });

  useDocumentTitle(term.length > 0 ? `Tela Lucis - Search: ${term}` : 'Tela Lucis - Search');

  return (
    __('main', {className: 'search'},
      __(Header, {}),
      __('section', {className: 'page-body' + (results.isFetching && results.data ? ' loading' : '')},
        __('div', {className: 'search-header'},
          __('h1', {}, 'Search'),
          __('p', {className: 'muted'}, 'Search the text of the collection by its words and by its meaning.'),
          __(SearchForm, {term}),
        ),
        term.length > 0 && match(results)
          .with({status: 'error'}, (r) => __('p', {className: 'error'}, "Couldn't search: ", r.error.message))
          .with({data: P.nonNullable}, (r) => __(Results, {term, page, results: r.data}))
          .otherwise(() => __(LoadingSpinner, {}))
      )
    )
  );
}

// The search term's box; sending it starts again from the first page
function SearchForm({term}: {term: string}) {
  const [, setSearchParams] = useSearchParams();
  const [draft, setDraft]   = useState(term);

  useEffect(() => setDraft(term), [term]);

  return (
    __('form', {
      className: 'search-form',
      role:      'search',
      onSubmit:  (e: React.FormEvent) => {
        e.preventDefault();
        const q = draft.trim();
        setSearchParams(q.length > 0 ? {q} : {});
      },
    },
      __('label', {className: 'search-input'},
        __(Search, {size: 18, 'aria-hidden': true}),
        __('input', {
          type:         'search',
          name:         'q',
          placeholder:  'grace and nature, "natural law", the soul as form of the body…',
          'aria-label': 'Search term',
          maxLength:    500,
          value:        draft,
          autoFocus:    term.length === 0,
          onChange:     (e: React.ChangeEvent<HTMLInputElement>) => setDraft(e.target.value),
        })
      ),
      __('button', {type: 'submit', disabled: draft.trim().length === 0}, 'Search')
    )
  );
}

function Results({term, page, results}: {term: string, page: number, results: BlockSearchResults}) {
  const {meta, items} = results;
  const pageCount     = Math.ceil(meta.count / PAGE_LENGTH);
  const first         = (page - 1) * PAGE_LENGTH + 1;
  const pageHref      = (n: number) => '?' + new URLSearchParams({q: term, page: String(n)}).toString();

  return (
    __('div', {className: 'search-results'},
      __('div', {className: 'showing-info'},
        match(meta.count)
          .with(0, () => `Nothing matches “${term}”.`)
          .otherwise(() => (
            items.length === 0
              ? 'No results on this page.'
              : `Showing ${first}–${first + items.length - 1} of ${meta.count} for “${term}”.`
          ))
      ),
      !meta.searchedBy.meaning && __('p', {className: 'muted search-note'},
        'Searched by words only: the search by meaning is unavailable right now.'
      ),
      __('ol', {className: 'search-result-list', start: first},
        items.map((item) => __(ResultItem, {key: item.blockId, item}))
      ),
      __(Pager, {page, pageCount, hrefFor: pageHref})
    )
  );
}

function ResultItem({item}: {item: BlockSearchResult}) {
  const pagePath = openBooksPath([{bookId: item.book.id, pageId: item.page.pageNumber}]);
  const printed  = item.page.printedPageNumber.length > 0 ? item.page.printedPageNumber : String(item.page.pageNumber);

  return (
    __('li', {className: 'search-result'},
      __('div', {className: 'search-result-title'},
        __(Link, {to: pagePath}, item.book.title),
      ),
      __('div', {className: 'search-result-byline'},
        item.authors.length > 0
          ? item.authors.map((a, i) => __('span', {key: a.id},
            i > 0 && ', ',
            __(Link, {to: '/authors/' + encodeURIComponent(a.id)}, a.name)
          ))
          : item.book.author.length > 0 && __('span', {}, item.book.author),
        item.journalIssue !== null && __('span', {className: 'search-result-issue'},
          ' · ',
          __(Link, {
            to: `/journals/${encodeURIComponent(item.journalIssue.journalId)}/issues/${encodeURIComponent(item.journalIssue.id)}`,
          }, issueLabel(item.journalIssue))
        ),
      ),
      __('blockquote', {className: 'search-result-snippet'},
        snippetParts(item.snippet).map((part, i) => (
          part.marked ? __('mark', {key: i}, part.text) : part.text
        ))
      ),
      __('div', {className: 'search-result-meta'},
        __(Link, {to: pagePath}, /^\d+$/.test(printed) ? `Page ${printed}` : printed),
        item.label === 'Footnote' && __('span', {className: 'search-result-tag'}, 'footnote'),
        item.label === 'SectionHeader' && __('span', {className: 'search-result-tag'}, 'heading'),
      ),
    )
  );
}
