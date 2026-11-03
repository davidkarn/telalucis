import { createElement as __ } from 'react'
import { Link, useNavigate } from 'react-router'
import { useQuery } from '@tanstack/react-query'
import { match } from 'ts-pattern';
import { journalsQuery } from '../queries'
import { assertCond, useDocumentTitle } from '../lib';
import { Header } from '../components/header';
import { LoadingSpinner } from '../components/loading_spinner'
import { journalPath, journalYears } from '../core/journals';
import './home.scss';
import './journals.scss';

// The journals in the collection, a card each, by name, with the years they span and how many
// issues and articles each has. A card opens the journal's issues (JournalPage).
export default function JournalsPage() {
  const navigate = useNavigate();
  const result   = useQuery(journalsQuery());
  const journals = result.data ?? null;
  const error    = result.error?.message ?? null;

  useDocumentTitle('Tela Lucis - Journals');

  return (
    __('main', {className: 'home journals'},
      __(Header, {}),
      __('section', {className: 'page-body'},
        __('section', {className: 'home-library'},
          __('div', {className: 'library-header'},
            __('h1', {}, 'Journals'),
          ),
          match<boolean, React.ReactElement>(true)
            .with(!!error, () => __('p', {className: 'error'}, "Couldn't load journals: ", error))
            .with(!journals, () => __(LoadingSpinner, {}))
            .with(journals?.items.length === 0, () => __('p', {className: 'muted'}, 'No journals.'))
            .otherwise(() => (
              assertCond(journals !== null),
              __('ol', {className: 'journal-cards'},
                journals.items.map((journal) => (
                  __('li', {
                    key:       journal.id,
                    className: 'journal-card',
                    onClick:   () => navigate(journalPath(journal.id)),
                  },
                    __(Link, {className: 'journal-name', to: journalPath(journal.id)}, journal.name),
                    __('div', {className: 'journal-detail'}, journalYears(journal)),
                    __('div', {className: 'spacer'}),
                    __('hr', {}),
                    __('div', {className: 'journal-stats'},
                      __('div', {className: 'stat'},
                        __('div', {className: 'stat-label'}, 'issues'),
                        __('span', {}, journal.issueCount)
                      ),
                      __('div', {className: 'stat'},
                        __('div', {className: 'stat-label'}, 'articles'),
                        __('span', {}, journal.articleCount)
                      ),
                    )
                  )
                ))
              )
            ))
        )
      )
    )
  );
}
