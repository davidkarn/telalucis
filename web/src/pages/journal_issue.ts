import { createElement as __ } from 'react'
import { Link, useParams } from 'react-router'
import { useQuery } from '@tanstack/react-query'
import { match } from 'ts-pattern';
import { journalIssueQuery } from '../queries'
import { Header } from '../components/header';
import { Library } from '../components/library';
import { useDocumentTitle } from '../lib';
import { LoadingSpinner } from '../components/loading_spinner';
import { issueTitle, journalPath } from '../core/journals';
import './home.scss';
import './journals.scss';

// One journal issue's articles (/journals/<id>/issues/<id>), as the library shows books, in the
// order they're printed; searchable (?q=) by title and author
export default function JournalIssuePage() {
  const {journalId = '', issueId = ''} = useParams();
  const result                         = useQuery(journalIssueQuery(journalId, issueId));
  const issue                          = result.data ?? null;

  useDocumentTitle(issue === null ? null : 'Tela Lucis - ' + issue.journalName + ', ' + issueTitle(issue));

  return (
    __('main', {className: 'home journal-issue'},
      __(Header, {}),
      result.isPending && __(LoadingSpinner, {}),
      match(result)
        .with({status: 'error'}, (r) => (
          __('section', {className: 'page-body'},
            __('p', {className: 'error'}, "Couldn't load this issue: ", r.error.message)
          )
        ))
        .otherwise(() => (
          __(Library, {
            heading:     issue?.journalName ?? '',
            subheading:  __('div', {className: 'journal-subheading'},
              __(Link, {to: journalPath(journalId)}, 'All issues'),
              issue && __('span', {}, ' · ' + issueTitle(issue)),
            ),
            journalIssueId: issueId,
            searchLabel:    'Search titles and authors',
          })
        ))
    )
  );
}
