import { createElement as __ } from 'react'
import { Link, useNavigate, useParams, useSearchParams } from 'react-router'
import { keepPreviousData, useQuery } from '@tanstack/react-query'
import { match } from 'ts-pattern';
import { journalIssuesQuery, journalQuery } from '../queries'
import { assertCond, useDocumentTitle } from '../lib';
import { Header } from '../components/header';
import Pager from '../components/pager';
import { LoadingSpinner } from '../components/loading_spinner'
import { issueNumbering, issuePeriod, journalIssuePath, journalYears } from '../core/journals';
import './home.scss';
import './journals.scss';

const PAGE_LENGTH = 24

// One journal's issues (/journals/<id>), a card each, in date order, paged (?page=), PAGE_LENGTH a
// page. A card opens the issue's articles (JournalIssuePage).
export default function JournalPage() {
  const navigate                        = useNavigate();
  const journalId                       = useParams().journalId ?? '';
  const [searchParams, setSearchParams] = useSearchParams();
  const page                            = Math.max(1, Number(searchParams.get('page')) || 1);

  const journal = useQuery(journalQuery(journalId)).data ?? null;
  const result  = useQuery({
    ...journalIssuesQuery(journalId, {offset: (page - 1) * PAGE_LENGTH, length: PAGE_LENGTH}),
    placeholderData: keepPreviousData,
  });
  const issues  = result.data ?? null;
  const error   = result.error?.message ?? null;

  useDocumentTitle(journal === null ? null : 'Tela Lucis - ' + journal.name);

  const pageCount = issues ? Math.ceil(issues.meta.count / PAGE_LENGTH) : 0;
  const goToPage  = (n: number) => {
    setSearchParams({page: String(n)});
    window.scrollTo({top: 0});
  };

  return (
    __('main', {className: 'home journal'},
      __(Header, {}),
      __('section', {className: 'page-body' + (result.isFetching && issues ? ' loading' : '')},
        __('section', {className: 'home-library'},
          __('div', {className: 'library-header'},
            __('h1', {}, journal?.name ?? ''),
            __('div', {className: 'journal-subheading'},
              __(Link, {to: '/journals'}, 'All journals'),
              journal && __('span', {},
                (journalYears(journal) ? ' · ' + journalYears(journal) : '')
                  + ' · ' + journal.issueCount + (journal.issueCount === 1 ? ' issue' : ' issues')
                  + ' · ' + journal.articleCount + (journal.articleCount === 1 ? ' article' : ' articles')
              ),
            ),
            issues && issues.items.length > 0 && __('div', {className: 'library-toolbar'},
              __('div', {className: 'spacer'}),
              __('div', {className: 'showing-info'},
                'Showing ' + ((page - 1) * PAGE_LENGTH + 1)
                  + '–' + ((page - 1) * PAGE_LENGTH + issues.items.length)
                  + ' of ' + issues.meta.count + ' issues.'
              )
            ),
          ),
          match<boolean, React.ReactElement>(true)
            .with(!!error, () => __('p', {className: 'error'}, "Couldn't load this journal's issues: ", error))
            .with(!issues, () => __(LoadingSpinner, {}))
            .with(issues?.meta.count === 0, () => __('p', {className: 'muted'}, 'No issues.'))
            .otherwise(() => (
              assertCond(issues !== null),
              __('ol', {className: 'journal-cards issue-cards'},
                issues.items.map((issue) => {
                  const path = journalIssuePath(journalId, issue.id);
                  return __('li', {key: issue.id, className: 'journal-card', onClick: () => navigate(path)},
                    __(Link, {className: 'journal-name', to: path}, issuePeriod(issue.startDate, issue.endDate)),
                    __('div', {className: 'journal-detail'}, issueNumbering(issue)),
                    __('div', {className: 'spacer'}),
                    __('hr', {}),
                    __('div', {className: 'journal-stats'},
                      __('div', {className: 'stat'},
                        __('div', {className: 'stat-label'}, 'articles'),
                        __('span', {}, issue.articleCount)
                      ),
                    )
                  );
                })
              )
            )),
          __(Pager, {page, pageCount, onPage: goToPage})
        )
      )
    )
  );
}
