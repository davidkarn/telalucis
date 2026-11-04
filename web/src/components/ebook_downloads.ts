// A book's e-book downloads, under its title: its EPUB, a plain link (made when asked for), and its
// Kindle .mobi, made in the background: clicked, it's asked for every few seconds (saying how it's
// going) until it's ready, then downloaded.
import { createElement as __, useEffect, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Download } from 'lucide-react'
import { match } from 'ts-pattern'
import { ebookUrl, requestMobi, type MobiStatus } from '../api'
import './ebook_downloads.scss'

// How often a .mobi being made is asked about
const POLL_MS = 3000;

const done = (status: MobiStatus | undefined) => status?.status === 'ready' || status?.status === 'failed';

export function EbookDownloads({bookId}: {bookId: string}) {
  // asked for since the book was opened (a click starts it; a new click after a failure, again)
  const [asked, setAsked] = useState(0);

  const mobi = useQuery({
    queryKey:        ['ebookMobi', bookId, asked],
    queryFn:         () => requestMobi(bookId),
    enabled:         asked > 0,
    refetchInterval: (query) => (done(query.state.data) ? false : POLL_MS),
    gcTime:          0,
  });

  // downloaded once it's ready
  useEffect(() => {
    if (mobi.data?.status === 'ready') {
      window.location.assign(ebookUrl(bookId, 'mobi'));
    }
  }, [mobi.data?.status, bookId]);

  const preparing = asked > 0 && !done(mobi.data) && !mobi.isError;

  return (
    __('div', {className: 'ebook-downloads'},
      __(Download, {'aria-hidden': true}),
      __('a', {href: ebookUrl(bookId, 'epub'), download: true}, 'EPUB'),
      __('button', {
        type:     'button',
        disabled: preparing,
        title:    'For older Kindles; newer ones and Send to Kindle take the EPUB',
        onClick:  () => setAsked((n) => n + 1),
      }, 'Kindle (.mobi)'),
      asked > 0 && __('span', {className: 'mobi-status', role: 'status'},
        mobi.isError
          ? `Couldn't make the .mobi: ${ mobi.error.message }`
          : match(mobi.data)
            .with(undefined, () => 'Preparing…')
            .with({status: 'queued'}, (s) => `Waiting (${ s.ahead } ahead)…`)
            .with({status: 'converting'}, () => 'Preparing… (a long book takes a minute)')
            .with({status: 'ready'}, () => 'Downloading')
            .with({status: 'failed'}, (s) => `Couldn't make the .mobi: ${ s.error }`)
            .exhaustive()
      )
    )
  );
}
