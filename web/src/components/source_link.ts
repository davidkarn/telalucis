// A link under a book's author to where the book came from: the page open, on archive.org, when
// it can be found there (see core/source_link.ts), else the source. Nothing without a source.
import { createElement as __ } from 'react'
import { ExternalLink } from 'lucide-react'
import { sourceLink } from '../core/source_link'
import './source_link.scss'

export function SourceLink({url, pageId, firstPageId}: {
  url: string | undefined,
  pageId: number,
  firstPageId: number,
}) {
  const link = sourceLink(url, pageId, firstPageId);

  return link === null
    ? null
    : __('a', {className: 'source-link', href: link.href, target: '_blank', rel: 'noopener noreferrer'},
      __(ExternalLink, {'aria-hidden': true}),
      link.label,
    );
}
