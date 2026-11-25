// Where a book came from (its url): the page open on archive.org when it can be found there, else
// the source itself. An archive.org book's pages are its scans in order, page 1 the first, and
// archive.org numbers them from 0 ("/page/n0"); a journal article's url is its first page's
// ("/page/n48"), from which its other pages follow on.
import { match, P } from 'ts-pattern'

export type SourceLink = { href: string, label: string }

const ARCHIVE_ITEM = /^https?:\/\/(?:www\.)?archive\.org\/details\/([^/?#]+)(?:\/page\/n(\d+))?/;

// The source's site, as the link names it ("ccel.org"); the url itself when it isn't one
const siteOf = (url: string): string => {
  try {
    return new URL(url).hostname.replace(/^www\./, '');
  }
  catch {
    return url;
  }
};

// A link to the book's source at page pageId, or null without a url. firstPageId: the book's
// first page, the one an archive.org url naming a page ("/page/n48") is to.
export const sourceLink = (
  url: string | undefined, pageId: number, firstPageId: number,
): SourceLink | null => {
  const item = url === undefined ? null : ARCHIVE_ITEM.exec(url);
  const leaf = match(item)
    .with(null, () => null)
    .with([P._, P._, P.string], ([, , first]) => Number(first) + pageId - firstPageId)
    .otherwise(() => pageId - 1);

  return match({ url, item, leaf })
    .with({ url: undefined }, () => null)
    .with({ item: P.nonNullable, leaf: P.when((n) => n !== null && n >= 0) }, ({ item, leaf }) => ({
      href:  `https://archive.org/details/${ item[1] }/page/n${ leaf }`,
      label: 'This page on archive.org',
    }))
    .otherwise(({ url }) => ({ href: url!, label: 'Source: ' + siteOf(url!) }));
};
