// A book's pageOrder (GET /books/{bookId}): for a short book with volumes, every volume's pages,
// with an entry (isVolume) where each volume starts
import type { PageOrderEntry, VolumeSummary } from '../api'

// The pages alone, without the entries marking where volumes start
export const pagesOnly = (pageOrder: PageOrderEntry[]): PageOrderEntry[] => (
  pageOrder.filter((p) => !p.isVolume)
);

// The volume each listed page is in, by page id: the last volume whose pages (firstPageId to
// lastPageId) take it in, as a page where one volume ends and the next starts goes with the next.
// Empty for a book without volumes.
export const pageVolumes = (
  pageOrder: PageOrderEntry[], volumes: VolumeSummary[]
): Map<number, number> => new Map(pagesOnly(pageOrder).flatMap((page) => {
  const volume = volumes.findLast((v) => v.firstPageId <= page.pageId && page.pageId <= v.lastPageId);
  return volume === undefined ? [] : [[page.pageId, volume.volume] as const];
}));

// A page as a title names it, as the API's pages for search engines do (core/seo.ts): "p. 12",
// "p. xiv"; a page named by its part as it is ("Book II, Chapter 6"); "page 3" without a number
export const pageLabel = (page: Pick<PageOrderEntry, 'pageId' | 'printedPageNumber'>): string => {
  const printed = page.printedPageNumber.trim();

  if (printed.length === 0) {
    return 'page ' + page.pageId;
  }
  else if (/^(\d+|[ivxlcdm]+)$/i.test(printed)) {
    return 'p. ' + printed;
  }
  else {
    return printed;
  }
};

// The title of a book open to a page: "Politics - Aristotle, p. 12"; without the page when it
// isn't open to one, or the page isn't among those listed
export const bookPageTitle = (
  book: { title: string, author: string, pageOrder: PageOrderEntry[] }, pageId: number | undefined,
): string => {
  const heading = book.author.length > 0 ? `${ book.title } - ${ book.author }` : book.title;
  const page    = pageId === undefined
    ? undefined
    : pagesOnly(book.pageOrder).find((p) => p.pageId === pageId);

  if (page === undefined) {
    return heading;
  }
  else {
    return heading + ', ' + pageLabel(page);
  }
};
