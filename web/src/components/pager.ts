import { createElement as __ } from 'react'
import { Link } from 'react-router'
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { unique } from '../../../lib/lib';
import "./pager.scss";

const pageNumbers = (page: number, pageCount: number): (number | null)[] => {
  const shown = unique([1, page - 1, page, page + 1, pageCount])
    .filter(n => n >= 1 && n <= pageCount)
    .sort((a, b) => a - b);

  return shown.flatMap((n, i) => {
    const prev = shown[i - 1];
    
    if (prev === undefined || n === prev + 1) {
      return [n];
    }
    else {
      return n === prev + 2
        ? [prev + 1, n]
        : [null, n];
    }
  });
};


// A page number's control: a link to it (hrefFor: crawlable, and opened in a new tab like any
// link), else a button calling onPage; the current page, or one past either end, can't be
// followed
const control = (opts: {
  page: number,
  hrefFor?: (page: number) => string,
  onPage?: (page: number) => void,
  disabled: boolean,
  props: Record<string, unknown>,
  content: React.ReactNode,
}) => {
  const { page, hrefFor, onPage, disabled, props, content } = opts;

  if (hrefFor !== undefined && disabled) {
    const className = ((props.className as string | undefined) ?? '') + ' disabled';
    return __('span', {...props, className, 'aria-disabled': true}, content);
  }
  else if (hrefFor !== undefined) {
    return __(Link, {...props, to: hrefFor(page), onClick: () => window.scrollTo({top: 0})}, content);
  }
  else {
    return __('button', {...props, disabled, onClick: () => onPage?.(page)}, content);
  }
};

// Previous / numbered pages / next: links when hrefFor is given (public pages, so search engines
// can follow them), else buttons calling onPage
export default function Pager({page, pageCount, onPage, hrefFor}: {
  page: number,
  pageCount: number,
  onPage?: (page: number) => void,
  hrefFor?: (page: number) => string,
}) {
  if (pageCount <= 1) {
    return null;
  }
  else {
    return (
      __('nav', {className: 'pager', 'aria-label': 'Pages'},
        control({
          page: page - 1, hrefFor, onPage, disabled: page <= 1,
          props: {key: 'previous', 'aria-label': 'Previous page', rel: 'prev'},
          content: __(ChevronLeft, {size: 18, 'aria-hidden': true}),
        }),
        pageNumbers(page, pageCount).map((n, i) => (
          n === null
            ? __('span', {key: 'gap' + i, className: 'gap'}, '…')
            : control({
              page: n, hrefFor, onPage, disabled: false,
              props: {
                key:            n,
                className:      n === page ? 'current' : '',
                'aria-current': n === page ? 'page' : undefined,
              },
              content: n,
            })
        )),
        control({
          page: page + 1, hrefFor, onPage, disabled: page >= pageCount,
          props: {key: 'next', 'aria-label': 'Next page', rel: 'next'},
          content: __(ChevronRight, {size: 18, 'aria-hidden': true}),
        }),
      )
    )
  };
}
