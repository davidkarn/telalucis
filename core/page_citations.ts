// Citing a book's pages. Pure functions.

// Citations give page numbers as integers, so a page printed with a Roman numeral, or none, can't
// be cited by number: null
export const citablePageNumber = (printedPageNumber: string): number | null => (
  /^\d+$/.test(printedPageNumber) ? Number(printedPageNumber) : null
);
