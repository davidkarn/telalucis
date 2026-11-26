// A book's page HTML (from OCR and imported texts, see page_blocks.html) made
// safe to put in the page: no scripts, event handlers, javascript: links,
// style sheets or forms, whatever the text it was made from held. The markup
// the pages use is kept: paragraphs, headings, sup, mark, tables, lists,
// links, MathML (OCR'd formulas), and inline styles (text-align).
import DOMPurify from 'dompurify'

export const safeHtml = (html: string): string => (
  DOMPurify.sanitize(html, {
    USE_PROFILES: { html: true, mathMl: true },
    FORBID_TAGS:  ['style', 'form', 'input', 'button', 'textarea', 'select'],
  })
);
