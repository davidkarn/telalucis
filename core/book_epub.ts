// A book's text (core/book_text.ts) as the files of an EPUB 3 e-book, for reading on an e-reader
// and converting to a Kindle's MOBI (see lib/ebook.ts): a title page, a page of contents, an XHTML
// file per section with its title as a heading (nested by its level), and its footnotes as
// pop-up notes (epub:type noteref and footnote) after its text, each linking back to where it's
// referred to. The navigation document (EPUB 3) and the NCX (EPUB 2, which Kindles and converters
// read) hold the table of contents. Pure functions; lib/ebook.ts zips the files.
import type { BookText, ContentsNode, Footnote, Paragraph, Section } from './book_text.ts';

export type EpubFile = { path: string, content: string };

export type EpubOptions = {
  // the book's language, BCP 47 ("en", "la"); en when not given
  language?: string,
  // when the e-book was made, for its dcterms:modified
  modified: Date,
};

const escapeXml = (s: string) => s
  .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');

const sectionFile = (s: Section) => `section-${ String(s.id).padStart(4, '0') }.xhtml`;

// What a section is called in the contents: its title, with the heading it starts with ("Chapter 2:
// What must be learned at the outset."); "Beginning" for the text before the first heading
const sectionLabel = (s: { title: string, heading?: string }) => {
  const title = s.title.length > 0 ? s.title : 'Beginning';
  return s.heading === undefined || s.heading === s.title ? title : `${ title }: ${ s.heading }`;
};

const xhtml = (title: string, language: string, body: string) => `<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" xml:lang="${ language }" lang="${ language }">
<head>
<meta charset="utf-8"/>
<title>${ escapeXml(title) }</title>
<link rel="stylesheet" type="text/css" href="style.css"/>
</head>
<body>
${ body }
</body>
</html>
`;

const STYLE = `body { font-family: serif; line-height: 1.4; }
h1, h2, h3, h4, h5, h6 { text-align: center; page-break-after: avoid; }
p { margin: 0; text-indent: 1.5em; text-align: justify; }
p.heading { text-indent: 0; text-align: center; font-weight: bold; margin: 1em 0 0.5em; }
.title-page { text-align: center; margin-top: 30%; }
.title-page .author { font-size: 1.2em; margin-top: 2em; }
a.noteref { font-size: 0.75em; vertical-align: super; line-height: 0; text-decoration: none; }
section.footnotes { margin-top: 2em; border-top: 1px solid #999; font-size: 0.85em; }
aside.footnote p { text-indent: 0; margin: 0.4em 0; }
.contents ol { list-style: none; }
`;

// Text as XHTML: its "[^id]" references as links to their footnotes
const linkedHtml = (text: string) => escapeXml(text).replace(/\[\^(\d+)\]/g, (_, id: string) => (
  `<a epub:type="noteref" class="noteref" id="ref-${ id }" href="#fn-${ id }">${ id }</a>`
));

const paragraphHtml = (p: Paragraph) => (
  p.kind === 'heading' ? `<p class="heading">${ linkedHtml(p.text) }</p>` : `<p>${ linkedHtml(p.text) }</p>`
);

// A footnote after its section's text: its number linking back to its reference, when it has one
const footnoteHtml = (f: Footnote) => {
  const number = f.inline ? `<a href="#ref-${ f.id }">${ f.id }</a>` : String(f.id);
  return `<aside epub:type="footnote" class="footnote" id="fn-${ f.id }"><p>${ number }. ${ escapeXml(f.text) }</p></aside>`;
};

const sectionHtml = (s: Section, depth: number, footnotes: Map<number, Footnote>) => {
  const level = Math.min(depth, 6);
  const notes = s.paragraphs.flatMap((p) => p.footnoteIds).flatMap((id) => (footnotes.has(id) ? [footnotes.get(id)!] : []));
  // the heading it starts with is its title's second line (its footnote references kept), not a
  // paragraph again
  const opening    = s.heading !== undefined && s.paragraphs[0]?.kind === 'heading' ? s.paragraphs[0] : undefined;
  const paragraphs = opening === undefined ? s.paragraphs : s.paragraphs.slice(1);
  const subtitle   = opening !== undefined ? linkedHtml(opening.text) : s.heading === undefined ? undefined : escapeXml(s.heading);

  return [
    `<section epub:type="chapter" id="section-${ s.id }">`,
    ...(s.title.length > 0 || subtitle !== undefined
      ? [`<h${ level }>${ escapeXml(s.title) }${ subtitle === undefined ? '' : `<br/>${ subtitle }` }</h${ level }>`]
      : []),
    ...paragraphs.map(paragraphHtml),
    ...(notes.length > 0 ? ['<section class="footnotes" epub:type="footnotes">', ...notes.map(footnoteHtml), '</section>'] : []),
    '</section>',
  ].join('\n');
};

// The contents as nested lists of links, for the navigation document and the contents page
const contentsList = (nodes: ContentsNode[], files: Map<number, string>): string => (
  nodes.length === 0 ? '' : `<ol>\n${ nodes.map((n) => (
    `<li><a href="${ files.get(n.sectionId) }">${ escapeXml(sectionLabel(n)) }</a>${ contentsList(n.children, files) }</li>`
  )).join('\n') }\n</ol>`
);

// The NCX's navPoints, numbered in reading order
const navPoints = (nodes: ContentsNode[], files: Map<number, string>, order: { n: number }): string => nodes.map((n) => {
  order.n += 1;
  return `<navPoint id="nav-${ n.sectionId }" playOrder="${ order.n }"><navLabel><text>${ escapeXml(sectionLabel(n)) }</text></navLabel>`
    + `<content src="${ files.get(n.sectionId) }"/>${ navPoints(n.children, files, order) }</navPoint>`;
}).join('\n');

// Each section's depth in the contents, from 1
const depths = (sections: Section[]) => sections.reduce((m, s) => (
  m.set(s.id, s.parentId === null ? 1 : (m.get(s.parentId) ?? 0) + 1)
), new Map<number, number>());

// The files of the book's EPUB, mimetype first (it must be the zip's first entry, stored)
export const bookEpubFiles = (book: BookText, options: EpubOptions): EpubFile[] => {
  const language  = options.language ?? 'en';
  const id        = `urn:tela-lucis:${ book.id }`;
  const modified  = options.modified.toISOString().replace(/\.\d{3}Z$/, 'Z');
  const files     = new Map(book.sections.map((s) => [s.id, sectionFile(s)]));
  const footnotes = new Map(book.footnotes.map((f) => [f.id, f]));
  const depthOf   = depths(book.sections);
  const sections  = book.sections;

  const titlePage = xhtml(book.title, language, `<section epub:type="titlepage" class="title-page">
<h1>${ escapeXml(book.title) }</h1>
<p class="author">${ escapeXml(book.author) }</p>
</section>`);

  const contentsPage = xhtml('Contents', language, `<nav epub:type="toc" id="toc" class="contents">
<h1>Contents</h1>
${ contentsList(book.contents, files) }
</nav>`);

  const ncx = `<?xml version="1.0" encoding="utf-8"?>
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1" xml:lang="${ language }">
<head><meta name="dtb:uid" content="${ escapeXml(id) }"/></head>
<docTitle><text>${ escapeXml(book.title) }</text></docTitle>
<navMap>
${ navPoints(book.contents, files, { n: 0 }) }
</navMap>
</ncx>
`;

  const manifest = [
    '<item id="style" href="style.css" media-type="text/css"/>',
    '<item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>',
    '<item id="title-page" href="title.xhtml" media-type="application/xhtml+xml"/>',
    '<item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav"/>',
    ...sections.map((s) => `<item id="s${ s.id }" href="${ files.get(s.id) }" media-type="application/xhtml+xml"/>`),
  ];
  const spine    = ['title-page', 'nav', ...sections.map((s) => `s${ s.id }`)].map((i) => `<itemref idref="${ i }"/>`);

  const opf = `<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://www.idpf.org/2007/opf" version="3.0" unique-identifier="book-id" xml:lang="${ language }">
<metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
<dc:identifier id="book-id">${ escapeXml(id) }</dc:identifier>
<dc:title>${ escapeXml(book.title) }</dc:title>
<dc:creator>${ escapeXml(book.author) }</dc:creator>
<dc:language>${ language }</dc:language>
<meta property="dcterms:modified">${ modified }</meta>
</metadata>
<manifest>
${ manifest.join('\n') }
</manifest>
<spine toc="ncx">
${ spine.join('\n') }
</spine>
</package>
`;

  return [
    { path: 'mimetype', content: 'application/epub+zip' },
    { path:    'META-INF/container.xml', content: `<?xml version="1.0" encoding="utf-8"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
<rootfiles><rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/></rootfiles>
</container>
` },
    { path: 'OEBPS/content.opf', content: opf },
    { path: 'OEBPS/toc.ncx', content: ncx },
    { path: 'OEBPS/nav.xhtml', content: contentsPage },
    { path: 'OEBPS/title.xhtml', content: titlePage },
    { path: 'OEBPS/style.css', content: STYLE },
    ...sections.map((s) => ({
      path:    `OEBPS/${ files.get(s.id) }`,
      content: xhtml(sectionLabel(s), language, sectionHtml(s, depthOf.get(s.id) ?? 1, footnotes)),
    })),
  ];
};
