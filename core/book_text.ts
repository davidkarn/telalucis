// A book's text as a reader of the work (not of its pages) wants it: its title, author, table of
// contents, and each section's paragraphs, with the page furniture gone. Running headers and
// footers and printed page numbers are left out; a paragraph broken by a page (or a column) is
// joined again, a word broken by a line-end hyphen made whole; and each footnote is kept where its
// marker is printed, as "[^id]" in the paragraph's text, its own text and citations in the book's
// footnotes. Sections come from how the book's pages are cited (book_pages_to_citations: "Question
// 2, Article 3"), for the works divided so, or else from its headings (SectionHeader blocks, nested
// by their h1-h6 level). Pure functions; the blocks, page parts, part names and citations are read
// from the database by the caller.
import { squash, strip_tags } from './book_pages.ts';
import { partNamer, type PartName } from './book_part_names.ts';
import { paragraph_marker } from './citation_pages.ts';
import type { CitationPart } from './summa_thml.ts';
import type { PageBlock } from '../types.ts';

// What's read of a book: its pages' blocks in reading order; how its pages are cited, those that
// are, each page's parts rows in order (a page can hold the end of one article and the start of the
// next); the proper names of its parts; and the citations found in its footnotes
export type BookSource = {
  id: string,
  title: string,
  author: string,
  pages: { pageNumber: number, printedPageNumber: string, blocks: { label: PageBlock['label'], html: string }[] }[],
  pageParts?: { pageNumber: number, parts: CitationPart[] }[],
  partNames?: PartName[],
  citations?: FootnoteCitation[],
};

export type FootnoteCitation = {
  footnotePage: number,
  footnoteIdentifier: string,
  author: string,
  title: string,
  location: string,
  raw: string,
  referenceBookId: string | null,
};

// A paragraph's text: plain, with "[^id]" where a footnote's marker is printed. footnoteIds: the
// footnotes it refers to, in order, then any of its last page whose marker wasn't found in the
// text (inline: false on the footnote). heading: a heading within a section (only in books
// divided by their citations, whose sections are titled by their parts).
export type Paragraph = {
  kind: 'text' | 'heading',
  text: string,
  pageNumber: number,
  endPageNumber: number,
  footnoteIds: number[],
};

// id: its place among the book's footnotes, from 1. marker: as printed ("12", "*"). pageNumber: the
// page it starts on. inline: whether its marker was found in the text.
export type Footnote = {
  id: number,
  marker: string,
  text: string,
  pageNumber: number,
  printedPageNumber: string,
  inline: boolean,
  citations: FootnoteCitation[],
};

// id: its place among the book's sections, from 0, which are in reading order. parts: its citation
// parts, outermost first (none for a section from a heading). title: '' for the text before the
// first heading. heading: in a section titled by its parts ("Chapter 22"), the heading it starts
// with, if it does ("XXII. On the power of God…").
export type Section = {
  id: number,
  parentId: number | null,
  level: number,
  title: string,
  heading?: string,
  parts: CitationPart[],
  pageNumber: number,
  printedPageNumber: string,
  endPageNumber: number,
  paragraphs: Paragraph[],
};

export type ContentsNode = {
  sectionId: number,
  title: string,
  heading?: string,
  pageNumber: number,
  printedPageNumber: string,
  children: ContentsNode[],
};

export type BookText = {
  id: string,
  title: string,
  author: string,
  contents: ContentsNode[],
  sections: Section[],
  footnotes: Footnote[],
};

// ---------------------------------------------------------------------------
// Paragraphs
// ---------------------------------------------------------------------------

const FOOTNOTE_REF = /\[\^\d+\]/g;

// Whether the paragraph after `text` carries it on: text (its footnote references aside) doesn't
// end a sentence ("…the power of" / "God exceeds…" across a page)
export const continuesParagraph = (text: string) => (
  !/[.!?:;"”’)\]]$/.test(text.replace(FOOTNOTE_REF, '').trimEnd())
);

// text and next joined: a word broken by a line-end hyphen ("contin-" "uation") made whole
export const joinedParagraphs = (text: string, next: string) => (
  /\p{Ll}-$/u.test(text) && /^\p{Ll}/u.test(next) ? text.slice(0, -1) + next : `${ text } ${ next }`
);

const markerKey = (marker: string) => marker.trim().replace(/[).]+$/, '');

const SYMBOLS = '*†‡§';

// A footnote paragraph's marker, null for one carrying on the footnote before: as
// paragraph_marker reads it, or a symbol run into the text ("<p>*Abelard when…")
const footnoteMarker = (html: string): string | null => (
  paragraph_marker(html) ?? html.match(new RegExp(`^\\s*<p>\\s*([${ SYMBOLS }]+)`))?.[1] ?? null
);

// A footnote paragraph's text, its marker left out
const footnoteParagraphText = (html: string, marker: string | null) => {
  const text = squash(strip_tags(html.replace(/^\s*<p>\s*<sup>[^<]*<\/sup>/i, '<p>')));
  return marker !== null && text.startsWith(marker) ? text.slice(marker.length).replace(/^[).]?\s*/, '') : text;
};

// A block's paragraphs' HTML
const paragraphsOf = (html: string) => html.split(/(?=<p[\s>])/i).filter((p) => strip_tags(p).trim().length > 0);

// Whether a block is only a printed page number: the page's own, or a number or numeral alone
const isPageNumber = (text: string, printed: string) => (
  (printed.length > 0 && text.toLowerCase() === printed.toLowerCase())
  || /^[[(-]?\s*(?:\d{1,4}|[ivxlc]{1,7})\s*[\])-]?$/i.test(text)
);

// A heading's level: its h1-h6 tag's number, 3 (the commonest) when it has none
const headingLevel = (html: string) => Number(html.match(/^\s*<h([1-6])/i)?.[1] ?? 3);

// The headings printed at the top of several pages: running heads OCR'd as headings, not the
// book's own ("THE SUMMA THEOLOGICA" over every page)
const runningHeadings = (pages: BookSource['pages']): Set<string> => {
  const tops = pages.flatMap((p) => {
    const first = p.blocks.find((b) => b.label !== 'PageHeader' && b.label !== 'PageFooter');
    return first?.label === 'SectionHeader' ? [squash(strip_tags(first.html)).toLowerCase()] : [];
  });
  const counts = tops.reduce((m, t) => m.set(t, (m.get(t) ?? 0) + 1), new Map<string, number>());
  return new Set([...counts].filter(([, n]) => n >= 3).map(([t]) => t));
};

// ---------------------------------------------------------------------------
// Footnotes
// ---------------------------------------------------------------------------

// Each page's footnotes, numbered through the book: a footnote paragraph without a marker carries
// on the footnote before it, on its page or the one before
const bookFootnotes = (source: BookSource): { footnotes: Footnote[], byPage: Map<number, Footnote[]> } => {
  const citationsOf = (page: number, marker: string) => (source.citations ?? []).filter((c) => (
    c.footnotePage === page && markerKey(c.footnoteIdentifier) === marker
  ));

  const footnotes: Footnote[] = [];
  const byPage                = new Map<number, Footnote[]>();

  for (const page of source.pages) {
    for (const block of page.blocks.filter((b) => b.label === 'Footnote')) {
      for (const html of paragraphsOf(block.html)) {
        const marker = footnoteMarker(html);
        const text   = footnoteParagraphText(html, marker);
        const last   = footnotes.at(-1);

        if (marker === null && last !== undefined) {
          last.text = joinedParagraphs(last.text, text);
        }
        else {
          const footnote: Footnote = {
            id:                footnotes.length + 1,
            marker:            marker ?? '',
            text,
            pageNumber:        page.pageNumber,
            printedPageNumber: page.printedPageNumber,
            inline:            false,
            citations:         citationsOf(page.pageNumber, marker ?? ''),
          };
          footnotes.push(footnote);
          byPage.set(page.pageNumber, [...(byPage.get(page.pageNumber) ?? []), footnote]);
        }
      }
    }
  }

  return { footnotes, byPage };
};

// A body paragraph's text, its footnote markers (<sup>) made references to its page's footnotes
// ("[^12]"), those with no footnote left out
const textWithReferences = (html: string, pageFootnotes: Footnote[]): { text: string, footnoteIds: number[] } => {
  const footnoteIds: number[] = [];
  const marked                = html.replace(/<sup>\s*([^<]*?)\s*<\/sup>/gi, (_, marker: string) => {
    const footnote = pageFootnotes.find((f) => f.marker === markerKey(marker) && !footnoteIds.includes(f.id));
    if (footnote) {
      footnote.inline = true;
      footnoteIds.push(footnote.id);
      return ` [^${ footnote.id }] `;
    }
    else {
      return ' ';
    }
  });

  // a symbol marker printed in the text itself, not raised ("…is omnipotent.*")
  const withSymbols = pageFootnotes
    .filter((f) => !f.inline && f.marker.length > 0 && [...f.marker].every((c) => SYMBOLS.includes(c)))
    .reduce((html, f) => {
      const at = html.search(new RegExp(`(?<=[\\p{L}\\p{P}>])${ f.marker.replace(/[*]/g, '\\*') }(?![${ SYMBOLS }])`, 'u'));
      if (at < 0) {
        return html;
      }
      else {
        f.inline = true;
        footnoteIds.push(f.id);
        return `${ html.slice(0, at) } [^${ f.id }] ${ html.slice(at + f.marker.length) }`;
      }
    }, marked);

  // a reference goes right after the word or stop it follows, and right before a stop
  const text = squash(strip_tags(withSymbols)).replace(/\s+(\[\^\d+\])/g, '$1').replace(/(\[\^\d+\])\s+(?=[,.;:!?)\]])/g, '$1');
  return { text, footnoteIds };
};

// ---------------------------------------------------------------------------
// Sections
// ---------------------------------------------------------------------------

const partsKey  = (parts: CitationPart[]) => parts.map((p) => `${ p.type }:${ p.value }`).join('/');
const sameParts = (a: CitationPart[], b: CitationPart[]) => partsKey(a) === partsKey(b);

const capitalized = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);

// A body block, in reading order: a paragraph or a heading, and the part it starts (in books
// divided by their citations: the page's next parts row, at the page's start or its headings)
type Unit = {
  kind: 'text' | 'heading',
  html: string,
  blockIndex: number,
  page: BookSource['pages'][number],
  startsParts?: CitationPart[],
};

const bodyUnits = (source: BookSource): Unit[] => {
  const running = runningHeadings(source.pages);
  const partsOf = new Map<number, CitationPart[][]>();
  for (const row of source.pageParts ?? []) {
    partsOf.set(row.pageNumber, [...(partsOf.get(row.pageNumber) ?? []), row.parts]);
  }

  let current: CitationPart[] | null = null;
  let blockIndex                     = 0;

  return source.pages.flatMap((page) => {
    const body = page.blocks.filter((b) => b.label === 'Text' || b.label === 'SectionHeader')
      .filter((b) => !(b.label === 'SectionHeader' && running.has(squash(strip_tags(b.html)).toLowerCase())))
      .filter((b) => !isPageNumber(squash(strip_tags(b.html)), page.printedPageNumber));

    // the parts rows starting on this page: all but one continuing the part before
    const rows   = partsOf.get(page.pageNumber) ?? [];
    const starts = current !== null && rows.length > 0 && sameParts(rows[0]!, current) ? rows.slice(1) : rows;
    let next     = 0;

    return body.flatMap((b, i) => {
      const heading = b.label === 'SectionHeader';
      // the first row starts at the page's top (unless it continues), later ones at its headings
      const starting = next < starts.length && ((i === 0 && next === 0 && starts.length === rows.length) || heading)
        ? starts[next++]
        : undefined;
      if (starting) {
        current = starting;
      }
      blockIndex++;

      return (heading ? [b.html] : paragraphsOf(b.html)).map((html) => ({
        kind: heading ? 'heading' as const : 'text' as const,
        html,
        blockIndex,
        page,
        ...(starting ? { startsParts: starting } : {}),
      }));
    });
  });
};

// ---------------------------------------------------------------------------
// The book
// ---------------------------------------------------------------------------

const contentsOfSections = (sections: Section[]): ContentsNode[] => {
  const nodes = new Map(sections.map((s): [number, ContentsNode] => [s.id, {
    sectionId:         s.id,
    title:             s.title,
    ...(s.heading === undefined ? {} : { heading: s.heading }),
    pageNumber:        s.pageNumber,
    printedPageNumber: s.printedPageNumber,
    children:          [],
  }]));

  return sections.flatMap((s) => {
    const node = nodes.get(s.id)!;
    if (s.parentId === null) {
      return [node];
    }
    else {
      nodes.get(s.parentId)!.children.push(node);
      return [];
    }
  });
};

// The book's text: its sections in reading order, each with its paragraphs, and its footnotes
export const bookText = (source: BookSource): BookText => {
  const { footnotes, byPage } = bookFootnotes(source);
  const byParts               = (source.pageParts ?? []).length > 0;
  const nameOf                = partNamer(source.partNames ?? []);

  const sections: Section[] = [];
  // the sections open, outermost first: those a new one may go in
  let open: Section[] = [];
  let lastBlock       = -1;

  const newSection = (fields: Pick<Section, 'level' | 'title' | 'parts'>, page: Unit['page'], parent: Section | null): Section => {
    const section: Section = {
      id:                sections.length,
      parentId:          parent?.id ?? null,
      ...fields,
      pageNumber:        page.pageNumber,
      printedPageNumber: page.printedPageNumber,
      endPageNumber:     page.pageNumber,
      paragraphs:        [],
    };
    sections.push(section);
    return section;
  };

  // A section for each of these parts not open already (in books divided by their citations)
  const openParts = (parts: CitationPart[], page: Unit['page']) => {
    const kept = open.filter((s, i) => i < parts.length && sameParts(s.parts, parts.slice(0, i + 1)));
    open       = parts.slice(kept.length).reduce((chain, part, i) => {
      const path  = parts.slice(0, kept.length + i + 1);
      const title = nameOf(path) ?? `${ capitalized(part.type) } ${ part.value }`;
      return [...chain, newSection({ level: path.length, title, parts: path }, page, chain.at(-1) ?? null)];
    }, kept);
  };

  // A section for a heading, in the nearest open one of a higher level; a heading right after
  // another of its level, with nothing between, adds to its title
  const openHeading = (title: string, level: number, page: Unit['page']) => {
    const last = open.at(-1);
    if (last && last.paragraphs.length === 0 && last.level === level && sections.at(-1) === last && last.title !== '') {
      last.title = `${ last.title } — ${ title }`;
    }
    else {
      open = open.filter((s) => s.level < level);
      open = [...open, newSection({ level, title, parts: [] }, page, open.at(-1) ?? null)];
    }
  };

  for (const unit of bodyUnits(source)) {
    if (byParts && unit.startsParts) {
      openParts(unit.startsParts, unit.page);
    }

    const pageFootnotes         = byPage.get(unit.page.pageNumber) ?? [];
    const { text, footnoteIds } = textWithReferences(unit.html, pageFootnotes);

    if (!byParts && unit.kind === 'heading') {
      openHeading(text.replace(FOOTNOTE_REF, '').trim(), headingLevel(unit.html), unit.page);
    }
    else {
      const section = open.at(-1) ?? (open = [newSection({ level: 1, title: '', parts: [] }, unit.page, null)])[0]!;
      const last    = section.paragraphs.at(-1);

      // a paragraph broken by a page or a column (a new block) is joined again
      if (last && last.kind === 'text' && unit.kind === 'text' && unit.blockIndex !== lastBlock && continuesParagraph(last.text)) {
        last.text          = joinedParagraphs(last.text, text);
        last.endPageNumber = unit.page.pageNumber;
        last.footnoteIds   = [...last.footnoteIds, ...footnoteIds];
      }
      else {
        section.paragraphs.push({
          kind: unit.kind, text, pageNumber: unit.page.pageNumber, endPageNumber: unit.page.pageNumber, footnoteIds,
        });
      }
    }

    for (const s of open) {
      s.endPageNumber = unit.page.pageNumber;
    }
    lastBlock = unit.blockIndex;
  }

  // a section titled by its parts gets the heading it starts with
  for (const section of sections) {
    const first = section.paragraphs[0];
    if (section.parts.length > 0 && first?.kind === 'heading') {
      section.heading = first.text.replace(FOOTNOTE_REF, '').trim();
    }
  }

  // footnotes whose marker wasn't found go with the last paragraph of their page
  const paragraphs = sections.flatMap((s) => s.paragraphs);
  for (const footnote of footnotes.filter((f) => !f.inline)) {
    const holder = paragraphs.findLast((p) => p.pageNumber <= footnote.pageNumber && p.endPageNumber >= footnote.pageNumber)
      ?? paragraphs.findLast((p) => p.endPageNumber <= footnote.pageNumber);
    holder?.footnoteIds.push(footnote.id);
  }

  return {
    id:       source.id,
    title:    source.title,
    author:   source.author,
    contents: contentsOfSections(sections),
    sections,
    footnotes,
  };
};
