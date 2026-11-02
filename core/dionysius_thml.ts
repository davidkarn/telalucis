// The works of Dionysius the Areopagite in John Parker's translation (1897),
// from CCEL's ThML edition (../thml/dionysius.xml), as books: the Divine
// Names, the Mystical Theology, the Letters, the Celestial Hierarchy and the
// Ecclesiastical Hierarchy, a page per section. Parker's prefaces, essays and
// appendix are left out, as is the Liturgy he prints with them (not of the
// corpus). Pure functions; book_importers/dionysius_thml.ts reads the file and
// saves the books.
//
// The works are cited by chapter (a letter by its number) and section:
// "Div. Nom. c. 4, § 7", "Coel. Hier. iii, 2", "Ep. 9". The sections are only
// headings in the text ("SECTION I."), so each chapter's are split into pages
// here, cited by chapter and position. The Ecclesiastical Hierarchy's chapters
// are in parts ("I. Concerning things done in Illumination", "II.
// Mysterion", "III. Contemplation"), some numbering their sections from I
// again; its sections are cited by chapter, part and position.
import { parseDivs, type Div } from './summa_thml.ts';
import { workPages, type Work, type WorkPage } from './fathers_thml.ts';

export const DIONYSIUS = 'Dionysius the Areopagite';

export const DIONYSIUS_TRANSLATOR = 'John Parker';

// The works, by the title of their div in the file
export const DIONYSIAN_WORKS: { div: string, id: string, title: string }[] = [
  { div: 'On Divine Names', id: 'dionysius-divine-names', title: 'On the Divine Names' },
  { div: 'Mystic Theology', id: 'dionysius-mystical-theology', title: 'The Mystical Theology' },
  { div: 'Letters', id: 'dionysius-letters', title: 'Letters' },
  { div: 'On the Heavenly Hierarchy', id: 'dionysius-celestial-hierarchy', title: 'The Celestial Hierarchy' },
  { div: 'Ecclesiastical Hierarchy', id: 'dionysius-ecclesiastical-hierarchy', title: 'The Ecclesiastical Hierarchy' },
];

const ROMAN_VALUES: Record<string, number> = { i: 1, v: 5, x: 10, l: 50 };

// "XIV" -> 14
const romanValue = (roman: string): number => [...roman.toLowerCase()].reduce((n, ch, i, all) => {
  const v    = ROMAN_VALUES[ch]!;
  const next = ROMAN_VALUES[all[i + 1] ?? ''] ?? 0;
  return n + (v < next ? -v : v);
}, 0);

// A section's heading ("SECTION I.", "Section IV."), and a part's ("II.
// Mysterion of Illumination.")
// ("SECTION I. To Polycarp—Hierarch." too, the letter's title again)
const SECTION = /<p class="center"[^>]*>\s*section\s+([ivxl]+)\b[\s\S]*?<\/p>/gi;
const PART    = /<p class="subject"[^>]*>(?:\s|<[^>]+>)*([ivx]+)\.\s/gi;

type Mark = { at: number, end: number, value: number };

const marks = (html: string, pattern: RegExp): Mark[] => (
  [...html.matchAll(pattern)].map((m) => ({ at: m.index!, end: m.index! + m[0].length, value: romanValue(m[1]!) }))
);

const div = (parent: Div, n: number, title: string, own: string, children: Div[] = []): Div => ({
  level: parent.level + 1, id: `${ parent.id }.${ n }`, title, shortTitle: title, kind: '', own, children,
});

// A chapter's (or letter's) div with its sections as divs of their own, each
// from its heading to the next (the chapter's heading and subject before the
// first section going with it; the heading itself left out, the page's
// header saying it), in the chapter's parts when it has them ("II.
// Mysterion": the sections after it, till the next part, are in part 2). A
// chapter with no sections is as it was.
export const withSections = (chapter: Div): Div => {
  const sections = marks(chapter.own, SECTION);
  const parts    = marks(chapter.own, PART);

  if (sections.length === 0) {
    return chapter;
  }
  else {
    // a section starts at its heading, or at a part's heading before it
    const starts = sections.map((s, i) => {
      const part = parts.find((p) => p.at < s.at && (i === 0 || p.at > sections[i - 1]!.at));
      return i === 0 ? 0 : part?.at ?? s.at;
    });
    const divs = sections.map((s, i) => ({
      part:    parts.filter((p) => p.at < s.at).at(-1)?.value,
      section: div(
        chapter, i + 1, `Section ${ toRoman(s.value) }`,
        chapter.own.slice(starts[i], s.at) + chapter.own.slice(s.end, starts[i + 1]),
      ),
    }));

    if (parts.length === 0) {
      return { ...chapter, own: '', children: divs.map((d) => d.section) };
    }
    else {
      // the sections in their parts (those before any part in the first)
      const byPart = divs.reduce<{ part: number, sections: Div[] }[]>((out, d) => {
        const last = out.at(-1);
        const part = d.part ?? 1;
        return last && last.part === part
          ? [...out.slice(0, -1), { part, sections: [...last.sections, d.section] }]
          : [...out, { part, sections: [d.section] }];
      }, []);
      return {
        ...chapter,
        own:      '',
        children: byPart.map((p, i) => div(chapter, 100 + i, `Part ${ toRoman(p.part) }`, '', p.sections)),
      };
    }
  }
};

const toRoman = (n: number): string => (
  [[50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']] as [number, string][]
).reduce((out, [v, r]) => ({ rest: out.rest % v, text: out.text + r.repeat(Math.floor(out.rest / v)) }),
         { rest: n, text: '' }).text;

// A chapter's div labelled as the work's pages are cited: "Caput IV." ->
// "Chapter IV", a letter by its number ("Letter IX. To Titus" -> "Letter
// IX")
const labelled = (chapter: Div): Div => {
  const caput  = chapter.title.match(/^\s*caput\s+([ivxl]+)\b/i);
  const letter = chapter.title.match(/^\s*letter\s+([ivxl]+)\b/i);
  const label  = caput ? `Chapter ${ caput[1]!.toUpperCase() }`
    : letter ? `Letter ${ letter[1]!.toUpperCase() }`
      : chapter.shortTitle;
  return { ...chapter, shortTitle: label };
};

// A page's divisions as citations give them: the chapter or letter, then (in
// the Ecclesiastical Hierarchy) the part, then the section, as a position
// (the generic reading of the divs takes a section for a chapter)
const asCited = (page: WorkPage): WorkPage => ({
  ...page,
  citationParts: page.citationParts.map((p, i) => (
    i > 0 && p.type === 'chapter' ? { ...p, type: 'position' } : p
  )),
});

const allDivs = (divs: Div[]): Div[] => divs.flatMap((d) => [d, ...allDivs(d.children)]);

// The works in the file, as books (see DIONYSIAN_WORKS), with their pages
export const dionysianWorks = (xml: string): Work[] => {
  const divs = allDivs(parseDivs(xml.slice(xml.indexOf('<ThML.body'))));

  return DIONYSIAN_WORKS.map((work) => {
    const found = divs.find((d) => d.title === work.div);
    if (!found) {
      throw new Error(`no "${ work.div }" in the file`);
    }

    // the editor's note after the Divine Names is left out
    const chapters = found.children.filter((c) => !/^\s*note\b/i.test(c.title)).map(labelled).map(withSections);
    return {
      id:     work.id,
      title:  work.title,
      author: DIONYSIUS,
      url:    `https://www.ccel.org/ccel/dionysius/works.${ found.id }.html`,
      pages:  workPages({ ...found, children: chapters }, work.title, work.id).map(asCited),
    };
  });
};
