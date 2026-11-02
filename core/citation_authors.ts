// Filling in the author of citations the LLM left without one, from their
// titles: a footnote citing "CG, III, 69" or "Quaest. de An., a. 1" names no
// author, and the model, unless told by insights, gives none, a placeholder
// ("unknown"), or the title again. Only Aquinas's works for now, cited that
// way all through the Thomist journals. Pure functions;
// lib/footnote_citations.ts applies them to the citations it extracts.
import type { Citation } from '../types.ts';
import { AQUINAS } from './medieval_citations.ts';

// The author given citations of Aquinas's works
export const AQUINAS_AUTHOR = 'St. Thomas Aquinas';

// What the model writes for an author or title it doesn't know
const PLACEHOLDER = /^(?:unknown|unspecified(?: work| author)?|n\/?a|none)$/i;

// A title as compared: lower case, without dots and commas, its spacing
// collapsed, and without a leading "In" and book number ("In III Phys." ->
// "phys"), which commentary says whether it had
type Title = { text: string, commentary: boolean };

const titleOf = (title: string): Title => {
  const text = title.toLowerCase().replace(/[.,:;]/g, ' ').replace(/\s+/g, ' ').trim();
  const bare = text.replace(/^in\s+/, '');

  return {
    text:       bare.replace(/^(?:[ivxl]+|\d+)\s+/, '').replace(/^(?:l\s+)/, ''),
    commentary: bare !== text,
  };
};

// Titles only Aquinas's works are cited by: the Summae, the disputed
// questions, the opuscula. "De Anima" alone is Aristotle's, so the Quaestio de
// Anima only with "Q."/"Quaest." before it.
const AQUINAS_TITLES: RegExp[] = [
  /^(?:summa )?(?:theol(?:ogica|ogiae)?|sum theol|s theol|s ?t|st)$/,
  /^summa theol/,
  /^(?:summa )?(?:contra|cont|c) ?gent(?:iles)?$|^s ?c ?g$|^c ?g$/,
  /^(?:q(?:q)?|quaest(?:iones)?|qu) ?(?:d|disp(?:utatae|utata)?)? ?de (?:ver|pot|malo|an(?:ima)?|spir|virt|car|unione|ver(?:itate)?)\b/,
  /^de (?:ver(?:itate)?|pot(?:entia)?|malo|spir(?:itualibus)? creat|virt(?:utibus)? in comm|car(?:itate)?)\b/,
  /^comp(?:endium)? theol/,
  /^de ente(?: et ess(?:entia)?)?$/,
  /^de reg(?:imine|no)? princ/,
  /^expos(?:itio)? in symb/,
  /^de unit(?:ate)? intell/,
  /^de subst(?:antiis)? sep/,
  /^de aetern(?:itate)? mundi/,
  /^(?:in )?boet(?:h(?:ium)?)? de (?:trin|hebd)/,
  /^de mixt(?:ione)? elem/,
  /^de perf(?:ectione)? vitae spir/,
  /^contra impugn/,
];

// Titles of Aristotle's works, whose commentaries ("In III Phys.", "In V
// Meta.") are Aquinas's: cited with "In" before them, or by lectio ("VI
// Physic., lect. 8"), as Aristotle's own works never are
const ARISTOTLE_TITLES = /^(?:phys|meta|metaph|ethic|eth|de an(?:ima)?|polit|de caelo|de coelo|de gen|peri ?herm|post anal|de sensu|de mem)/;

// Titles of works others wrote too (the Sentences commentaries of Bonaventure
// and Scotus, their Quodlibets), taken as Aquinas's only where the book cites
// him by name elsewhere
const SHARED_TITLES = /^(?:sent(?:ent(?:ia|iarum|iis|ces))?|quodl(?:ibet(?:a|um)?)?)\b/;

// Whether an author is missing: none, a placeholder, or the title over again
const hasNoAuthor = (c: Pick<Citation, 'author' | 'title'>): boolean => {
  const author = c.author.trim();
  return author.length === 0 || PLACEHOLDER.test(author) || titleOf(author).text === titleOf(c.title).text;
};

// Whether a title is that of a work of Aquinas: any of AQUINAS_TITLES, a
// commentary on Aristotle, or (aquinasCited) one of SHARED_TITLES. location:
// the place cited in it
export const isAquinasTitle = (title: string, aquinasCited: boolean, location = ''): boolean => {
  const t = titleOf(title);
  return AQUINAS_TITLES.some((p) => p.test(t.text))
    || ((t.commentary || /\blect\b/i.test(location)) && ARISTOTLE_TITLES.test(t.text))
    || (aquinasCited && t.commentary && SHARED_TITLES.test(t.text));
};

// The citations with the authors missing filled in where the title says whose
// work it is, and placeholder authors and titles ("unknown") blanked, an
// author that only repeats the title too; the others as they are
export const fillCitationAuthors = <C extends Pick<Citation, 'author' | 'title' | 'location'>>(citations: C[]): C[] => {
  const aquinasCited = citations.some((c) => AQUINAS.test(c.author.trim()));

  return citations.map((c) => {
    const title = PLACEHOLDER.test(c.title.trim()) ? '' : c.title;

    if (!hasNoAuthor(c)) {
      return { ...c, title };
    }
    else if (isAquinasTitle(title, aquinasCited, c.location)) {
      return { ...c, title, author: AQUINAS_AUTHOR };
    }
    else {
      return { ...c, title, author: '' };
    }
  });
};
