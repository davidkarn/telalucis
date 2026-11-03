// Citations of Boethius's Consolation of Philosophy, Aquinas's Summa Contra Gentiles and Hugh of
// Saint Victor's De Sacramentis (imported by book_importers/consolation_gutenberg.ts,
// single_thml.ts and hugh_sacraments.ts): the aliases citedFatherWork
// matches them by (see core/fathers_citations.ts), for
// book_importers/fathers_update_incoming_citations.ts --works boethius|contra-gentiles.
import type { FatherWork } from './fathers_citations.ts';

const BOETHIUS = /^(?:st\.?\s+)?boethius\.?$/i;

// "Thomas Aquinas", "St. Thomas", "S. Thom.", "Aquinas"
export const AQUINAS = /^(?:(?:st\.?|s\.|saint)\s+)?(?:thom(?:as|\.)?(?:\s+(?:of\s+)?aquinas)?|aquinas)\.?$/i;

// The Consolation, cited by book, and by prose section ("iii, 11": book 3, prose 11, the scholastics
// numbering its prose sections); its pages are its prose sections and metres (poems), so the
// places cited are read as book and prose
export const BOETHIUS_WORKS: FatherWork[] = [
  {
    bookId: 'boethius-consolation-of-philosophy',
    author: BOETHIUS,
    title:  /^de\s*consol/i,
    types:  ['book', 'prose'],
  },
];

// The Summa Contra Gentiles by Aquinas, not Athanasius's Contra Gentes (its "C. Gentes" too). Its
// page numbers in Rickaby's "Of God and His Creatures" ("p. 57") aren't the chapters' pages here,
// and aren't read.
export const CONTRA_GENTILES_WORKS: FatherWork[] = [
  {
    bookId: 'summa-contra-gentiles',
    author: AQUINAS,
    title:  /^(?:summa\s*)?(?:contra|cont\.?|c\.)\s*gent/i,
  },
];

// Hugh of Saint Victor: "Hugh", "Hugo", "Hugh of St. Victor", "Hugo de S. Victore"; not Hugon
// (Édouard Hugon, whose De Sacramentis in Communi is another work)
const HUGH = /^(?:st\.?\s+)?hugh?o?(?:\s+(?:of|de|a)\s+(?:st\.?|saint|s\.|sancto)\s*vict\w*)?\.?$/i;

// Hugh's De Sacramentis Christianae Fidei (imported by book_importers/hugh_sacraments.ts), cited
// by book, part and chapter ("De Sacram., II, 3, 12"; core/hugh_sacraments.ts)
export const HUGH_WORKS: FatherWork[] = [
  {
    bookId: 'hugh-of-st-victor-de-sacramentis',
    author: HUGH,
    title:  /^(?:de\s*sacram|on the sacraments)/i,
    types:  ['book', 'part', 'chapter'],
  },
];

// Anselm: "Anselm", "St. Anselm", "S. Anselmus", "Anselm of Canterbury"
const ANSELM = /^(?:st\.?\s+|s\.\s+|saint\s+)?anselm(?:us)?\.?(?:\s+of\s+canterbury)?$/i;

// Anselm's treatises in Deane's translation (imported by book_importers/anselm_works.ts; see
// core/anselm_works.ts): the Proslogium and Monologium cited by chapter ("Monolog. 7"), Cur Deus
// Homo by book and chapter ("Cur Deus Homo, II, 6"). Webb's Proslogion translates the same work;
// citations of it are pointed at Deane's.
export const ANSELM_ALIAS_WORKS: FatherWork[] = [
  { bookId: 'anselm-proslogium', author: ANSELM, title: /^proslog/i, types: ['chapter'] },
  { bookId: 'anselm-monologium', author: ANSELM, title: /^monol/i, types: ['chapter'] },
  { bookId: 'anselm-cur-deus-homo', author: ANSELM, title: /^cur\s+deus/i, types: ['book', 'chapter'] },
];
