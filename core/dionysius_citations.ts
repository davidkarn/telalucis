// Citations of the works of Dionysius the Areopagite, and his works imported
// by book_importers/dionysius_thml.ts (see core/dionysius_thml.ts): the
// aliases citedFatherWork matches them by (see core/fathers_citations.ts), for
// relink-citations and book_importers/fathers_update_incoming_citations.ts
// --works dionysius.
//
// He is cited as "Dionysius", "Dionys.", "Pseudo-Dionysius", "the Areopagite",
// "Denys", and his works by their Latin titles, abbreviated: "De div. nom.",
// "Coel. Hier.", "Coelest. Hier.", "Eccl. Hier.", "Hier. Eccles.", "Myst. Theol.", "Ep. ix". Not Dionysius of
// Alexandria or of Corinth: their titles are none of these.
import type { FatherWork } from './fathers_citations.ts';

const DIONYSIUS = new RegExp('^(?:(?:st\\.?|s\\.|saint|pseudo|ps\\.)[\\s.-]*)*'
  + '(?:(?:dion(?:ys(?:ius|ios|\\.)?|\\.)?|denys|denis)(?:[\\s,]+(?:the\\s+)?areop(?:agite|agita|\\.)?)?'
  + '|(?:the\\s+)?areopagite)\\.?$', 'i');

const work = (id: string, title: RegExp): FatherWork => ({ bookId: `dionysius-${ id }`, author: DIONYSIUS, title });

export const DIONYSIUS_WORKS: FatherWork[] = [
  work('divine-names', /^(?:de\s*)?div(?:in(?:e|is|a)?)?\.?\s*nom|^(?:on\s+)?(?:the\s+)?divine\s+names/i),
  work('celestial-hierarchy',
       /^(?:de\s*)?(?:coel|cael|cel)(?:est(?:i|ial|e)?)?\.?\s*hier|^(?:on\s+)?(?:the\s+)?(?:celestial|heavenly)\s+hier/i),
  work('ecclesiastical-hierarchy',
       /^(?:de\s*)?eccl(?:es(?:iastica|iastical)?)?\.?\s*hier|^(?:the\s+)?ecclesiastical\s+hier|^hier(?:\.|archia)?\s*eccl/i),
  work('mystical-theology', /^(?:de\s*)?myst(?:ic(?:a|al)?)?\.?\s*theol|^the\s+myst(?:ic|ical)\s+theol/i),
  work('letters', /^(?:ep(?:ist)?(?:\.|ola[e]?|les?)?|letters?)\b/i),
];
