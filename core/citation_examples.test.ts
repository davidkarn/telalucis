import { describe, expect, it } from '@jest/globals';
import {
  batchExamples, citationTerms, exampleIndex, examplesPart, similarExamples, titleInRaw,
  type CitationExample,
} from './citation_examples.ts';

const example = (id: string, raw: string, author: string, title: string, sourceBookId = 'a'): CitationExample => (
  { id, sourceBookId, raw, author, title, location: '' }
);

const EXAMPLES = [
  example('1', 'S. Thom., In IV Sent., d. 17, q. 3, a. 1', 'St. Thomas Aquinas', 'Scriptum super Sententiis'),
  example('2', 'S. Thom., Opusc. de Ente et Essentia, c. 4', 'St. Thomas Aquinas', 'De Ente et Essentia'),
  example('3', 'Migne, Patrol. Lat., XXXII, 1220', 'Migne', 'Patrologia Latina'),
  example('4', 'Pohle-Preuss, Christology, pp. 161 sqq.', 'Pohle-Preuss', 'Christology'),
  example('5', 'Pohle-Preuss, Christology, p. 230', 'Pohle-Preuss', 'Christology'),
  example('6', 'Migne, Patrol. Graec., III, 585', 'Migne', 'Patrologia Graeca', 'own'),
];
const index = exampleIndex(EXAMPLES);

describe('citationTerms', () => {
  it('keeps words, lowercased and unaccented, without numbers, Roman ones too, or common words', () => {
    expect(citationTerms('Cf. S. Thom., In IV Sent., d. 17, pp. 3 sqq.; Théologie')).toEqual(
      ['thom', 'sent', 'd', 'theologie'],
    );
  });
});

describe('titleInRaw', () => {
  it('is true when a word of the text begins one of the title, or the other way round', () => {
    expect(titleInRaw('De civ. D. lib. XII, c. 9', 'De civitate Dei')).toBe(true);
    expect(titleInRaw('Metaph., VI, c. 2', 'Metaphysics')).toBe(true);
    expect(titleInRaw('Pohle-Preuss, Christology, p. 3', 'Christology')).toBe(true);
  });

  it('is false for a title the text does not bear out', () => {
    expect(titleInRaw('In II Sent., d. 20, Q. 2, a. 5.', 'Summa Theologica')).toBe(false);
    expect(titleInRaw('Cf. above note 2.', 'Summa Theologica')).toBe(false);
    expect(titleInRaw('I John. loc. cit.', '1 Corinthians')).toBe(false);
  });
});

describe('exampleIndex', () => {
  it('leaves out the examples whose text does not bear out their title', () => {
    const wrong = example('9', 'In II Sent., d. 20', 'St. Thomas', 'Summa Theologica');
    expect(exampleIndex([...EXAMPLES, wrong]).examples.map((e) => e.id)).not.toContain('9');
  });
});

describe('similarExamples', () => {
  it('finds the citations sharing a footnote\'s rarer words', () => {
    expect(similarExamples(index, 'Cf. In II Sent., dist. 3', { limit: 1 }).map((e) => e.id)).toEqual(['1']);
    expect(similarExamples(index, 'Migne, Patrol. Lat., XL, 22', { limit: 1 }).map((e) => e.id)).toEqual(['3']);
  });

  it('gives one example to a work', () => {
    expect(similarExamples(index, 'Pohle-Preuss, Christology, p. 12', { limit: 5 }).map((e) => e.id)).toEqual(['4']);
  });

  it('leaves out the book being extracted', () => {
    expect(similarExamples(index, 'Migne, Patrol. Graec., IV', { limit: 5, excludeBookId: 'own' }).map((e) => e.id))
      .not.toContain('6');
  });

  it('finds none for a footnote sharing no words', () => {
    expect(similarExamples(index, '12, 13', { limit: 5 })).toEqual([]);
  });
});

describe('batchExamples', () => {
  it('takes each footnote\'s, without repeats, up to the limit', () => {
    const found = batchExamples(index, ['In II Sent.', 'In III Sent.', 'Migne, Patrol. Lat.'], { perFootnote: 1, limit: 5 });
    expect(found.map((e) => e.id)).toEqual(['1', '3']);
    expect(batchExamples(index, ['In II Sent.', 'Migne, Patrol. Lat.'], { perFootnote: 1, limit: 1 })).toHaveLength(1);
  });
});

describe('examplesPart', () => {
  it('is empty without examples', () => {
    expect(examplesPart([])).toBe('');
  });

  it('shows each example\'s text and what was read from it', () => {
    expect(examplesPart([EXAMPLES[2]!])).toContain(
      '- "Migne, Patrol. Lat., XXXII, 1220" -> author "Migne", title "Patrologia Latina", location ""',
    );
  });
});
