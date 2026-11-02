import { describe, expect, it } from '@jest/globals';
import { AQUINAS_AUTHOR, fillCitationAuthors, isAquinasTitle } from './citation_authors.ts';

const cite = (author: string, title: string, location = '') => ({ author, title, location });

describe('isAquinasTitle', () => {
  it('knows the titles his works are cited by', () => {
    for (const title of [
      'ST', 'S.T.', 'Summa Theol.', 'Summa Theologica', 'CG', 'C.G.', 'IV Cont. Gent.', 'Contra Gentiles',
      'Quaest. de An.', 'Q. D. de Ver.', 'De Ver.', 'De Pot.', 'De Malo', 'De Virt. in Comm.', 'Comp. Theol.',
      'De Ente et Essentia', 'Expos. in Symbol.', 'De Mixtione Elementorum', 'In III Phys.', 'In V Meta.',
    ]) {
      expect([title, isAquinasTitle(title, false)]).toEqual([title, true]);
    }
  });

  it('leaves out Aristotle, the Bible, and others\' works', () => {
    for (const title of ['De Anima', 'Physica', 'VI Physic.', 'Phil. Nat.', 'Disputationes Metaphysicae', 'I Cor.', 'Op. cit.']) {
      expect([title, isAquinasTitle(title, false)]).toEqual([title, false]);
    }
  });

  it('takes a commentary on Aristotle cited by lectio as his', () => {
    expect(isAquinasTitle('VI Physic.', false, 'lect. 8, No. 14')).toBe(true);
    expect(isAquinasTitle('Physica', false, 'iii. 3. 202a')).toBe(false);
  });

  it('takes the Sentences and Quodlibets as his only where he is cited by name', () => {
    expect(isAquinasTitle('In III Sent.', false)).toBe(false);
    expect(isAquinasTitle('In III Sent.', true)).toBe(true);
    expect(isAquinasTitle('Sentences', true)).toBe(false);
  });
});

describe('fillCitationAuthors', () => {
  it('fills in a missing author, or one that only repeats the title', () => {
    expect(fillCitationAuthors([cite('CG', 'CG'), cite('', 'Quaest. de An.'), cite('unknown', 'ST')]))
      .toEqual([cite(AQUINAS_AUTHOR, 'CG'), cite(AQUINAS_AUTHOR, 'Quaest. de An.'), cite(AQUINAS_AUTHOR, 'ST')]);
  });

  it('keeps an author given, and blanks placeholders and repeated titles of other works', () => {
    expect(fillCitationAuthors([
      cite('Aristotle', 'De Anima'), cite('Kant', 'unknown'), cite('Phil. Nat.', 'Phil. Nat.'),
    ])).toEqual([cite('Aristotle', 'De Anima'), cite('Kant', ''), cite('', 'Phil. Nat.')]);
  });

  it('fills in the Sentences when another citation names St. Thomas', () => {
    expect(fillCitationAuthors([cite('', 'In IV Sent.')])).toEqual([cite('', 'In IV Sent.')]);
    expect(fillCitationAuthors([cite('St. Thomas', 'Summa Theologica'), cite('', 'In IV Sent.')])[1])
      .toEqual(cite(AQUINAS_AUTHOR, 'In IV Sent.'));
  });
});
