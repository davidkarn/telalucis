import { describe, expect, it } from '@jest/globals';
import { withSections } from './dionysius_thml.ts';
import { citedFatherWork } from './fathers_citations.ts';
import { DIONYSIUS_WORKS } from './dionysius_citations.ts';
import type { Div } from './summa_thml.ts';

const chapter = (own: string): Div => ({ level: 3, id: 'c', title: 'Caput II.', shortTitle: 'Chapter II', kind: '', own, children: [] });
const section = (n: string) => `<p class="center" id="x">SECTION ${ n }.</p>`;
const part    = (n: string, title: string) => `<p class="subject" id="y"><span>${ n }. </span>${ title }</p>`;

describe('withSections', () => {
  it('makes a div of each section, the chapter\'s heading going with the first, the section headings left out', () => {
    const split = withSections(chapter(`<h3>CAPUT II.</h3>${ section('I') }<p>one</p>${ section('II') }<p>two</p>`));
    expect(split.own).toBe('');
    expect(split.children.map((c) => [c.shortTitle, c.own])).toEqual([
      ['Section I', '<h3>CAPUT II.</h3><p>one</p>'],
      ['Section II', '<p>two</p>'],
    ]);
  });

  it('puts the sections in the chapter\'s parts, a part\'s heading starting its first section', () => {
    const split = withSections(chapter(
      `${ part('I', 'Concerning things done') }<p>intro</p>${ part('II', 'Mysterion') }${ section('I') }<p>a</p>`
      + `${ section('II') }<p>b</p>${ part('III', 'Contemplation') }${ section('I') }<p>c</p>`,
    ));
    expect(split.children.map((p) => [p.shortTitle, p.children.map((s) => s.shortTitle)])).toEqual([
      ['Part II', ['Section I', 'Section II']],
      ['Part III', ['Section I']],
    ]);
    expect(split.children[1]!.children[0]!.own).toBe(`${ part('III', 'Contemplation') }<p>c</p>`);
  });

  it('leaves a chapter with no sections as it was', () => {
    const plain = chapter('<p>text</p>');
    expect(withSections(plain)).toBe(plain);
  });
});

describe('DIONYSIUS_WORKS', () => {
  const cited = (author: string, title: string) => citedFatherWork(author, title, '', DIONYSIUS_WORKS)?.bookId ?? null;

  it('knows his works by their Latin and English titles', () => {
    expect([
      cited('Dionysius', 'Div. Nom.'), cited('Pseudo-Dionysius', 'De divinis nominibus'),
      cited('Dionys.', 'Coel. Hier.'), cited('Dionysius the Areopagite', 'The Celestial Hierarchy'),
      cited('S. Dionysius', 'De eccl. hier.'), cited('Areopagite', 'Myst. Theol.'), cited('Denys', 'Ep.'),
      cited('Dionysius', 'Coelest. Hier.'), cited('Dionysius', 'Divin. Nom.'), cited('Dionysius', 'Hier. Eccles.'),
    ]).toEqual([
      'dionysius-divine-names', 'dionysius-divine-names', 'dionysius-celestial-hierarchy',
      'dionysius-celestial-hierarchy', 'dionysius-ecclesiastical-hierarchy', 'dionysius-mystical-theology',
      'dionysius-letters', 'dionysius-celestial-hierarchy', 'dionysius-divine-names',
      'dionysius-ecclesiastical-hierarchy',
    ]);
  });

  it('is not another Dionysius, or another author', () => {
    expect(cited('Dionysius of Alexandria', 'Ep.')).toBeNull();
    expect(cited('St. Thomas', 'In De div. nom.')).toBeNull();
    expect(cited('', 'Div. Nom.')).toBeNull();
  });
});
