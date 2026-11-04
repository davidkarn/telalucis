import { describe, expect, it } from '@jest/globals';
import { bookEpubFiles } from './book_epub.ts';
import { bookText, type BookSource } from './book_text.ts';

const source: BookSource = {
  id:     'hugh-de-sacramentis',
  title:  'On the Sacraments <De Sacramentis>',
  author: 'Hugh of Saint Victor',
  pages:  [
    { pageNumber:        1,
      printedPageNumber: '3',
      blocks:            [
        { label: 'SectionHeader', html: '<h2>I. What must be learned & why<sup>2</sup></h2>' },
        { label: 'Text', html: '<p>He who would learn<sup>1</sup> must first</p>' },
        { label: 'Footnote', html: '<p><sup>1</sup> Cf. Rom. 1, 20.</p>' },
        { label: 'Footnote', html: '<p><sup>2</sup> The traditional heading.</p>' },
      ] },
    { pageNumber:        2,
      printedPageNumber: '4',
      blocks:            [
        { label: 'Text', html: '<p>know the order of things.</p>' },
        { label: 'SectionHeader', html: '<h2>II. Of the works of God</h2>' },
        { label: 'Text', html: '<p>The works of God are two.</p>' },
      ] },
  ],
  pageParts: [
    { pageNumber: 1, parts: [{ type: 'chapter', value: 1 }] },
    { pageNumber: 2, parts: [{ type: 'chapter', value: 1 }] }, { pageNumber: 2, parts: [{ type: 'chapter', value: 2 }] },
  ],
};

const files   = bookEpubFiles(bookText(source), { modified: new Date('2026-11-03T12:00:00.123Z') });
const content = (path: string) => files.find((f) => f.path === path)?.content ?? '';

describe('bookEpubFiles', () => {
  it('starts with the mimetype, and has a file per section', () => {
    expect(files.map((f) => f.path)).toEqual([
      'mimetype', 'META-INF/container.xml', 'OEBPS/content.opf', 'OEBPS/toc.ncx', 'OEBPS/nav.xhtml',
      'OEBPS/title.xhtml', 'OEBPS/style.css', 'OEBPS/section-0000.xhtml', 'OEBPS/section-0001.xhtml',
    ]);
    expect(content('mimetype')).toBe('application/epub+zip');
  });

  it("describes the book, escaping its title, with a spine in reading order", () => {
    const opf = content('OEBPS/content.opf');
    expect(opf).toContain('<dc:title>On the Sacraments &lt;De Sacramentis&gt;</dc:title>');
    expect(opf).toContain('<dc:creator>Hugh of Saint Victor</dc:creator>');
    expect(opf).toContain('<meta property="dcterms:modified">2026-11-03T12:00:00Z</meta>');
    expect([...opf.matchAll(/<itemref idref="([^"]+)"/g)].map((m) => m[1])).toEqual(['title-page', 'nav', 's0', 's1']);
  });

  it('lists the sections in the contents, by title and the heading they start with', () => {
    expect(content('OEBPS/nav.xhtml')).toContain(
      '<li><a href="section-0000.xhtml">Chapter 1: I. What must be learned &amp; why</a></li>',
    );
    expect(content('OEBPS/toc.ncx')).toContain('<navPoint id="nav-1" playOrder="2"><navLabel><text>Chapter 2: II. Of the works of God</text>');
  });

  it("gives a section its title and heading, its paragraphs, and its footnotes linked both ways", () => {
    const section = content('OEBPS/section-0000.xhtml');
    expect(section).toContain(
      '<h1>Chapter 1<br/>I. What must be learned &amp; why<a epub:type="noteref" class="noteref" id="ref-2" href="#fn-2">2</a></h1>',
    );
    expect(section).not.toContain('<p class="heading">');
    expect(section).toContain(
      '<p>He who would learn<a epub:type="noteref" class="noteref" id="ref-1" href="#fn-1">1</a> must first know the order of things.</p>',
    );
    expect(section).toContain('<aside epub:type="footnote" class="footnote" id="fn-1"><p><a href="#ref-1">1</a>. Cf. Rom. 1, 20.</p></aside>');
    expect(section).toContain('<aside epub:type="footnote" class="footnote" id="fn-2"><p><a href="#ref-2">2</a>. The traditional heading.</p></aside>');
  });
});
