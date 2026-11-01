import { describe, expect, it } from '@jest/globals';
import {
  aquinasPages, blockCitations, bracketReference, bracketWorks, contentsTargets, divisionsIn,
  englishColumn, isLatin, labelOf, markScripture, numberOf, textOf, withProseCitations, wpFootnotes,
  type AquinasWork,
} from './aquinas_html.ts';

const source = { bookId: 'book', footnotePage: 1 };
const cited  = (html: string, bible?: string) => blockCitations(html, source, 1, bible).footnotes
  .map((f) => f.citations.map((c) => `${ c.author }|${ c.title }|${ c.location }`));

describe('numberOf', () => {
  it('reads digits, Roman numerals and spelled numbers', () => {
    expect([numberOf('14'), numberOf('XIV'), numberOf('xiv'), numberOf('Twenty-Nine'), numberOf('Physics')])
      .toEqual([14, 14, 14, 29, null]);
  });
});

describe('divisionsIn', () => {
  it('reads the divisions a work is cited by, the largest first', () => {
    expect(divisionsIn('BOOK I LECTURE 3', ['book', 'lecture'])).toEqual([
      { type: 'book', value: 1 }, { type: 'lecture', value: 3 },
    ]);
    expect(divisionsIn('Question One: Truth', ['question', 'article'])).toEqual([{ type: 'question', value: 1 }]);
    expect(divisionsIn('The Second Lesson: Satan\'s Request', ['chapter', 'lecture'])).toEqual([
      { type: 'lecture', value: 2 },
    ]);
  });

  it('reads a commentary on scripture\'s "chapter-lecture" entries', () => {
    expect(divisionsIn('1-2: 1 Cor 1:10-17a', ['chapter', 'lecture'])).toEqual([
      { type: 'chapter', value: 1 }, { type: 'lecture', value: 2 },
    ]);
  });

  it('leaves out divisions the work isn\'t cited by (Aristotle\'s chapters in a lecture)', () => {
    expect(divisionsIn('Chapter 6', ['book', 'lecture'])).toEqual([]);
  });
});

describe('labelOf', () => {
  it('writes books in Roman numerals', () => {
    expect(labelOf([{ type: 'book', value: 3 }, { type: 'lecture', value: 4 }])).toBe('Book III, Lecture 4');
  });
});

describe('wpFootnotes', () => {
  it('reads the footnotes kept as JavaScript strings, continued over lines', () => {
    const html = "var WPFootnote1 = 'Cf. Summa I-II, 105, 1.\\\nCarlyle\\'s view.\\\n'";
    expect(wpFootnotes(html).get('WPFootnote1')).toBe("Cf. Summa I-II, 105, 1. Carlyle's view. ");
  });
});

describe('isLatin', () => {
  it('tells Latin paragraphs from English ones', () => {
    expect(isLatin('Unde Philosophus dicit in tertio De anima quod movens organice est ubi est principium et finis'))
      .toBe(true);
    expect(isLatin('Hence the Philosopher says in the third book of On the Soul that the organic mover is where'))
      .toBe(false);
  });
});

describe('englishColumn', () => {
  it('drops the first cell (the Latin) of rows of two', () => {
    expect(englishColumn('<table><tr><td>Latina</td><td>English</td></tr><tr><td>Heading</td></tr></table>'))
      .toBe('<table><tr><td>English</td></tr><tr><td>Heading</td></tr></table>');
  });
});

describe('contentsTargets', () => {
  it('reads the contents\' links, not those to footnotes', () => {
    const html = '<a href="Work.htm#1">What is truth?</a> <a href="#2">2</a> <a href="Other.htm#3">x</a>'
      + ' text<sup><a href="#f3">[3]</a></sup>';
    expect([...contentsTargets(html, 'Work.htm')]).toEqual([['1', 'What is truth?'], ['2', '2']]);
  });
});

describe('markScripture', () => {
  it('marks references with a book, and those after it in the same parenthesis', () => {
    expect(markScripture('(Rom. 5:12; 6:3)')).toBe(
      '(<scripRef passage="Rom 5:12">Rom. 5:12</scripRef>; <scripRef passage="Rom 6:3">6:3</scripRef>)'
    );
  });

  it('takes a bare reference to be in the book named just before it, else the one commented on', () => {
    expect(cited('as Isaiah says (40:18)')).toEqual([['Bible|Isaias|40:18']]);
    expect(cited('as was said below (14:28)', 'John')).toEqual([['Bible|John|14:28']]);
    expect(cited('as was said below (14:28)')).toEqual([]);
  });

  it('reads references in brackets, Roman chapters, and books printedBookNumber doesn\'t know', () => {
    expect(cited('“sin” [Rom 14:23]')).toEqual([['Bible|Romans|14:23']]);
    expect(cited('(1 Tim. vi. 8)')).toEqual([['Bible|1 Timothy|6:8']]);
    expect(cited('(Sirach 14:22)')).toEqual([['Bible|Ecclesiasticus|14:22']]);
  });

  it('leaves works cited by book and chapter alone', () => {
    expect(cited('(Ethic. ii, 1)')).toEqual([['|Ethic.|ii, 1']]);
  });
});

describe('bracketReference and bracketWorks', () => {
  it('moves a book\'s number written before its title after it, and drops "Cf."', () => {
    expect(bracketReference('Cf. S.T., I. 16; I Sent., 19, 5, 1', '')).toBe('S.T., I. 16; Sent. I, 19, 5, 1');
  });

  it('gives a bracket that is only a location the title before it', () => {
    expect(bracketReference('VIII, 5', 'as is proved in the Physics')).toBe('Physics VIII, 5');
    expect(bracketReference('VIII, 5', 'he shows this in two ways. First')).toBe('VIII, 5');
  });

  it('reads abbreviated titles in capitals, and places in the work before', () => {
    expect(bracketWorks('C.G., I, 60; ii, 5')).toEqual([
      { author: '', title: 'C.G.', location: 'I, 60' },
      { author: '', title: 'C.G.', location: 'ii, 5' },
    ]);
  });
});

describe('blockCitations', () => {
  it('numbers the footnotes of parentheses, brackets and notes in the order they appear', () => {
    const found = blockCitations(
      'He says [Cf. <i>S.T.</i>, I. 16] and (Wis 7:27)<note>See (Gen 1:1).</note> too.', source, 4,
    );
    expect(found.html).toBe('He says [Cf. <i>S.T.</i>, I. 16]<sup>4</sup> and (Wis 7:27)<sup>5</sup><sup>6</sup> too.');
    expect(found.footnotes.map((f) => [f.identifier, f.raw, f.citations.map((c) => c.title)])).toEqual([
      ['4', 'S.T., I. 16', ['S.T.']],
      ['5', 'Wis 7:27', ['Wisdom']],
      ['6', 'See (Gen 1:1).', ['Genesis']],
    ]);
  });

  it('passes over Bekker numbers and outline letters', () => {
    expect(cited('as he says (1094 a 1) and (C 6)')).toEqual([]);
    expect(blockCitations('(1094 a 1)', source, 1).html).toBe('(1094 a 1)');
  });
});

const work = (levels: AquinasWork['levels'], extra: Partial<AquinasWork> = {}): AquinasWork => ({
  id: 'work', title: 'Work', translator: null, files: [], levels, ...extra,
});
const page = (w: AquinasWork, html: string, parts?: { type: string, value: number }[]) => (
  aquinasPages(w, [{ file: { path: 'english/Work.htm', ...(parts ? { parts } : {}) }, html }])
);

describe('aquinasPages', () => {
  it('starts a page at each anchor the contents link to, once, cited by its file\'s and its own divisions', () => {
    const html  = `<h2>Question Two</h2>
      <p><a href="Work.htm#1">Does God know?</a><p><a href="Work.htm#2">Does God understand?</a>
      <a name="1"></a><p>ARTICLE I<p>Text of the first article (Wis 7:27).
      <a name="1"></a><p>More of the first.
      <a name="2"></a><p>ARTICLE II<p>Text of the second.`;
    const pages = page(work(['question', 'article']), html, [{ type: 'question', value: 2 }]);

    expect(pages.map((p) => [p.printedPageNumber, p.citationParts])).toEqual([
      ['Question 2, Article 1', [{ type: 'question', value: 2 }, { type: 'article', value: 1 }]],
      ['Question 2, Article 2', [{ type: 'question', value: 2 }, { type: 'article', value: 2 }]],
    ]);
    expect(pages[0]!.blocks.map((b) => `${ b.label }: ${ textOf(b.html) }`)).toEqual([
      'PageHeader: Work',
      'PageHeader: Question 2',
      'SectionHeader: ARTICLE I',
      'Text: Text of the first article (Wis 7:27)1.',
      'Text: More of the first.',
      'Footnote: 1 Wis 7:27',
    ]);
  });

  it('splits a file without anchors at headings naming its divisions', () => {
    const pages = page(work(['lecture']), '<p>LECTURE 1<p>One.<p>Chapter 6<p>Still one.<p>LECTURE 2<p>Two.');
    expect(pages.map((p) => [p.printedPageNumber, p.blocks.length])).toEqual([['Lecture 1', 5], ['Lecture 2', 3]]);
  });

  it('cites a division by its first page only', () => {
    const long  = `<p>${ 'word '.repeat(1500) }</p>`;
    const pages = page(work(['lecture']), `<p>LECTURE 1${ long }${ long }${ long }`);
    expect(pages.map((p) => [p.printedPageNumber, p.citationParts.length])).toEqual([
      ['Lecture 1', 1], ['Lecture 1 (2)', 0], ['Lecture 1 (3)', 0],
    ]);
  });

  it('turns a translator\'s footnote into a footnote', () => {
    const html   = "<script>var WPFootnote1 = 'Cf. (Gen 1:1).'</script>"
      + '<p>Text.<a href="javascript:WPShow(\'WPFootnote1\', WPFootnote1 )"><img src="f.gif"></a> More.';
    const blocks = page(work([]), html)[0]!.blocks;
    expect(blocks.map((b) => [b.label, textOf(b.html), b.citations.length])).toEqual([
      ['PageHeader', 'Work', 0],
      ['Text', 'Text.1 More.', 0],
      ['Footnote', '1 Cf. (Gen 1:1).', 1],
    ]);
  });
});

describe('withProseCitations', () => {
  it('puts each citation on the text block it quotes or that names its work, leaving out the others', () => {
    const [first] = page(work([]), '<p>He spoke.<p>Then, the Philosopher says in Book VIII of the Physics, motion is eternal.');
    const placed  = withProseCitations(first!, 'work', {
      citations: [
        { author: 'Aristotle', title: 'Physics', location: 'Book VIII', raw: 'the Philosopher says in Book VIII of the Physics', locationsCited: [] },
        { author: 'Avicenna', title: '', location: '', raw: 'not on the page', locationsCited: [] },
        { author: 'Augustine', title: 'De Trinitate', location: '', raw: 'Augustine says in On the Trinity', locationsCited: [] },
        { author: 'Aristotle', title: 'Physics', location: '', raw: 'as he says in the Physics', locationsCited: [] },
        { author: 'Aristotle', title: 'Physics', location: '', raw: 'As the Philosopher says in Book VIII', locationsCited: [] },
      ],
    });
    expect(placed.blocks.map((b) => b.citations.map((c) => c.author))).toEqual([[], [], ['Aristotle', 'Aristotle', 'Aristotle']]);
    expect(placed.blocks[2]!.citations[0]!.source).toEqual({ bookId: 'work', footnoteIdentifier: '', footnotePage: 1 });
  });
});
