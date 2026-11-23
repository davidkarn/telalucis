import { describe, expect, it } from '@jest/globals';
import { RRF_K } from './passage_search.ts';
import {
  fusedBlocks, MAX_QUERY_LENGTH, passageBlock, passageHoldsBlock, searchTerm, type PassageBlock,
} from './block_search.ts';

describe('fusedBlocks', () => {
  it('puts a block found both ways first, then by rank', () => {
    const fused = fusedBlocks({ words: ['a', 'b', 'c'], meaning: ['d', 'c'] });

    expect(fused.map((f) => f.blockId)).toEqual(['c', 'a', 'd', 'b']);
    expect(fused[0]).toEqual({
      blockId: 'c', score: 1 / (RRF_K + 2) + 1 / (RRF_K + 1), foundBy: ['words', 'meaning'],
    });
    expect(fused[1]!.foundBy).toEqual(['words']);
  });

  it('counts a block once per list', () => {
    expect(fusedBlocks({ words: ['a', 'a'], meaning: [] })).toEqual([
      { blockId: 'a', score: 1 / RRF_K, foundBy: ['words'] },
    ]);
  });

  it('is empty with nothing found', () => {
    expect(fusedBlocks({ words: [], meaning: [] })).toEqual([]);
  });
});

const block = (blockId: string, html: string, wordScore = 0, pageNumber = 1, position = 0): PassageBlock => (
  { blockId, html, wordScore, pageNumber, position }
);

describe('passageHoldsBlock and passageBlock', () => {
  const before  = block('1', '<p>The end of the passage before this one, on the same page as its start.</p>');
  const first   = block('2', '<p>Grace perfects nature<sup>3</sup> and does not destroy it, as the Doctor says.</p>', 0, 1, 1);
  const second  = block('3', '<p>For the natural law is a participation of the eternal law in the rational creature.</p>', 0.4, 1, 2);
  const passage = 'Grace perfects nature and does not destroy it, as the Doctor says.\n\n'
    + 'For the natural law is a participation of the eternal law in the rational creature.';

  it("finds the passage's blocks by their start or end, footnote markers left out", () => {
    expect(passageHoldsBlock('gracepe' + 'rfectsnatureanddoesnotdestroyitasthedoctorsays', first.html)).toBe(true);
    expect(passageHoldsBlock('somethingelse', first.html)).toBe(false);
    expect(passageHoldsBlock('anything', '<p><sup>1</sup></p>')).toBe(false);
  });

  it('takes the block it holds with the most words of the search', () => {
    expect(passageBlock(passage, [before, first, second])?.blockId).toBe('3');
  });

  it('takes its first block without any of them, and none of another passage', () => {
    expect(passageBlock(passage, [before, { ...second, wordScore: 0 }, first])?.blockId).toBe('2');
    expect(passageBlock(passage, [before])).toBeNull();
  });

  it('finds a block broken across a page by its start', () => {
    const broken = block('4', '<p>For the natural law is a participation of the eter-</p>');
    expect(passageBlock(passage, [broken])?.blockId).toBe('4');
  });
});

describe('searchTerm', () => {
  it('collapses spaces and refuses an empty or overlong term', () => {
    expect(searchTerm('  natural   law ')).toEqual({ term: 'natural law' });
    expect(searchTerm('  ')).toHaveProperty('error');
    expect(searchTerm(undefined)).toHaveProperty('error');
    expect(searchTerm(['a'])).toHaveProperty('error');
    expect(searchTerm('x'.repeat(MAX_QUERY_LENGTH + 1))).toHaveProperty('error');
  });
});
