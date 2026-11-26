import { describe, expect, it } from '@jest/globals';
import { usageColumns } from './llm_requests.ts';

describe('usageColumns', () => {
  it("keeps the tokens and cost OpenRouter reports", () => {
    expect(usageColumns({
      prompt_tokens:             5210,
      completion_tokens:         812,
      cost:                      0.0091234,
      prompt_tokens_details:     { cached_tokens: 1024 },
      completion_tokens_details: { reasoning_tokens: 0 },
    })).toEqual({
      prompt_tokens: 5210, completion_tokens: 812, reasoning_tokens: 0, cached_tokens: 1024, cost: 0.0091234,
    });
  });

  it('leaves out what was not reported, or not a count', () => {
    expect(usageColumns(null)).toEqual({
      prompt_tokens: null, completion_tokens: null, reasoning_tokens: null, cached_tokens: null, cost: null,
    });
    expect(usageColumns({ prompt_tokens: -1, cost: Number.NaN })).toMatchObject({ prompt_tokens: null, cost: null });
  });
});
