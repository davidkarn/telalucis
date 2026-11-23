// Searching the collection's blocks of text (GET /search): by a search term's
// words and by its meaning, merged (see lib/block_search.ts)
import { BadRequestException, Controller, Get, Inject, Query } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { searchTerm } from '../core/block_search';
import { searchBlocks } from '../lib/block_search';
import { DB } from './database.module';
import type { Database } from './database';
import { paging_params } from './params';

@Controller('search')
export class SearchController {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {}

  @Get()
  search(
    @Query('q') q?: unknown,
    @Query('offset') offset?: unknown,
    @Query('length') length?: unknown,
  ) {
    const term = searchTerm(q);

    if ('error' in term) {
      throw new BadRequestException(term.error);
    }
    else {
      return searchBlocks(this.db, term.term, paging_params(offset, length));
    }
  }
}
