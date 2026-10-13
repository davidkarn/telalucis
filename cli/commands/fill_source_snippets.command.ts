// Cache the source snippet of every citation that hasn't one yet (citations.source_snippet: the
// citing book's words around its footnote's marker, see footnoteContext in
// core/citation_pages.ts), so no page's "Cited by" list has to work its citations' out when it's
// first shown. A batch at a time; run again, it does only those still missing.
//   npm run cli -- fill-source-snippets
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { footnoteContext } from '../../core/citation_pages.ts';
import {
  findCitationsWithoutSnippet, findFootnoteContextBlocks, saveSourceSnippets,
} from '../../model/citation_source_pages.ts';

const BATCH = 1000;

@Command({
  name:        'fill-source-snippets',
  description: "Cache every citation's source snippet (the citing text around its footnote marker)",
})
export class FillSourceSnippetsCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  async run(): Promise<void> {
    const started = Date.now();
    let done      = 0;
    let found     = 0;

    for (;;) {
      const citations = await findCitationsWithoutSnippet(this.db, BATCH);
      if (citations.length === 0) {
        break;
      }

      const blocks   = await findFootnoteContextBlocks(this.db, citations.map((c) => c.id));
      const snippets = citations.map((c) => ({
        citationId: c.id,
        snippet:    footnoteContext(blocks.get(c.id) ?? [], c.source_footnote_identifier) ?? '',
      }));
      await saveSourceSnippets(this.db, snippets);

      done  += snippets.length;
      found += snippets.filter((s) => s.snippet.length > 0).length;
      console.log(`${ done } citations (${ found } with a snippet), ${ Math.round((Date.now() - started) / 1000) }s`);
    }

    console.log(`done: ${ done } citations, ${ found } with a snippet, ${ done - found } whose marker wasn't found`);
  }
}
