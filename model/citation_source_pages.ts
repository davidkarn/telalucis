// The page a citation's footnote is on: the citation's place in it, and the page's blocks (see
// core/citation_pages.ts).
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { SourceBlock } from '../core/citation_pages.ts';

// A citation's footnote block and identifier, with every block of the page the footnote is on in
// reading order; undefined when there is no such citation
export const findCitationSourcePage = async(
  db: Kysely<Database>, citationId: string
): Promise<{ bookId: string, blockId: string, identifier: string, blocks: SourceBlock[] } | undefined> => {
  const citation = await db.selectFrom('citations')
    .select([
      'citations.page_block_id', 'citations.source_book_id', 'citations.source_footnote_page',
      'citations.source_footnote_identifier',
    ])
    .where('citations.id', '=', citationId)
    .executeTakeFirst();

  if (!citation) {
    return undefined;
  }
  else {
    const blocks = await db.selectFrom('page_blocks')
      .select(['page_blocks.id', 'page_blocks.label', 'page_blocks.html'])
      .where('page_blocks.book_id', '=', citation.source_book_id)
      .where('page_blocks.page_number', '=', citation.source_footnote_page)
      .orderBy('page_blocks.position')
      .execute();

    return {
      bookId:     citation.source_book_id,
      blockId:    citation.page_block_id,
      identifier: citation.source_footnote_identifier,
      blocks,
    };
  }
};

// The body blocks (not footnotes, headers or footers) of the pages these citations' footnotes are
// on that may have a citation's footnote marker (see footnoteContext in core/citation_pages.ts), by
// citation id, in reading order; one query for them all
export const findFootnoteContextBlocks = async(
  db: Kysely<Database>, citationIds: string[]
): Promise<Map<string, SourceBlock[]>> => {
  const rows = citationIds.length === 0 ? [] : await db.selectFrom('citations')
    .innerJoin('page_blocks', (join) => join
      .onRef('page_blocks.book_id', '=', 'citations.source_book_id')
      .onRef('page_blocks.page_number', '=', 'citations.source_footnote_page'))
    .select([
      'citations.id as citation_id', 'page_blocks.id', 'page_blocks.label', 'page_blocks.html',
    ])
    .where('citations.id', 'in', citationIds)
    .where('citations.source_footnote_identifier', '<>', '')
    .where('page_blocks.label', 'not in', ['Footnote', 'PageHeader', 'PageFooter'])
    // the marker as a superscript ("<sup>8</sup>", "<sup>4)</sup>"); footnoteContext matches it
    // exactly
    .where(sql<boolean>`(
      strpos(page_blocks.html, '<sup>' || citations.source_footnote_identifier || '</sup>') > 0
      or strpos(page_blocks.html, '<sup>' || citations.source_footnote_identifier || ')</sup>') > 0
      or strpos(page_blocks.html, '<sup>' || citations.source_footnote_identifier || '.</sup>') > 0)`)
    .orderBy('citations.id')
    .orderBy('page_blocks.position')
    .execute();

  const byCitation = new Map<string, SourceBlock[]>();
  for (const { citation_id, ...block } of rows) {
    byCitation.set(citation_id, [...(byCitation.get(citation_id) ?? []), block]);
  }
  return byCitation;
};

// Cache citations' source snippets (citations.source_snippet), '' for one whose marker isn't found
export const saveSourceSnippets = async(
  db: Kysely<Database>, snippets: { citationId: string, snippet: string }[]
): Promise<void> => {
  if (snippets.length > 0) {
    await sql`
      update citations set source_snippet = s.snippet
      from unnest(
        ${ snippets.map((s) => s.citationId) }::bigint[],
        ${ snippets.map((s) => s.snippet) }::text[]
      ) as s(citation_id, snippet)
      where citations.id = s.citation_id`.execute(db);
  }
};

// Up to `limit` citations whose source snippet isn't cached yet, by id
export const findCitationsWithoutSnippet = (db: Kysely<Database>, limit: number) => (
  db.selectFrom('citations')
    .select(['citations.id', 'citations.source_footnote_identifier'])
    .where('citations.source_snippet', 'is', null)
    .orderBy('citations.id')
    .limit(limit)
    .execute()
);
