// The passages of the books' body text, with their embeddings (passages; see core/passages.ts and
// migrations/0030_create_passages.ts): the books still to cut into passages, their body blocks,
// saving a book's passages, and those still to embed and their embeddings.
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import type { Passage, PassageSourcePage } from '../core/passages.ts';

// Rows per insert, well under Postgres's 65535 parameters
const CHUNK = 1000;

// The ids of the books with body text (Text or SectionHeader blocks) and no passages, in id order;
// the first `limit` of them, or all
const findBooksWithoutPassages = (db: Kysely<Database>, limit?: number) => {
  const query = db.selectFrom('books')
    .select('books.id')
    .where(({ exists, not, selectFrom }) => exists(
      selectFrom('page_blocks').select('page_blocks.id')
        .whereRef('page_blocks.book_id', '=', 'books.id')
        .where('page_blocks.label', 'in', ['Text', 'SectionHeader']),
    ).and(not(exists(
      selectFrom('passages').select('passages.id').whereRef('passages.book_id', '=', 'books.id'),
    ))))
    .orderBy('books.id');

  return (limit === undefined ? query : query.limit(limit)).execute().then((rows) => rows.map((r) => r.id));
};

// A book's pages with their body blocks (the rest left out), in order
const findPassageSource = async(db: Kysely<Database>, bookId: string): Promise<PassageSourcePage[]> => {
  const rows = await db.selectFrom('page_blocks')
    .select(['page_blocks.page_number', 'page_blocks.label', 'page_blocks.html'])
    .where('page_blocks.book_id', '=', bookId)
    .where('page_blocks.label', 'in', ['Text', 'SectionHeader'])
    .orderBy('page_blocks.page_number')
    .orderBy('page_blocks.position')
    .execute();

  return rows.reduce<PassageSourcePage[]>((pages, r) => {
    const last = pages.at(-1);
    return last?.pageNumber === r.page_number
      ? [...pages.slice(0, -1), { ...last, blocks: [...last.blocks, { label: r.label, html: r.html }] }]
      : [...pages, { pageNumber: r.page_number, blocks: [{ label: r.label, html: r.html }] }];
  }, []);
};

// Replace a book's passages with these (their embeddings with them)
const replaceBookPassages = (db: Kysely<Database>, bookId: string, passages: Passage[]) => (
  db.transaction().execute(async(trx) => {
    await trx.deleteFrom('passages').where('passages.book_id', '=', bookId).execute();

    for (let i = 0; i < passages.length; i += CHUNK) {
      await trx.insertInto('passages')
        .values(passages.slice(i, i + CHUNK).map((p) => ({
          book_id:         bookId,
          page_number:     p.pageNumber,
          end_page_number: p.endPageNumber,
          position:        p.position,
          text:            p.text,
          word_count:      p.wordCount,
        })))
        .execute();
    }
  })
);

// The next `limit` passages with no embedding, in id order, with their books' titles and authors
const findPassagesToEmbed = (db: Kysely<Database>, limit: number) => (
  db.selectFrom('passages')
    .innerJoin('books', 'books.id', 'passages.book_id')
    .select(['passages.id', 'passages.text', 'books.title', 'books.author'])
    .where('passages.embedding', 'is', null)
    .orderBy('passages.id')
    .limit(limit)
    .execute()
);

// How many passages there are, and how many have an embedding
const countPassages = async(db: Kysely<Database>) => {
  const row = await db.selectFrom('passages')
    .select((eb) => [
      eb.fn.countAll<string>().as('passages'),
      eb.fn.count<string>('passages.embedding').as('embedded'),
    ])
    .executeTakeFirstOrThrow();

  return { passages: Number(row.passages), embedded: Number(row.embedded) };
};

// Save passages' embeddings, from `model`
const saveEmbeddings = async(
  db: Kysely<Database>, model: string, embeddings: { id: string, embedding: number[] }[],
) => {
  if (embeddings.length > 0) {
    const values = sql.join(embeddings.map((e) => sql`(${ e.id }::bigint, ${ `[${ e.embedding.join(',') }]` }::halfvec)`));

    await sql`
      update passages set embedding = v.embedding, embedding_model = ${ model }
      from (values ${ values }) as v(id, embedding)
      where passages.id = v.id
    `.execute(db);
  }
};

// Forget every embedding that isn't from `model`, for them to be worked out again with it
const clearOtherModelsEmbeddings = (db: Kysely<Database>, model: string) => (
  db.updateTable('passages')
    .set({ embedding: null, embedding_model: null })
    .where('passages.embedding_model', '!=', model)
    .executeTakeFirst()
    .then((r) => Number(r.numUpdatedRows))
);

export const PassageQueries = { findBooksWithoutPassages, findPassageSource, findPassagesToEmbed, countPassages };
export const PassageActions = { replaceBookPassages, saveEmbeddings, clearOtherModelsEmbeddings };
