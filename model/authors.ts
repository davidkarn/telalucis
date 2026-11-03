// Authors (authors), the names they go by (alternate_author_names) and their
// books (book_authors; see core/authors.ts, migrations/0020_create_authors.ts
// and migrations/0028_create_book_authors.ts)
import { sql, type Kysely } from 'kysely';
import type { Database } from '../api/database.ts';
import { authorNames } from '../core/authors.ts';
import { likePattern, type DbExprBuilder, type DbSelectQuery } from './model_utils.ts';
import { CopyrightStatusCheckSql } from './copyright_status_checks.ts';

// Authors going by a name containing `searchQuery` (any of theirs: "aquinas" finds Thomas
// Aquinas), ignoring case; every author when it's empty
const matchingName = <O>(searchQuery: string | undefined) => (
  query: DbSelectQuery<'authors', O>
) => {
  if (searchQuery === undefined || searchQuery.trim().length === 0) {
    return query;
  }
  else {
    return query.where(({ exists, selectFrom }) => exists(
      selectFrom('alternate_author_names')
        .select('alternate_author_names.id')
        .whereRef('alternate_author_names.author_id', '=', 'authors.id')
        .where('alternate_author_names.name', 'ilike', likePattern(searchQuery.trim()))
    ));
  }
};

// Authors with a book in the collection shown in its searches (not likely under copyright)
const withBooks = <O>() => (query: DbSelectQuery<'authors', O>) => (
  query.where(({ exists, selectFrom }) => exists(
    selectFrom('book_authors').select('book_authors.book_id')
      .whereRef('book_authors.author_id', '=', 'authors.id')
      .where(CopyrightStatusCheckSql.isShownInSearches('book_authors.book_id'))
  ))
);

const sortedByName = <O>() => (query: DbSelectQuery<'authors', O>) => (
  query.orderBy('authors.name').orderBy('authors.id')
);

// The author's books shown in the collection's searches (not likely under copyright)
const bookCount = (eb: DbExprBuilder<'authors'>) => (
  eb.selectFrom('book_authors')
    .select(eb.fn.countAll<string>().as('n'))
    .whereRef('book_authors.author_id', '=', 'authors.id')
    .where(CopyrightStatusCheckSql.isShownInSearches('book_authors.book_id'))
    .as('book_count')
);

// Citations in other books of any of the author's books, pointing at it or one of its
// alternate ids
const citedByCount = (eb: DbExprBuilder<'authors'>) => (
  eb.selectFrom('citations')
    .select(eb.fn.countAll<string>().as('n'))
    .where('citations.reference_book_id', 'in', sql<string>`(
      select ba.book_id from book_authors ba where ba.author_id = authors.id
      union select alternate_ids.alternate_id from alternate_ids
        join book_authors ba on ba.book_id = alternate_ids.book_id
      where ba.author_id = authors.id)`)
    .as('cited_by_count')
);

// The id of the author going by `name` (ignoring case); null when none does
const findAuthorId = async(db: Kysely<Database>, name: string): Promise<string | null> => {
  const row = await db.selectFrom('alternate_author_names')
    .select('alternate_author_names.author_id')
    .where(sql`lower(alternate_author_names.name)`, '=', name.trim().toLowerCase())
    .executeTakeFirst();

  return row?.author_id ?? null;
};

// A new author named `name`, that name its first; its id
const createAuthor = async(db: Kysely<Database>, name: string): Promise<string> => {
  const author = await db.insertInto('authors')
    .values({ name })
    .returning('id')
    .executeTakeFirstOrThrow();

  await db.insertInto('alternate_author_names')
    .values({ author_id: author.id, name })
    .execute();

  return author.id;
};

// The ids of the authors a book's author (books.author) names, in order: each
// name's (see authorNames), the one going by it, else a new one named it.
// A name some author goes by as a whole is theirs, not split: one matched by
// hand ("Denzinger, Heinrich, 1819-1883; Stahl, Ignaz, 1839-1902"). None for
// "Anonymous".
const findOrCreateAuthors = async(db: Kysely<Database>, author: string): Promise<string[]> => {
  const whole = await findAuthorId(db, author);

  if (whole !== null) {
    return [whole];
  }
  else {
    const ids: string[] = [];
    for (const name of authorNames(author)) {
      ids.push(await findAuthorId(db, name) ?? await createAuthor(db, name));
    }
    return ids.filter((id, i) => ids.indexOf(id) === i);
  }
};

// Make a book's authors these, in order
const replaceBookAuthors = async(db: Kysely<Database>, bookId: string, authorIds: string[]) => {
  await db.deleteFrom('book_authors').where('book_id', '=', bookId).execute();
  if (authorIds.length > 0) {
    await db.insertInto('book_authors')
      .values(authorIds.map((authorId, position) => ({ book_id: bookId, author_id: authorId, position })))
      .execute();
  }
};

export const AuthorScopes = { matchingName, withBooks, sortedByName };
export const AuthorSelectors = { bookCount, citedByCount };
export const AuthorQueries = { findAuthorId };
// Merge authors who are one person ("M. J. Adler" into "Mortimer J. Adler"), in one
// transaction: the others' books and names become the kept one's, and the others are deleted. A
// book of both keeps the kept one's place among its authors. name: what to call the kept one, its
// name as it is when not given; it's among its names either way.
const mergeAuthors = (
  db: Kysely<Database>, keepId: string, otherIds: string[], name?: string,
) => db.transaction().execute(async(trx) => {
  const others = otherIds.filter((id) => id !== keepId);
  if (others.length > 0) {
    await sql`insert into book_authors (book_id, author_id, position)
      select book_id, ${ keepId }, position from book_authors
      where author_id in (${ sql.join(others) })
      on conflict (book_id, author_id) do nothing`.execute(trx);
    await trx.updateTable('alternate_author_names')
      .set({ author_id: keepId })
      .where('author_id', 'in', others)
      .execute();
    await trx.deleteFrom('authors').where('id', 'in', others).execute();
  }

  if (name !== undefined) {
    await trx.updateTable('authors').set({ name }).where('id', '=', keepId).execute();
    await sql`insert into alternate_author_names (author_id, name) values (${ keepId }, ${ name })
      on conflict ((lower(name))) do nothing`.execute(trx);
  }
});

export const AuthorActions = { findOrCreateAuthors, replaceBookAuthors, mergeAuthors };
