import { Kysely, sql } from 'kysely';

// A book's authors (book_authors), in order (position, from 0), in place of
// its one author (books.author_id): "Mortimer J. Adler and Walter Farrell,
// O. P." is a book of each. books.author still names them as the book's
// source gives it.
//
// Each book's authors are taken from books.author, split at "and" or "&"
// (see authorNames in core/authors.ts): each name found among the names
// authors go by (alternate_author_names, ignoring case), else added as a new
// author. A book naming one author keeps the one it had (some were matched by
// hand: "Denzinger, Heinrich, 1819-1883; Stahl, Ignaz, 1839-1902" is Heinrich
// Denzinger's). The authors named after several ("M. J. Adler and Walter
// Farrell"), left without books, are deleted, and such names of other
// authors ("Joseph Wilhelm and Thomas B. Scannell", one of Joseph Wilhelm's)
// with them.

// What books give as the author of a book that has none, or many
const NO_AUTHOR = ['anonymous', 'bible', 'various'];

const BETWEEN_AUTHORS = /\s+(?:and|&)\s+/i;

const authorNames = (author: string): string[] => (
  author.split(BETWEEN_AUTHORS)
    .map((name) => name.replace(/\s+/g, ' ').trim())
    .filter((name) => name.length > 0 && !NO_AUTHOR.includes(name.toLowerCase()))
    .filter((name, i, all) => all.findIndex((n) => n.toLowerCase() === name.toLowerCase()) === i)
);

const isSeveral = (name: string) => authorNames(name).length > 1;

export async function up(db: Kysely<unknown>): Promise<void> {
  await sql`create table book_authors (
    book_id text not null references books(id) on delete cascade,
    author_id bigint not null references authors(id) on delete cascade,
    position integer not null,
    created_at timestamptz not null default now(),
    primary key (book_id, author_id)
  )`.execute(db);
  await sql`create index book_authors_author_id_idx on book_authors (author_id)`.execute(db);

  const books = (await sql<{ id: string, author: string, author_id: string | null }>`
    select id, author, author_id from books order by id`.execute(db)).rows;

  const authorId = async(name: string): Promise<string> => {
    const { rows: [found] } = await sql<{ author_id: string }>`
      select author_id from alternate_author_names where lower(name) = ${ name.toLowerCase() }`.execute(db);
    if (found) {
      return found.author_id;
    }
    else {
      const { rows: [author] } = await sql<{ id: string }>`
        insert into authors (name) values (${ name }) returning id`.execute(db);
      await sql`insert into alternate_author_names (author_id, name) values (${ author!.id }, ${ name })`
        .execute(db);
      return author!.id;
    }
  };

  for (const book of books) {
    const names = authorNames(book.author);
    const ids   = names.length > 1
      ? await Promise.all(names.map(authorId))
      : book.author_id === null ? [] : [book.author_id];
    // one row to an author: two names of one author ("Lewis and Lewes") are one
    const own = ids.filter((id, i) => ids.indexOf(id) === i);

    for (const [position, id] of own.entries()) {
      await sql`insert into book_authors (book_id, author_id, position)
        values (${ book.id }, ${ id }, ${ position })`.execute(db);
    }
  }

  // the authors named after several, now without books, and such names
  const combined = (await sql<{ id: string, name: string }>`
    select a.id, a.name from authors a
    where not exists (select 1 from book_authors ba where ba.author_id = a.id)`.execute(db)).rows
    .filter((a) => isSeveral(a.name));
  if (combined.length > 0) {
    await sql`delete from authors where id in (${ sql.join(combined.map((a) => a.id)) })`.execute(db);
  }
  const names = (await sql<{ id: string, name: string }>`
    select id, name from alternate_author_names`.execute(db)).rows.filter((n) => isSeveral(n.name));
  if (names.length > 0) {
    await sql`delete from alternate_author_names where id in (${ sql.join(names.map((n) => n.id)) })`
      .execute(db);
  }

  await sql`alter table books drop column author_id`.execute(db);
}

// Each book back to one author, its first; the authors named after several
// aren't added back
export async function down(db: Kysely<unknown>): Promise<void> {
  await sql`alter table books
    add column author_id bigint references authors(id) on delete set null`.execute(db);
  await sql`create index books_author_id_idx on books (author_id)`.execute(db);
  await sql`update books set author_id = ba.author_id
    from book_authors ba where ba.book_id = books.id and ba.position = 0`.execute(db);
  await sql`drop table book_authors`.execute(db);
}
