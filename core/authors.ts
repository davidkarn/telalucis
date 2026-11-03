// Authors (the authors table) and the names they go by
// (alternate_author_names). Pure functions; model/authors.ts reads and
// saves them.

// What books give as the author of a book that has none, or many (a
// journal's issue)
const NO_AUTHOR = ['anonymous', 'bible', 'various'];

// Whether a book's author (books.author) names an author: not "Anonymous",
// the Bible, or nothing
export const namesAnAuthor = (author: string): boolean => {
  const name = author.trim().toLowerCase();
  return name.length > 0 && !NO_AUTHOR.includes(name);
};

// Between the names of a book's authors: "Yves R. Simon and Karl Menger",
// "Lewis & Langford". Not a comma, which is also within a name ("Walter
// Farrell, O. P.") or a catalogue form ("Humphrey, William, 1839-1910").
const BETWEEN_AUTHORS = /\s+(?:and|&)\s+/i;

// The names of the authors a book's author (books.author) names, in order,
// once each (ignoring case): one for most books, several for "Mortimer J.
// Adler and Walter Farrell, O. P.", none for "Anonymous" (see namesAnAuthor)
export const authorNames = (author: string): string[] => (
  author.split(BETWEEN_AUTHORS)
    .map((name) => name.replace(/\s+/g, ' ').trim())
    .filter(namesAnAuthor)
    .filter((name, i, all) => all.findIndex((n) => n.toLowerCase() === name.toLowerCase()) === i)
);
