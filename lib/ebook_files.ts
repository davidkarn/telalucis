// The books' .mobi files, kept in EBOOK_CACHE_DIR (output/ebooks when not set) once made: named
// after the book and a hash of its text, so one made before the book changed (imported again) or
// before the e-book's format changed (EBOOK_FORMAT) isn't served; making a new one deletes the
// book's old ones. The API serves them (api/ebooks.service.ts); the e-book worker makes them
// (lib/ebook_queue.ts), so both read the same folder.
import { createHash } from 'node:crypto';
import fs from 'node:fs/promises';
import path from 'node:path';
import type { BookText } from '../core/book_text.ts';
import { writeMobi } from './ebook.ts';

// Bumped when the e-books are made differently (core/book_text.ts, core/book_epub.ts), so the
// .mobi files made before are made again
const EBOOK_FORMAT = '1';

const cacheDir = () => process.env.EBOOK_CACHE_DIR ?? path.join('output', 'ebooks');

// A file name for the book: its title, in letters, digits and hyphens ("Summa-Theologiae.epub")
export const ebookFileName = (title: string, bookId: string, extension: string) => {
  const name = title.normalize('NFKD').replace(/[̀-ͯ]/g, '')
    .replace(/[^A-Za-z0-9]+/g, '-').replace(/^-|-$/g, '').slice(0, 80).replace(/-$/, '');
  return `${ name.length > 0 ? name : bookId.replace(/[^A-Za-z0-9_-]+/g, '_') }.${ extension }`;
};

// What a book's .mobi files are named from: its id made safe for a file name, with a hash of the
// id (two ids can be made the same)
const fileStem = (bookId: string) => (
  `${ bookId.replace(/[^A-Za-z0-9_-]+/g, '_').slice(0, 100) }-${ createHash('sha1').update(bookId).digest('hex').slice(0, 8) }`
);

// The .mobi file of the book's text as it is now
export const mobiPath = (text: BookText) => {
  const hash = createHash('sha256').update(EBOOK_FORMAT).update(JSON.stringify(text)).digest('hex').slice(0, 16);
  return path.join(cacheDir(), `${ fileStem(text.id) }.${ hash }.mobi`);
};

export const fileExists = (file: string) => fs.access(file).then(() => true, () => false);

// Convert the book to its .mobi file (mobiPath): written beside it first, so a file of that name
// is always whole; then the book's other .mobi files deleted
export const makeMobiFile = async(text: BookText): Promise<string> => {
  const file    = mobiPath(text);
  const dir     = path.dirname(file);
  const partial = file.replace(/\.mobi$/, '.partial.mobi');

  await fs.mkdir(dir, { recursive: true });
  await writeMobi(text, partial, { modified: new Date() });
  await fs.rename(partial, file);

  const stem = `${ fileStem(text.id) }.`;
  for (const old of await fs.readdir(dir)) {
    if (old.startsWith(stem) && old.endsWith('.mobi') && path.join(dir, old) !== file) {
      await fs.rm(path.join(dir, old), { force: true });
    }
  }
  return file;
};
