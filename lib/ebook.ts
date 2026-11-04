// A book's text (core/book_text.ts) as an e-book file: an EPUB (core/book_epub.ts), zipped here, or
// a MOBI for older Kindles, converted from the EPUB by Calibre's ebook-convert
// (https://calibre-ebook.com; on a Mac: brew install --cask calibre). EBOOK_CONVERT_BIN: where
// ebook-convert is, when it isn't on the PATH or in Calibre's app.
import { spawn } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { strToU8, zipSync, type Zippable } from 'fflate';
import { bookEpubFiles, type EpubOptions } from '../core/book_epub.ts';
import type { BookText } from '../core/book_text.ts';

const CALIBRE_MAC = '/Applications/calibre.app/Contents/MacOS/ebook-convert';

const ebookConvert = async() => (
  process.env.EBOOK_CONVERT_BIN
    ?? await fs.access(CALIBRE_MAC).then(() => CALIBRE_MAC, () => 'ebook-convert')
);

// The book as an EPUB's bytes: its mimetype first and stored, as the format requires, the rest
// compressed
export const epubBytes = (book: BookText, options: EpubOptions): Uint8Array => {
  const files = bookEpubFiles(book, options);
  const tree  = Object.fromEntries(files.map((f) => [
    f.path, f.path === 'mimetype' ? [strToU8(f.content), { level: 0 }] : strToU8(f.content),
  ])) as Zippable;

  return zipSync(tree, { level: 9 });
};

export const writeEpub = (book: BookText, file: string, options: EpubOptions) => (
  fs.writeFile(file, epubBytes(book, options))
);

const run = (command: string, args: string[]) => new Promise<void>((resolve, reject) => {
  const child = spawn(command, args, { stdio: ['ignore', 'ignore', 'pipe'] });
  let stderr  = '';
  child.stderr.on('data', (d) => {
    stderr += String(d);
  });
  child.on('error', (e) => reject(
    (e as NodeJS.ErrnoException).code === 'ENOENT'
      ? new Error(`${ command } not found: install Calibre (brew install --cask calibre) or set EBOOK_CONVERT_BIN`)
      : e,
  ));
  child.on('exit', (code) => (
    code === 0 ? resolve() : reject(new Error(`${ command } exited with ${ code }: ${ stderr.trim().split('\n').slice(-3).join(' ') }`))
  ));
});

// The book as a MOBI file: made an EPUB in a scratch folder, converted by ebook-convert
export const writeMobi = async(book: BookText, file: string, options: EpubOptions) => {
  const dir  = await fs.mkdtemp(path.join(os.tmpdir(), 'ebook-'));
  const epub = path.join(dir, 'book.epub');

  try {
    await writeEpub(book, epub, options);
    await run(await ebookConvert(), [epub, path.resolve(file), '--output-profile', 'kindle']);
  }
  finally {
    await fs.rm(dir, { recursive: true, force: true });
  }
};
