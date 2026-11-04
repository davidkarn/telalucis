// Books as e-books: an EPUB, made when it's asked for (about a second for the longest book), and a
// .mobi for older Kindles, converted from it by Calibre (a minute for the longest; see
// lib/ebook.ts) by the e-book worker (the ebook-worker command), one book at a time, from a
// BullMQ queue (lib/ebook_queue.ts), and kept until the book changes (lib/ebook_files.ts). Books
// withheld for copyright aren't made into e-books.
import { Inject, Injectable, type OnModuleDestroy } from '@nestjs/common';
import type { Queue } from 'bullmq';
import type { Kysely } from 'kysely';
import { bookText, type BookText } from '../core/book_text';
import { epubBytes } from '../lib/ebook';
import { ebookFileName, fileExists, mobiPath } from '../lib/ebook_files';
import { mobiJobId, openMobiQueue, type MobiJob } from '../lib/ebook_queue';
import { BookTextQueries } from '../model/book_text';
import { assertNotWithheld } from './copyright';
import { DB } from './database.module';
import type { Database } from './database';

// The MobiStatus schema in api.yaml: ready to download, waiting behind `ahead` others, being
// converted, or failed (asking again tries again)
export type MobiStatus =
  | { status: 'ready' }
  | { status: 'queued', ahead: number }
  | { status: 'converting' }
  | { status: 'failed', error: string };

export type EbookDownload = { fileName: string, bytes: Uint8Array };

// How long a book's .mobi file name is remembered, so the browser asking how its .mobi is going
// every few seconds doesn't have the book's text made each time (a second for the longest)
const FILE_MEMO_MS = 60_000;

@Injectable()
export class EbooksService implements OnModuleDestroy {
  private queue: ReturnType<typeof openMobiQueue> | null = null;
  private readonly files               = new Map<string, { file: string, title: string, expires: number }>();

  constructor(@Inject(DB) private readonly db: Kysely<Database>) {}

  async onModuleDestroy() {
    await this.queue?.close();
  }

  // The queue, opened the first time a .mobi is asked for
  private mobiQueue(): Queue<MobiJob> {
    const opened = this.queue ?? openMobiQueue();
    this.queue   = opened;
    return opened.queue;
  }

  // The book's text, null when there's no such book; throws 451 when it's withheld
  private async text(bookId: string): Promise<BookText | null> {
    await assertNotWithheld(this.db, bookId);
    const source = await BookTextQueries.findBookSource(this.db, bookId);
    return source === null ? null : bookText(source);
  }

  // The book's .mobi file (made or not) and title, remembered a minute; null when there's no such
  // book
  private async mobiFileOf(bookId: string): Promise<{ file: string, title: string } | null> {
    const known = this.files.get(bookId);

    if (known !== undefined && known.expires > Date.now()) {
      await assertNotWithheld(this.db, bookId);
      return known;
    }
    else {
      const text = await this.text(bookId);
      if (text === null) {
        return null;
      }
      else {
        const found = { file: mobiPath(text), title: text.title, expires: Date.now() + FILE_MEMO_MS };
        this.files.set(bookId, found);
        return found;
      }
    }
  }

  // The book as an EPUB; null when there's no such book
  async epub(bookId: string): Promise<EbookDownload | null> {
    const text = await this.text(bookId);
    return text === null ? null : {
      fileName: ebookFileName(text.title, text.id, 'epub'),
      bytes:    epubBytes(text, { modified: new Date() }),
    };
  }

  // Ask for the book's .mobi: ready, or its job queued (if it isn't already) and how it's going. A
  // failure is told once: its job is removed, so asking again tries again. null when there's no
  // such book.
  async requestMobi(bookId: string): Promise<MobiStatus | null> {
    const found = await this.mobiFileOf(bookId);

    if (found === null) {
      return null;
    }
    else if (await fileExists(found.file)) {
      return { status: 'ready' };
    }
    else {
      try {
        const queue = this.mobiQueue();
        const jobId = mobiJobId(found.file);
        const job   = await queue.getJob(jobId) ?? await queue.add('mobi', { bookId }, { jobId });
        const state = await job.getState();

        if (state === 'active') {
          return { status: 'converting' };
        }
        else if (state === 'failed') {
          await job.remove();
          return { status: 'failed', error: job.failedReason || 'the conversion failed' };
        }
        else if (state === 'completed' || state === 'unknown') {
          // finished (or just removed) without its file: made for an older version of the book
          return await fileExists(found.file) ? { status: 'ready' } : { status: 'converting' };
        }
        else {
          const [waiting, active] = await Promise.all([queue.getWaiting(), queue.getActiveCount()]);
          // taken by the worker meanwhile, it's among those counted active
          return await job.getState() === 'active'
            ? { status: 'converting' }
            : { status: 'queued', ahead: active + waiting.filter((w) => w.timestamp < job.timestamp).length };
        }
      }
      catch (e) {
        return { status: 'failed', error: `the e-book queue is unavailable: ${ e instanceof Error ? e.message : String(e) }` };
      }
    }
  }

  // The book's .mobi file, if it's ready; null when there's no such book or it isn't
  async mobiFile(bookId: string): Promise<{ path: string, fileName: string } | null> {
    const found = await this.mobiFileOf(bookId);

    return found !== null && await fileExists(found.file)
      ? { path: found.file, fileName: ebookFileName(found.title, bookId, 'mobi') }
      : null;
  }
}
