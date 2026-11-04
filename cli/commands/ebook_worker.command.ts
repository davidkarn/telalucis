// The e-book worker: makes the .mobi files the API's readers ask for (lib/ebook_queue.ts), one at
// a time, from each book's text as it is when its turn comes, until stopped (SIGINT, SIGTERM),
// finishing the book it's on. Needs Redis (REDIS_URL) and Calibre's ebook-convert (lib/ebook.ts).
//   npm run cli -- ebook-worker
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { bookText } from '../../core/book_text.ts';
import { fileExists, makeMobiFile, mobiPath } from '../../lib/ebook_files.ts';
import { startMobiWorker } from '../../lib/ebook_queue.ts';
import { BookTextQueries } from '../../model/book_text.ts';

@Command({
  name:        'ebook-worker',
  description: "Make the .mobi e-books readers ask for, one at a time, until stopped",
})
export class EbookWorkerCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  async run(): Promise<void> {
    const { worker, close } = startMobiWorker(async(job) => {
      const source = await BookTextQueries.findBookSource(this.db, job.data.bookId);

      if (source === null) {
        throw new Error(`no book with id ${ job.data.bookId }`);
      }
      else {
        const text    = bookText(source);
        const started = Date.now();

        if (!await fileExists(mobiPath(text))) {
          await makeMobiFile(text);
          console.log(`${ job.data.bookId }: made its .mobi in ${ ((Date.now() - started) / 1000).toFixed(1) } s`);
        }
      }
    });

    worker.on('failed', (job, error) => console.error(`${ job?.data.bookId }: ${ error.message }`));
    worker.on('error', (error) => console.error(`e-book worker: ${ error.message }`));
    console.log('e-book worker: waiting for .mobi jobs');

    await new Promise<void>((resolve) => {
      const stop = () => {
        console.log('e-book worker: stopping after the book it is on');
        void close().then(resolve);
      };
      process.once('SIGINT', stop);
      process.once('SIGTERM', stop);
    });
  }
}
