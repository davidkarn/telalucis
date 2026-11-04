// The queue of .mobi e-books to make (BullMQ, in Redis at REDIS_URL; redis://localhost:6380 when
// not set, the development container's port): the API adds a book's job when its .mobi is asked
// for (api/ebooks.service.ts), the e-book worker (the ebook-worker command) makes them, one at a
// time. A job's id is its .mobi file's name (lib/ebook_files.ts), so a book is queued once for
// each version of its text. Finished jobs are removed; failed ones are kept a day, for the API to
// tell (and remove, so asking again tries again).
import { Queue, Worker, type Job } from 'bullmq';
import { Redis, type RedisOptions } from 'ioredis';
import path from 'node:path';

export const MOBI_QUEUE = 'ebook-mobi';

export type MobiJob = { bookId: string };

const DAY_SECONDS = 24 * 60 * 60;

// A client of the Redis at REDIS_URL (BullMQ is given one: its own loading of ioredis doesn't work
// here, under ESM)
const redis = (options: Pick<RedisOptions, 'enableOfflineQueue' | 'maxRetriesPerRequest'>) => new Redis(process.env.REDIS_URL ?? 'redis://localhost:6380', options);

// The id of the job making the .mobi file `file`
export const mobiJobId = (file: string) => path.basename(file, '.mobi');

// The queue, for adding jobs and reading their states, and closing it (and its client, which
// BullMQ leaves open, having been given it). Its commands fail at once while Redis is unreachable
// (rather than waiting for it), so a request asking for a .mobi isn't left hanging.
export const openMobiQueue = () => {
  const client = redis({ enableOfflineQueue: false });
  const queue  = new Queue<MobiJob>(MOBI_QUEUE, {
    connection:        client,
    defaultJobOptions: { attempts: 1, removeOnComplete: true, removeOnFail: { age: DAY_SECONDS } },
  });

  return { queue, close: () => queue.close().finally(() => client.disconnect()) };
};

// The worker making the queue's .mobi files, one at a time, and stopping it (after the job it's
// on). maxRetriesPerRequest: null, as BullMQ's workers need: they wait for Redis to come back
// rather than fail.
export const startMobiWorker = (make: (job: Job<MobiJob>) => Promise<void>) => {
  const client = redis({ maxRetriesPerRequest: null });
  const worker = new Worker<MobiJob>(MOBI_QUEUE, make, { connection: client, concurrency: 1 });

  return { worker, close: () => worker.close().finally(() => client.disconnect()) };
};
