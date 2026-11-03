import type { QueueStatus } from '../api';

// The statuses in the order a book goes through them (QUEUE_STATUSES in the API's
// core/queued_books.ts)
export const QUEUE_STATUSES: QueueStatus[] = [
  'pending', 'queued', 'inProgress', 'processingContents', 'imported', 'importedAndCrawled', 'complete',
];

// A queue status as the admin dashboard shows it: what the book is waiting for
export const STATUS_LABELS: Record<QueueStatus, string> = {
  pending:            'Pending',
  queued:             'Queued for OCR',
  inProgress:         "OCR'd, to import",
  processingContents: 'Importing',
  imported:           'Imported, to crawl',
  importedAndCrawled: 'Crawled',
  complete:           'Complete',
};

// The statuses a command takes books from in queue order (ocr-queued-books, process-ocred-books,
// find-and-queue-cited-books), so a book with one can be moved to the top of the queue
export const TAKEN_IN_ORDER: QueueStatus[] = ['queued', 'inProgress', 'imported'];
