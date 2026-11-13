// Search the collection's passages for a question, printing what's found (see
// lib/passage_search.ts), or measure the search on questions whose answers are known: for each
// way of searching (by meaning alone, with words, with the reranker, with citing passages and the
// places shared among authors), where the first answer is and how many of the expected answers
// are found. Needs the embeddings server, and the reranker's for reranking.
//   npm run cli -- search-passages "<question>" [--no-words] [--no-cites] [--no-rerank] [--limit <n>]
//   npm run cli -- search-passages --eval eval/passage_search.json
import fs from 'node:fs/promises';
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { answerScore, DEFAULT_LIMITS, type ExpectedAnswer } from '../../core/passage_search.ts';
import { DEFAULT_SEARCH, searchPassages, type SearchOptions } from '../../lib/passage_search.ts';
import { PassageSearchQueries } from '../../model/passage_search.ts';

type Options = { words?: boolean, cites?: boolean, rerank?: boolean, limit?: number, eval?: string };

type EvalFile = { questions: { question: string, expect: { bookId: string, parts: [string, number][] }[] }[] };

const NO_LIMITS = (limit: number) => ({ limit, perAuthor: limit, perBook: limit });

// The ways of searching compared by --eval
const WAYS: { name: string, options: (limit: number) => SearchOptions }[] = [
  { name: 'meaning', options: (limit) => ({ words: false, cites: false, rerank: 0, limits: NO_LIMITS(limit) }) },
  { name: '+words', options: (limit) => ({ words: true, cites: false, rerank: 0, limits: NO_LIMITS(limit) }) },
  { name: '+rerank 30', options: (limit) => ({ words: true, cites: false, rerank: 30, limits: NO_LIMITS(limit) }) },
  { name: '+rerank 60', options: (limit) => ({ words: true, cites: false, rerank: 60, limits: NO_LIMITS(limit) }) },
  { name: '+cites', options: (limit) => ({ words: true, cites: true, rerank: 60, limits: NO_LIMITS(limit) }) },
  { name: '+voices', options: (limit) => ({ ...DEFAULT_SEARCH, limits: { ...DEFAULT_LIMITS, limit } }) },
];

@Command({
  name:        'search-passages',
  arguments:   '[question]',
  description: "Search the collection's passages for a question, or measure the search (--eval)",
})
export class SearchPassagesCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--no-words', description: 'without searching by words' })
  parseWords(): boolean {
    return false;
  }

  @Option({ flags: '--no-cites', description: 'without the passages citing those found' })
  parseCites(): boolean {
    return false;
  }

  @Option({ flags: '--no-rerank', description: 'without the reranker' })
  parseRerank(): boolean {
    return false;
  }

  @Option({ flags: '--limit <n>', description: `results (${ DEFAULT_LIMITS.limit } when not given)` })
  parseLimit(value: string): number {
    return Number(value);
  }

  @Option({ flags: '--eval <file>', description: 'measure the search on the questions of this file' })
  parseEval(value: string): string {
    return value;
  }

  async run([question]: string[], options: Options = {}): Promise<void> {
    const limit = options.limit ?? DEFAULT_LIMITS.limit;

    if (options.eval !== undefined) {
      await this.evaluate(options.eval, limit);
    }
    else if (question === undefined) {
      throw new Error('give a question, or --eval <file>');
    }
    else {
      const report = await searchPassages(this.db, question, {
        words:  options.words !== false,
        cites:  options.cites !== false,
        rerank: options.rerank === false ? 0 : DEFAULT_SEARCH.rerank,
        limits: { ...DEFAULT_LIMITS, limit },
      });

      console.log(`${ JSON.stringify(report.timings) } ${ report.reranked === true ? 'reranked' : report.reranked || 'not reranked' }`);
      report.results.forEach((r, i) => {
        const where = `${ r.title.slice(0, 50) } — ${ r.author.slice(0, 30) }, p. ${ r.printedPageNumber || r.pageNumber }`;
        console.log(`\n${ i + 1 }. [${ r.score.toFixed(2) } ${ r.foundBy.join('+') }] ${ where }`
          + (r.cites ? `\n   cites ${ r.cites.bookId } (page ${ r.cites.pageNumber })` : '')
          + (r.copies.length > 0 ? `\n   also in: ${ r.copies.map((c) => c.title.slice(0, 40)).join('; ') }` : '')
          + `\n   ${ r.text.replace(/\s+/g, ' ').slice(0, 220) }…`);
      });
    }
  }

  // Each way of searching, measured on the file's questions
  private async evaluate(file: string, limit: number) {
    const spec      = JSON.parse(await fs.readFile(file, 'utf8')) as EvalFile;
    const questions = await Promise.all(spec.questions.map(async(q) => ({
      question: q.question,
      expected: await Promise.all(q.expect.map(async(e): Promise<ExpectedAnswer> => ({
        bookId: e.bookId, pages: await PassageSearchQueries.findPagesByParts(this.db, e.bookId, e.parts),
      }))),
    })));

    const unresolved = questions.flatMap((q) => q.expected.filter((e) => e.pages.length === 0).map((e) => `${ q.question }: ${ e.bookId }`));
    if (unresolved.length > 0) {
      console.log(`expected answers with no pages (left out): ${ unresolved.join('; ') }`);
    }

    const rows = [];
    for (const way of WAYS) {
      const scores = [];
      let ms       = 0;
      for (const q of questions) {
        const started = Date.now();
        const report  = await searchPassages(this.db, q.question, way.options(limit));
        ms           += Date.now() - started;
        scores.push({ question: q.question, ...answerScore(report.results, q.expected.filter((e) => e.pages.length > 0)), reranked: report.reranked });
      }

      const expectedCount = questions.reduce((n, q) => n + q.expected.filter((e) => e.pages.length > 0).length, 0);
      rows.push({
        way:      way.name,
        answered: `${ scores.filter((s) => s.firstAnswer !== null).length }/${ scores.length }`,
        mrr:      (scores.reduce((s, x) => s + (x.firstAnswer === null ? 0 : 1 / x.firstAnswer), 0) / scores.length).toFixed(3),
        found:    `${ scores.reduce((n, s) => n + s.answersFound, 0) }/${ expectedCount }`,
        authors:  (scores.reduce((n, s) => n + s.authors, 0) / scores.length).toFixed(1),
        msEach:   Math.round(ms / scores.length),
        firsts:   scores.map((s) => s.firstAnswer ?? '-').join(' '),
      });
      const notReranked = scores.find((s) => typeof s.reranked === 'string');
      if (notReranked) {
        console.log(`${ way.name }: ${ notReranked.reranked }`);
      }
    }

    console.log(`\nTop ${ limit } results of ${ questions.length } questions. answered: a result on an expected page; mrr: mean `
      + '1/place of the first; found: expected answers found; authors: in the results, on average; firsts: the first answer\'s place, by question');
    console.table(rows);
  }
}
