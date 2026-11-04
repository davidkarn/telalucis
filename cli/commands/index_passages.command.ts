// Index the collection's text for searching (see core/passages.ts): cut each book with body text
// and no passages into passages, then embed the passages with none, a batch at a time (see
// lib/embeddings.ts, which needs its server running). Re-runnable: it carries on where it stopped.
//   npm run cli -- index-passages [bookIds...] [--cut-only] [--embed-only] [--batch <n>] [--reembed]
// bookIds: cut only these books, again even when they have passages (all needing it when none are
// given). --cut-only / --embed-only: only cut / only embed. --batch: passages per embeddings
// request, 32 when not given. --reembed: first forget the embeddings of models other than
// EMBEDDINGS_MODEL's, to embed those passages again with it.
import { Inject } from '@nestjs/common';
import type { Kysely } from 'kysely';
import { Command, CommandRunner, Option } from 'nest-commander';
import { DB } from '../../api/database.module.ts';
import type { Database } from '../../api/database.ts';
import { bookPassages, embeddingInput } from '../../core/passages.ts';
import { embed, embeddingsModel } from '../../lib/embeddings.ts';
import { PassageActions, PassageQueries } from '../../model/passages.ts';

const DEFAULT_BATCH = 32;

type Options = { cutOnly?: boolean, embedOnly?: boolean, batch?: number, reembed?: boolean };

@Command({
  name:        'index-passages',
  arguments:   '[bookIds...]',
  description: "Cut the books' body text into passages and embed them, for searching the collection",
})
export class IndexPassagesCommand extends CommandRunner {
  constructor(@Inject(DB) private readonly db: Kysely<Database>) {
    super();
  }

  @Option({ flags: '--cut-only', description: 'only cut books into passages, without embedding them' })
  parseCutOnly(): boolean {
    return true;
  }

  @Option({ flags: '--embed-only', description: 'only embed the passages with no embedding' })
  parseEmbedOnly(): boolean {
    return true;
  }

  @Option({ flags: '--batch <n>', description: `passages per embeddings request (${ DEFAULT_BATCH } when not given)` })
  parseBatch(value: string): number {
    const n = Number(value);

    if (!Number.isInteger(n) || n < 1) {
      throw new Error(`--batch must be a whole number, 1 or more, not ${ value }`);
    }
    else {
      return n;
    }
  }

  @Option({ flags: '--reembed', description: "embed again the passages embedded with a model other than EMBEDDINGS_MODEL's" })
  parseReembed(): boolean {
    return true;
  }

  async run(bookIds: string[], options: Options = {}): Promise<void> {
    if (!options.embedOnly) {
      await this.cut(bookIds);
    }
    if (!options.cutOnly) {
      await this.embedAll(options.batch ?? DEFAULT_BATCH, options.reembed === true);
    }
  }

  // Cut these books into passages, or every book with body text and none
  private async cut(bookIds: string[]) {
    const books  = bookIds.length > 0 ? bookIds : await PassageQueries.findBooksWithoutPassages(this.db);
    let passages = 0;

    for (const [i, bookId] of books.entries()) {
      const cut = bookPassages(await PassageQueries.findPassageSource(this.db, bookId));
      await PassageActions.replaceBookPassages(this.db, bookId, cut);
      passages += cut.length;

      if ((i + 1) % 50 === 0 || i === books.length - 1) {
        console.log(`cut ${ i + 1 } of ${ books.length } books: ${ passages } passages`);
      }
    }
  }

  // Embed every passage with no embedding, `batch` at a time
  private async embedAll(batch: number, reembed: boolean) {
    const model = embeddingsModel();

    if (reembed) {
      console.log(`forgot ${ await PassageActions.clearOtherModelsEmbeddings(this.db, model) } embeddings of other models`);
    }

    const counts  = await PassageQueries.countPassages(this.db);
    const total   = counts.passages - counts.embedded;
    const started = Date.now();
    let embedded  = 0;

    console.log(`embedding ${ total } passages with ${ model }`);

    for (let rows = await PassageQueries.findPassagesToEmbed(this.db, batch); rows.length > 0;
      rows = await PassageQueries.findPassagesToEmbed(this.db, batch)) {
      const embeddings = await embed(rows.map((r) => embeddingInput(r, r)));
      await PassageActions.saveEmbeddings(this.db, model, rows.map((r, i) => ({ id: r.id, embedding: embeddings[i]! })));
      embedded += rows.length;

      if (embedded % (batch * 50) < batch || rows.length < batch) {
        const perSecond = embedded / ((Date.now() - started) / 1000);
        const minutes   = Math.round((total - embedded) / perSecond / 60);
        console.log(`embedded ${ embedded } of ${ total } (${ perSecond.toFixed(1) }/s, about ${ minutes } min to go)`);
      }
    }
  }
}
