import { Module } from '@nestjs/common';
import { DatabaseModule } from '../api/database.module.ts';
import { CheckCopyrightCommand } from './commands/check_copyright.command.ts';
import { ConsolidateFootnoteInsightsCommand } from './commands/consolidate_footnote_insights.command.ts';
import { FindAndQueueCitedBooksCommand } from './commands/find_and_queue_cited_books.command.ts';
import { OcrQueuedBooksCommand } from './commands/ocr_queued_books.command.ts';
import { ProcessOcredBooksCommand } from './commands/process_ocred_books.command.ts';
import { SaveCoverCommand } from './commands/save_cover.command.ts';
import { ReplCommand } from './commands/repl.command.ts';
import { FillSourceSnippetsCommand } from './commands/fill_source_snippets.command.ts';
import { ImportAquinasCommand } from './commands/import_aquinas.command.ts';
import { ImportJournalIssuesCommand } from './commands/import_journal_issues.command.ts';
import { RelinkCitationsCommand } from './commands/relink_citations.command.ts';
import { IndexPassagesCommand } from './commands/index_passages.command.ts';
import { EbookWorkerCommand } from './commands/ebook_worker.command.ts';
import { SearchPassagesCommand } from './commands/search_passages.command.ts';
import { ExportFootnoteTrainingCommand } from './commands/export_footnote_training.command.ts';
import { MakeFootnoteEvalSetCommand } from './commands/make_footnote_eval_set.command.ts';
import { EvalFootnotesCommand } from './commands/eval_footnotes.command.ts';
import { RelabelFootnoteTrainingCommand } from './commands/relabel_footnote_training.command.ts';

// The commands of cli/main.ts, sharing the API's database connection
@Module({
  imports:   [DatabaseModule],
  providers: [
    OcrQueuedBooksCommand,
    ProcessOcredBooksCommand,
    FindAndQueueCitedBooksCommand,
    ConsolidateFootnoteInsightsCommand,
    SaveCoverCommand,
    CheckCopyrightCommand,
    FillSourceSnippetsCommand,
    ImportAquinasCommand,
    ImportJournalIssuesCommand,
    RelinkCitationsCommand,
    IndexPassagesCommand,
    EbookWorkerCommand,
    SearchPassagesCommand,
    ExportFootnoteTrainingCommand,
    MakeFootnoteEvalSetCommand,
    EvalFootnotesCommand,
    RelabelFootnoteTrainingCommand,
    ReplCommand,
  ],
})
export class CliModule {}
