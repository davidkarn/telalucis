import { Module } from '@nestjs/common';
import { AdminController } from './admin.controller';
import { AdminGuard } from './admin.guard';
import { AdminService } from './admin.service';
import { AuthorsController } from './authors.controller';
import { AuthorsService } from './authors.service';
import { BooksController } from './books.controller';
import { BooksService } from './books.service';
import { CitationSourcePagesController, CitationsController } from './citations.controller';
import { CitationsService } from './citations.service';
import { DatabaseModule } from './database.module';
import { EbooksController } from './ebooks.controller';
import { EbooksService } from './ebooks.service';
import { InsightsController } from './insights.controller';
import { InsightsService } from './insights.service';
import { JournalsController } from './journals.controller';
import { JournalsService } from './journals.service';

@Module({
  imports:     [DatabaseModule],
  controllers: [
    AdminController, AuthorsController, BooksController, CitationsController, CitationSourcePagesController,
    EbooksController, InsightsController, JournalsController,
  ],
  providers:   [
    AdminGuard, AdminService, AuthorsService, BooksService, CitationsService, EbooksService, InsightsService,
    JournalsService,
  ],
})
export class AppModule {}
