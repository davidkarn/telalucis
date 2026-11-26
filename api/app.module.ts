import { Module } from '@nestjs/common';
import { PassportModule } from '@nestjs/passport';
import { AdminController } from './admin.controller';
import { AdminGuard } from './admin.guard';
import { AdminService } from './admin.service';
import { AuthController } from './auth.controller';
import { AuthorsController } from './authors.controller';
import { AuthorsService } from './authors.service';
import { ChatsController } from './chats.controller.ts';
import { ChatsService } from './chats.service.ts';
import { BooksController } from './books.controller';
import { BooksService } from './books.service';
import { CitationSourcePagesController, CitationsController } from './citations.controller';
import { CitationsService } from './citations.service';
import { DatabaseModule } from './database.module';
import { EbooksController } from './ebooks.controller';
import { EbooksService } from './ebooks.service';
import { GoogleStrategy } from './google.strategy';
import { JournalsController } from './journals.controller';
import { JournalsService } from './journals.service';
import { SearchController } from './search.controller';
import { SeoController } from './seo.controller';
import { SeoService } from './seo.service';
import { SessionSerializer } from './session.serializer';
import { googleAuthConfig } from '../core/user_auth';

@Module({
  imports:     [DatabaseModule, PassportModule.register({ session: true })],
  controllers: [
    AdminController, AuthController, AuthorsController, BooksController, ChatsController, CitationsController, CitationSourcePagesController,
    EbooksController, JournalsController, SearchController, SeoController,
  ],
  providers:   [
    AdminGuard, AdminService, AuthorsService, BooksService, ChatsService, CitationsService, EbooksService,
    JournalsService, SeoService, SessionSerializer,
    // signing in with Google, when GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET are set
    ...(googleAuthConfig(process.env) === undefined ? [] : [GoogleStrategy]),
  ],
})
export class AppModule {}
