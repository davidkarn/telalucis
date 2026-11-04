// A book as an e-book to download (see EbooksService): GET its EPUB; POST to ask for its .mobi,
// then GET it once it's ready
import { createReadStream } from 'node:fs';
import { Controller, Get, HttpCode, Inject, NotFoundException, Param, Post, StreamableFile } from '@nestjs/common';
import { EbooksService } from './ebooks.service';

// An attachment's Content-Disposition: the plain name, and the name as given for browsers reading
// filename* (RFC 6266)
const attachment = (fileName: string) => (
  `attachment; filename="${ fileName.replace(/[^\x20-\x7e]|"/g, '_') }"; filename*=UTF-8''${ encodeURIComponent(fileName) }`
);

@Controller('books')
export class EbooksController {
  constructor(@Inject(EbooksService) private readonly ebooks: EbooksService) {}

  // The book as an EPUB
  @Get(':bookId/ebook.epub')
  async epub(@Param('bookId') bookId: string) {
    const epub = await this.ebooks.epub(bookId);

    if (!epub) {
      throw new NotFoundException(`No book with id ${ bookId }`);
    }
    else {
      return new StreamableFile(epub.bytes, { type: 'application/epub+zip', disposition: attachment(epub.fileName) });
    }
  }

  // Ask for the book's .mobi: ready to GET, or being made (asked again, how it's going)
  @Post(':bookId/ebook.mobi')
  @HttpCode(200)
  async requestMobi(@Param('bookId') bookId: string) {
    const status = await this.ebooks.requestMobi(bookId);

    if (!status) {
      throw new NotFoundException(`No book with id ${ bookId }`);
    }
    else {
      return status;
    }
  }

  // The book's .mobi, once it's ready (POST asks for it)
  @Get(':bookId/ebook.mobi')
  async mobi(@Param('bookId') bookId: string) {
    const mobi = await this.ebooks.mobiFile(bookId);

    if (!mobi) {
      throw new NotFoundException(`The .mobi of ${ bookId } isn't ready: POST /books/${ bookId }/ebook.mobi asks for it`);
    }
    else {
      return new StreamableFile(createReadStream(mobi.path), {
        type: 'application/x-mobipocket-ebook', disposition: attachment(mobi.fileName),
      });
    }
  }
}
