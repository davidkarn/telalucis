// The web app's pages for search engines and link previews, and its robots.txt and sitemaps (see
// SeoService). nginx passes them here (docker/nginx.conf): /books/... is /seo/html/books/...,
// /robots.txt is /seo/robots.txt, /sitemap.xml and /sitemaps/<name> are /seo/sitemap.xml and
// /seo/sitemaps/<name>. The site's origin in the URLs given is PUBLIC_ORIGIN, else the request's.
import { Controller, Get, Header, Inject, NotFoundException, Param, Req, Res } from '@nestjs/common';
import { SeoService } from './seo.service';

type SeoRequest = {
  url: string,
  protocol: string,
  headers: Record<string, string | string[] | undefined>,
};
type SeoResponse = {
  status: (code: number) => unknown,
  setHeader: (name: string, value: string) => unknown,
};

const firstHeader = (value: string | string[] | undefined): string | undefined => (
  (Array.isArray(value) ? value[0] : value)?.split(',')[0]?.trim()
);

// The site's origin: PUBLIC_ORIGIN, else the host and scheme the request came to (through nginx)
const originOf = (req: SeoRequest): string => {
  const configured = process.env.PUBLIC_ORIGIN;

  if (configured !== undefined && configured.length > 0) {
    return configured.replace(/\/+$/, '');
  }
  else {
    const proto = firstHeader(req.headers['x-forwarded-proto']) ?? req.protocol;
    const host  = firstHeader(req.headers['x-forwarded-host'])
      ?? firstHeader(req.headers.host)
      ?? 'localhost';
    return `${ proto }://${ host }`;
  }
};

const HTML_PREFIX = '/seo/html';

@Controller('seo')
export class SeoController {
  constructor(@Inject(SeoService) private readonly seo: SeoService) {}

  @Get('robots.txt')
  @Header('Content-Type', 'text/plain; charset=utf-8')
  robots(@Req() req: SeoRequest) {
    return this.seo.robotsTxt(originOf(req));
  }

  @Get('sitemap.xml')
  @Header('Content-Type', 'application/xml; charset=utf-8')
  sitemapIndex(@Req() req: SeoRequest) {
    return this.seo.sitemapIndex(originOf(req));
  }

  @Get('sitemaps/:name')
  @Header('Content-Type', 'application/xml; charset=utf-8')
  async sitemap(@Req() req: SeoRequest, @Param('name') name: string) {
    const xml = await this.seo.sitemap(originOf(req), name);

    if (xml === null) {
      throw new NotFoundException(`No sitemap ${ name }`);
    }
    else {
      return xml;
    }
  }

  // The app's page at the path after /seo/html; always the latest app (index.html names the
  // current build's bundles)
  @Get(['html', 'html/*path'])
  @Header('Content-Type', 'text/html; charset=utf-8')
  @Header('Cache-Control', 'no-cache')
  async html(@Req() req: SeoRequest, @Res({ passthrough: true }) res: SeoResponse) {
    const pathname = req.url.split('?')[0]!.slice(HTML_PREFIX.length) || '/';
    const page     = await this.seo.page(originOf(req), pathname);

    res.status(page.status);
    res.setHeader('X-Robots-Tag', page.robots);
    return page.html;
  }
}
