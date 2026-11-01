// Saving a work of Aquinas (see cli/commands/import_aquinas.command.ts) as
// actions: its book, pages and the citations in them, then how its pages are
// cited, once the book is saved. Pure functions; actions/app_actions.ts
// carries the actions out.
import type { AppActionStep } from '../actions/app_actions.ts';
import type { AquinasPage, AquinasWork } from './aquinas_html.ts';

// The author of the works without one of their own
export const AQUINAS = 'Thomas Aquinas';

export const importAquinasWorkActions = (
  work: AquinasWork, pages: AquinasPage[],
): AppActionStep[] => [
  {
    id:   'saveBook',
    cmd:  'modelAction',
    data: {
      model:  'Books',
      act:    'saveBook',
      params: [{
        id:         work.id,
        title:      work.title,
        author:     work.author ?? AQUINAS,
        url:        `https://isidore.co/aquinas/${ work.files[0]!.path }`,
        translator: work.translator,
      }, pages],
    },
  },
  (results) => (
    results['saveBook']?.success === true
      ? {
          id:   'savePageCitations',
          cmd:  'modelAction',
          data: {
            model:  'BookPagesToCitations',
            act:    'replacePageCitations',
            params: [work.id, pages
              .filter((p) => p.citationParts.length > 0)
              .map((p) => ({ pageNumber: p.pageNumber, parts: p.citationParts }))],
          },
        }
      : null
  ),
  (results) => ({
    cmd:  'log',
    data: [`  saved ${ work.id }:`, results['saveBook']?.result ?? results['saveBook']?.errorMessage,
           'page citations:', results['savePageCitations']?.result ?? 'not saved'],
  }),
];
