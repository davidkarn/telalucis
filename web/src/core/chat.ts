// An answer in a chat, as it's shown (see pages/chats.ts): its markdown read
// into paragraphs, headings, lists and quotes, and within them bold, italics,
// code and its citations, the links to the passages it was given by their
// numbers ("[ST I q76 a1](3)"). Links to anything else are shown as their text,
// as are citations of numbers no passage has (see the page). Only what the
// model writes is read, not all of markdown. Pure functions.

export type Inline =
  | { kind: 'text', text: string }
  | { kind: 'strong', children: Inline[] }
  | { kind: 'em', children: Inline[] }
  | { kind: 'code', text: string }
  | { kind: 'cite', n: number, label: string };

export type Block =
  | { kind: 'paragraph', inlines: Inline[] }
  | { kind: 'heading', level: number, inlines: Inline[] }
  | { kind: 'quote', inlines: Inline[] }
  | { kind: 'list', ordered: boolean, items: Inline[][] };

// A citation's link: its text, and the passage's number, as the model may
// write it ("3", "passage-3", "#3", "p3")
const CITE = /\[([^\]\n]+)\]\(\s*#?(?:passage[\s_-]*|p)?(\d{1,4})\s*\)/iy;
const LINK = /\[([^\]\n]+)\]\([^)\n]*\)/y;
const STRONG = /\*\*(.+?)\*\*|__(.+?)__/y;
const EM = /\*(?!\s)(.+?)\*|_(?!\s)(.+?)_(?![\p{L}\p{N}])/uy;
const CODE = /`([^`\n]+)`/y;

// The inline parts of a run of text
export const inlineParts = (text: string): Inline[] => {
  const parts: Inline[] = [];
  let plain             = '';
  let at                = 0;

  const flush = () => {
    if (plain.length > 0) {
      parts.push({ kind: 'text', text: plain });
      plain = '';
    }
  };
  const matchAt = (re: RegExp) => {
    re.lastIndex = at;
    return re.exec(text);
  };

  while (at < text.length) {
    const cite   = text[at] === '[' ? matchAt(CITE) : null;
    const link   = text[at] === '[' && cite === null ? matchAt(LINK) : null;
    const strong = text[at] === '*' || text[at] === '_' ? matchAt(STRONG) : null;
    const em     = (text[at] === '*' || text[at] === '_') && strong === null ? matchAt(EM) : null;
    const code   = text[at] === '`' ? matchAt(CODE) : null;

    if (cite !== null) {
      flush();
      parts.push({ kind: 'cite', n: Number(cite[2]), label: cite[1]!.trim() });
      at += cite[0].length;
    }
    else if (link !== null) {
      plain += link[1];
      at += link[0].length;
    }
    else if (strong !== null) {
      flush();
      parts.push({ kind: 'strong', children: inlineParts(strong[1] ?? strong[2] ?? '') });
      at += strong[0].length;
    }
    else if (em !== null) {
      flush();
      parts.push({ kind: 'em', children: inlineParts(em[1] ?? em[2] ?? '') });
      at += em[0].length;
    }
    else if (code !== null) {
      flush();
      parts.push({ kind: 'code', text: code[1]! });
      at += code[0].length;
    }
    else {
      plain += text[at];
      at += 1;
    }
  }
  flush();

  return parts;
};

const LIST_ITEM = /^\s*(?:([-*+•])|(\d+)[.)])\s+(.*)$/;
const HEADING = /^(#{1,6})\s+(.*)$/;
const QUOTE = /^>\s?(.*)$/;

// An answer's blocks: lines apart from blank ones run together into
// paragraphs; a heading is a line; a list or a quote, its lines in a row
export const answerBlocks = (content: string): Block[] => {
  const blocks: Block[] = [];
  let lines: string[]   = [];

  const endParagraph = () => {
    if (lines.length > 0) {
      blocks.push({ kind: 'paragraph', inlines: inlineParts(lines.join(' ')) });
      lines = [];
    }
  };

  for (const raw of content.split('\n')) {
    const line    = raw.trimEnd();
    const item    = LIST_ITEM.exec(line);
    const heading = HEADING.exec(line);
    const quote   = QUOTE.exec(line);
    const last    = blocks.at(-1);

    if (line.trim().length === 0) {
      endParagraph();
    }
    else if (heading !== null) {
      endParagraph();
      blocks.push({ kind: 'heading', level: heading[1]!.length, inlines: inlineParts(heading[2]!) });
    }
    else if (item !== null) {
      endParagraph();
      const ordered = item[2] !== undefined;
      if (last?.kind === 'list' && last.ordered === ordered) {
        last.items.push(inlineParts(item[3]!));
      }
      else {
        blocks.push({ kind: 'list', ordered, items: [inlineParts(item[3]!)] });
      }
    }
    else if (quote !== null) {
      endParagraph();
      if (last?.kind === 'quote') {
        last.inlines.push({ kind: 'text', text: ' ' }, ...inlineParts(quote[1]!));
      }
      else {
        blocks.push({ kind: 'quote', inlines: inlineParts(quote[1]!) });
      }
    }
    else if (lines.length === 0 && last?.kind === 'list' && /^\s{2,}/.test(raw)) {
      // a list item's line carried on
      last.items.at(-1)!.push({ kind: 'text', text: ' ' }, ...inlineParts(line.trim()));
    }
    else {
      lines.push(line.trim());
    }
  }
  endParagraph();

  return blocks;
};

const citesIn = (inlines: Inline[]): number[] => inlines.flatMap((p) => {
  if (p.kind === 'cite') {
    return [p.n];
  }
  else if (p.kind === 'strong' || p.kind === 'em') {
    return citesIn(p.children);
  }
  else {
    return [];
  }
});

// The numbers of the passages an answer cites, each once, in the order it
// first cites them
export const citedNumbers = (content: string): number[] => [
  ...new Set(answerBlocks(content).flatMap((b) => (
    b.kind === 'list' ? b.items.flatMap(citesIn) : citesIn(b.inlines)
  ))),
];
