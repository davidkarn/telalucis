// Chatting with an LLM about the collection (/chats, /chats/<id>), for a
// signed-in reader (signed out, they're sent to the login page): their
// conversations on the left, newest first; the one open in the middle, with a
// box to write in; and on the right, the passages of the collection the answer
// chosen cites (the latest, or the one whose citation was last followed), then
// the others it was given. A citation in an answer is a link to its passage:
// following it shows the passage, which links to its page in the book. An
// answer is shown as it's written (see sendChatMessage), and can be stopped.
import { createElement as __, Fragment, useEffect, useMemo, useRef, useState, type ReactElement } from 'react'
import { Link, Navigate, useLocation, useNavigate, useParams } from 'react-router'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { match, P } from 'ts-pattern'
import { ArrowUp, BookOpen, CircleStop, MessageSquarePlus, PanelLeft, Trash2, X } from 'lucide-react'
import {
  ApiError, deleteConversation, sendChatMessage, type ChatMessage, type ChatPassage, type ChatUsage,
  type ConversationList,
} from '../api'
import { conversationsQuery, conversationQuery, currentUserQuery } from '../queries'
import { loginPath } from '../core/auth'
import { answerBlocks, citedNumbers, type Block, type Inline } from '../core/chat'
import { openBooksPath } from '../core/open_books'
import { Header } from '../components/header'
import { LoadingSpinner } from '../components/loading_spinner'
import { formatDateTime, useDocumentTitle } from '../lib'
import './chats.scss'

const MAX_MESSAGE_LENGTH = 2000;  // MAX_MESSAGE_LENGTH in the API's core/chats.ts

// A passage's text longer than this is cut until it's opened
const PASSAGE_PREVIEW = 600;

const EXAMPLES = [
  'How does Thomas Aquinas distinguish the natural law from the eternal law?',
  'What did the scholastics mean by the real distinction between essence and existence?',
  'Why do some theologians hold that grace perfects nature rather than destroying it?',
];

// A message being answered: the conversation it's in (null until a new one is
// made), the message as saved, the passages found for it, and the answer so far
type Pending = {
  conversationId: string | null,
  content: string,
  userMessage: ChatMessage | null,
  passages: ChatPassage[] | null,
  searchedByMeaning: boolean,
  text: string,
};

// A message that couldn't be sent or answered, and so wasn't kept
type Failure = { conversationId: string | null, message: string };

// The answer whose passages are shown (in the conversation it's in), and the passage last
// followed to in it
type Shown = { conversationId: string | null, answerId: string, n: number | null, followed: number };

const PENDING_ANSWER_ID = 'pending';

const NO_MESSAGES: ChatMessage[] = [];

export default function ChatsPage() {
  const current  = useQuery(currentUserQuery());
  const location = useLocation();
  useDocumentTitle('Tela Lucis - Chat');

  return (
    __('main', {className: 'chats'},
      __(Header, {}),
      match(current)
        .with({status: 'pending'}, () => __(LoadingSpinner, {}))
        .with({status: 'error'}, (c) => __('p', {className: 'error'}, "Couldn't reach the API: ", c.error.message))
        .with({data: {user: P.nonNullable}}, () => __(Chats, {}))
        .with({status: 'success'}, () => __(Navigate, {to: loginPath(location.pathname), replace: true}))
        .exhaustive()
    )
  );
}

// Why no more can be sent today; null when more can
const limitMessage = (usage: ChatUsage | undefined): string | null => {
  const resets = usage === undefined ? '' : formatDateTime(usage.resetsAt);

  if (usage === undefined) {
    return null;
  }
  else if (usage.used >= usage.limit) {
    return `You've sent the ${usage.limit} messages you can send today. You can send more from ${resets}.`;
  }
  else if (usage.appUsed >= usage.appLimit) {
    return `Chat has answered as many messages as it can today while it's being tried out. It will answer more from ${resets}.`;
  }
  else {
    return null;
  }
};

function Chats() {
  const params         = useParams();
  const conversationId = params['conversationId'] ?? null;
  const navigate       = useNavigate();
  const queryClient    = useQueryClient();
  const list           = useQuery(conversationsQuery());
  const chat           = useQuery({...conversationQuery(conversationId ?? ''), enabled: conversationId !== null});

  const [pending, setPending]           = useState<Pending | null>(null);
  const [failure, setFailure]           = useState<Failure | null>(null);
  const [draft, setDraft]               = useState('');
  const [chosen, setShown]              = useState<Shown | null>(null);
  const [sidebarOpen, setSidebarOpen]   = useState(false);
  const [passagesOpen, setPassagesOpen] = useState(false);
  const stop                            = useRef<AbortController | null>(null);
  // a conversation opened starts with its latest answer's passages
  const shown                           = chosen?.conversationId === conversationId ? chosen : null;

  const saved    = conversationId !== null && chat.data?.conversation.id === conversationId ? chat.data.messages : NO_MESSAGES;
  const here     = pending !== null && pending.conversationId === conversationId;
  const messages = useMemo((): ChatMessage[] => [
    ...saved.filter((m) => m.id !== pending?.userMessage?.id),
    ...(here && pending !== null ? [
      pending.userMessage ?? {
        id: 'pending-message', role: 'user' as const, content: pending.content, passages: [], error: null,
        createdAt: new Date().toISOString(),
      },
      ...(pending.passages === null ? [] : [{
        id: PENDING_ANSWER_ID, role: 'assistant' as const, content: pending.text, passages: pending.passages,
        error: null, createdAt: new Date().toISOString(),
      }]),
    ] : []),
  ], [saved, pending, here]);

  // every passage of the conversation, by its number
  const passages = useMemo(() => new Map(messages.flatMap((m) => m.passages.map((p) => [p.n, p] as const))), [messages]);
  const answers  = messages.filter((m) => m.role === 'assistant');
  const answer   = answers.find((m) => m.id === shown?.answerId) ?? answers.at(-1);
  const usage    = list.data?.usage;
  const limit    = limitMessage(usage);

  const follow = (answerId: string, n: number) => {
    setShown({conversationId, answerId, n, followed: Date.now()});
    setPassagesOpen(true);
  };

  const send = async (content: string) => {
    const startedIn  = conversationId;
    const controller = new AbortController();
    let madeId: string | null = null;
    let failed: string | null = null;

    stop.current = controller;
    setFailure(null);
    setShown(null);
    setDraft('');
    setPending({conversationId: startedIn, content, userMessage: null, passages: null, searchedByMeaning: true, text: ''});

    try {
      await sendChatMessage({conversationId: startedIn, content}, (event) => {
        match(event)
          .with({type: 'conversation'}, (e) => {
            madeId = e.conversation.id;
            setPending((p) => p && {...p, conversationId: e.conversation.id, userMessage: e.message});
            queryClient.setQueryData(conversationsQuery().queryKey, (old: ConversationList | undefined) => (
              old && {items: [e.conversation, ...old.items.filter((c) => c.id !== e.conversation.id)], usage: e.usage}
            ));
            if (startedIn === null) {
              navigate(`/chats/${encodeURIComponent(e.conversation.id)}`);
            }
          })
          .with({type: 'passages'}, (e) => setPending((p) => p && {...p, passages: e.passages, searchedByMeaning: e.searchedByMeaning}))
          .with({type: 'text'}, (e) => setPending((p) => p && {...p, text: p.text + e.text}))
          .with({type: 'done'}, () => undefined)
          .with({type: 'error'}, (e) => { failed = e.message; })
          .exhaustive();
      }, controller.signal);
    }
    catch (e) {
      if (!controller.signal.aborted) {
        failed = e instanceof Error ? e.message : String(e);
      }
    }

    stop.current = null;
    if (failed !== null) {
      // the message wasn't kept: back in the box, and a conversation it started is gone
      setDraft((d) => (d.length === 0 ? content : d));
      setFailure({conversationId: startedIn, message: failed});
      if (startedIn === null && madeId !== null) {
        navigate('/chats', {replace: true});
      }
    }
    if (controller.signal.aborted) {
      // the API saves what was written once it sees the answer stopped
      await new Promise((resolve) => setTimeout(resolve, 500));
    }
    await queryClient.invalidateQueries({queryKey: ['chats']});
    setPending(null);
  };

  const remove = useMutation({
    mutationFn: deleteConversation,
    onSuccess:  async (_, id) => {
      if (id === conversationId) {
        navigate('/chats');
      }
      await queryClient.invalidateQueries({queryKey: ['chats']});
    },
  });

  const title = match({conversationId, chat})
    .with({conversationId: null}, () => 'New chat')
    .with({chat: {data: P.nonNullable}}, ({chat: c}) => c.data.conversation.title)
    .otherwise(() => '');

  return (
    __('div', {className: 'chats-layout' + (sidebarOpen ? ' sidebar-open' : '') + (passagesOpen ? ' passages-open' : '')},
      __('aside', {className: 'chats-sidebar'},
        __(Link, {className: 'chats-new', to: '/chats', onClick: () => {
          setFailure(null);
          setSidebarOpen(false);
        }},
          __(MessageSquarePlus, {size: 16, 'aria-hidden': true}), 'New chat'
        ),
        __(ConversationLinks, {list: list.data, error: list.error, current: conversationId,
          onOpen: () => setSidebarOpen(false),
          onDelete: (id: string) => {
            if (window.confirm('Delete this conversation?')) {
              remove.mutate(id);
            }
          },
        }),
        usage !== undefined && __('div', {className: 'chats-usage muted'},
          `${usage.used} of ${usage.limit} messages today`
        ),
      ),
      __('section', {className: 'chats-conversation'},
        __('div', {className: 'chats-conversation-header'},
          __('button', {
            type: 'button', className: 'chats-icon-button chats-sidebar-toggle', title: 'Conversations',
            'aria-label': 'Conversations', onClick: () => setSidebarOpen(!sidebarOpen),
          }, __(PanelLeft, {size: 18})),
          __('h1', {}, title),
          answer !== undefined && __('button', {
            type: 'button', className: 'chats-passages-toggle', onClick: () => setPassagesOpen(true),
          }, __(BookOpen, {size: 16, 'aria-hidden': true}), 'Passages'),
        ),
        conversationId !== null && chat.isError
          ? __('p', {className: 'error chats-notice'},
            chat.error instanceof ApiError && chat.error.status === 404 ? 'There is no such conversation.' : chat.error.message)
          : conversationId !== null && chat.isPending
            ? __(LoadingSpinner, {})
            : __(Messages, {
              messages, passages, pending: here ? pending : null,
              shownAnswer: answer?.id ?? null,
              onFollow: follow,
              onExample: (text: string) => setDraft(text),
            }),
        failure !== null && failure.conversationId === conversationId && __('p', {className: 'error chats-notice'},
          failure.message
        ),
        __(Composer, {
          draft, setDraft,
          answering: pending !== null,
          limit,
          onSend: send,
          onStop: () => stop.current?.abort(),
        }),
      ),
      __('aside', {className: 'chats-passages'},
        __('div', {className: 'chats-passages-header'},
          __('h2', {}, 'Passages'),
          __('button', {
            type: 'button', className: 'chats-icon-button chats-passages-close', title: 'Close',
            'aria-label': 'Close passages', onClick: () => setPassagesOpen(false),
          }, __(X, {size: 18})),
        ),
        answer === undefined
          ? __('p', {className: 'muted'}, 'The passages each answer draws on will be shown here.')
          : __(AnswerPassages, {
            key: answer.id,
            answer,
            passages,
            highlighted: shown !== null && shown.answerId === answer.id ? shown : null,
            searchedByMeaning: answer.id !== PENDING_ANSWER_ID || pending === null || pending.searchedByMeaning,
          }),
      ),
    )
  );
}

function ConversationLinks({list, error, current, onOpen, onDelete}: {
  list: ConversationList | undefined, error: Error | null, current: string | null,
  onOpen: () => void, onDelete: (id: string) => void,
}) {
  if (error !== null) {
    return __('p', {className: 'error'}, "Couldn't load your conversations: ", error.message);
  }
  else if (list === undefined) {
    return __(LoadingSpinner, {});
  }
  else if (list.items.length === 0) {
    return __('p', {className: 'muted chats-none'}, 'Your conversations will be listed here.');
  }
  else {
    return (
      __('ul', {className: 'chats-list'},
        list.items.map((c) => (
          __('li', {key: c.id, className: c.id === current ? 'current' : undefined},
            __(Link, {to: `/chats/${encodeURIComponent(c.id)}`, title: c.title, onClick: onOpen},
              __('span', {className: 'chats-list-title'}, c.title),
              __('span', {className: 'chats-list-date'}, formatDateTime(c.updatedAt)),
            ),
            __('button', {
              type: 'button', className: 'chats-icon-button', title: 'Delete', 'aria-label': `Delete ${c.title}`,
              onClick: () => onDelete(c.id),
            }, __(Trash2, {size: 14})),
          )
        ))
      )
    );
  }
}

function Messages({messages, passages, pending, shownAnswer, onFollow, onExample}: {
  messages: ChatMessage[], passages: Map<number, ChatPassage>, pending: Pending | null,
  shownAnswer: string | null, onFollow: (answerId: string, n: number) => void,
  onExample: (text: string) => void,
}) {
  const list = useRef<HTMLDivElement>(null);
  const tail = useRef(true);

  // kept at the bottom as the answer comes, unless the reader has scrolled up
  useEffect(() => {
    const el = list.current;
    if (el !== null && tail.current) {
      el.scrollTop = el.scrollHeight;
    }
  }, [messages, pending?.text]);

  if (messages.length === 0) {
    return (
      __('div', {className: 'chats-messages chats-empty'},
        __('h2', {}, 'Ask about the collection'),
        __('p', {className: 'muted'},
          'Each message is searched for in the books and journals of the library, and answered from the '
          + 'passages found, with links to them. Answers can be wrong: check them against the passages.'
        ),
        __('div', {className: 'chats-examples'},
          EXAMPLES.map((text) => __('button', {key: text, type: 'button', onClick: () => onExample(text)}, text))
        ),
      )
    );
  }
  else {
    return (
      __('div', {
        className: 'chats-messages',
        ref:       list,
        onScroll:  (e: React.UIEvent<HTMLDivElement>) => {
          const el     = e.currentTarget;
          tail.current = el.scrollHeight - el.scrollTop - el.clientHeight < 40;
        },
      },
        messages.map((m) => (
          m.role === 'user'
            ? __('div', {key: m.id, className: 'chat-message chat-user'}, m.content)
            : __(AnswerView, {key: m.id, message: m, passages, shown: m.id === shownAnswer, onFollow})
        )),
        pending !== null && pending.passages === null && __('div', {className: 'chat-message chat-answer chat-working muted'},
          'Searching the collection…'
        ),
      )
    );
  }
}

function AnswerView({message, passages, shown, onFollow}: {
  message: ChatMessage, passages: Map<number, ChatPassage>, shown: boolean,
  onFollow: (answerId: string, n: number) => void,
}) {
  const writing = message.id === PENDING_ANSWER_ID;
  const blocks  = answerBlocks(message.content);
  const cite    = (n: number) => onFollow(message.id, n);

  return (
    __('div', {className: 'chat-message chat-answer' + (shown ? ' shown' : '')},
      blocks.length === 0 && writing && __('p', {className: 'muted'}, 'Writing…'),
      blocks.map((b, i) => __(BlockView, {key: i, block: b, passages, cite})),
      message.error !== null && __('p', {className: 'chat-answer-error'}, message.error),
    )
  );
}

function BlockView({block, passages, cite}: {
  block: Block, passages: Map<number, ChatPassage>, cite: (n: number) => void,
}) {
  const inlines = (parts: Inline[]) => __(InlinesView, {parts, passages, cite});

  return match(block)
    .with({kind: 'paragraph'}, (b) => __('p', {}, inlines(b.inlines)))
    .with({kind: 'heading'}, (b) => __(b.level <= 2 ? 'h3' : 'h4', {}, inlines(b.inlines)))
    .with({kind: 'quote'}, (b) => __('blockquote', {}, inlines(b.inlines)))
    .with({kind: 'list'}, (b) => __(b.ordered ? 'ol' : 'ul', {},
      b.items.map((item, i) => __('li', {key: i}, inlines(item)))
    ))
    .exhaustive();
}

function InlinesView({parts, passages, cite}: {
  parts: Inline[], passages: Map<number, ChatPassage>, cite: (n: number) => void,
}): ReactElement {
  return __(Fragment, {},
    parts.map((p, i): ReactElement => match(p)
      .with({kind: 'text'}, (t) => __(Fragment, {key: i}, t.text))
      .with({kind: 'code'}, (t) => __('code', {key: i}, t.text))
      .with({kind: 'strong'}, (t) => __('strong', {key: i}, __(InlinesView, {parts: t.children, passages, cite})))
      .with({kind: 'em'}, (t) => __('em', {key: i}, __(InlinesView, {parts: t.children, passages, cite})))
      .with({kind: 'cite'}, (t) => {
        const passage = passages.get(t.n);
        // a number no passage has: the model made it up, so it's no link
        return passage === undefined
          ? __(Fragment, {key: i}, t.label)
          : __('a', {
            key:       i,
            className: 'chat-cite',
            href:      `#chat-passage-${t.n}`,
            title:     `${passage.title}${passage.author.length > 0 ? ', ' + passage.author : ''}`,
            onClick:   (e: React.MouseEvent) => {
              e.preventDefault();
              cite(t.n);
            },
          }, t.label, __('sup', {}, t.n));
      })
      .exhaustive())
  );
}

// The passages an answer cites, in the order it cites them, then those it was given and didn't
function AnswerPassages({answer, passages, highlighted, searchedByMeaning}: {
  answer: ChatMessage, passages: Map<number, ChatPassage>, highlighted: Shown | null,
  searchedByMeaning: boolean,
}) {
  const cited    = citedNumbers(answer.content).flatMap((n) => {
    const p = passages.get(n);
    return p === undefined ? [] : [p];
  });
  const citedSet = new Set(cited.map((p) => p.n));
  const others   = answer.passages.filter((p) => !citedSet.has(p.n));
  const [showOthers, setShowOthers] = useState(false);
  const othersOpen = showOthers || cited.length === 0
    || (highlighted?.n !== null && highlighted?.n !== undefined && !citedSet.has(highlighted.n));

  // the passage followed to, brought into view
  useEffect(() => {
    if (highlighted?.n !== null && highlighted?.n !== undefined) {
      document.getElementById(`chat-passage-${highlighted.n}`)?.scrollIntoView({behavior: 'smooth', block: 'start'});
    }
  }, [highlighted?.n, highlighted?.followed]);

  const card = (p: ChatPassage) => __(PassageCard, {
    key: p.n, passage: p, highlighted: highlighted?.n === p.n,
    followed: highlighted?.n === p.n ? highlighted.followed : 0,
  });

  return (
    __('div', {className: 'chats-passage-lists'},
      !searchedByMeaning && __('p', {className: 'muted chats-note'},
        'Searched by words only: the search by meaning is unavailable right now.'
      ),
      cited.length > 0 && __('ol', {className: 'chats-passage-list'}, cited.map(card)),
      others.length > 0 && (
        othersOpen
          ? __(Fragment, {},
            __('h3', {}, cited.length === 0 ? 'Passages found' : 'Also found, not cited'),
            __('ol', {className: 'chats-passage-list'}, others.map(card)),
          )
          : __('button', {type: 'button', className: 'chats-others', onClick: () => setShowOthers(true)},
            `${others.length} other passage${others.length === 1 ? '' : 's'} found, not cited`
          )
      ),
      cited.length === 0 && others.length === 0 && __('p', {className: 'muted'},
        answer.id === PENDING_ANSWER_ID ? 'Searching the collection…' : 'No passages were found for this message.'
      ),
    )
  );
}

function PassageCard({passage, highlighted, followed}: {
  passage: ChatPassage, highlighted: boolean, followed: number,
}) {
  const [open, setOpen] = useState(false);
  const pagePath        = openBooksPath([{bookId: passage.bookId, pageId: passage.pageNumber}]);
  const place           = pagePlace(passage);
  const long            = passage.text.length > PASSAGE_PREVIEW;
  const text            = long && !open ? passage.text.slice(0, PASSAGE_PREVIEW).replace(/\s+\S*$/, '') + '…' : passage.text;

  return (
    __('li', {
      id:        `chat-passage-${passage.n}`,
      // remade when followed to again, so the highlight plays again
      key:       followed,
      className: 'chat-passage' + (highlighted ? ' highlighted' : ''),
    },
      __('div', {className: 'chat-passage-head'},
        __('span', {className: 'chat-passage-n'}, passage.n),
        __('div', {},
          __(Link, {className: 'chat-passage-title', to: pagePath}, passage.title),
          __('div', {className: 'muted'}, passage.author.length > 0 ? passage.author : 'Anonymous'),
        ),
      ),
      __('blockquote', {className: 'chat-passage-text'},
        text.split(/\n\s*\n/).map((para, i) => __('p', {key: i}, para.trim()))
      ),
      __('div', {className: 'chat-passage-actions'},
        __(Link, {to: pagePath}, __(BookOpen, {size: 14, 'aria-hidden': true}), place),
        long && __('button', {type: 'button', onClick: () => setOpen(!open)}, open ? 'Show less' : 'Show all'),
      ),
    )
  );
}

// Where a passage starts: its printed page number, else (none, or a running head
// taken for one: "QUAESTIONES DISPUTATAE DE ANIMA") its scan's page
const pagePlace = (p: ChatPassage): string => {
  const printed = p.printedPageNumber.trim();

  if (/^\d+$/.test(printed)) {
    return `Page ${printed}`;
  }
  else if (printed.length === 0 || printed.length > 24 || /^[^a-z]*[A-Z]{4}[^a-z]*$/.test(printed)) {
    return `Scan page ${p.pageNumber}`;
  }
  else {
    return printed;
  }
};

// The box to write in: Enter sends, Shift+Enter starts a line; while an answer comes, it can be stopped
function Composer({draft, setDraft, answering, limit, onSend, onStop}: {
  draft: string, setDraft: (d: string) => void, answering: boolean, limit: string | null,
  onSend: (content: string) => void, onStop: () => void,
}) {
  const content = draft.trim();
  const canSend = !answering && limit === null && content.length > 0 && content.length <= MAX_MESSAGE_LENGTH;
  const submit  = () => {
    if (canSend) {
      onSend(content);
    }
  };

  return (
    __('form', {
      className: 'chats-composer',
      onSubmit:  (e: React.FormEvent) => {
        e.preventDefault();
        submit();
      },
    },
      limit !== null && __('p', {className: 'chats-limit'}, limit),
      __('div', {className: 'chats-composer-box'},
        __('textarea', {
          value:        draft,
          rows:         2,
          maxLength:    MAX_MESSAGE_LENGTH,
          placeholder:  'Ask about the collection…',
          'aria-label': 'Your message',
          disabled:     limit !== null,
          onChange:     (e: React.ChangeEvent<HTMLTextAreaElement>) => setDraft(e.target.value),
          onKeyDown:    (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
            if (e.key === 'Enter' && !e.shiftKey && !e.nativeEvent.isComposing) {
              e.preventDefault();
              submit();
            }
          },
        }),
        answering
          ? __('button', {type: 'button', className: 'chats-send', title: 'Stop', 'aria-label': 'Stop the answer', onClick: onStop},
            __(CircleStop, {size: 18}))
          : __('button', {type: 'submit', className: 'chats-send', title: 'Send', 'aria-label': 'Send', disabled: !canSend},
            __(ArrowUp, {size: 18})),
      ),
      draft.length > MAX_MESSAGE_LENGTH - 200 && __('div', {className: 'chats-count muted'},
        `${draft.length} / ${MAX_MESSAGE_LENGTH}`
      ),
    )
  );
}
