# Tela Lucis: the API (Nest, run with tsx as in development) and the web app (Vite, built and
# served by nginx, which passes /api to the API). Build a target:
#   docker build --target api -t tela-lucis-api .
#   docker build --target worker -t tela-lucis-worker .   (the e-book worker, with Calibre)
#   docker build --target web -t tela-lucis-web .
# or run everything, with Postgres and the migrations, with docker compose (docker-compose.yml).

ARG NODE_VERSION=22.17.1

# --- the API, its CLI commands and the migrations ---
FROM node:${NODE_VERSION}-slim AS api
WORKDIR /app

# all dependencies, dev ones too: the API runs its TypeScript with tsx, a dev dependency (so
# NODE_ENV is set only after installing)
COPY package.json package-lock.json ./
RUN npm ci && npm cache clean --force
ENV NODE_ENV=production NPM_CONFIG_UPDATE_NOTIFIER=false

COPY tsconfig.json types.ts migrate.ts api.yaml ./
COPY actions ./actions
COPY api ./api
COPY book_importers ./book_importers
COPY cli ./cli
COPY core ./core
COPY lib ./lib
COPY migrations ./migrations
COPY model ./model

USER node
EXPOSE 3000
CMD ["npx", "tsx", "api/main.ts"]

# --- the e-book worker: the API's code, with Calibre, making the .mobi files readers ask for ---
FROM node:${NODE_VERSION}-slim AS worker
WORKDIR /app

# Calibre's ebook-convert, which makes .mobi files of the books' EPUBs (lib/ebook.ts): Debian's
# calibre, without its recommended extras (most of the image's size), installed before the code
# so a change to the code doesn't install it again. QT_QPA_PLATFORM=offscreen: Calibre's Qt runs
# with no display.
RUN apt-get update \
  && apt-get install -y --no-install-recommends calibre \
  && rm -rf /var/lib/apt/lists/*
ENV QT_QPA_PLATFORM=offscreen NODE_ENV=production NPM_CONFIG_UPDATE_NOTIFIER=false

COPY --from=api /app /app

USER node
CMD ["npx", "tsx", "cli/main.ts", "ebook-worker"]

# --- building the web app ---
FROM node:${NODE_VERSION}-slim AS web-build
WORKDIR /app/web

COPY web/package.json web/package-lock.json ./
RUN npm ci

# the web app imports ../lib/lib.ts (components/pager.ts)
COPY lib /app/lib
COPY web ./
RUN npx vite build

# --- the web app, served by nginx ---
FROM nginx:1.29-alpine AS web
COPY docker/nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=web-build /app/web/dist /usr/share/nginx/html
EXPOSE 80
