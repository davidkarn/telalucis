# Postgres with pgvector (https://github.com/pgvector/pgvector), for the passages' embeddings (see
# migrations/0030_create_passages.ts). Built on the same Alpine image as before rather than
# pgvector/pgvector (Debian): an existing data folder's text indexes are sorted by the C library's
# collation, which differs between Alpine's musl and Debian's glibc.
#   docker build -f docker/postgres.Dockerfile -t tela-lucis-postgres .
FROM postgres:16-alpine

ARG PGVECTOR_VERSION=0.8.7

# OPTFLAGS="": built for any CPU of the architecture, not only the one building it.
# DOCKER_PG_LLVM_DEPS: the clang and llvm the image's Postgres was built with, for its JIT bitcode.
RUN apk add --no-cache --virtual .pgvector-build git build-base $DOCKER_PG_LLVM_DEPS \
  && git clone --branch v${PGVECTOR_VERSION} --depth 1 https://github.com/pgvector/pgvector.git /tmp/pgvector \
  && make -C /tmp/pgvector OPTFLAGS="" \
  && make -C /tmp/pgvector install \
  && rm -rf /tmp/pgvector \
  && apk del .pgvector-build
