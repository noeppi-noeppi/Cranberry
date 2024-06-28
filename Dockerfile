FROM debian:bookworm AS base
RUN apt-get update && apt-get install -y tini libffi8 libgmp10 libtinfo5 libpq5 zlib1g libtasn1-6 && apt-get clean

FROM node:bookworm AS nodeenv
RUN npm install --global uglify-js

FROM base AS build
RUN apt-get install -y build-essential curl libffi-dev libgmp-dev libncurses5 libncurses-dev postgresql-client libpq-dev zlib1g-dev && apt-get clean
WORKDIR /data/bin
ENV PATH="/data/bin:${PATH}"
RUN curl -Lo ghcup 'https://downloads.haskell.org/~ghcup/0.1.22.0/x86_64-linux-ghcup-0.1.22.0' && \
    echo 'bf213f4dfd2271b46ca52e2f14e96850ce32e9115e5acc90f1dc5a4e815e32af  ghcup' | sha256sum -c && \
    chmod +x ghcup
RUN ghcup --downloader curl install ghc --isolate /data --force --set 9.4.8 && rm -rf "${HOME}/.ghcup"
RUN ghcup --downloader curl install cabal --isolate /data/bin --set && cabal update && rm -rf "${HOME}/.ghcup"
RUN curl -Lo - https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz | gunzip > elm && \
    echo 'f8f12a61a61f64ac71a85d57284cc4d14fb81f1cbebb8b150839d9731034092e  elm' | sha256sum -c && \
    chmod +x /data/bin/elm
COPY --from=nodeenv --link /usr/local /node
ENV PATH="/node/bin:${PATH}"
COPY . /data/build
WORKDIR /data/build/web
RUN elm make --optimize --output=index.full.js src/Main.elm && \
    npx uglify-js index.full.js --compress \
      'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | \
    npx uglify-js --mangle --output index.js
RUN mv src/Main.js src/Main.full.js && npx uglify-js src/Main.full.js --compress --mangle --enclose --output src/Main.js
WORKDIR /data/build
RUN cabal build && cp "$(cabal -v0 list-bin exe:cranberry)" /data/bin/cranberry

FROM base
COPY --from=build /data/bin/cranberry /usr/local/bin/cranberry
WORKDIR /data
ENTRYPOINT [ "tini", "--" ]
CMD [ "cranberry" ]
