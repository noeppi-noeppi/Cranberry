FROM debian:trixie AS base
RUN apt-get update && apt-get install -y tini libffi8 libgmp10 libtinfo6 libpq5 zlib1g libtasn1-6 && apt-get clean

FROM base AS build
RUN apt-get install -y build-essential curl git make libffi-dev libgmp-dev libncurses6 libncurses-dev postgresql-client libpq-dev zlib1g-dev elm-compiler node-uglify && apt-get clean
WORKDIR /data/bin
ENV PATH="/data/bin:${PATH}"
RUN curl -Lo ghcup 'https://downloads.haskell.org/~ghcup/0.1.50.2/x86_64-linux-ghcup-0.1.50.2' && \
    echo 'ff6288df9758211372d8242fe830d8e6be6a8365d9406f1c9bde144b7e744143  ghcup' | sha256sum -c && \
    chmod +x ghcup
RUN ghcup --downloader curl install ghc --isolate /data --force --set 9.14.1 && rm -rf "${HOME}/.ghcup"
RUN ghcup --downloader curl install cabal --isolate /data/bin --set && cabal update && rm -rf "${HOME}/.ghcup"
COPY . /data/build
WORKDIR /data/build
RUN make clean && make all

FROM base
COPY --from=build /data/build/bin/cranberry /usr/local/bin/cranberry
WORKDIR /data
ENTRYPOINT [ "tini", "--" ]
CMD [ "cranberry", "/data/config.yaml" ]
