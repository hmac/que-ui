FROM fpco/stack-build:lts-9.17 as build
WORKDIR /build

COPY que-ui.cabal .
COPY stack.yaml .
COPY Setup.hs .
COPY LICENSE .

RUN stack build --system-ghc --only-snapshot

COPY app app
COPY src src

RUN stack build --system-ghc

RUN cp $(stack path --system-ghc --local-install-root)/bin/que-ui /usr/local/bin/que-ui

FROM fpco/ubuntu-with-libgmp:14.04
RUN apt-get update && apt-get install -y libpq-dev

COPY --from=build /usr/local/bin/que-ui /usr/local/bin/que-ui

COPY assets /ekg
COPY client/build/static/js/main.*.js main.js
CMD ["/usr/local/bin/que-ui"]
