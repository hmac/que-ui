FROM fpco/stack-build:lts-9.17 as build
WORKDIR /build

COPY app .
COPY src .
COPY que-ui.cabal .
COPY stack.yaml .
COPY Setup.hs .

RUN stack build --system-ghc

RUN cp $(stack path --system-ghc --local-bin)/que-ui /usr/local/bin/que-ui

FROM fpco/ubuntu-with-libgmp:14.04
RUN apt-get update && apt-get install -y libpq-dev

COPY --from=build /usr/local/bin/que-ui /usr/local/bin/que-ui

COPY client/build/static/js/main.*.js main.js
CMD ["/usr/local/bin/que-ui"]
