FROM fpco/stack-build:lts-9.17 as build
WORKDIR /build
COPY . .
RUN stack build --system-ghc

RUN cp $(stack path --local-bin)/bin/que-ui /usr/local/bin/que-ui

FROM fpco/ubuntu-with-libgmp:14.04
RUN apt-get update && apt-get install -y libpq-dev

COPY --from=build /usr/local/bin/que-ui /usr/local/bin/que-ui
CMD ["/usr/local/bin/que-ui"]
