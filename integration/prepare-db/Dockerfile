FROM ubuntu:14.04
RUN apt-get update && apt-get install -y postgresql-client

WORKDIR /app

COPY run.sh ./run
ENTRYPOINT ./run
