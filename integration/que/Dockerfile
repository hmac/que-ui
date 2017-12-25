FROM ruby:2.4

RUN apt-get update && apt-get install -y netcat

WORKDIR /app
RUN gem install bundler
COPY Gemfile* ./
RUN bundle install

COPY words.txt /usr/share/dict/words

COPY main.rb .
COPY run.sh .
ENTRYPOINT ./run.sh
