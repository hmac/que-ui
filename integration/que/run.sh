#!/usr/bin/env bash

echo "attempting to connect to database at $PGHOST $PGPORT"
until nc -w 1 "$PGHOST" "$PGPORT"
do
  echo "waiting for database..."
  sleep 0.2
done
echo "connected to database"

bundle exec ruby main.rb
