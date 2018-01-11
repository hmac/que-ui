#!/usr/bin/env bash

echo "attempting to connect to database at $PGHOST"
until nc -w 1 "$PGHOST" 5432
do
  echo "waiting for database..."
  sleep 0.2
done
echo "connected to database"

bundle exec ruby main.rb
