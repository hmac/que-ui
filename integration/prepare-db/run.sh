#!/usr/bin/env bash
until psql
do
  echo "waiting for database..."
  sleep 0.1
done
echo "CREATE TABLE IF NOT EXISTS que_jobs (
    priority smallint DEFAULT 100 NOT NULL,
    run_at timestamp with time zone DEFAULT now() NOT NULL,
    job_id bigserial NOT NULL,
    job_class text NOT NULL,
    args json DEFAULT '[]'::json NOT NULL,
    error_count integer DEFAULT 0 NOT NULL,
    last_error text,
    queue text DEFAULT ''::text NOT NULL,
    retryable boolean DEFAULT true,
    failed_at timestamp with time zone
)" | psql
echo "CREATE TABLE IF NOT EXISTS users (
    id serial NOT NULL,
    name text NOT NULL,
    removed_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
)" | psql
