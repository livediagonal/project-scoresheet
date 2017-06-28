#!/usr/bin/env bash

set -eux

[ -z "$PGUSER" ] && echo "PGUSER env var is not set" && exit 1
[ -z "$PGHOST" ] && echo "PGHOST env var is not set" && exit 1
[ -z "$PGDATABASE" ] && echo "PGDATABASE env var is not set" && exit 1
[ -z "$PGPASSWORD" ] && echo "PGPASSWORD env var is not set" && exit 1
[ -z "$1" ] && echo "Expected events source file as first argument" && exit 1
[ -z "$2" ] && echo "Expected target table name as second argument" && exit 1

echo "Generating batting stats from $1"
stack build && stack exec batting-stats-csv $1 > /tmp/batting-stats.csv

echo "Creating table $2"
psql -c "DROP TABLE IF EXISTS $2" > /dev/null
psql -c "CREATE TABLE $2 (player_id varchar(8) NOT NULL, at_bats integer NOT
NULL, runs integer NOT NULL, hits integer NOT NULL, rbi integer NOT NULL, walks
integer NOT NULL, strikeouts integer NOT NULL, lob integer NOT NULL)" > /dev/null

psql -c "COPY $2 FROM '/tmp/batting-stats.csv' DELIMITER ',' CSV;"
