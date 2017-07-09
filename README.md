# Project Scoresheet

Open-source Haskell library for modeling baseball and working with [Retrosheet event data](http://www.retrosheet.org/eventfile.htm).

## Getting started

```
stack build
stack exec -- boxscore -y 2016 -t bos
stack exec -- event -y 2016 -t bos
```

## Pulling in Retrosheet data
Chadwick Bureau maintains a git repository containing the Retrosheet data files. This repository is setup as a submodule.

Run `git submodule init` to initialize the submodule,
and `git submodule update` to populate or update the data.

## Loading batting stats into Postgres
This assumes Postgres is already set up with user `postgres` for a local database
`baseball`:
```
PGUSER=postgres PGPASSWORD=password PGHOST=localhost PGDATABASE=baseball ./scripts/create-batting-stats-table.sh testgames.txt blah
```

## Running the tests

```
stack test
```

## Further Reading
* [Retrosheet](http://www.retrosheet.org/)
* [Chadwick](http://chadwick.sourceforge.net/doc/index.html)
* [GD2](http://gd2.mlb.com/components/game/mlb)