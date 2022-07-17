# message-db - Client library for Eventide's Message DB

Collection of libraries for interacting with [Eventide's Message DB](http://docs.eventide-project.org/user-guide/message-db/) via PostgreSQL. 
The purpose of these libraries are to make it easier to implement event sourcing 
systems in Haskell.

---


## Repository Structure

This repository contains the following packages:
- [message-db](./message-db) - Low-level library built on top of [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple).
- [message-db-temp](./message-db-temp) - Creates temporary message-db instances using [tmp-postgres](https://hackage.haskell.org/package/tmp-postgres) and the [official message-db scripts](https://github.com/message-db/message-db). This package is also used by [message-db tests](./message-db/test/) for running integration tests.
- [message-db-monad](./message-db-monad) - Utilizes [MonadUnliftIO](https://hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO) to allow running message-db actions in a custom monad.


## How to Build

Currently this project supports that last three major versions of `ghc` with `cabal`, and the last two major `lts` snapshots plus `nightly` for `stack`.

If you need to setup `ghc`, `cabal`, or `stack`; then I'd highly recommend using [ghcup](https://www.haskell.org/ghcup/).

### Preparation

Regardless of which tool chain you use, the first thing you'll need to do is clone the repo and initialize all of the submodules. This may be done with one command:

`git clone https://github.com/Disco-Dave/message-db.git --recurse-submodules`

Or if you already cloned the repo, then you may run the following inside of the repo folder to initialize the submodules:

`git submodule update --init --recursive`

You may also need some additional system dependencies, if you're on a debian based system then run the following:

`apt update && apt install -y libpq-dev`

### Cabal Instructions

1. `cabal update` - Ensures you have the latest package list from [Hackage](https://hackage.haskell.org/).
2. `cabal build all` - Builds _all_ packages.
3. `cabal test all` - Runs the test-suite for _all_ packages.

### Stack Instructions
1. `stack init` - Initializes the stack project. You can also specify a specific `resolver` instead:
  - `stack init --resolver lts-18`
  - `stack init --resolver lts-19`
  - `stack init --resolver nightly`
2. `stack build` - Builds _all_ packages.
3. `stack test` - Runs the test-suite for _all_ packages.
