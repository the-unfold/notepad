# notepad

A fullstack note keeping application built on Haskell and Elm

<img src="docs/icon.png" alt="notepad" width="300"/>

- [TODO](docs/TODO.md)

## Why?

It's a proof-of-concept project. Its priorities are consistency, type-safety, reproducibility.

- Haskell
- Event Sourcing
- Postgres as an event log
- Postgres as a relational database for projections
- Type checking against a real database at compile time (monorepository, no data needed, only migrations, and if CI passes, then backend and DB are consistent at particular commit)
- Editor setup for formatting and all the tools

- Elm
- Code generation from Haskell (monorepository, frontend type-checks against backend, if CI passes, then frontend and backend are consisntent at particular commit)
- Elm-UI
- Paak-UI
- Elm-test and elm-review
- Editor setup for formatting and all the tools

- Everything is deployed in Docker containers

### User story

- User can authenticate with Okta
- User can view his notes with the web application
- User can add a note, edit a note or delete a note

## Prerequisites

- For building docker images from the codebase you only need Docker and `docker-compose`.
- For developing with good editor support and running apps outside of docker, you need SCode + all recommended extensions
  - For frontend, global: `elm`, `elm-format`, `elm-review`.
  - For backend, global: `stack` _(`ghc`, `ormolu`, `hlint`, `ghc` and `cabal` will probably be installed by language extensions, not sure. But they are used anyways, so you can install them yourself.)_

## Initial setup

- Copy `.env.example` to `.env`

```sh
cp backend/.env.example backend/.env
cp frontend/.env.example frontend/.env
```

- Install frontend dependencies

```sh
cd frontend
npm i
```

## Dev workflow with VSCode

- Run VSCode with a special command, opening the project as a multi-root workspace with configured extensions. 
  - It will probably ask you to install the recommended extensions. Just do it.
  - Then, when running all other commands in vscode terminal, you no longer need to explicitly load env files.
  - Also, when using LiveShare terminal output is visible for all participants, which is really helpful.
  - And When using docker-compose for everything, you just shouldn't care about envs, cause they are already set in `yaml` files.

```sh
bash -c 'export $(cat backend/.env | sed "s/#.*//g" | xargs) && code ./notepad.code-workspace'
```

### Backend outside docker

- Run PostgreSQL before building any backend executable or codegen. `postgresql-typed` will run a compile-time connection to the database and typecheck all your queries. So, local DB should be always running, but it doesn't require you to populate it with data.
  - `-V` means that volumes will be erased on container exit.
  - Note: you are not able to build a backend docker image on Apple Silicon processor yet, but once it's built, you can pull the image and run it successfully.

```sh
docker-compose up -V postgres
```

- Rebuild backend watching files (PostgreSQL must be running and `.env` file must be filled with connection details)

```sh
stack build --file-watch
```

- Run server (does not reload on code change, needs manual stopping and re-running)

```sh
stack run server
```
- Run codegen locally

```sh
stack run server
```

### Frontend outside docker

- Run frontend in dev mode, watching files

```sh
npm start
```

- Run frontend the same way, but with the Elm debugger

```sh
npm run start-debug
```

- Ensure that you're not committing a crime

```sh
npm run precommit
```

- Fix mistakes like a lazy bitch (and then look what you've done, idiot).

```sh
npm run precommit-fix
```

### Build and run production docker images locally

Note: the main reason to run this file is to **debug** production docker images locally before building them on Github Actions (which should be rare).

- Build

```sh
bash prod-local-build-all.sh
```

- Run _(TODO: backend should wait for postgres to become ready)_

```sh
docker-compose -f dc.prod-local.yml up -Vd
```
