# haskell-spock

## Dev workflow

- Copy `.env.example` to `.env`

- Run PostgreSQL

```sh
docker-compose up -d db
```

- Rebuild backend watching files (PostgreSQL must be running and `.env` file must be filled with connection details)

```sh
bash -c 'export $(cat .env | sed "s/#.*//g" | xargs) && stack build --file-watch'
```

- Rebuild backend watching files

```sh
bash -c 'export $(cat .env | sed "s/#.*//g" | xargs) && stack run'
```