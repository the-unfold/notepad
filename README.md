# haskell-spock

## Dev workflow

- Copy `.env.example` to `.env`

- RUn VSCode (or you favorite editor, which is VSCode).

```sh
bash -c 'export $(cat backend/.env | sed "s/#.*//g" | xargs) && code ./notepad.code-workspace'
```

- Run PostgreSQL

```sh
docker-compose up -V db
```

(This way db runs in an attached mode, but volumes will be erased on container exit.)

- Rebuild backend watching files (PostgreSQL must be running and `.env` file must be filled with connection details)

```sh
bash -c 'export $(cat .env | sed "s/#.*//g" | xargs) && stack build --file-watch'
```
(or just `stack build --file-watch` in vscode terminal)

- Rebuild backend watching files

```sh
bash -c 'export $(cat .env | sed "s/#.*//g" | xargs) && stack run'
```
(or just `stack run` in vscode terminal)