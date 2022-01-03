# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* search for metadata on openlibrary.org and semanticscholar.org
* metadata includes authors, title, publishing year, tags, and DOI/ISBN
* tag management (to be implemented)

## Dependencies
```
opam install lablgtk3 lwt irmin-unix
```

## Compile
```
dune build
```

## Configure
Edit `doculib_config.json`, where `"root"` should denote a path to tthe librar's location, and `"data"` should denote a location to store the  database.It should suffice treplace `user` with username.

## Run
```
dune exec ./doculib.exe
```