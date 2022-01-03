# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
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
Edit `doculib_config.json`, where `"root"` should denote a path to the library's location, and `"data"` should denote a location to store the  database. It should suffice to replace `user` with your username.

## Run
```
dune exec ./doculib.exe
```