# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* tag management (to be implemented)

## Dependencies
Note this requires lablgtk3 (>= 3.1.2)
```
opam install dune lablgtk3 lwt irmin-unix tls
```

## Compile
```
dune build
```

## Configure
Edit `doculib_config.json`, where `"root"` should denote a path to the library's location.
The database is stored in `$HOME/.doculib/data`. 

## Run
```
dune exec ./doculib.exe
```