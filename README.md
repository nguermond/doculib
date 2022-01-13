# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* tag management (to be implemented)

## Configuration
Library configuration is stored in `$HOME/.doculib/libraries.json`.
Metadata is stored in `$HOME/.doculib/data`.

## Dependencies
```
sudo apt install libev-dev libgtk-3-0
```

Note this requires lablgtk3 (>= 3.1.2)
```
opam install dune lablgtk3 lwt irmin-unix tls
```

## Compile
```
dune build
```

## Run
```
dune exec ./doculib.exe
```