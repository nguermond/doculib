# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

![screenshot](/icons/screenshot.png)


## Features
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* error permissive search

## Configuration
Metadata for DocuLib is tored in `$HOME/.doculib/`.

## How to build from source:
1. Install [opam](https://opam.ocaml.org/)
2. Install OCaml >= 4.12.0:
```
opam switch create 4.12.0 && eval $(opam env)
```
2. Install system dependencies:
```
sudo apt install libgmp-dev pkg-config libcairo2-dev libexpat1-dev libgtk-3-dev
```
3. Install Ocaml dependencies:
```
opam install dune cohttp-lwt-unix yojson lablgtk3.3.1.2
```
4. Compile with `dune build`
5. Run with `dune exec ./doculib.exe`

## To be (maybe) implemented
* fancy tag management (synonyms, subtags...)
* local compressed zbMath library for faster metadata search