# DocuLib
A GUI for managing document metadata for books, textbooks, or articles.

![screenshot](/icons/screenshot.png)


## Features
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* error permissive search

## Configuration
Metadata for DocuLib is tored in `$HOME/.doculib/`.

## Install from binary (linux):
1. Install system dependencies:
```
sudo apt install libev-dev libgtk-3-0
```
2. Download binary:
```
wget https://github.com/nguermond/doculib/releases/download/v1.0.0/doculib_x86_64_v1.0.0.tar.gz
```
3. Extract:
```
tar -xzf doculib_x86_64_v1.0.0.tar.gz
```
4. Install (into /usr/local/bin):
```
cd 1.0.0 && sudo sh ./install.sh
```
4. Run: `doculib &`.

## Build from source (unix):
1. Install [opam](https://opam.ocaml.org/)
2. Install OCaml >= 4.12.0:
```
opam switch create 4.12.0 && eval $(opam env)
```
3. Install system dependencies:
```
sudo apt install libgmp-dev pkg-config libcairo2-dev libexpat1-dev libgtk-3-dev
```
4. Install Ocaml dependencies:
```
opam install dune cohttp-lwt-unix yojson lablgtk3.3.1.2
```
5. Compile with `dune build`
6. Run with `dune exec ./doculib.exe`

## To be (maybe) implemented
* fancy tag management (synonyms, subtags...)
* local compressed [zbMATH](https://zbmath.org/) library for faster metadata search