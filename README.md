# DocuLib
![screenshot.png](/icons/screenshot.png)
A simple GUI for managing document metadata for books, textbooks, or articles.

## Features
* files can be moved or renamed without losing metadata
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* manage multiple libraries in different locations
* error permissive search

## Installation (Debian/Ubuntu)
On Debian/Ubuntu:
```
wget https://github.com/nguermond/doculib/releases/download/v1.3.0/doculib_1.3.0_amd64.deb
sudo dpkg -i doculib_1.3.0_amd64.deb
sudo apt -f install
```
and run with `doculib`.

## Build from source (linux):
Build with [opam](https://opam.ocaml.org/). Sytem dependencies:
```
pkg-config libev-dev libgmp-dev libcairo2-dev libexpat1-dev libgtk-3-dev
```

## Build from source (MacOS):
Build with [opam](https://opam.ocaml.org/). System dependencies:
```
pkg-config libev gmp cairo expat gtk+3
```
