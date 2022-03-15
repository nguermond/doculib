# DocuLib
![screenshot.png](/icons/screenshot.png)
A GUI for managing document metadata for books, textbooks, or articles.


## Features
* multiple libraries (and two document types - book/article)
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* error permissive search

## Configuration
Metadata for DocuLib is stored in `$HOME/.doculib/`.

## Install from binary (linux):
1. Download binary:
```
wget https://github.com/nguermond/doculib/releases/download/v1.0.0/doculib-linux-x86_64-v1.0.0.tar.gz
```
2. Extract:
```
tar -xzf doculib-linux-x86_64-v1.0.0.tar.gz
```
3. Install (into `/usr/local/bin`):
```
cd doculib-linux-x86_64-v1.0.0 && sudo ./install.sh
```
4. Run: `doculib`

## Install from binary (Windows):
1. Install linux
2. Follow instructions above

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
4. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
5. Install:
```
cd doculib && opam install .
```

## To be (maybe) implemented
* fancy tag management (synonyms, subtags...)
* local compressed [zbMATH](https://zbmath.org/) library for faster metadata search