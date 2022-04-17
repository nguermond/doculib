# DocuLib
![screenshot.png](/icons/screenshot.png)
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* files can be moved or renamed without losing metadata
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* manage multiple libraries in different locations
* error permissive search

## Installation (linux)
On Debian/Ubuntu, enter the commands
```
wget https://github.com/nguermond/doculib/releases/download/v1.2.1/doculib_1.2.1_amd64.deb
sudo dpkg -i doculib_1.2.1_amd64.deb
sudo apt -f install
```
and run with `doculib`.

## Build from source (linux):
1. Install [opam](https://opam.ocaml.org/):
```
sudo apt install opam
```
2. Install system dependencies:
```
sudo apt install pkg-config libev-dev libgmp-dev libcairo2-dev libexpat1-dev libgtk-3-dev
```
4. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
3. Build:
```
cd doculib && make
```
5. Install (into `$HOME/.local/bin/`):
```
make install
```

## Build from source (MacOS):
1. Install [opam](https://opam.ocaml.org/):
```
brew install opam
```
2. Install system dependencies:
```
brew install pkg-config libev gmp cairo expat gtk+3
```
3. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
4. Build:
```
cd doculib && make
```
5. Install (into `$HOME/.local/bin/`):
```
make install
```
