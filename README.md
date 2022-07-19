# DocuLib
![screenshot-1.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-1.png)
![screenshot-4.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-4.png)
![screenshot-1.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-3.png)
A simple GUI for managing document metadata for books, textbooks, or articles.

## Features
* files can be moved or renamed without losing metadata
* mark duplicate files
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* manage multiple libraries in different locations
* error permissive search

## Installation
On Debian/Ubuntu:
```
wget https://github.com/nguermond/doculib/releases/download/v1.3.2/doculib_1.3.2_amd64.deb
sudo dpkg -i doculib_1.3.2_amd64.deb
sudo apt -f install
```

With the Opam package manager (see [how to install opam](https://opam.ocaml.org/doc/Install.html))
```
opam install doculib
```
