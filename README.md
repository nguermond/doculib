# [DocuLib](https://github.com/nguermond/doculib)
A simple GUI for managing document metadata for books, textbooks, or articles.

## Design principle
The underlying idea is to locally store certain types of files in different libraries. Inside a library the directory structure becomes irrelevant and DocuLib keeps track of metadata for you, such as author, title, tags, DOI, etc. You can then quickly find a certain file by using keywords in the metadata. The key features of DocuLib are the following:

- Files can be moved or renamed within or between libraries without losing metadata, changes are detected automatically
- Automatically detect and mark duplicate files
- Metadata includes
  - authors
  - title
  - publishing year
  - tags
  - bookmark
  - notes
  - DOI/ISBN
- Search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
- Obtain bibTex from [crossref.org](https://corssref.org/)
- Manage multiple libraries in different locations
- Error permissive search

## Screenshots
![screenshot-1.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-1.png?raw=true)

![screenshot-4.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-4.png?raw=true)

![screenshot-3.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-3.png?raw=true)


## Installation
On Debian/Ubuntu:
```bash
wget https://github.com/nguermond/doculib/releases/download/v1.3.4/doculib_1.3.4_amd64.deb
sudo dpkg -i doculib_1.3.4_amd64.deb
sudo apt -f install
```

On all other systems, with the Opam package manager (see [how to install opam](https://opam.ocaml.org/doc/Install.html))
```bash
opam install doculib
```
