# [DocuLib](https://github.com/nguermond/doculib)
A simple GUI for managing document metadata for books, textbooks, or articles.

## Design principle
The underlying idea is to locally store certain types of files in different libraries. Inside a library the directory structure becomes irrelevant and DocuLib keeps track of metadata for you, such as author, title, tags, DOI, etc. You can then quickly find a certain file by using keywords in the metadata. The key features of DocuLib are the following:

- Files can be moved or renamed within or between libraries without losing metadata, changes are detected automatically
- Portable libraries (libraries can be shared or synced independently)
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

## Installation
On Debian/Ubuntu:
```bash
wget https://github.com/nguermond/doculib/releases/download/v1.3.5/doculib_1.3.5_amd64.deb
sudo dpkg -i doculib_1.3.5_amd64.deb
sudo apt -f install
```

On all other systems, with the Opam package manager (see [how to install opam](https://opam.ocaml.org/doc/Install.html))
```bash
opam install doculib
```

## Screenshots
![screenshot-1.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-1.png?raw=true)

![screenshot-4.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-4.png?raw=true)

![screenshot-3.png](https://github.com/nguermond/doculib/blob/screenshots/screenshots/screenshot-3.png?raw=true)



## Comparison with [Zotero](https://www.zotero.org/)
DocuLib shares many similarities to Zotero, however the focus of DocuLib is in giving local control over your files rather than creating bibliographic entries. This difference lies in the way in which data is stored:

- Zotero puts a priority on metadata entries where a physical file is a child of that entry if it exists, whereas DocuLib entries are one-to-one with physical files on your computer
- files added to Zotero are either stored in a Zotero data directory (over which you have no control) or a link to a file on your computer (which you have to keep track of manually, so clearly not the preferred method), whereas DocuLib files are stored in libraries, of which you can have multiple. A library is a directory containing files you want DocuLib to index, but metadata for that library is also stored in that library. This means libraries are portable, so you can share them or sync them independently of DocuLib.
- Zotero stores metadata in a database, whereas DocuLib stores metadata as json files one-to-one with the corresponding document
