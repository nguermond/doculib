# DocuLib
![screenshot.png](/icons/screenshot.png)
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* files can be moved or renamed without losing metadata
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* manage multiple libraries in different locations
* error permissive search

## Installation
On Debian/Ubuntu, enter the commands
```
wget https://github.com/nguermond/doculib/releases/download/v1.2.1/doculib_1.2.1_amd64.deb
sudo dpkg -i doculib_1.2.1_amd64.deb && sudo apt -f install
```
and run with `doculib`.

## Build from source
See [BUILD](./BUILD.md).