# DocuLib
![screenshot.png](/icons/screenshot.png)
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* multiple libraries (and two document types - book/article)
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* error permissive search
* files can be moved or renamed within library

## Usage
Each library points to a unique directory. You can choose whether to automatically keep track of all items in that directory to the library,
or add items manually. The default search query is the name of the file.
The type of the library determines the following:
* "book": store ISBN and search on [openlibrary.org](https://openlibrary.org), it works best to name files by author and title
* "article": store DOI and search on [semanticscholar.org](https://semanticscholar.org), it works best to name files by title only

Example directory structure is as follows:
* Library "Textbooks" (type: book):
```
/home/user/Libraries/Textbooks
> example0.pdf
> math/example1.pdf
> math/example2.djvu
> cs/example3.ps
```
* Library "Articles" (type: article):
```
/home/user/Libraries/Articles
> example0.pdf
> example1.pdf
```
* Library "eBooks" (type: book):
```
/home/user/other/path/eBooks
> path/to/example0.epub
> example1.mobi
```

## Installation
On Debian/Ubuntu, enter the commands
```
wget https://github.com/nguermond/doculib/releases/download/v1.1.0/doculib_1.1.0_amd64.deb
sudo dpkg -i doculib_1.1.0_amd64.deb && sudo apt -f install
```
and run with `doculib`.

## Build from source
See [BUILD](./BUILD.md).