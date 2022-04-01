# DocuLib
![screenshot.png](/icons/screenshot.png)
A GUI for managing document metadata for books, textbooks, or articles.

## Features
* files can be moved or renamed without losing metadata
* metadata includes authors, title, publishing year, tags, bookmark, and DOI/ISBN
* search for metadata on [openlibrary.org](https://openlibrary.org/) and [semanticscholar.org](https://www.semanticscholar.org/)
* manage multiple libraries in different locations
* error permissive search

## Usage
Each library points to a unique directory, and all files in that directory are automatically added to that library. Each library must have a unique document type, book or article, which indicates
* the type of new items added to the library
* the default database to search for metadata ([openlibrary.org](https://openlibrary.org/) or [semanticscholar.org](https://www.semanticscholar.org/))
* whether to store DOI or ISBN.

When searching for metadata, the default search query is either the title or the name of the original file. To open a file, search for a files metadata, or perform other actions, right click on an entry. Double click on an entry to edit.

An example directory structure is as follows:
* Library "Math\Textbooks" (type: book):
```
~/Libraries/Math/Textbooks
> Analysis/Rudin_principles_of_mathematical_analysis.djvu
> "Algebra/Category Theory/MacLane Categories for the working mathematician.pdf"
> Trash/Stewart_calculus_14th_edition.pdf
```
* Library "Math\Articles" (type: article):
```
~/Libraries/Math/Articles
> obscure_journal_71_v2.ps
> fancy_math_with_numbers.pdf
```
* Library "CS\Articles" (type: article):
```
~/Libraries/CS/Articles
> COMPUTING_MACHINERY_AND_INTELLIGENCE.pdf
> "Notions of Computation and Monads.pdf"
```
* Library "eBooks" (type: book):
```
~/Documents/Stuff/eBooks
> scifi/asimov-foundation_trilogy.epub
> mystery/2923497_203098.mobi
```

## Installation
On Debian/Ubuntu, enter the commands
```
wget https://github.com/nguermond/doculib/releases/download/v1.2.0/doculib_1.2.0_amd64.deb
sudo dpkg -i doculib_1.2.0_amd64.deb && sudo apt -f install
```
and run with `doculib`.

## Build from source
See [BUILD](./BUILD.md).