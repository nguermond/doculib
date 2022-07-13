(lang dune 2.9)
(name doculib)


(generate_opam_files true)
(source (github nguermond/doculib))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

(package
 (name metadb)
  (version 1.0.1)
 (synopsis "A database for storing and managing file metadata in JSON format")
 (description
   "Metadb is a library to locally store and manipulate a JSON database of file metadata. A key feature of Metadb is that a user may rename or move files within or between libraries, and Metadb will resolve missing or renamed files according to their MD5 hash. Metadb can also find duplicate files across libraries."
 )
 (depends
  dune
  (fileutils (>= 0.5.3))
  (yojson (>= 2.0.0))
  (ocaml (>= 4.13.0))
))

(package
 (name doculib)
  (version 1.3.1)
 (synopsis "A GUI for tagging and managing document metadata for books, textbooks, or articles")
 (description
   "DocuLib manages locally stored libraries of books, textbooks, or articles by keeping track of data such as authors, title, year, tags, and DOI/ISBN. Metadata can be searched through openlibrary.org and semanticscholar.org. DocuLib automatically detects renamed, moved, and duplicate files."
   )
 (depends
  dune
  (cohttp-lwt-unix (>= 2.5.0))
  (metadb (>= 1.0.1))
  (lablgtk3 (>= 3.1.2))
  (ocaml (>= 4.12.0))
  (tls (>= 0.13.0))
  ))
