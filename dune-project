(lang dune 2.9)
(name doculib)


(generate_opam_files true)
(source (github nguermond/doculib))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

(package
 (name doculib)
 (version %%VERSION_NUM%%)
 (synopsis "A GUI for tagging and managing document metadata for books, textbooks, or articles")
 (description
   "DocuLib manages locally stored libraries of books, textbooks, or articles by keeping track of data such as authors, title, year, tags, and DOI/ISBN. Metadata can be searched through openlibrary.org and semanticscholar.org. DocuLib automatically detects renamed, moved, and duplicate files."
   )
 (depends
  dune
  (cohttp-lwt-unix (>= 2.5.0))
  (metadb (= 1.1.0))
  (lablgtk3 (>= 3.1.2))
  (ocaml (>= 4.13.0))
  (ocamlgraph (>= 1.8.1))
  (xdg-basedir (< 0.1.0))
  tls-lwt
  ))
