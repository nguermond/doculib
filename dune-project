(lang dune 2.9)
(name doculib)
(version 1.3.0)

(generate_opam_files true)
(source (github nguermond/doculib))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

(package
 (name metadb)
 (synopsis "A database for storing and managing file metadata in JSON format")
 (depends
   dune
   yojson
  (ocaml (>= 4.12.0))
  ))

(package
 (name doculib)
 (synopsis "A GUI for tagging and managing document metadata for books, textbooks, or articles")
 (depends
  dune
  cohttp-lwt-unix
  yojson
  (lablgtk3 (>= 3.1.2))
  (ocaml (>= 4.12.0))
  tls
  ))
