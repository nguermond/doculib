(lang dune 2.9)
(name doculib)


(generate_opam_files true)
(source (github nguermond/doculib))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

(package
 (name metadb)
  (version 1.0.0)
 (synopsis "A database for storing and managing file metadata in JSON format")
 (depends
  dune
  (fileutils (>= 0.5.3))
  (yojson (>= 2.0.0))
  (ocaml (>= 4.12.0))
))

(package
 (name doculib)
  (version 1.3.0)
 (synopsis "A GUI for tagging and managing document metadata for books, textbooks, or articles")
 (depends
  dune
  (cohttp-lwt-unix (>= 2.5.0))
  (metadb (>= 1.0.0))
  (lablgtk3 (>= 3.1.2))
  (ocaml (>= 4.12.0))
  (tls (>= 0.13.0))
  ))
