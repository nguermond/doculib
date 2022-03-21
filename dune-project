(lang dune 2.9)
(name doculib)
(version 1.0.1)

(generate_opam_files true)
(source (github nguermond/doculib))
(license GPL-3.0)
(authors "Nathan Guermond")
(maintainers "nguermond@protonmail.com")
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