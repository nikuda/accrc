(lang dune 2.4)
(name accrc)

(generate_opam_files true)

(authors "Ante")
(maintainers "nikuda@gmail.com")

(package
 (name accrc)
 (synopsis "Assetto Corsa Competizione Race Control")
 (depends
  (dune (>= 2.4))
  (cmdliner (>= 1.0.4))
  (uutf (>= 1.0.2))
  (yojson (>= 1.7.0))))
