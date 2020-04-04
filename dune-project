(lang dune 1.11)
(name accrc)

(generate_opam_files true)

(authors "Ante")
(maintainers "nikuda@gmail.com")

(package
 (name accrc)
 (synopsis "Assetto Corsa Competizione results")
 (depends
  (dune (> 1.5))
  (uutf (> 1.0.2))
  (yojson (> 1.7.0))))
