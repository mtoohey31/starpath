(lang dune 3.11)

(name starpath)

(generate_opam_files true)

(source
 (github mtoohey31/starpath))

(authors "Matthew Toohey <contact@mtoohey.com>")

(maintainers "Matthew Toohey <contact@mtoohey.com>")

(license BSD-3-Clause)

(package
 (name starpath)
 (synopsis "A position-aware parser combinator library with readable errors")
 (depends (ocaml (>= 4.11)) dune (ounit2 :with-test))
 (tags
  (parser parser-combinators)))
