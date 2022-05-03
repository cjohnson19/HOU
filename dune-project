(lang dune 3.1)

(name higher_ord_unif)

(generate_opam_files true)

(source
 (github cjohnson19/HOU))

(authors "Chase Johnson")

(maintainers "Chase Johnson")

(license MIT)

(package
 (name higher_ord_unif)
 (synopsis "An implementation of higher order unification")
 (description "Nipkow's presentation of higher order unification")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
