(lang dune 3.0)

(name cgi)

(version 0.12)

(generate_opam_files true)

(source (github rixed/ocaml-cgi))

(authors
  "Daniel de Rauglaudre"
  "Jean-Christophe FILLIATRE"
  "Cedric Cellier"
)

(maintainers "rixed-opam@happyleptic.org")

(license LICENSE)

(package
 (name cgi)
 (synopsis "Library for writing CGIs")
 (depends (ocaml (>= 4.02.0)) dune)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
