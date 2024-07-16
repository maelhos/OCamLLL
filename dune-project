(lang dune 3.7)

(name OCamLLL)

(generate_opam_files true)

(source
 (github maelhos/OCamLLL))

(authors "Augustin Perrin"
         "Maël Hostettler"
)

(maintainers "Didier Raoult")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name OCamLLL)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune zarith)
 (tags
  (topics "Lattice"
           "LLL")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
