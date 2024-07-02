(lang dune 3.16)

(name camlbinators)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name camlbinators)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml (= 5.1.1))
   dune
   ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
