(lang dune 3.22)

(name cccc)

(generate_opam_files true)

(source
 (github penelopeysm/cc))

(authors "Penelope Yong <penelopeysm@gmail.com>")

(maintainers "Penelope Yong <penelopeysm@gmail.com>")

(license LICENSE)

(documentation https://github.com/penelopeysm/cc)

(package
 (name cccc)
 (synopsis "C Compiler Compiling C")
 (description "I tried calling it cc for 'C Compiler', but that caused lots of conflicts with the system's cc.")
 (depends ocaml)
 (tags
  ("c" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
