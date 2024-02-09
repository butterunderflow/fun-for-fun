(lang dune 3.7)

(name fun4fun)

(using menhir 2.1) 

(package
 (name fun4fun)
 (depends sexplib menhirLib ppx_sexp_conv ppx_inline_test ppx_jane visitors FrontC))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
