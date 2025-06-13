(lang dune 3.18)

(name habu)

(using menhir 3.0)

(package
 (name habu)
 (allow_empty)
 (depends
  ocaml
  (ocaml-variants
   (= 5.2.0+ox))
  ppxlib
  stdint
  menhir
  sexplib
  ppx_sexp_conv))
