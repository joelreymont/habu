(lang dune 3.18)

(name habu)

(using menhir 3.0)

(package
 (name habu)
 (allow_empty)
 (depends
  (ocaml
   (= 5.2.0))
  (ocaml-variants
   (= 5.2.0+ox))
  ; (ocaml-base-compiler
  ;  (= 5.2.0))
  ppxlib
  menhir
  stdint
  sexplib
  ppx_sexp_conv
  ppx_expect
  ppx_inline_test))
