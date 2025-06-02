(lang dune 3.18)

(name bop)

(using menhir 3.0)

(package
 (name bop)
 (allow_empty)
 (depends
  (ocaml
   (>= 5.3.0))
  dune
  stdint
  menhir
  sexplib
  ppx_sexp_conv))
