\ maki/array.f - float tensor storage + tensor-scale ops (runs on the Habu host).
\
\ Maki's element rules (optim/loss/autograd) apply per-weight; this lifts them to
\ whole TENSORS so training runs at tensor scale NOW on the CPU host (no GPU
\ codegen needed) - the same ops the Habu-PTX kernels run on device. A v0 tensor
\ is a contiguous float-cell buffer (base ptr) + length; Habu floats are cells, so
\ @/! store them. maki -> habu only.
\
\ Wrapped in `package MAKI`: the tensor-scale ops export as MAKI:T-GET / MAKI:T-SET /
\ MAKI:T-FILL / MAKI:T-SUM / MAKI:T-SGD! / MAKI:T-ADD!, so a bare reference does not
\ resolve from global/habu (docs/forth.md "Packages"). The address helper T-AT stays
\ private. maki-internal callers reopen `package MAKI` and use the bare names.

package MAKI

: T-AT  ( ptr a n -- ptr a )  cells + ;        \ address of element n (private helper)

public

: T-GET ( ptr a n -- r )      T-AT @ ;
: T-SET ( r ptr a n -- )      T-AT ! ;

: T-FILL ( r ptr a n -- ) {: val base len :}
   len 0 ?do  val base i T-SET  loop ;

: T-SUM ( ptr a n -- r ) {: base len :}
   0.0  len 0 ?do  base i T-GET f+  loop ;

\ in-place tensor SGD step: w[i] -= lr * g[i]  over the whole weight tensor
: T-SGD! ( r ptr a ptr a n -- ) {: lr wbase gbase len :}
   len 0 ?do
      wbase i T-GET   lr  gbase i T-GET  f*  f-   wbase i T-SET
   loop ;

\ in-place elementwise add: a[i] += b[i]
: T-ADD! ( ptr a ptr a n -- ) {: abase bbase len :}
   len 0 ?do
      abase i T-GET  bbase i T-GET  f+  abase i T-SET
   loop ;

end-package
