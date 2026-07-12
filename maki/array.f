\ maki/array.f - float tensor storage + tensor-scale ops (runs on the Habu host).
\
\ Maki's element rules (optim/loss/autograd) apply per-weight; this lifts them to
\ whole TENSORS so training runs at tensor scale NOW on the CPU host (no GPU
\ codegen needed) - the same ops the Habu-PTX kernels run on device. A v0 tensor
\ is a contiguous float-cell buffer (base ptr) + length; Habu floats are cells, so
\ @/! store them. maki -> habu only.

: T-AT  ( ptr a n -- ptr a )  cells + ;        \ address of element n
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

\ squared L2 distance: sum_i (a[i]-b[i])^2
: T-DIST2 ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :}
   0.0
   len 0 ?do
      abase i T-GET  bbase i T-GET  f-  dup f*  f+
   loop ;

\ squared L2 norm: sum_i b[i]^2
: T-NORM2 ( ptr a n -- r ) {: bbase:ptr len:n :}
   0.0
   len 0 ?do
      bbase i T-GET  dup f*  f+
   loop ;

\ ||a-b|| / ||b|| : relative L2 distance to the reference tensor b
: T-REL-L2 ( ptr a ptr a n -- r ) {: abase:ptr bbase:ptr len:n :}
   abase bbase len T-DIST2 fsqrt
   bbase len T-NORM2 fsqrt
   f/ ;

\ |a-b| / |b| : scalar relative error against the reference b
: T-REL1 ( r r -- r ) {: a:r b:r :}
   a b f- fabs  b fabs  f/ ;
