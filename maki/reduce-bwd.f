\ maki/reduce-bwd.f - buffer-level reduce-backward reference execution (cad-9e).
\
\ The golden oracles for the two reduce backward op-kinds the parameter adjoints
\ need (maki/adjoint.f, maki/backward.f). A bias is broadcast over the R rows of its
\ input, so its gradient is the COLUMN-WISE sum of the cotangent (RxC -> 1xC); a
\ scale is a 1x1 factor, so its gradient is the FULL-REDUCE dot of the cotangent
\ with the saved input (sum over all elements of ct (.) x -> 1x1). Both accumulate
\ in f32 (ACC-F32 in the op registry) and are buffer-granularity in the maki/move.f
\ style: contiguous float-cell buffers (base ptr) addressed row-major (row cols *
\ + col), reusing T-GET/T-SET. Fail closed on a rowsum column mismatch. These are
\ NOT movement rewrites (they compute a reduction) - hence their own file, distinct
\ from the exact movement references (maki/move.f) and scatters (maki/scatter.f).
\ maki -> habu only; reduce-bwd owns -5120.

require maki/array.f

-5120 constant E-RB-COLS     \ rowsum destination column count disagrees with the source

package MAKI
public

\ ROWSUM-BWD: d[0,c] = sum over rows i of s[i,c]; source sr x sc, dst 1 x dc (dc=sc).
\ The bias gradient: reduce the cotangent over its broadcast (row) axis.
: ROWSUM-BWD ( ptr a n n ptr a n -- ) {: s:ptr sr:n sc:n d:ptr dc:n :}
   dc sc <> if E-RB-COLS throw then
   sc 0 ?do                                     \ outer = column (i here, j inside the row loop)
      0.0                                        \ column accumulator (f32)
      sr 0 ?do  s  i sc * j +  T-GET  f+  loop   \ inner = row i, column j: += s[i,j]
      d i T-SET                                  \ back in the column loop: dst[0,i]
   loop ;

\ FULLSUM-DOT-BWD: d[0,0] = sum over all n elements of ct[k] * x[k]; dst is 1x1.
\ The scale gradient: dot the cotangent with the saved input, reduced to a scalar.
: FULLSUM-DOT-BWD ( ptr a ptr a n ptr a -- ) {: ct:ptr x:ptr n:n d:ptr :}
   0.0  n 0 ?do  ct i T-GET  x i T-GET  f*  f+  loop
   d 0 T-SET ;

;package
