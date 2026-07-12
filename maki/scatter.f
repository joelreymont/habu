\ maki/scatter.f - buffer-level scatter-backward reference execution (cad-9e).
\
\ The golden oracles for the two scatter backward op-kinds the movement adjoints
\ need (maki/adjoint.f, maki/backward.f). A slice copies a row range out, so its
\ input gradient PADS the cotangent back into a zero buffer at the slice offset
\ (PAD-SCATTER); a gather selects rows by index, so its input gradient SCATTERS the
\ cotangent rows back to those indices, ACCUMULATING where an index repeats
\ (SCATTER-ADD). Both start from an all-zero destination (unwritten rows have no
\ gradient). Buffer-granularity in the maki/move.f style: contiguous float-cell
\ buffers (base ptr) addressed row-major (row cols * + col), reusing T-GET/T-SET/
\ T-FILL. Fail closed on an out-of-range slice offset or gather index. Distinct
\ concern from the exact forward movement references (maki/move.f): these zero-fill
\ then place / accumulate. maki -> habu only; scatter owns -5125..-5126.

require maki/array.f

-5125 constant E-SC-RANGE   \ pad-scatter row offset outside [0, dst-rows]
-5126 constant E-SC-INDEX   \ scatter-add gather index outside [0, dst-rows)

package MAKI
private

\ copy one cols-wide row: ct row cr -> dst row dr (both cols wide, row-major)
: SC-COPY-ROW ( ptr a n ptr a n n -- ) {: c:ptr cr:n d:ptr dr:n cols:n :}
   cols 0 ?do  c cr cols * i +  T-GET   d dr cols * i +  T-SET  loop ;

\ add one cols-wide row in place: dst row dr += ct row cr (both cols wide)
: SC-ADD-ROW ( ptr a n ptr a n n -- ) {: c:ptr cr:n d:ptr dr:n cols:n :}
   cols 0 ?do
      d dr cols * i +  T-GET   c cr cols * i +  T-GET  f+   d dr cols * i +  T-SET
   loop ;

\ fetch + validate one gather index; fail closed outside [0, dr)
: SC-IDX@ ( ptr a n n -- n ) {: ix:ptr k:n dr:n :}
   ix k cells + @  dup 0 < over dr >= or if E-SC-INDEX throw then ;

public

\ PAD-SCATTER: zero a dr x sc destination, then copy the cr x sc cotangent into rows
\ [r0, r0+cr). The slice adjoint: cr = r1-r0, r0 = the forward slice offset.
: PAD-SCATTER ( ptr a n n n n ptr a -- ) {: ct:ptr cr:n sc:n r0:n dr:n d:ptr :}
   r0 0 < r0 cr + dr > or if E-SC-RANGE throw then
   0.0 d dr sc * T-FILL
   cr 0 ?do  ct i  d  r0 i +  sc  SC-COPY-ROW  loop ;

\ SCATTER-ADD: zero a dr x sc destination, then add each of the k cotangent rows
\ into destination row idx[k]; duplicate indices accumulate. The gather adjoint.
: SCATTER-ADD ( ptr a n n ptr a n ptr a -- ) {: ct:ptr k:n sc:n ix:ptr dr:n d:ptr :}
   0.0 d dr sc * T-FILL
   k 0 ?do  ct i  d  ix i dr SC-IDX@  sc  SC-ADD-ROW  loop ;

;package
