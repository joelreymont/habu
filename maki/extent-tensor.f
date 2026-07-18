\ maki/extent-tensor.f - extent-typed tensor accessor definers (TENSOR:/ITENSOR:).
\
\ docs/golden-syntax.md candidate B. TENSOR: bakes a tensor's row-major stride math
\ ONCE, in the defining word, and emits extent-typed fetch/store accessors so a
\ transposed index pair is an author-time checker reject:
\
\   128 EXTENT: #M   64 EXTENT: #K
\   TENSOR: AT ( #M #K )      \ AT@ ( ix<extm> ix<extk> -- r )   AT! ( r ix<extm> ix<extk> -- )
\
\ `NAME-BIND ( ptr a -- )` binds the tensor's base buffer (the accessor closes over
\ it); `NAME@` reads, `NAME!` writes. Rank is arbitrary (the extent list length), so
\ a batched 3D tensor (docs/batch-sequence-design.md #B/#T/#H) rides the same
\ generator - the offset is the row-major Horner fold over the declared extents.
\
\ ITENSOR: is the gather witness (docs/golden-syntax.md:61,69-71): an index tensor
\ whose ELEMENT is itself a typed row index.
\
\   ITENSOR: IXT ( #M #M )    \ IXT@ ( ix<extm> -- ix<extm> ) : gather a row index
\
\ `IXT@` reads the raw index stored at position m and re-types it into the codomain
\ extent's index space, so a gathered index feeds a data accessor's row slot
\ type-checked (`m>#M IXT@` -> ix<extm>, exactly what AT@'s row arg demands).
\
\ Generated bodies reuse maki/array.f T-AT for the ptr+offset step and the package
\ MAKI codegen boundary (maki/extent.f XG-EVAL); nothing here is unchecked beyond
\ that one audited evaluate.
\
\ ACCESSOR SAFETY - what is and is not guarded:
\   - BIND BEFORE USE: `NAME@`/`NAME!`/gather `NAME@` dereference the hidden base
\     `NAME-BASE`, which is null until `NAME-BIND ( ptr a -- )` sets it. Calling an
\     accessor before binding dereferences null (a runtime fault); the caller owns
\     binding each tensor's buffer before first use (the tests bind in FILL/RUN).
\   - INDEX SIDE (guarded): each index argument entered the accessor through an
\     extent injector (`>#name`), which range-checks n against its extent value and
\     throws E-EXT-RANGE out of [0, extent). The gather retypes its stored cell
\     through the codomain injector too, so a corrupt index buffer value is caught
\     at that crossing, not walked into the address arithmetic. There is no
\     SEPARATE bound check at the address step - the injector range check is the
\     single choke point (maki/extent.f).
\ maki -> habu only.

require maki/extent.f

package MAKI

private

\ ---- definer scratch: the accessor NAME (stable across the extent parse) and the
\ collected extent registry slots (one per rank). ----------------------------
create TG-NAME-BUF 32 allot
variable TG-NAME-U
: TG-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 32 > if E-EXT-CAP throw then
   a TG-NAME-BUF u BYTE-COPY  u TG-NAME-U ! ;
: TG-NAME$ ( -- ptr u8 n )  TG-NAME-BUF TG-NAME-U @ ;

8 constant TG-RANK-CAP
create TG-SLOT-A TG-RANK-CAP cells allot
variable TG-NR
: TG-SLOT@ ( n -- n )  cells TG-SLOT-A + @ ;
: TG-SLOT+ ( n -- ) {: s:n :}
   TG-NR @ TG-RANK-CAP >= if E-EXT-CAP throw then
   s TG-NR @ cells TG-SLOT-A + !  TG-NR @ 1 + TG-NR ! ;

\ read `( #E0 #E1 ... )` off the input stream, recording each extent's registry
\ slot; an undeclared extent fails closed (XR-REQUIRE -> E-EXT-UNDECL).
: TG-PARSE-EXTS ( -- )
   parse-name 2dup s" (" STR= 0= if E-EXT-NAME throw then 2drop
   0 TG-NR !
   begin
      parse-name dup 0= if 2drop E-EXT-NAME throw then
      2dup s" )" STR= if 2drop exit then
      XR-REQUIRE TG-SLOT+
   again ;

\ ---- generated-text fragments (append into the XG codegen buffer) ------------
\ signature args: `ix<tail> ` per extent, in declared order.
: TG-SIG-ARGS ( -- )
   TG-NR @ 0 ?do
      s" ix<" XG+  i TG-SLOT@ XR-TAIL@ XG+  s" > " XG+
   loop ;
\ projection prologue: bind each index to a plain-cell local x0..x{r-1}. The top
\ of stack is the LAST index, so project high->low (x{r-1} first).
: TG-PROJ ( -- )
   TG-NR @ 0 ?do
      TG-NR @ 1 - i -  {: k:n :}
      s" IX>N {: x" XG+  k XG-INT  s" :n :} " XG+
   loop ;
\ row-major offset as a Horner fold: x0 (*val1 +x1) (*val2 +x2) ... - valN is the
\ N-th extent's runtime size, so the outer extent's size never enters the stride.
: TG-OFFSET ( -- )
   s" x0 " XG+
   TG-NR @ 1 ?do
      i TG-SLOT@ XR-VAL@ XG-INT  s"  * x" XG+  i XG-INT  s"  + " XG+
   loop ;
\ shared `variable NAME-BASE` + `NAME-BIND ( ptr a -- )` framing.
: TG-EMIT-BASE ( -- )
   s" variable " XG+  TG-NAME$ XG+  s" -BASE " XG+
   s" : " XG+  TG-NAME$ XG+  s" -BIND ( ptr a -- ) " XG+  TG-NAME$ XG+  s" -BASE ! ; " XG+ ;
\ `NAME-BASE @ <offset> T-AT` - the shared address computation both accessors end on.
: TG-EMIT-ADDR ( -- )
   TG-NAME$ XG+  s" -BASE @ " XG+  TG-OFFSET  s" T-AT " XG+ ;

public

\ TENSOR: NAME ( #E0 #E1 ... ) - define extent-typed float-tensor accessors. Emits
\ `NAME-BASE` (hidden), `NAME-BIND ( ptr a -- )`, `NAME@ ( ix<e0> .. -- r )`, and
\ `NAME! ( r ix<e0> .. -- )`. Top-level-interpret-only (parses the stream + mutates
\ the dictionary through the checked XG-EVAL boundary), like EXTENT:/SUMTYPE.
: TENSOR: ( -- )
   parse-name TG-NAME!
   TG-PARSE-EXTS
   XG-RESET
   TG-EMIT-BASE
   s" : " XG+  TG-NAME$ XG+  s" @ ( " XG+  TG-SIG-ARGS  s" -- r ) " XG+
      TG-PROJ  TG-EMIT-ADDR  s" @ ; " XG+
   s" : " XG+  TG-NAME$ XG+  s" ! ( r " XG+  TG-SIG-ARGS  s" -- ) " XG+
      TG-PROJ  TG-EMIT-ADDR  s" ! ; " XG+
   XG-EVAL ;

\ ITENSOR: NAME ( #DOM #COD ) - define a gather index tensor. `NAME@
\ ( ix<dom> -- ix<cod> )` reads the row index stored at the domain position and
\ retypes it into the codomain extent's index space (the gather witness). The
\ index buffer holds #DOM entries; `NAME-BIND ( ptr a -- )` binds it.
: ITENSOR: ( -- )
   parse-name TG-NAME!
   TG-PARSE-EXTS
   TG-NR @ 2 <> if E-EXT-NAME throw then           \ exactly domain + codomain
   XG-RESET
   TG-EMIT-BASE
   s" : " XG+  TG-NAME$ XG+  s" @ ( ix<" XG+  0 TG-SLOT@ XR-TAIL@ XG+  s" > -- ix<" XG+
      1 TG-SLOT@ XR-TAIL@ XG+  s" > ) " XG+
      s" IX>N {: p:n :} " XG+  TG-NAME$ XG+  s" -BASE @ p T-AT @ >" XG+
      1 TG-SLOT@ XR-SURF@ XG+  s"  ; " XG+
   XG-EVAL ;

;package
