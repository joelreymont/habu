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
require lib/adt/option.f             \ option<tr-slot>: TR-FIND returns a present/absent slot
require lib/type/value-nominal.f     \ NOMINAL:: the tensor-registry slot index is its own type

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
\ TG-SLOT-A holds one extent-registry slot per rank, so it stores `xr-slot` values;
\ the input to TG-SLOT@ is a rank position (0..r-1).
: TG-SLOT@ ( n -- xr-slot )  cells TG-SLOT-A + @ >XR-SLOT ;
: TG-SLOT+ ( xr-slot -- ) {: s:xr-slot :}
   TG-NR @ TG-RANK-CAP >= if E-EXT-CAP throw then
   s XR-SLOT>N TG-NR @ cells TG-SLOT-A + !  TG-NR @ 1 + TG-NR ! ;

\ ---- tensor registry: accessor NAME -> (rank, kind). SPEC: (maki/spec.f) reads it
\ to give named throws for an undeclared tensor or a factor whose index count does
\ not match the tensor's rank, instead of an undefined-word crash inside the
\ generated evaluate. Rank of a gather is 1 (the single domain index a gather call
\ IX[m] consumes). ------------------------------------------------------------
\ tensor kind: a real sum type, not a bare integer flag. The kind is its own
\ checker type, so a rank or a raw 0/1 cannot pose as a kind, and every branch on
\ the kind is an exhaustive MATCH (a forgotten arm fails certification).
\ `data` = a data tensor (NAME@ / NAME!); `gather` = a gather index tensor (NAME@).
public
SUMTYPE tensor-kind 0
   VARIANT data ;VARIANT
   VARIANT gather ;VARIANT
;SUMTYPE

\ tr-slot is the tensor-registry row index, its own type: a rank or an extent slot
\ cannot address a tensor row without the explicit `>TR-SLOT` crossing.
NOMINAL: TR-SLOT

\ kind predicates SPEC: (maki/spec.f) uses to demand a data tensor or a gather.
: TR-KIND-DATA?   ( tensor-kind -- bool )  MATCH tensor-kind  data OF true  ENDOF  gather OF false ENDOF ;MATCH ;
: TR-KIND-GATHER? ( tensor-kind -- bool )  MATCH tensor-kind  data OF false ENDOF  gather OF true  ENDOF ;MATCH ;

private

\ short constructors + the raw-cell tag projection/rebuild. The kind is stored as
\ its variant tag in a flat cell (TR-KIND-A) and rebuilt into the sum on read, so
\ the registry array stays a plain cell array while every reader sees `tensor-kind`.
: KIND-DATA   ( -- tensor-kind )  MAKI-TENSOR--KIND:DATA ;
: KIND-GATHER ( -- tensor-kind )  MAKI-TENSOR--KIND:GATHER ;
: KIND>TAG ( tensor-kind -- n )  MATCH tensor-kind  data OF 0 ENDOF  gather OF 1 ENDOF ;MATCH ;
: TAG>KIND ( n -- tensor-kind )  0 = if KIND-DATA else KIND-GATHER then ;

32 constant TR-NAME-CAP
64 constant TR-CAP
create TR-NAMES  TR-CAP TR-NAME-CAP * allot
create TR-NLEN   TR-CAP cells allot
create TR-RANK-A TR-CAP cells allot
create TR-KIND-A TR-CAP cells allot
variable TR-N
: TR-NAME-PTR ( tr-slot -- ptr a )  TR-SLOT>N TR-NAME-CAP *  TR-NAMES + ;
: TR-ADD ( ptr u8 n n tensor-kind -- )
   KIND>TAG {: a:ptr u:n rank:n tag:n :}
   TR-N @ TR-CAP >= if E-EXT-CAP throw then
   u TR-NAME-CAP > if E-EXT-CAP throw then
   TR-N @ >TR-SLOT {: i:tr-slot :}
   a i TR-NAME-PTR u BYTE-COPY  u i TR-SLOT>N cells TR-NLEN + !
   rank i TR-SLOT>N cells TR-RANK-A + !  tag i TR-SLOT>N cells TR-KIND-A + !
   TR-N @ 1 + TR-N ! ;

public

: TR-NAME@ ( tr-slot -- ptr u8 n ) {: s:tr-slot :}  s TR-NAME-PTR  s TR-SLOT>N cells TR-NLEN + @ ;
\ NAME -> registry slot; absent = option<tr-slot> none, so a caller must handle it.
: TR-FIND ( ptr u8 n -- option<tr-slot> ) {: a:ptr u:n :}
   TR-N @ 0 ?do
      a u  i >TR-SLOT TR-NAME@  STR= if  i >TR-SLOT OPTION:SOME  unloop exit  then
   loop  OPTION:NONE ;
: TR-RANK@ ( tr-slot -- n )            TR-SLOT>N cells TR-RANK-A + @ ;
: TR-KIND@ ( tr-slot -- tensor-kind )  TR-SLOT>N cells TR-KIND-A + @ TAG>KIND ;

private

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
   TG-NAME$ TG-NR @ KIND-DATA TR-ADD              \ record NAME -> (rank, data tensor) for SPEC:
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
   TG-NAME$ 1 KIND-GATHER TR-ADD                    \ record NAME -> (rank 1, gather) for SPEC:
   XG-RESET
   TG-EMIT-BASE
   s" : " XG+  TG-NAME$ XG+  s" @ ( ix<" XG+  0 TG-SLOT@ XR-TAIL@ XG+  s" > -- ix<" XG+
      1 TG-SLOT@ XR-TAIL@ XG+  s" > ) " XG+
      s" IX>N {: p:n :} " XG+  TG-NAME$ XG+  s" -BASE @ p T-AT @ >" XG+
      1 TG-SLOT@ XR-SURF@ XG+  s"  ; " XG+
   XG-EVAL ;

;package
