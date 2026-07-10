\ maki/move-facts.f - movement layout-transform facts + dissolution verdicts.
\
\ A movement node (CLASS-MOVEMENT) is not a kernel; it is a layout REWRITE the
\ planner either dissolves into index arithmetic or converts into an explicit
\ materialization (CAD-PLAN 6.3). This file owns the two facts a movement node
\ records in its IR attrs cell - the compact TRANSFORM tag + PARAMETERS, and the
\ dissolution VERDICT - plus the pure classifier that derives the verdict from the
\ recorded facts. model-ir.f reads these back per node; plan-ops.f packs them at
\ capture. One concern: movement fact encoding + dissolution classification (no
\ node table, no buffer execution - those are model-ir.f and move.f).
\
\ Attrs-cell encoding (one 64-bit cell, movement nodes only; 0 for compute ops):
\
\   bits [ 3: 0]  transform tag   MV-RESHAPE..MV-GATHER (0..4)  - op-kind is 15..19,
\                                 too wide for 4 bits, so a compact tag is stored
\   bits [ 7: 4]  verdict tag     MVV-FREE..MVV-GATHERED (0..3)
\   bits [27: 8]  param A (20b)   reshape target-rows / slice r0  (0..1048575)
\   bits [47:28]  param B (20b)   reshape target-cols / slice r1
\
\ transpose/concat/gather carry no scalar params (A=B=0); their output extents come
\ from operand descriptors. maki -> habu only; move-facts owns -5068..-5071.

require maki/op-kind.f
require maki/tensor-value.f

-5068 constant E-MV-TF       \ movement transform tag out of range
-5069 constant E-MV-VD       \ dissolution verdict tag out of range
-5070 constant E-MV-PARAM    \ attrs param outside the packable 20-bit field
-5071 constant E-MV-NOTMOVE  \ movement fact requested from a non-movement op

package MAKI
public

\ ---- compact transform tags (packable in 4 bits; op-kind 15..19 is not) -----
0 constant MV-RESHAPE
1 constant MV-TRANSPOSE
2 constant MV-SLICE
3 constant MV-CONCAT
4 constant MV-GATHER
5 constant MV-TF-N

\ ---- dissolution verdicts (CAD-PLAN 6.3) -----------------------------------
0 constant MVV-FREE          \ stride/offset rewrite: reshape on contiguous, slice aligned
1 constant MVV-STAGED        \ transpose: dissolves into lane mapping inside a staged region
2 constant MVV-MATERIALIZE   \ concat (v1) / slice unaligned / reshape non-contiguous
3 constant MVV-GATHERED      \ gather: prologue-only indexed read; downstream flagged
4 constant MVV-N

\ ---- attrs bit layout ------------------------------------------------------
$F     constant MV-NIB       \ 4-bit tag/verdict mask
$FFFFF constant MV-PMASK     \ 20-bit param mask (max 1048575)
4      constant MV-VD-SH     \ verdict shift
8      constant MV-PA-SH     \ param-A shift
28     constant MV-PB-SH     \ param-B shift

: MV-TF-CK ( n -- n )  dup 0 < over MV-TF-N >= or if E-MV-TF throw then ;
: MV-VD-CK ( n -- n )  dup 0 < over MVV-N  >= or if E-MV-VD throw then ;
: MV-PARAM-CK ( n -- n )  dup 0 < over MV-PMASK > or if E-MV-PARAM throw then ;

\ ---- op-kind <-> compact transform tag (movement op-kinds are contiguous) ---
: MV-OF-OP ( n -- n )                          \ movement op-kind -> transform tag
   dup OP-RESHAPE < over OP-GATHER > or if E-MV-NOTMOVE throw then
   OP-RESHAPE - ;
: OP-OF-MV ( n -- n )  MV-TF-CK OP-RESHAPE + ; \ transform tag -> movement op-kind

\ ---- pack / unpack the attrs cell ------------------------------------------
: MV-PACK ( n n n n -- n ) {: tf:n vd:n pa:n pb:n :}
   tf MV-TF-CK drop  vd MV-VD-CK drop  pa MV-PARAM-CK drop  pb MV-PARAM-CK drop
   tf  vd MV-VD-SH lshift or  pa MV-PA-SH lshift or  pb MV-PB-SH lshift or ;

: MV-TF@ ( n -- n )  MV-NIB and ;
: MV-VD@ ( n -- n )  MV-VD-SH rshift MV-NIB and ;
: MV-PA@ ( n -- n )  MV-PA-SH rshift MV-PMASK and ;
: MV-PB@ ( n -- n )  MV-PB-SH rshift MV-PMASK and ;

\ ---- verdict text (fail closed) --------------------------------------------
: MV-VD-NAME ( n -- ptr u8 n )
   case
      MVV-FREE        of s" free"        endof
      MVV-STAGED      of s" staged"      endof
      MVV-MATERIALIZE of s" materialize" endof
      MVV-GATHERED    of s" gathered"    endof
      E-MV-VD throw
   endcase ;

\ ---- dissolution classifiers (pure functions of recorded facts, 6.3) -------
4 constant MV-VEC            \ vectorization lane width (f32x4); slice offset must
                             \ be lane-aligned to stay a pure offset rewrite (4.1 tail)

: MV-RESHAPE-VERDICT ( layout -- n )           \ in-layout -> free (contiguous) | materialize
   LAYOUT-ROW? if MVV-FREE else MVV-MATERIALIZE then ;

: MV-TRANSPOSE-VERDICT ( -- n )  MVV-STAGED ;  \ always dissolves inside a staged region

\ the in-layout family is consumed by the contiguity predicate before the
\ extent locals bind (families cannot bind into locals)
: MV-SLICE-VERDICT ( layout n n -- n ) {: r0:n cols:n :}    \ in-layout r0 cols -> verdict
   LAYOUT-ROW? 0= if MVV-MATERIALIZE exit then \ strided rows -> materialize
   r0 cols * MV-VEC mod 0= if MVV-FREE else MVV-MATERIALIZE then ;

: MV-CONCAT-VERDICT ( -- n )  MVV-MATERIALIZE ; \ v1: lane-range dispatch is a later extension
: MV-GATHER-VERDICT ( -- n )  MVV-GATHERED ;

\ ---- report gating: verdicts that carry a real traffic cost MEMORY flags ----
: MV-VD-REPORTS? ( n -- bool )  {: vd:n :}
   vd MVV-MATERIALIZE = vd MVV-GATHERED = or ;

\ materialization reason per movement op-kind (naming the cause in the report)
: MV-REASON$ ( n -- ptr u8 n )
   case
      OP-RESHAPE     of s" reshape on non-contiguous layout"          endof
      OP-SLICE       of s" slice offset unaligned (masked tail)"      endof
      OP-CONCAT      of s" concat materialized (v1)"                  endof
      OP-GATHER      of s" gather prologue; downstream gathered"      endof
      \ backward scatters (cad-9e): also CLASS-MOVEMENT, also materialize a buffer
      OP-PAD-SCATTER of s" pad-scatter (slice adjoint) materialized"  endof
      OP-SCATTER-ADD of s" scatter-add (gather adjoint) materialized" endof
      E-MV-TF throw
   endcase ;

end-package
