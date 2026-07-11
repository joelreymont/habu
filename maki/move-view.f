\ maki/move-view.f - dissolved-movement VIEW resolution for the compute lowerings.
\
\ CAD-PLAN 6.3 device leg, slice 4 (deliverable A). A DISSOLVED movement node
\ (CLASS-MOVEMENT, MIR-MAT@ = 0, verdict MVV-FREE / MVV-STAGED) is not a kernel: the
\ fusion planner folds it into the LOAD INDEX MATH of the compute op that reads it
\ (maki/fusion-plan.f FP-JOIN?). This file owns the pure resolution a compute lowering
\ (maki/lower-ew.f / lower-red.f / lower-mm.f) needs to fold one: given the operand ref a
\ compute node reads, decide whether it reads THROUGH a dissolved movement, and if so
\ resolve the ultimate SOURCE input plus the constant element OFFSET the fold bakes into
\ the reading kernel's base pointer.
\
\ MVV-FREE (reshape = identity flat, slice = a lane-aligned row offset) folds into all three
\ compute kernels (EW flat index, RED row span, MM K-loop) as one `base += r0*cols` on the
\ reading operand pointer (reshape r0=0), when the movement's source is a model INPUT SLOT.
\ MVV-STAGED (transpose) is a lane PERMUTATION, not a base offset, so it folds only where the
\ per-element load index can absorb it: the FLAT EW kernel, via MVW-CHECK-EW + MVW-XPOSE-DIMS
\ (src_flat = (e mod dstC)*srcC + e/dstC). RED (coalesced row loads) and MM (K-loop A/B
\ addressing) cannot express a transposed column load, so the planner MATERIALIZES a staged
\ transpose feeding them (maki/fusion-plan.f FP-STAGED-FOLDABLE?) and their lowerings keep
\ E-MVW-STAGED via MVW-CHECK as a defense-in-depth guard. Every harder case fails closed:
\   MVV-STAGED into RED / MM     -> E-MVW-STAGED   (a lane permutation the row/K-loop cannot hold)
\   MVV-MATERIALIZE/GATHERED    -> E-MVW-NOTFREE  (mat=1: a region boundary, not a fold)
\   source not an input slot    -> E-MVW-SRC      (chained / cross-region movement, v1)
\   unexpected movement op-kind -> E-MVW-OP
\ maki -> habu only; move-view owns -5201..-5204.

require lib/prelude.f
require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/model-ir.f

-5201 constant E-MVW-STAGED    \ a staged (transpose) movement cannot fold into a compute region (v1)
-5202 constant E-MVW-SRC       \ a dissolved movement's source is not a model input slot (v1)
-5203 constant E-MVW-NOTFREE   \ movement fold requested for a non-free (mat) verdict
-5204 constant E-MVW-OP        \ unexpected movement op-kind during view resolution

package MAKI
public

\ Is this operand ref an interior DISSOLVED movement node the region reads through?
\ (a movement op-kind, committed node, MIR-MAT@ = 0 i.e. not a materialized boundary)
: MVW-DISSOLVED? ( n -- bool ) {: ref:n :}
   ref MIR-REF-INPUT? if false exit then
   ref MIR-MOVE? 0= if false exit then
   ref MIR-MAT@ 0= ;

\ a dissolved movement's ultimate source (operand 0) - v1 requires it to be a model slot
: MVW-SRC-REF ( n -- n ) {: mv:n :}
   mv 0 MIR-IN@ {: src:n :}
   src MIR-REF-INPUT? 0= if E-MVW-SRC throw then
   src ;

\ ---- foldable element offset (reshape identity 0 ; slice r0*cols) ------------
\ fail closed on staged / mat verdicts and on any non {reshape,slice} movement op.
: MVW-OFF-ELEMS ( n -- n ) {: mv:n :}
   mv MIR-MOVE-VERDICT@ {: vd:n :}
   vd MVV-FREE <> if
      vd MVV-STAGED = if E-MVW-STAGED throw then
      E-MVW-NOTFREE throw
   then
   mv MIR-OP@                                        \ ( op )  family stays on the stack
   dup MAKI-OPKIND:RESHAPE MAKI-OPKIND:EQ if drop 0 exit then  \ contiguous reshape: identity flat
   MAKI-OPKIND:SLICE MAKI-OPKIND:EQ if mv MIR-ATTR@ MV-PA@  mv MIR-COLS@ *  exit then   \ r0 * cols (source stride)
   E-MVW-OP throw ;

\ source-buffer element count to upload (reshape = output elems ; slice = full source rows ;
\ transpose = full source, permuted per element by the fold)
: MVW-SRC-ELEMS ( n -- n ) {: mv:n :}
   mv MVW-SRC-REF MIR-REF-SLOT {: s:n :}
   s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ * ;

\ ---- staged transpose fold (dst[i,j]=src[j,i]) -------------------------------
\ Unlike a FREE reshape/slice (a constant base offset), a STAGED transpose is a lane
\ PERMUTATION: it folds into the reading kernel's per-element index math, not its base
\ pointer. MVW-STAGED? gates that path; MVW-XPOSE-DIMS exposes the remap dims so the
\ consumer emits src_flat = (e mod dstC)*srcC + e/dstC over its flat output index e
\ (dstC = output cols = source rows ; srcC = output rows = source cols).
: MVW-STAGED? ( n -- bool ) {: ref:n :}
   ref MVW-DISSOLVED? 0= if false exit then
   ref MIR-MOVE-VERDICT@ MVV-STAGED = ;

: MVW-XPOSE-DIMS ( n -- n n ) {: mv:n :}
   mv MIR-OP@ MAKI-OPKIND:TRANSPOSE MAKI-OPKIND:EQ 0= if E-MVW-OP throw then   \ the staged verdict is transpose-only (v1)
   mv MVW-SRC-REF drop                                 \ v1: source must be a model slot (E-MVW-SRC)
   mv MIR-COLS@  mv MIR-ROWS@ ;                         \ dstC (out cols = src rows) ; srcC (out rows = src cols)

\ ---- resolution for a region operand (movement node or plain external ref) ---
\ the source ref a folded operand actually uploads (its slot), else the ref itself
: MVW-RESOLVE-SRC ( n -- n ) {: ref:n :}
   ref MVW-DISSOLVED? if ref MVW-SRC-REF else ref then ;

\ the byte offset the fold bakes into that operand's base pointer (0 for a plain ref)
: MVW-RESOLVE-OFF ( n -- n ) {: ref:n :}
   ref MVW-DISSOLVED? if ref MVW-OFF-ELEMS 4 * else 0 then ;

\ prove a region's dissolved movement members are all v1-foldable (free, slot source);
\ a staged/chained/mat one throws here BEFORE any PTX is emitted. This is the RED/MM
\ contract (they fold FREE offsets only); EW additionally folds a staged transpose and
\ uses MVW-CHECK-EW below.
: MVW-CHECK ( n -- ) {: ref:n :}
   ref MVW-DISSOLVED? 0= if exit then
   ref MVW-OFF-ELEMS drop  ref MVW-SRC-REF drop ;

\ EW foldability: FREE via a base offset (MVW-OFF-ELEMS) OR a STAGED transpose via the
\ per-element remap (MVW-XPOSE-DIMS). A chained (non-slot source) or unexpected-op one
\ still throws here BEFORE any PTX is emitted; the flat EW kernel is the only v1 consumer
\ whose load index can absorb a full lane permutation.
: MVW-CHECK-EW ( n -- ) {: ref:n :}
   ref MVW-DISSOLVED? 0= if exit then
   ref MVW-STAGED? if ref MVW-XPOSE-DIMS 2drop exit then
   ref MVW-OFF-ELEMS drop  ref MVW-SRC-REF drop ;

end-package
