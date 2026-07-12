\ maki/checkpoint.f - segment-boundary gradient checkpointing over the host
\ executor (dot habu-maki-training-loop). The policy SPEC is docs/maki/train.md
\ "Gradient checkpointing" - this file implements that section exactly; cite it
\ for every policy question.
\
\ Segments are the fusion planner's regions over the CAPTURED FORWARD IR
\ (maki/fusion-plan.f FP-BUILD), snapshotted before BW-BUILD appends the
\ backward region. A node the planner materializes is a BOUNDARY activation and
\ is SAVED: it keeps a persistent arena buffer across the whole step. Every
\ other forward node is a segment INTERIOR and is RECOMPUTED: all segments
\ overlay ONE shared scratch window, so at most one segment's interior is live
\ at a time - live forward buffers <= boundaries + one segment's interior,
\ vs every forward buffer under EX-RUN.
\
\ CK-FWD runs the forward slice in index order (the scratch overlay is safe
\ because v1 requires each segment to be one contiguous node interval -
\ E-CK-INTERVAL otherwise). CK-BWD walks the backward region in index order;
\ before each backward node it finds the ONE segment whose interiors the node
\ reads (BW-BUILD guarantees an adjoint reads only its forward node's operands
\ and output = same-segment interiors or boundaries; two segments in one node
\ is fail-closed E-CK-CROSS) and re-runs that segment's interiors into the
\ scratch window on demand. Host execution is deterministic, so the recompute
\ is bit-identical to the discarded originals (maki/checkpoint-test.f asserts
\ exact f= losses, gradients, and rematerialized interiors). Remat is
\ idempotent and self-contained (a segment's interiors depend only on saved
\ boundaries, model inputs, and earlier same-segment interiors), so any need
\ order is correct; reverse emission order re-runs each segment at most once.
\
\ Preconditions mirror the trainers: CK-SETUP needs a forward-only IR (fresh
\ MODEL: capture or hand-built table). It plans the fusion regions, snapshots
\ them, runs BW-BUILD, and places every node buffer (EX-OFF!). The caller then
\ EX-RESETs, EX-BINDs every slot (seed included), and drives CK-FWD -> seed
\ write -> CK-BWD. The offsets live in the executor's plan table, so any
\ EX-RUN(-N) invalidates them until the next CK-SETUP (which needs a fresh
\ capture, exactly like BW-BUILD). maki -> habu only; checkpoint owns
\ -5135..-5137.

require maki/model-ir.f
require maki/fusion-plan.f
require maki/backward.f
require maki/executor.f

-5135 constant E-CK-STATE     \ run / accessor used before a successful CK-SETUP
-5136 constant E-CK-INTERVAL  \ a forward segment is not one contiguous node interval (v1)
-5137 constant E-CK-CROSS     \ one backward node reads interiors of two segments

package MAKI
private

\ per-forward-node plan snapshot + per-segment spans (mirrors MIR-CAP, so the
\ model-ir capacity guard already bounds every index that can reach these)
128 constant CK-NCAP

create CK-SEG CK-NCAP cells allot   \ per-forward-node segment id
create CK-BND CK-NCAP cells allot   \ per-forward-node boundary flag (1 = saved)
create CK-LO  CK-NCAP cells allot   \ per-segment first node (-1 = unopened)
create CK-HI  CK-NCAP cells allot   \ per-segment last node
variable CK-SEGS                    \ segment count
variable CK-NF                      \ forward node count snapshot
variable CK-CUR                     \ segment whose interiors are live in scratch (-1 none)
variable CK-REMATS                  \ segments re-run by the last CK-BWD
variable CK-BUILT?

: CK-CK ( -- )  CK-BUILT? @ 0= if E-CK-STATE throw then ;

: CK-SEG@ ( n -- n )     cells CK-SEG + @ ;
: CK-BND? ( n -- bool )  cells CK-BND + @ 0<> ;

\ ---- forward-plan snapshot (segment ids + boundary flags, before BW-BUILD) --
: CK-SNAP ( -- )
   MIR-N@ CK-NF !
   FP-RESET FP-BUILD
   CK-NF @ 0 ?do
      i FP-RID@                  i cells CK-SEG + !
      i MIR-MAT@ if 1 else 0 then  i cells CK-BND + !
   loop
   FP-REGION-COUNT CK-SEGS ! ;

\ ---- v1 domain: each segment is one contiguous interval of the node order ---
: CK-SPAN+ ( n -- ) {: nd:n :}
   nd CK-SEG@ {: s:n :}
   s cells CK-LO + @ 0< if
      nd s cells CK-LO + !  nd s cells CK-HI + !  exit then
   s cells CK-HI + @ 1+ nd <> if E-CK-INTERVAL throw then
   nd s cells CK-HI + ! ;

: CK-SPANS ( -- )
   CK-SEGS @ 0 ?do  -1 i cells CK-LO + !  loop
   CK-NF @ 0 ?do  i CK-SPAN+  loop ;

\ ---- buffer plan: persistent boundaries, overlaid interior scratch, backward -
variable CK-BUMP     \ persistent-band high-water (cells)
variable CK-SCR0     \ scratch window base (cells)
variable CK-SCRW     \ scratch window width (cells) = max segment interior cells
variable CK-B-N      \ boundary (saved) buffer count
variable CK-IMAX     \ max interior buffers in one segment
variable CK-W        \ current segment's interior cells (plan walk)
variable CK-I        \ current segment's interior buffers (plan walk)

: CK-PLACE ( n -- ) {: nd:n :}          \ persistent slot at the bump pointer
   CK-BUMP @ nd EX-OFF!
   CK-BUMP @ nd EX-NODE-ELEMS + CK-BUMP ! ;

: CK-PLAN-BND ( -- )
   0 CK-BUMP !  0 CK-B-N !
   CK-NF @ 0 ?do
      i CK-BND? if  i CK-PLACE  CK-B-N @ 1+ CK-B-N !  then
   loop
   CK-BUMP @ CK-SCR0 ! ;

: CK-INT-PLACE ( n -- ) {: nd:n :}      \ interior slot inside the scratch window
   CK-SCR0 @ CK-W @ + nd EX-OFF!
   CK-W @ nd EX-NODE-ELEMS + CK-W !
   CK-I @ 1+ CK-I ! ;

: CK-PLAN-SEG ( n -- ) {: s:n :}        \ pack one segment's interiors from the base
   0 CK-W !  0 CK-I !
   s cells CK-HI + @ 1+  s cells CK-LO + @ ?do
      i CK-BND? 0= if i CK-INT-PLACE then
   loop
   CK-W @ CK-SCRW @ > if CK-W @ CK-SCRW ! then
   CK-I @ CK-IMAX @ > if CK-I @ CK-IMAX ! then ;

: CK-PLAN-SCRATCH ( -- )
   0 CK-SCRW !  0 CK-IMAX !
   CK-SEGS @ 0 ?do  i CK-PLAN-SEG  loop ;

: CK-PLAN-BWD ( -- )                    \ backward nodes persist past the scratch window
   CK-SCR0 @ CK-SCRW @ + CK-BUMP !
   MIR-N@ CK-NF @ ?do  i CK-PLACE  loop ;

public

\ ---- setup: plan segments, build backward, place every node buffer ----------
: CK-SETUP ( -- )
   0 CK-BUILT? !
   CK-SNAP
   CK-SPANS
   BW-BUILD
   CK-PLAN-BND  CK-PLAN-SCRATCH  CK-PLAN-BWD
   -1 CK-CUR !  0 CK-REMATS !
   -1 CK-BUILT? ! ;

private

: CK-INT-RUN ( n -- ) {: nd:n :}        \ run one interior; it now owns the scratch
   nd EX-STEP  nd CK-SEG@ CK-CUR ! ;

\ re-run one segment's interiors into the scratch window (deterministic ->
\ bit-identical to the discarded forward values)
: CK-REMAT ( n -- ) {: s:n :}
   s cells CK-HI + @ 1+  s cells CK-LO + @ ?do
      i CK-BND? 0= if i EX-STEP then
   loop
   s CK-CUR !
   CK-REMATS @ 1+ CK-REMATS ! ;

\ ---- the one interior segment a backward node reads (or -1) ------------------
variable CK-NEED-V

: CK-REF-SEG ( n -- n ) {: ref:n :}     \ operand ref -> needed interior segment or -1
   ref MIR-REF-INPUT? if -1 exit then
   ref CK-NF @ >= if -1 exit then       \ backward operand: persistent buffer
   ref CK-BND? if -1 exit then          \ boundary: saved buffer
   ref CK-SEG@ ;

: CK-NEED-1 ( n -- ) {: ref:n :}
   ref CK-REF-SEG {: s:n :}
   s 0< if exit then
   CK-NEED-V @ 0< if s CK-NEED-V ! exit then
   CK-NEED-V @ s <> if E-CK-CROSS throw then ;

: CK-NEED ( n -- n ) {: nd:n :}
   -1 CK-NEED-V !
   nd MIR-IN-COUNT@ 0 ?do  nd i MIR-IN@ CK-NEED-1  loop
   CK-NEED-V @ ;

: CK-BWD-STEP ( n -- ) {: nd:n :}
   nd CK-NEED {: s:n :}
   s 0<  s CK-CUR @ =  or 0= if s CK-REMAT then
   nd EX-STEP ;

public

\ ---- checkpointed execution: forward slice, then the backward region --------
: CK-FWD ( -- )
   CK-CK
   -1 CK-CUR !
   CK-NF @ 0 ?do
      i CK-BND? if i EX-STEP else i CK-INT-RUN then
   loop ;

: CK-BWD ( -- )
   CK-CK
   0 CK-REMATS !
   MIR-N@ CK-NF @ ?do  i CK-BWD-STEP  loop ;

\ ---- plan facts + live-buffer accounting (docs/maki/train.md memory bound) ---
: CK-SEG-N@  ( -- n )  CK-CK CK-SEGS @ ;
: CK-BND-N@  ( -- n )  CK-CK CK-B-N @ ;
: CK-IMAX@   ( -- n )  CK-CK CK-IMAX @ ;
: CK-REMATS@ ( -- n )  CK-CK CK-REMATS @ ;

\ buffers saved across the forward->backward cut: boundaries vs every forward node
: CK-SAVED@      ( -- n )  CK-CK CK-B-N @ ;
: CK-SAVED-FULL@ ( -- n )  CK-CK CK-NF @ ;

\ peak live node buffers for a whole step: checkpointed vs full materialization
: CK-LIVE@      ( -- n )  CK-CK  CK-B-N @ CK-IMAX @ +  MIR-N@ CK-NF @ -  + ;
: CK-LIVE-FULL@ ( -- n )  CK-CK  MIR-N@ ;

end-package
