\ maki/traffic.f - the fusion traffic (global-bytes) estimate (dot cad-2).
\
\ CAD-PLAN section 9 bytes model, elements-based v1: bytes = elements * dtype-width
\ (f32=4). BEFORE fusion every node reads its inputs and writes its output (each op
\ a separate kernel). AFTER fusion, using the maki/fusion-plan.f region assignment,
\ each region reads its EXTERNAL inputs once (broadcast discount: an input consumed
\ by several nodes in the SAME region counts once) and writes its materialized
\ outputs once; movement free/staged nodes contribute zero (they carry MAT-FLAG 0),
\ and a gathered node's indexed read is counted with a `gathered` warning row.
\
\ Honesty over fabrication: any unbound extent (a 0 dimension) makes the whole
\ estimate unknown - REPORT:BYTES! is never called and a warning names the unbound input
\ or node. maki -> habu only; traffic owns -5075. Depends on the region ids being
\ current (call FP-BUILD before TRF-AFTER / TRF-INTO).

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/model-ir.f
require maki/op-registry.f
require maki/move-facts.f
require maki/fusion-plan.f
require maki/report.f

-5075 constant E-TRF-CAP     \ per-region external-source set capacity exceeded

package MAKI
private

\ ---- element / byte counts (slot and node outputs) -------------------------
\ (dtype byte width is tensor.f DT-SIZE, an exhaustive MATCH over the family
\ returning a CAD-KIND:dim; the old numeric DT-BYTES duplicate is retired)
: TRF-SLOT-ELEMS ( MIR:input-slot -- n ) {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ SHAPE-ELEMS DIM-RAW ;
: TRF-NODE-ELEMS ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd MIR-ROWS@ nd MIR-COLS@ SHAPE-ELEMS DIM-RAW ;
: TRF-SLOT-BYTES ( MIR:input-slot -- n ) {: s:MIR:input-slot :}
   s TRF-SLOT-ELEMS s MIR-SLOT-DT@ DT-SIZE DIM-RAW * ;
: TRF-NODE-BYTES ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd TRF-NODE-ELEMS nd MIR-DT@ DT-SIZE DIM-RAW * ;

\ operand ref -> the bytes of the tensor it names (input slot or producer node)
: TRF-REF-BYTES ( MIR:operand-ref -- n ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if
      ref MIR-REF-SLOT TRF-SLOT-BYTES
   else
      ref MIR-REF-NODE TRF-NODE-BYTES
   then ;

\ ---- boundness (an unbound extent poisons the whole estimate) ---------------
: TRF-SLOT-BOUND? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ ROWS-RAW 0 > s MIR-SLOT-COLS@ COLS-RAW 0 > and ;
: TRF-NODE-BOUND? ( CAD-KIND:node-id -- bool ) {: nd:CAD-KIND:node-id :}
   nd MIR-ROWS@ ROWS-RAW 0 > nd MIR-COLS@ COLS-RAW 0 > and ;

public

: TRF-BOUND? ( -- bool )
   MIR-IN-SLOTS@ 0 ?do
      i MIR-SLOT-ID TRF-SLOT-BOUND? 0= if unloop false exit then
   loop
   MIR-N@ 0 ?do
      i MIR-NODE-ID TRF-NODE-BOUND? 0= if unloop false exit then
   loop
   true ;

private

\ ---- before-fusion traffic (every node reads inputs + writes output) --------
: TRF-NODE-IN-BYTES ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   0 nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-INPUT-IDX MIR-IN@ TRF-REF-BYTES +
   loop ;

public

: TRF-BEFORE ( -- n )
   0 MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node TRF-NODE-IN-BYTES node TRF-NODE-BYTES + +
   loop ;

private

\ ---- after-fusion: per-region external reads (deduped) + materialized writes -
MIR-INCAP constant TRF-SCAP
create TRF-SRC TRF-SCAP cells allot   variable TRF-SRC-N

: TRF-SRC-RESET ( -- )  0 TRF-SRC-N ! ;

: TRF-SRC@ ( n -- MIR:operand-ref )  cells TRF-SRC + @ ;
: TRF-SRC! ( MIR:operand-ref n -- )  cells TRF-SRC + ! ;

: TRF-SRC-HAS? ( MIR:operand-ref -- bool ) {: ref:MIR:operand-ref :}
   TRF-SRC-N @ 0 ?do
      i TRF-SRC@ ref MIR-REF= if unloop true exit then
   loop
   false ;

: TRF-SRC-ADD ( MIR:operand-ref -- ) {: ref:MIR:operand-ref :}   \ intern a distinct external source ref
   ref TRF-SRC-HAS? if exit then
   TRF-SRC-N @ TRF-SCAP >= if E-TRF-CAP throw then
   ref TRF-SRC-N @ TRF-SRC!
   TRF-SRC-N @ 1+ TRF-SRC-N ! ;

\ is operand ref external to region r? (an input slot, or a node in another region)
: TRF-EXT? ( MIR:operand-ref n -- bool ) {: ref:MIR:operand-ref r:n :}
   ref MIR-REF-INPUT? if true exit then
   ref MIR-REF-NODE FP-RID@ r <> ;

: TRF-NODE-READS+ ( CAD-KIND:node-id n -- ) {: nd:CAD-KIND:node-id r:n :}
   nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-INPUT-IDX MIR-IN@ dup r TRF-EXT? if TRF-SRC-ADD else drop then
   loop ;

: TRF-RGN-READS ( n -- n ) {: r:n :}
   TRF-SRC-RESET
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r = if node r TRF-NODE-READS+ then
   loop
   0 TRF-SRC-N @ 0 ?do  i TRF-SRC@ TRF-REF-BYTES +  loop ;

: TRF-RGN-WRITES ( n -- n ) {: r:n :}
   0 MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r = node MIR-MAT@ and if node TRF-NODE-BYTES + then
   loop ;

public

: TRF-AFTER ( -- n )
   0  FP-REGION-COUNT 0 ?do  i TRF-RGN-READS +  i TRF-RGN-WRITES +  loop ;

private

\ ---- warning rows ----------------------------------------------------------
\ Append shape dims into SB directly: MIR-*-SHAPE-KEY reset the shared SB and would
\ clobber a row under construction, so we render "RxC" (unbound extent -> "?") here.
: TRF-DIM+ ( n -- )  dup 0= if drop s" ?" SB-APPEND else SB-INT then ;
: TRF-SHAPE+ ( CAD-KIND:rows CAD-KIND:cols -- )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   rows ROWS-RAW TRF-DIM+ $78 SB-APPEND-C cols COLS-RAW TRF-DIM+ ;

: TRF-GATHERED-ROW$ ( CAD-KIND:node-id -- ptr u8 n )
   {: nd:CAD-KIND:node-id :}
   SB-RESET s" traffic.gathered: node " SB-APPEND nd NODE>RAW SB-INT
   $20 SB-APPEND-C nd MIR-OP@ OPR-NAME SB-APPEND
   s"  indexed read (non-coalesced)" SB-APPEND SB$ ;

: TRF-UNBOUND-SLOT$ ( MIR:input-slot -- ptr u8 n ) {: s:MIR:input-slot :}
   SB-RESET s" traffic.unbound: input " SB-APPEND s SLOT>RAW SB-INT
   s"  shape " SB-APPEND s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ TRF-SHAPE+ SB$ ;

: TRF-UNBOUND-NODE$ ( CAD-KIND:node-id -- ptr u8 n )
   {: nd:CAD-KIND:node-id :}
   SB-RESET s" traffic.unbound: node " SB-APPEND nd NODE>RAW SB-INT
   $20 SB-APPEND-C nd MIR-OP@ OPR-NAME SB-APPEND
   s"  shape " SB-APPEND nd MIR-ROWS@ nd MIR-COLS@ TRF-SHAPE+ SB$ ;

: TRF-GATHERED+ ( report -- report )
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node MIR-MOVE? if
         node MIR-MOVE-VERDICT@ MVV-GATHERED = if
            node TRF-GATHERED-ROW$ REPORT:WARN+
         then
      then
   loop ;

: TRF-UNBOUND+ ( report -- report )
   MIR-IN-SLOTS@ 0 ?do
      i MIR-SLOT-ID {: s:MIR:input-slot :}
      s TRF-SLOT-BOUND? 0= if s TRF-UNBOUND-SLOT$ REPORT:WARN+ then
   loop
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node TRF-NODE-BOUND? 0= if node TRF-UNBOUND-NODE$ REPORT:WARN+ then
   loop ;

public

\ write the traffic estimate into a report: bytes when bound, unbound warnings
\ otherwise; a gathered warning row for every gathered read either way.
: TRF-INTO ( report -- report )
   TRF-BOUND? if  TRF-BEFORE TRF-AFTER REPORT:BYTES!  else  TRF-UNBOUND+  then
   TRF-GATHERED+ ;

end-package
