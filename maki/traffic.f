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
public

\ ---- dtype byte width (fail closed on an unknown dtype) ---------------------
: DT-BYTES ( n -- n )
   case
      DT-F32  of 4 endof
      DT-F16  of 2 endof
      DT-BF16 of 2 endof
      DT-U32  of 4 endof
      DT-I32  of 4 endof
      E-MK-DTYPE throw
   endcase ;

private

\ ---- element / byte counts (slot and node outputs) -------------------------
: TRF-SLOT-ELEMS ( n -- n ) {: s:n :}  s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ * ;
: TRF-NODE-ELEMS ( n -- n ) {: nd:n :}  nd MIR-ROWS@ nd MIR-COLS@ * ;
: TRF-SLOT-BYTES ( n -- n ) {: s:n :}  s TRF-SLOT-ELEMS s MIR-SLOT-DT@ DT-BYTES * ;
: TRF-NODE-BYTES ( n -- n ) {: nd:n :}  nd TRF-NODE-ELEMS nd MIR-DT@ DT-BYTES * ;

\ operand ref -> the bytes of the tensor it names (input slot or producer node)
: TRF-REF-BYTES ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT TRF-SLOT-BYTES else ref TRF-NODE-BYTES then ;

\ ---- boundness (an unbound extent poisons the whole estimate) ---------------
: TRF-SLOT-BOUND? ( n -- bool ) {: s:n :}  s MIR-SLOT-ROWS@ 0 >  s MIR-SLOT-COLS@ 0 >  and ;
: TRF-NODE-BOUND? ( n -- bool ) {: nd:n :}  nd MIR-ROWS@ 0 >  nd MIR-COLS@ 0 >  and ;

public

: TRF-BOUND? ( -- bool )
   MIR-IN-SLOTS@ 0 ?do  i TRF-SLOT-BOUND? 0= if unloop false exit then  loop
   MIR-N@       0 ?do  i TRF-NODE-BOUND? 0= if unloop false exit then  loop
   true ;

private

\ ---- before-fusion traffic (every node reads inputs + writes output) --------
: TRF-NODE-IN-BYTES ( n -- n ) {: nd:n :}
   0  nd MIR-IN-COUNT@ 0 ?do  nd i MIR-IN@ TRF-REF-BYTES +  loop ;

public

: TRF-BEFORE ( -- n )
   0  MIR-N@ 0 ?do  i TRF-NODE-IN-BYTES  i TRF-NODE-BYTES +  +  loop ;

private

\ ---- after-fusion: per-region external reads (deduped) + materialized writes -
MIR-INCAP constant TRF-SCAP
create TRF-SRC TRF-SCAP cells allot   variable TRF-SRC-N

: TRF-SRC-RESET ( -- )  0 TRF-SRC-N ! ;

: TRF-SRC-HAS? ( n -- bool ) {: ref:n :}
   TRF-SRC-N @ 0 ?do  i cells TRF-SRC + @ ref = if unloop true exit then  loop  false ;

: TRF-SRC-ADD ( n -- ) {: ref:n :}         \ intern a distinct external source ref
   ref TRF-SRC-HAS? if exit then
   TRF-SRC-N @ TRF-SCAP >= if E-TRF-CAP throw then
   ref TRF-SRC-N @ cells TRF-SRC + !  TRF-SRC-N @ 1+ TRF-SRC-N ! ;

\ is operand ref external to region r? (an input slot, or a node in another region)
: TRF-EXT? ( n n -- bool ) {: ref:n r:n :}
   ref MIR-REF-INPUT? if true exit then
   ref FP-RID@ r <> ;

: TRF-NODE-READS+ ( n n -- ) {: nd:n r:n :}
   nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-IN@  dup r TRF-EXT? if TRF-SRC-ADD else drop then
   loop ;

: TRF-RGN-READS ( n -- n ) {: r:n :}
   TRF-SRC-RESET
   MIR-N@ 0 ?do  i FP-RID@ r = if i r TRF-NODE-READS+ then  loop
   0  TRF-SRC-N @ 0 ?do  i cells TRF-SRC + @ TRF-REF-BYTES +  loop ;

: TRF-RGN-WRITES ( n -- n ) {: r:n :}
   0  MIR-N@ 0 ?do  i FP-RID@ r =  i MIR-MAT@  and if i TRF-NODE-BYTES + then  loop ;

public

: TRF-AFTER ( -- n )
   0  FP-REGION-COUNT 0 ?do  i TRF-RGN-READS +  i TRF-RGN-WRITES +  loop ;

private

\ ---- warning rows ----------------------------------------------------------
\ Append shape dims into SB directly: MIR-*-SHAPE-KEY reset the shared SB and would
\ clobber a row under construction, so we render "RxC" (unbound extent -> "?") here.
: TRF-DIM+ ( n -- )  dup 0= if drop s" ?" SB-APPEND else SB-INT then ;
: TRF-SHAPE+ ( n n -- ) {: rows:n cols:n :}  rows TRF-DIM+  $78 SB-APPEND-C  cols TRF-DIM+ ;

: TRF-GATHERED-ROW$ ( n -- ptr u8 n ) {: nd:n :}
   SB-RESET s" traffic.gathered: node " SB-APPEND nd SB-INT
   $20 SB-APPEND-C nd MIR-OP@ OPR-NAME SB-APPEND
   s"  indexed read (non-coalesced)" SB-APPEND SB$ ;

: TRF-UNBOUND-SLOT$ ( n -- ptr u8 n ) {: s:n :}
   SB-RESET s" traffic.unbound: input " SB-APPEND s SB-INT
   s"  shape " SB-APPEND s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ TRF-SHAPE+ SB$ ;

: TRF-UNBOUND-NODE$ ( n -- ptr u8 n ) {: nd:n :}
   SB-RESET s" traffic.unbound: node " SB-APPEND nd SB-INT
   $20 SB-APPEND-C nd MIR-OP@ OPR-NAME SB-APPEND
   s"  shape " SB-APPEND nd MIR-ROWS@ nd MIR-COLS@ TRF-SHAPE+ SB$ ;

: TRF-GATHERED+ ( report -- report )
   MIR-N@ 0 ?do
      i MIR-MOVE? if
         i MIR-MOVE-VERDICT@ MVV-GATHERED = if i TRF-GATHERED-ROW$ REPORT:WARN+ then
      then
   loop ;

: TRF-UNBOUND+ ( report -- report )
   MIR-IN-SLOTS@ 0 ?do  i TRF-SLOT-BOUND? 0= if i TRF-UNBOUND-SLOT$ REPORT:WARN+ then  loop
   MIR-N@       0 ?do  i TRF-NODE-BOUND? 0= if i TRF-UNBOUND-NODE$ REPORT:WARN+ then  loop ;

public

\ write the traffic estimate into a report: bytes when bound, unbound warnings
\ otherwise; a gathered warning row for every gathered read either way.
: TRF-INTO ( report -- report )
   TRF-BOUND? if  TRF-BEFORE TRF-AFTER REPORT:BYTES!  else  TRF-UNBOUND+  then
   TRF-GATHERED+ ;

end-package
