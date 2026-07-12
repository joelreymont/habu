\ maki/mem-plan.f - per-tensor memory access facts + coalescing classifier (cad-3).
\
\ CAD-PLAN section 6. After the fusion planner assigns regions and materialization
\ flags (maki/fusion-plan.f FP-BUILD), every tensor that touches GLOBAL memory is
\ either a model INPUT read or a materialized node WRITE; interior fused tensors
\ never leave registers, so they carry no memory row. This file classifies each such
\ access into the section 6.4 vocabulary - coalesced-v4 / coalesced / strided /
\ broadcast-register / gathered - derives the section 6.2 vector width and masked-tail
\ facts, and emits the MEMORY report rows (REPORT:HOT+ status + tail/align warnings).
\ One concern: access classification + its report rows (no region growth, no traffic
\ bytes - those are maki/fusion-plan.f and maki/traffic.f).
\
\ Facts are recorded, never assumed (CAD-PLAN 4.1). A model input carries the base
\ alignment CLASS recorded on its IR slot (AL-UNKNOWN until a bound buffer records a
\ real one, so the report says "unknown -> scalar" honestly). Buffers the compiler
\ allocates (fusion intermediates + materialized output writes) are aligned by
\ construction, so a write is classified at AL-16 (CAD-PLAN 6.4: coalesced-v4 store).
\
\ Vector width (6.2.2): largest w in {4,2,1} with alignment >= w*esize (esize from the
\ dtype table), unit stride (v1: LAY-ROW contiguous innermost), and extent >= w; a
\ non-zero extent mod w is a masked tail (the tile-v4 pattern). Broadcasts (a 1-row /
\ 1-col input into a wider compute op) hoist to a register (6.2.3); a column-major read
\ is strided; the data read of a gather is gathered (6.3).
\
\ Fail closed: a name overflow is a named throw; an out-of-range alignment class
\ is a checker reject (the align family cannot hold one).
\ maki -> habu only; mem-plan owns -5076..-5079 (schedule/sched-key own -5080..-5089).

require lib/string.f
require lib/fmt.f
require maki/report.f
require maki/model-ir.f

\ -5076 (E-MP-ALIGN) retired: the align family makes an out-of-range class a
\ checker reject; the code stays reserved to mem-plan.
-5077 constant E-MP-NAME      \ staged tensor name exceeds the name buffer

package MAKI
private

\ ---- alignment class -> guaranteed base byte alignment (exhaustive MATCH) ----
: MP-AL-BYTES ( align -- n )
   MATCH align
      unknown OF 0  ENDOF
      byte    OF 1  ENDOF
      a4      OF 4  ENDOF
      a8      OF 8  ENDOF
      a16     OF 16 ENDOF
   ;MATCH ;

public

\ ---- vector width: largest w in {4,2,1} with align >= w*esize and extent >= w --
\ the align family converts to guaranteed bytes up front (families cannot bind
\ into locals), then the width pick is plain arithmetic
: MP-W ( align n n -- n )                          \ align esize extent -- w
   {: es:n ext:n :}
   MP-AL-BYTES {: ab:n :}
   4 es * ab <=  ext 4 >=  and if 4 exit then
   2 es * ab <=  ext 2 >=  and if 2 exit then
   1 ;

\ ---- classify a plain (align esize extent layout) access into a CO-* status ---
\ layout (top) is consumed by the contiguity predicate first; align stays on the
\ stack (dup for its two uses) while the extent locals bind
: MP-CLASSIFY ( align n n layout -- n )            \ align esize extent layout -- status
   LAYOUT-ROW? 0= if 2drop drop CO-STRIDED exit then   \ non-unit innermost stride
   {: es:n ext:n :}
   dup MP-AL-BYTES es < if drop CO-UNALIGNED exit then \ below element width -> scalar
   es ext MP-W 4 = if CO-COALESCED-V4 else CO-COALESCED then ;

private

\ ---- staged access facts (filled per tensor, read by the emitter) -----------
\ MP-AL / MP-LAY hold family values behind one-slot generative buffers (the
\ report.f F-ROOFLINE-AT precedent); the others stay n cells.
1 LAYOUT-BUFFER MP-AL  align
1 LAYOUT-BUFFER MP-LAY layout
variable MP-ES   variable MP-EXT
: MP-AL-AT  ( -- ptr align )   0 MP-AL ;
: MP-LAY-AT ( -- ptr layout )  0 MP-LAY ;
variable MP-OVR                    \ broadcast/gathered override status, or -1
variable MP-SLOT                   \ input-slot index for the align warning, or -1
64 constant MP-NM-CAP
create MP-NM MP-NM-CAP allot  variable MP-NM-U

: MP-NM! ( ptr u8 n -- ) {: a:ptr u:n :}           \ copy a tensor name into the stable buffer
   u MP-NM-CAP > if E-MP-NAME throw then
   0 begin dup u < while  dup a + c@ over MP-NM + c!  1+  repeat drop
   u MP-NM-U ! ;
: MP-NAME$ ( -- ptr u8 n )  MP-NM MP-NM-U @ ;

\ ---- override detection (input-only: gathered read + broadcast register) -----
: MP-USES ( CAD-KIND:node-id MIR:operand-ref -- n )
   {: nd:CAD-KIND:node-id ref:MIR:operand-ref :}    \ times node nd reads operand ref
   0 nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-INPUT-IDX MIR-IN@ ref MIR-REF= if 1+ then
   loop ;

: MP-SLOT-READ? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   s MIR-IN-REF {: ref:MIR:operand-ref :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID ref MP-USES 0 > if unloop true exit then
   loop false ;

: MP-SLOT-GATHERED? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   s MIR-IN-REF {: ref:MIR:operand-ref :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: nd:CAD-KIND:node-id :}
      nd MIR-OP@ MAKI-OPKIND:GATHER MAKI-OPKIND:EQ  nd MIR-IN-COUNT@ 0 >  and if
         nd 0 MIR-INPUT-IDX MIR-IN@ ref MIR-REF= if unloop true exit then
      then
   loop false ;

: MP-BROADCAST-AT? ( MIR:input-slot CAD-KIND:node-id -- bool )
   {: s:MIR:input-slot nd:CAD-KIND:node-id :}       \ 1-row/1-col input into a wider compute op
   nd MIR-MOVE? if false exit then                  \ movement operands are not register-hoisted
   s MIR-SLOT-ROWS@ ROWS-RAW 1 = nd MIR-ROWS@ ROWS-RAW 1 > and if true exit then
   s MIR-SLOT-COLS@ COLS-RAW 1 = nd MIR-COLS@ COLS-RAW 1 > and ;

: MP-SLOT-BROADCAST? ( MIR:input-slot -- bool ) {: s:MIR:input-slot :}
   s MIR-IN-REF {: ref:MIR:operand-ref :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: nd:CAD-KIND:node-id :}
      nd ref MP-USES 0 > if s nd MP-BROADCAST-AT? unloop exit then
   loop false ;

: MP-SLOT-OVERRIDE ( MIR:input-slot -- n ) {: s:MIR:input-slot :}
   s MP-SLOT-GATHERED?  if CO-GATHERED exit then
   s MP-SLOT-BROADCAST? if CO-BROADCAST exit then
   -1 ;

\ ---- stage a slot read / a node write ---------------------------------------
: MP-SLOT! ( MIR:input-slot -- )  MP-SLOT ! ;
: MP-SLOT@ ( -- MIR:input-slot )  MP-SLOT @ ;

: MP-SET-SLOT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s MIR-SLOT-AL@         MP-AL-AT !
   s MIR-SLOT-DT@ DT-SIZE DIM-RAW MP-ES !
   s MIR-SLOT-COLS@ COLS-RAW MP-EXT !               \ innermost extent (row-major: cols)
   s MIR-SLOT-LAY@        MP-LAY-AT !
   s                      MP-SLOT!
   s MP-SLOT-OVERRIDE     MP-OVR !
   SB-RESET s" i" SB-APPEND s SLOT>RAW SB-INT SB$ MP-NM! ;

: MP-SET-NODE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   MAKI-ALIGN:A16         MP-AL-AT !                \ compiler-allocated write: aligned by construction
   nd MIR-DT@ DT-SIZE DIM-RAW MP-ES !
   nd MIR-COLS@ COLS-RAW  MP-EXT !
   nd MIR-LAY@            MP-LAY-AT !
   \ keep the stale-slot invariant fail-obvious: MP-ALIGN-WARN$ is unreachable
   \ for node writes only while every dtype's element size is <= 16 bytes
   \ (AL-16 write can never classify CO-UNALIGNED); if that ever changes, a
   \ stale slot number must never render, so the cursor is reset here.
   -1                     MP-SLOT !
   -1                     MP-OVR !
   SB-RESET s" n" SB-APPEND nd NODE>RAW SB-INT SB$ MP-NM! ;

\ ---- read staged facts ------------------------------------------------------
: MP-OVERRIDDEN? ( -- bool )  MP-OVR @ 0 >= ;

: MP-STATUS ( -- n )
   MP-OVERRIDDEN? if MP-OVR @ exit then
   MP-AL-AT @ MP-ES @ MP-EXT @ MP-LAY-AT @ MP-CLASSIFY ;

: MP-VEC ( -- n )                                  \ chosen vector width for the staged access
   MP-OVERRIDDEN?                    if 1 exit then
   MP-LAY-AT @ LAYOUT-ROW? 0=        if 1 exit then
   MP-AL-AT @ MP-AL-BYTES MP-ES @ <  if 1 exit then
   MP-AL-AT @ MP-ES @ MP-EXT @ MP-W ;

: MP-TAIL-K ( -- n )                               \ masked-tail remainder (0 when none)
   MP-VEC {: w:n :}
   w 2 < if 0 exit then
   MP-EXT @ w mod ;

: MP-HAS-TAIL? ( -- bool )  MP-TAIL-K 0<> ;

\ ---- warning rows -----------------------------------------------------------
: MP-ALIGN-WARN$ ( -- ptr u8 n )
   SB-RESET s" memory.align: input " SB-APPEND MP-SLOT@ SLOT>RAW SB-INT
   MP-AL-AT @ ALIGN-UNKNOWN? if s"  unknown alignment -> scalar"
                             else s"  sub-4B alignment -> scalar" then SB-APPEND
   SB$ ;

: MP-TAIL-WARN$ ( -- ptr u8 n )
   SB-RESET s" memory.tail: " SB-APPEND MP-NAME$ SB-APPEND
   $20 SB-APPEND-C MP-EXT @ SB-INT
   s"  mod " SB-APPEND MP-VEC SB-INT
   s"  = "   SB-APPEND MP-TAIL-K SB-INT SB$ ;

\ ---- emit one staged access (hot status row + its warnings) -----------------
: MP-EMIT+ ( report -- report )
   MP-NAME$ MP-STATUS REPORT:HOT+
   MP-STATUS CO-UNALIGNED = if MP-ALIGN-WARN$ REPORT:WARN+ then
   MP-HAS-TAIL?           if MP-TAIL-WARN$  REPORT:WARN+ then ;

\ ---- per-tensor iteration ---------------------------------------------------
: MP-INPUT-STEP ( report MIR:input-slot -- report ) {: s:MIR:input-slot :}
   s MP-SLOT-READ? 0= if exit then                  \ unread slot: no global access
   s MP-SET-SLOT  MP-EMIT+ ;

: MP-OUTPUT-STEP ( report CAD-KIND:node-id -- report ) {: nd:CAD-KIND:node-id :}
   nd MIR-MAT@ 0= if exit then                       \ only materialized (global) tensors
   nd MIR-MOVE?   if exit then                       \ movement writes: MEM-MOVE-ROWS owns them
   nd MP-SET-NODE  MP-EMIT+ ;

public

\ emit per-hot-tensor coalescing rows into a report; caller runs FP-BUILD first so
\ the materialization flags reflect the fusion plan.
: MEM-PLAN-INTO ( report -- report )
   MIR-IN-SLOTS@ 0 ?do  i MIR-SLOT-ID MP-INPUT-STEP   loop
   MIR-N@        0 ?do  i MIR-NODE-ID MP-OUTPUT-STEP  loop ;

;package
