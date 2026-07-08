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
\ Fail closed: an out-of-range alignment class or a name overflow is a named throw.
\ maki -> habu only; mem-plan owns -5076..-5079 (schedule/sched-key own -5080..-5089).

require lib/string.f
require lib/fmt.f
require maki/report.f
require maki/model-ir.f

-5076 constant E-MP-ALIGN     \ alignment class out of range (AL-* domain)
-5077 constant E-MP-NAME      \ staged tensor name exceeds the name buffer

package MAKI
private

\ ---- alignment class -> guaranteed base byte alignment (fail closed) ---------
: MP-AL-BYTES ( n -- n )
   case
      AL-UNKNOWN of 0  endof
      AL-BYTE    of 1  endof
      AL-4       of 4  endof
      AL-8       of 8  endof
      AL-16      of 16 endof
      E-MP-ALIGN throw
   endcase ;

public

\ ---- vector width: largest w in {4,2,1} with align >= w*esize and extent >= w --
: MP-W ( n n n -- n ) {: al:n es:n ext:n :}        \ align esize extent -- w
   al MP-AL-BYTES {: ab:n :}
   4 es * ab <=  ext 4 >=  and if 4 exit then
   2 es * ab <=  ext 2 >=  and if 2 exit then
   1 ;

\ ---- classify a plain (align esize extent layout) access into a CO-* status ---
: MP-CLASSIFY ( n n n n -- n ) {: al:n es:n ext:n lay:n :}   \ align esize extent layout -- status
   lay LAY-ROW <> if CO-STRIDED exit then          \ non-unit innermost stride
   al MP-AL-BYTES es < if CO-UNALIGNED exit then    \ below element width -> scalar fallback
   al es ext MP-W 4 = if CO-COALESCED-V4 else CO-COALESCED then ;

private

\ ---- staged access facts (filled per tensor, read by the emitter) -----------
variable MP-AL   variable MP-ES   variable MP-EXT   variable MP-LAY
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
: MP-USES ( n n -- n ) {: nd:n ref:n :}            \ times node nd reads operand ref
   0 nd MIR-IN-COUNT@ 0 ?do  nd i MIR-IN@ ref = if 1+ then  loop ;

: MP-SLOT-CONSUMER ( n -- n ) {: s:n :}            \ first node reading slot s, or -1
   s MIR-IN-REF {: ref:n :}
   MIR-N@ 0 ?do  i ref MP-USES 0 > if i unloop exit then  loop  -1 ;

: MP-SLOT-GATHERED? ( n -- bool ) {: s:n :}        \ s is the data operand (0) of a gather
   s MIR-IN-REF {: ref:n :}
   MIR-N@ 0 ?do
      i MIR-OP@ OP-GATHER =  i MIR-IN-COUNT@ 0 >  and if
         i 0 MIR-IN@ ref = if unloop true exit then
      then
   loop false ;

: MP-SLOT-BROADCAST? ( n -- bool ) {: s:n :}       \ 1-row/1-col input into a wider compute op
   s MP-SLOT-CONSUMER {: nd:n :}
   nd 0 < if false exit then
   nd MIR-MOVE? if false exit then                  \ movement operands are not register-hoisted
   s MIR-SLOT-ROWS@ 1 =  nd MIR-ROWS@ 1 >  and if true exit then
   s MIR-SLOT-COLS@ 1 =  nd MIR-COLS@ 1 >  and if true exit then
   false ;

: MP-SLOT-OVERRIDE ( n -- n ) {: s:n :}            \ slot -> CO override or -1
   s MP-SLOT-GATHERED?  if CO-GATHERED exit then
   s MP-SLOT-BROADCAST? if CO-BROADCAST exit then
   -1 ;

\ ---- stage a slot read / a node write ---------------------------------------
: MP-SET-SLOT ( n -- ) {: s:n :}
   s MIR-SLOT-AL@         MP-AL !
   s MIR-SLOT-DT@ DT-SIZE MP-ES !
   s MIR-SLOT-COLS@       MP-EXT !                  \ innermost extent (LAY-ROW: cols)
   s MIR-SLOT-LAY@        MP-LAY !
   s                      MP-SLOT !
   s MP-SLOT-OVERRIDE     MP-OVR !
   SB-RESET s" i" SB-APPEND s SB-INT SB$ MP-NM! ;

: MP-SET-NODE ( n -- ) {: nd:n :}
   AL-16                  MP-AL !                   \ compiler-allocated write: aligned by construction
   nd MIR-DT@ DT-SIZE     MP-ES !
   nd MIR-COLS@           MP-EXT !
   nd MIR-LAY@            MP-LAY !
   -1                     MP-SLOT !
   -1                     MP-OVR !
   SB-RESET s" n" SB-APPEND nd SB-INT SB$ MP-NM! ;

\ ---- read staged facts ------------------------------------------------------
: MP-OVERRIDDEN? ( -- bool )  MP-OVR @ 0 >= ;

: MP-STATUS ( -- n )
   MP-OVERRIDDEN? if MP-OVR @ exit then
   MP-AL @ MP-ES @ MP-EXT @ MP-LAY @ MP-CLASSIFY ;

: MP-VEC ( -- n )                                  \ chosen vector width for the staged access
   MP-OVERRIDDEN?              if 1 exit then
   MP-LAY @ LAY-ROW <>         if 1 exit then
   MP-AL @ MP-AL-BYTES MP-ES @ < if 1 exit then
   MP-AL @ MP-ES @ MP-EXT @ MP-W ;

: MP-TAIL-K ( -- n )                               \ masked-tail remainder (0 when none)
   MP-VEC {: w:n :}
   w 2 < if 0 exit then
   MP-EXT @ w mod ;

: MP-HAS-TAIL? ( -- bool )  MP-TAIL-K 0<> ;

\ ---- warning rows -----------------------------------------------------------
: MP-ALIGN-WARN$ ( -- ptr u8 n )
   SB-RESET s" memory.align: input " SB-APPEND MP-SLOT @ SB-INT
   MP-AL @ AL-UNKNOWN = if s"  unknown alignment -> scalar"
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
: MP-INPUT-STEP ( report n -- report ) {: s:n :}
   s MP-SLOT-CONSUMER 0 < if exit then              \ unread slot: no global access
   s MP-SET-SLOT  MP-EMIT+ ;

: MP-OUTPUT-STEP ( report n -- report ) {: nd:n :}
   nd MIR-MAT@ 0= if exit then                       \ only materialized (global) tensors
   nd MIR-MOVE?   if exit then                       \ movement writes: MEM-MOVE-ROWS owns them
   nd MP-SET-NODE  MP-EMIT+ ;

public

\ emit per-hot-tensor coalescing rows into a report; caller runs FP-BUILD first so
\ the materialization flags reflect the fusion plan.
: MEM-PLAN-INTO ( report -- report )
   MIR-IN-SLOTS@ 0 ?do  i MP-INPUT-STEP   loop
   MIR-N@        0 ?do  i MP-OUTPUT-STEP  loop ;

end-package
