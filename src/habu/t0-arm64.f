split-str
\ t0-arm64.f - the arm64 emitter of the tier 0 compiler, part ARM64 of package
\ COMP.
\
\ Implements the EMIT- operations src/habu/t0-emit.f states for every target;
\ this file holds only arm64 encodings and register choices. FILL-SLOT stores
\ its words into the EMIT- and src/habu/t0-label.f (LABEL) cells for
\ CTARGET-ARCH:AARCH64.
\
\ PLACE (src/habu/t0-place.f) places the units. LINKED adds x30 to the frame,
\ and a `ret` and a tail `b` take an epilogue. A branch never leaves its unit,
\ so EMIT-PATCH leaves it alone; one its field cannot reach throws
\ E-A64EMIT-REACH.
\
\ A data address is three words, `movz lsl32`, `movk lsl16`, `movk`, every one
\ emitted so the publisher can rewrite it in place. A code address is `adrp`
\ and `add` relative to where the code runs, `adrp d, 0; add d, d, 0` until
\ PATCH writes it for a placed emission or the link for any other. A data
\ address's row carries the origin ADDR-DATA takes, a code address's -1. A
\ quotation's address is an `adr` relative to its own site and `recurse` a
\ `bl` to byte 0: neither files a row.
\
\ The frame cell stays the index's home (`j` and the runtime read it); x29 is
\ a copy written beside every store of it. HELD says whether the code emitted
\ next may read x29: a loop head sets it, and so do `i` and a step once they
\ load it. Every call clears it, since the callee may run a loop, and so does
\ every label a branch lands on (MARK and RESOLVE), since a branch may come
\ from code that wrote x29; LOOP-POP and a unit's start clear it too. While it
\ is clear, `i`, `loop` and `+loop` load x29 from the frame first. A rejected
\ token discards the whole emission, and OPEN begins the next one clear.

require src/lib/error.f
require src/lib/le.f
require src/lib/vector.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/arm64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f
require src/habu/t0-label.f
require src/habu/t0-place.f

package COMP
package ARM64
using A64ASM
using PLACE

15 constant NVREG
16 constant TMP
17 constant ADR
30 constant LR
29 constant IDX
9 constant CB-ID                      \ x9, the stub's slot for its thunk
4 constant INSN                       \ the bytes of one instruction word
31 constant SP                        \ as a base or an add/sub operand
4 constant REC-ARG                    \ x4, a reading quotation's record on entry

\ Registers. x19 is the data stack and grows up. x19, x20 (XREG-RBASE), x26, x27
\ and x28 are the engine's; compiled code writes none of them except x19 as
\ the stack. x18 is Darwin's. x29 caches the innermost loop index, and any
\ callee may write it. x16 and x17 are this file's temporaries, a literal and
\ an address, and hold no value between two of its words. A body is entered
\ by `bl` or `blr` and leaves by `ret` through x30.
\
\ The machine register of each vreg; an index past it throws E-LAYOUT-BOUNDS.
NVREG TYPED-BUFFER REGS n
 9 0 REGS !  10 1 REGS !  11 2 REGS !  12 3 REGS !  13 4 REGS !
14 5 REGS !  15 6 REGS !  25 7 REGS !  23 8 REGS !  24 9 REGS !
21 10 REGS !  22 11 REGS !  6 12 REGS !  7 13 REGS !  8 14 REGS !

: REG ( vreg -- n ) VREG>N REGS @ ;

6 TYPED-BUFFER CONDS n
C-EQ CC-EQ CC>N CONDS !
C-NE CC-NE CC>N CONDS !
C-LT CC-LT CC>N CONDS !
C-GE CC-GE CC>N CONDS !
C-LE CC-LE CC>N CONDS !
C-GT CC-GT CC>N CONDS !

\ The register form of each ALU operation, and the bitmask-immediate form of
\ the logical ones, OP-AND first.
5 TYPED-BUFFER ALU-RR [ n n n -- n ]
' ENC-ADD OP-ADD ALU>N ALU-RR !
' ENC-SUB OP-SUB ALU>N ALU-RR !
' ENC-AND OP-AND ALU>N ALU-RR !
' ENC-ORR OP-OR ALU>N ALU-RR !
' ENC-EOR OP-XOR ALU>N ALU-RR !

3 TYPED-BUFFER ALU-LOGI [ n n n -- n ]
' ENC-ANDI 0 ALU-LOGI !
' ENC-ORRI 1 ALU-LOGI !
' ENC-EORI 2 ALU-LOGI !

2 TYPED-BUFFER SH-RR [ n n n -- n ]
' ENC-LSLV SH-LEFT SHIFTER>N SH-RR !
' ENC-LSRV SH-RIGHT SHIFTER>N SH-RR !

2 TYPED-BUFFER SH-RI [ n n n -- n ]
' ENC-LSLI SH-LEFT SHIFTER>N SH-RI !
' ENC-LSRI SH-RIGHT SHIFTER>N SH-RI !

variable LINKED                       \ nonzero once this unit calls
variable HELD                         \ nonzero while x29 holds the index

0 constant FORM-B
1 constant FORM-BCOND
2 constant FORM-CBZ

4095 constant ADJ-MAX                 \ the widest add/sub immediate

: W32+ ( n ^VEC:vector<u8> -- ) {: w:n v:^ :}
   v VEC:COUNT {: at:n :}
   v at INSN + VEC:ENSURE
   w  v at VEC:AT  LE:U32! ;

: W ( n -- ) RAW @ W32+ ;

: W32! ( n n -- ) {: w:n at:n :}
   w  OUT @ at VEC:AT  LE:U32! ;

: HALF ( n n -- n ) 16 * rshift $FFFF and ;

: HALVES ( n n -- n ) {: v:n h:n :}
   0 4 0 ?do v i HALF h = if 1 + then loop ;

: LANE ( bool n n n bool ^VEC:vector<u8> -- bool )
   {: first:bool h:n hw:n r:n inv:bool buf:^ :}
   first 0= if r h hw MOVKHW buf W32+ false exit then
   inv if r h $FFFF xor hw MOVNHW buf W32+ false exit then
   r h hw MOVZHW buf W32+ false ;

\ Load the constant v into r with the shortest move-wide chain.
: LIT-IN ( n n ^VEC:vector<u8> -- ) {: v:n r:n buf:^ :}
   v $FFFF HALVES v 0 HALVES > {: inv:bool :}
   inv if $FFFF else 0 then {: fill:n :}
   true 4 0 ?do v i HALF fill <> if v i HALF i r inv buf LANE then loop
   0= if exit then
   inv if r 0 0 MOVNHW buf W32+ exit then
   r 0 0 MOVZHW buf W32+ ;

: LIT-R ( n n -- ) RAW @ LIT-IN ;

: PAGES? ( n -- bool ) {: v:n :}
   v 0 > v $FFF and 0= and v 12 rshift IMM12-LIM < and ;

\ rd becomes rn + v, in the fewest words: an immediate, its negation, a page
\ count, else v in x16. sp as rn is copied first there, since a register add
\ reads 31 as the zero register.
: ADD-IMM ( n n n -- ) {: rd:n rn:n v:n :}
   v 0 = if rd rn = if exit then rd rn 0 ENC-ADDI W exit then
   v 0 > v IMM12-LIM < and if rd rn v ENC-ADDI W exit then
   v negate {: nv:n :}
   nv 0 > nv IMM12-LIM < and if rd rn nv ENC-SUBI W exit then
   v PAGES? if rd rn v 12 rshift ENC-ADDI-LSL12 W exit then
   nv PAGES? if rd rn nv 12 rshift ENC-SUBI-LSL12 W exit then
   rn SP = if rd SP 0 ENC-ADDI W rd else rn then {: src:n :}
   v TMP LIT-R
   rd src TMP ENC-ADD W ;

\ Whether a cell access reaches base + off in one word: a scaled unsigned
\ offset, or an unscaled signed nine-bit one.
: SCALED? ( n -- bool ) {: off:n :}
   off 0 >= off 7 and 0= and off 3 rshift IMM12-LIM < and ;

: SIMM9? ( n -- bool ) {: off:n :} off -256 >= off 255 <= and ;

: FITS? ( n -- bool ) {: off:n :} off SCALED? off SIMM9? or ;

\ The base and offset one access reaches base + off by: x17 when it does not fit.
: ADDRESS ( n n -- n n ) {: base:n off:n :}
   off FITS? if base off exit then
   ADR base off ADD-IMM  ADR 0 ;

\ One cell access at base + off, which FITS? answered true for.
: ACCESS ( n n n bool -- ) {: rt:n base:n off:n st:bool :}
   off SCALED? if
      rt base off st if ENC-STR else ENC-LDR then W exit
   then
   rt base off st if ENC-STUR else ENC-LDUR then W ;

: XFER ( n n n bool -- ) {: rt:n base:n off:n st:bool :}
   base off ADDRESS {: b:n o:n :}
   rt b o st ACCESS ;

: DSP ( -- n ) ENGINE-GPR:A64-DSTACK ;

: SP-PAGES ( n bool -- n )
   if SP SP rot ENC-ADDI-LSL12 exit then
   SP SP rot ENC-SUBI-LSL12 ;

: SP-ADJ ( n bool ^VEC:vector<u8> -- ) {: bytes:n add:bool buf:^ :}
   bytes 12 rshift
   begin dup 0 > while
      dup ADJ-MAX min dup add SP-PAGES buf W32+ -
   repeat drop
   bytes ADJ-MAX and {: lo:n :}
   lo 0= if exit then
   add if SP SP lo ENC-ADDI buf W32+ exit then
   SP SP lo ENC-SUBI buf W32+ ;

\ Three halves, high first. Every data address a target holds is below 2^48:
\ the DATA window, 2^38 bytes at $340000000 on Linux and 2^40 at $44000000000
\ on macOS, and a build's space, which lies above hb's window.
: DATA-CHAIN ( n n -- ) {: v:n r:n :}
   r v 2 HALF 2 MOVZHW W
   r v 1 HALF 1 MOVKHW W
   r v 0 HALF 0 MOVKHW W ;

\ The return stack: x16 its depth, x17 the address of slot x16.
: RDEPTH ( -- ) TMP XREG-RBASE RSP-CELL ENC-LDR W ;
: RDEPTH! ( -- ) TMP XREG-RBASE RSP-CELL ENC-STR W ;

: RSLOT ( -- )
   ADR XREG-RBASE STACK-ABI:RETURN-BASE-CELL ENC-LDR W
   ADR ADR TMP 3 ENC-ADD-LSL W ;

\ ---- PLACE's hooks -----------------------------------------------------------
: UNIT-RESET ( -- ) 0 LINKED !  0 HELD ! ;

: PROLOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   LINKED @ if LR SP -16 ENC-STRPRE buf W32+ then
   bytes false buf SP-ADJ ;

: EPILOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   bytes true buf SP-ADJ
   LINKED @ if LR SP 16 ENC-LDRPOST buf W32+ then ;

: SELF-PUT ( n -- ) {: at:n :}  at negate ENC-BL  at W32! ;

\ The `adr` in OUT holds its register in its first byte.
: ADR-PUT ( n n -- ) {: at:n to:n :}
   OUT @ at VEC:AT c@ 31 and  to at - ENC-ADR  at W32! ;

\ Branch k's word, for its site at and target to in OUT.
: BR-WORD ( n n n -- n )
   {: k:n at:n to:n :}
   k BR-FORM @ {: form:n :}
   to at - {: d:n :}
   form FORM-B = if d ENC-B exit then
   form FORM-BCOND = if d k BR-ARG @ ENC-BCOND exit then
   k BR-ARG @ d ENC-CBZ ;

\ Every form is the placeholder's one word.
: BRANCH-LEN ( n n -- n ) 2drop 0 ;

: BRANCH-PUT ( n n n -- ) {: k:n at:n to:n :}
   k at to BR-WORD  at W32! ;

\ ---- the body ----------------------------------------------------------------
: RETURN ( -- ) EPI+  ENC-RET W ;

: SLOT-CLOSE ( -- )
   RETURN PLACE:CLOSE
   OUT-OFF INSN - EMIT-PATCH-SLOT! ;

\ The callback stub is appended after the last unit so each unit's
\ code is the same armed or not. Answers where it starts.
: CB-STUB ( -- n )
   OUT-OFF {: at:n :}
   NCBSTUB:SLOT@ CB-ID OUT @ LIT-IN
   NCBSTUB:THUNK@ {: thunk:n :}
   OUT-OFF NEMIT:ADDR-CODE thunk -1 EMIT-ADDR-ROW+
   TMP 0 ENC-ADRP OUT @ W32+  TMP TMP 0 ENC-ADDI OUT @ W32+
   TMP ENC-BR OUT @ W32+
   at ;

\ Every call row's `bl` or `b`, and every code address row's `adrp` and `add`
\ in the site's register, from its site placed at place to its target.
: PATCH ( n -- )
   {: place:n :}
   EMIT-CALL-ROWS 0 ?do
      i EMIT-CALL-ROW {: at:n kind:n to:n :}
      to place at + -  kind NEMIT:CALL = if ENC-BL else ENC-B then  at W32!
   loop
   EMIT-ADDR-ROWS 0 ?do
      i EMIT-ADDR-ROW {: at:n kind:n to:n :}
      kind NEMIT:ADDR-CODE = if
         OUT @ at VEC:AT c@ 31 and  to  place at +  ENC-ADRP-ADD
         at INSN + W32!  at W32!
      then
   loop ;

\ ---- values ------------------------------------------------------------------
: LIT ( n vreg -- ) REG LIT-R ;

: ADDR-DATA ( n n vreg -- )
   {: v:n o:n d:vreg :}
   HERE-OFF NEMIT:ADDR-DATA v o PEND+
   v d REG DATA-CHAIN ;

: ADDR-CODE ( n vreg -- )
   {: v:n d:vreg :}
   HERE-OFF NEMIT:ADDR-CODE v -1 PEND+
   d REG 0 ENC-ADRP W  d REG d REG 0 ENC-ADDI W ;

\ `adr rd, 0` until ADR-PUT writes the distance to unit n.
: ADDR-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   HERE-OFF ADDR-QUOT-KIND n -1 PEND+
   d REG 0 ENC-ADR W ;

: DSP+ ( n -- ) {: k:n :}  DSP DSP k CELL * ADD-IMM ;

\ A push moves at most the virtual stack's 15 cells, within the offset's
\ reach.
: PUSH-R ( n n -- ) {: r:n k:n :}  r DSP k CELL * ENC-STRPOST W ;

: PUSH ( vreg n -- ) {: s:vreg k:n :}  s REG k PUSH-R ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v TMP LIT-R
   TMP k PUSH-R ;

\ The stack pointer moved down k cells, then the cell there loaded: one
\ pre-index `ldr` while the move fits its offset.
: POP ( vreg n -- ) {: d:vreg k:n :}
   k CELL * negate {: off:n :}
   off SIMM9? if d REG DSP off ENC-LDRPRE W exit then
   k negate DSP+
   d REG DSP 0 false ACCESS ;

: SPILL ( vreg n -- )
   {: s:vreg k:n :}
   s REG DSP k CELL * true XFER ;

\ The address first, since its fallback uses x16 too, then the value.
: SPILL-LIT ( n n -- ) {: v:n k:n :}
   DSP k CELL * ADDRESS {: b:n o:n :}
   v TMP LIT-R
   TMP b o true ACCESS ;

: FILL ( vreg n -- )
   {: d:vreg k:n :}
   d REG DSP k CELL * false XFER ;

: MOV ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG ENC-MOV W ;

: BASE ( vreg -- ) REG XREG-RBASE ENC-MOV W ;

: EXCHANGE ( vreg vreg -- )
   {: a:vreg b:vreg :}
   TMP a REG ENC-MOV W
   a REG b REG ENC-MOV W
   b REG TMP ENC-MOV W ;

: SLOT-STORE ( vreg n -- )
   {: s:vreg k:n :}
   s REG SP k SLOT-OFF true XFER ;

: SLOT-LOAD ( vreg n -- )
   {: d:vreg k:n :}
   d REG SP k SLOT-OFF false XFER ;

: ENV-OPEN ( -- )
   BODY-EMPTY
   1 ENV !
   REC-ARG SP 0 ENC-STR W ;

: REC-STORE ( vreg n -- )
   {: s:vreg n:n :}
   s REG SP n REC-OFF true XFER ;

: REC-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   d REG {: r:n :}
   r SP n REC-OFF ADD-IMM
   r r $8000000000000000 >LIMM ENC-ORRI W ;

: ENV-LOAD ( vreg n -- )
   {: d:vreg n:n :}
   ENV @ 0= if E-T0-UNIT throw then
   d REG {: r:n :}
   r SP 0 false ACCESS
   r r n CELL * false XFER ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( vreg vreg alu -- )
   {: d:vreg s:vreg o:alu :}
   d REG d REG s REG o ALU>N ALU-RR @ execute W ;

: OP-LIT ( vreg n alu -- )
   {: d:vreg v:n o:alu :}
   d REG {: r:n :}  o ALU>N {: k:n :}
   k OP-ADD ALU>N = if r r v ADD-IMM exit then
   k OP-SUB ALU>N = if r r v negate ADD-IMM exit then
   v LIMM? if
      r r v >LIMM  k OP-AND ALU>N - ALU-LOGI @ execute W exit
   then
   v TMP LIT-R
   r r TMP k ALU-RR @ execute W ;

\ The hardware masks a register count to six bits; SHIFT-LIT masks its own.
: SHIFT ( vreg vreg shifter -- )
   {: d:vreg s:vreg o:shifter :}
   d REG d REG s REG o SHIFTER>N SH-RR @ execute W ;

: SHIFT-LIT ( vreg n shifter -- )
   {: d:vreg v:n o:shifter :}
   d REG d REG v 63 and o SHIFTER>N SH-RI @ execute W ;

: MULX ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG d REG s REG ENC-MUL W ;

\ arm64 multiplies registers only.
: MUL-LIT ( vreg n -- )
   {: d:vreg v:n :}
   v TMP LIT-R
   d REG d REG TMP ENC-MUL W ;

\ rd becomes rn, plus 2^k - 1 when rn is negative, so that dropping its low k
\ bits rounds toward zero; k is 1 to 62.
: ROUNDED ( n n n -- )
   {: rd:n rn:n k:n :}
   k 1 = if rd rn rn 63 ENC-ADD-LSR W exit then
   TMP rn 63 ENC-ASRI W
   rd rn TMP 64 k - ENC-ADD-LSR W ;

\ r less r rounded toward zero to a multiple of 2^k.
: MOD-POW2 ( n n -- )
   {: r:n k:n :}
   k 0= if 0 r LIT-R exit then
   ADR r k ROUNDED
   ADR ADR 1 k lshift negate >LIMM ENC-ANDI W
   r r ADR ENC-SUB W ;

: DIV-POW2 ( n n -- )
   {: r:n k:n :}
   k 0= if exit then
   r r k ROUNDED
   r r k ENC-ASRI W ;

\ `sdiv` appears only here and in MOD-LIT, whose divisor is never 0 or -1:
\ the hardware answers 0 for a zero divisor.
: DIV-LIT ( vreg n -- )
   {: d:vreg v:n :}
   v POW2-EXP {: k:n :}
   k 0 >= if d REG k DIV-POW2 exit then
   v TMP LIT-R
   d REG d REG TMP ENC-SDIV W ;

: MOD-LIT ( vreg n -- )
   {: d:vreg v:n :}
   d REG {: r:n :}
   v POW2-EXP {: k:n :}
   k 0 >= if r k MOD-POW2 exit then
   v TMP LIT-R
   ADR r TMP ENC-SDIV W
   r ADR TMP r ENC-MSUB W ;

: NEG ( vreg -- ) REG dup ENC-NEG W ;
: NOT ( vreg -- ) REG dup ENC-MVN W ;

: CMP ( vreg vreg -- )
   {: a:vreg b:vreg :}
   a REG b REG ENC-CMP W ;

: CMP-LIT ( vreg n -- )
   {: a:vreg v:n :}
   a REG v negate {: r:n nv:n :}
   v 0 >= v IMM12-LIM < and if r v ENC-CMPI W exit then
   nv 0 > nv IMM12-LIM < and if r nv ENC-CMNI W exit then
   v TMP LIT-R
   r TMP ENC-CMP W ;

\ csetm makes the full-cell flag of the condition.
: FLAG-CC ( vreg cc -- )
   {: d:vreg c:cc :}
   d REG c CC>N CONDS @ ENC-CSETM W ;

: FLAG ( vreg vreg vreg cc -- )
   {: d:vreg a:vreg b:vreg c:cc :}
   a b CMP  d c FLAG-CC ;

\ A cell's sign fills the flag of < 0, and inverted that of >= 0.
: FLAG-LIT ( vreg vreg n cc -- )
   {: d:vreg a:vreg v:n c:cc :}
   v 0= if
      c CC>N {: k:n :}
      k CC-LT CC>N = if d REG a REG 63 ENC-ASRI W exit then
      k CC-GE CC>N = if d REG a REG 63 ENC-MVN-ASR W exit then
   then
   a v CMP-LIT  d c FLAG-CC ;

\ Every literal is admitted: OP-LIT, MUL-LIT, CMP-LIT, FLAG-LIT and OF-LIT load
\ one no immediate field holds into x16.
: IMM? ( n -- bool ) drop true ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( vreg vreg width -- )
   {: d:vreg a:vreg w:width :}
   w WIDTH>N {: n:n :}
   n 1 = if d REG a REG 0 ENC-LDRB W exit then
   n 2 = if d REG a REG 0 ENC-LDRH W exit then
   n 4 = if d REG a REG 0 ENC-LDRW W exit then
   d REG a REG 0 ENC-LDR W ;

: STORE ( vreg vreg width -- )
   {: s:vreg a:vreg w:width :}
   w WIDTH>N {: n:n :}
   n 1 = if s REG a REG 0 ENC-STRB W exit then
   n 2 = if s REG a REG 0 ENC-STRH W exit then
   n 4 = if s REG a REG 0 ENC-STRW W exit then
   s REG a REG 0 ENC-STR W ;

\ ---- the return stack --------------------------------------------------------
\ RSP-CELL cells deep from STACK-ABI:RETURN-BASE-CELL; slot d is base + 8d.
\ Vreg i of p moved to or from the slot at x17 + off.
: RS-XFER ( ^vreg n n bool -- ) {: p:^ i:n off:n st:bool :}
   p i VREG-AT REG ADR off st ACCESS ;

: TOR ( ^vreg n -- ) {: p:^ n:n :}
   RDEPTH RSLOT
   n 0 ?do p i i CELL * true RS-XFER loop
   TMP TMP n ENC-ADDI W
   RDEPTH! ;

: RFROM ( ^vreg n -- ) {: p:^ n:n :}
   RDEPTH
   TMP TMP n ENC-SUBI W
   RDEPTH! RSLOT
   n 0 ?do p i i CELL * false RS-XFER loop ;

\ Unscaled offsets under x17, the slot past the top, while the lowest fits nine
\ bits; otherwise x17 moves down to the lowest slot first.
: RFETCH ( ^vreg n n -- ) {: p:^ n:n k:n :}
   RDEPTH RSLOT
   n k + CELL * negate {: lo:n :}
   lo SIMM9? if lo else ADR ADR lo ADD-IMM 0 then {: at:n :}
   n 0 ?do p i at i CELL * + false RS-XFER loop ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- )
   {: to:n loc:n :}
   1 LINKED !  0 HELD !
   HERE-OFF NEMIT:CALL to loc PCALL+
   0 ENC-BL W ;


: TAIL ( n n -- )
   {: to:n loc:n :}
   EPI+
   HERE-OFF NEMIT:TAIL to loc PCALL+
   0 ENC-B W ;


: CALL-SELF ( -- )
   1 LINKED !  0 HELD !
   HERE-OFF SELF+
   0 ENC-BL W ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives LABEL emits through. A forward branch is its number
\ in the open unit's list, and a head a raw offset.

\ A branch of the form, with its condition or register, to a raw offset or -1.
: BRANCH ( n n n -- LABEL:branch ) INSN BR+ ;

: RESOLVE ( LABEL:branch -- ) PLACE:RESOLVE  0 HELD ! ;

: MARK ( -- LABEL:head ) 0 HELD !  PLACE:MARK ;

: FWD-JUMP ( -- LABEL:branch ) FORM-B 0 -1 BRANCH ;
: FWD-BCOND ( n -- LABEL:branch ) FORM-BCOND swap -1 BRANCH ;
: FWD-ZERO ( vreg -- LABEL:branch ) REG FORM-CBZ swap -1 BRANCH ;

: FWD-NE ( vreg vreg -- LABEL:branch )
   {: sel:vreg v:vreg :}
   sel REG v REG ENC-CMP W
   C-NE FWD-BCOND ;

: FWD-NE-LIT ( vreg n -- LABEL:branch )
   {: sel:vreg v:n :}
   sel v CMP-LIT
   C-NE FWD-BCOND ;

: FWD-GE ( vreg vreg -- LABEL:branch )
   {: s:vreg l:vreg :}
   s REG l REG ENC-CMP W
   C-GE FWD-BCOND ;

: FWD-CC ( cc -- LABEL:branch ) CC>N CONDS @ FWD-BCOND ;

\ The b.cond branch becomes b.eq.
: ENTRY-EQ ( LABEL:branch -- ) LABEL:BRANCH>N {: k:n :}  C-EQ k BR-ARG ! ;

: BACK-JUMP ( LABEL:head -- ) LABEL:HEAD>N FORM-B 0 rot BRANCH drop ;

: BACK-BCOND ( n LABEL:head -- )
   LABEL:HEAD>N {: c:n m:n :}
   FORM-BCOND c m BRANCH drop ;

: BACK-ZERO ( vreg LABEL:head -- )
   LABEL:HEAD>N {: c:vreg m:n :}
   FORM-CBZ c REG m BRANCH drop ;

: BACK-CC ( cc LABEL:head -- )
   {: c:cc m:LABEL:head :}
   c CC>N CONDS @ m BACK-BCOND ;

\ ---- loops -------------------------------------------------------------------
\ x17 holds the address one frame past the innermost.
: LTOP ( -- ) ADR XREG-RBASE LOOPSP-CELL ENC-LDR W ;
: LTOP! ( -- ) ADR XREG-RBASE LOOPSP-CELL ENC-STR W ;

\ The index and limit of the innermost frame, below LTOP's address.
-16 constant LIDX-OFF
-8 constant LLIM-OFF

: LOOP-J ( vreg -- )
   {: d:vreg :}
   LTOP
   d REG ADR LIDX-OFF STACK-ABI:LOOP-FRAME-BYTES - false XFER ;

\ x17 the address past the innermost frame, and x29 its index unless HELD
\ says it holds it already.
: FRAME-IDX ( -- )
   LTOP
   HELD @ if exit then
   IDX ADR LIDX-OFF false XFER
   1 HELD ! ;

: LOOP-PUSH ( vreg vreg -- )
   {: s:vreg l:vreg :}
   LTOP
   ADR ADR STACK-ABI:LOOP-FRAME-BYTES ENC-ADDI W
   LTOP!
   s REG l REG ADR LIDX-OFF ENC-STP W
   IDX s REG ENC-MOV W ;

\ The head binds with x29 holding the index: the entry arrives from LOOP-PUSH
\ and every back edge from LOOP-NEXT or LOOP-STEP.
: LOOP-OPEN ( vreg vreg bool -- )
   LABEL:LOOP-OPEN  1 HELD ! ;

: LOOP-POP ( -- )
   LTOP
   ADR ADR STACK-ABI:LOOP-FRAME-BYTES ENC-SUBI W
   LTOP!
   0 HELD ! ;

: LOOP-I ( vreg -- )
   HELD @ 0= if FRAME-IDX then
   REG IDX ENC-MOV W ;

\ The step is x29's, so this target leaves the free vreg alone.
: LOOP-NEXT ( vreg LABEL:head -- )
   nip {: m:LABEL:head :}
   FRAME-IDX
   IDX IDX 1 ENC-ADDI W
   IDX ADR LIDX-OFF true XFER
   TMP ADR LLIM-OFF false XFER
   IDX TMP ENC-CMP W
   C-LT m BACK-BCOND ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative.
: LOOP-STEP ( vreg vreg LABEL:head -- )
   {: s:vreg t:vreg m:LABEL:head :}
   s REG {: sr:n :}  t REG {: r:n :}
   FRAME-IDX
   TMP ADR LLIM-OFF false XFER
   r IDX TMP ENC-SUB W
   IDX IDX sr ENC-ADD W
   IDX ADR LIDX-OFF true XFER
   TMP r sr ENC-ADD W
   TMP TMP r ENC-EOR W
   sr sr r ENC-EOR W
   TMP TMP sr ENC-AND W
   TMP 0 ENC-CMPI W
   C-GE m BACK-BCOND ;

\ ---- the seam ----------------------------------------------------------------
: FILL-SLOT ( n -- )
   {: k:n :}
   NVREG k EMIT-VREGS-N !
   ['] PLACE:OPEN k EMIT-OPEN-XT !
   ['] PLACE:QUOT-OPEN k EMIT-QUOT-OPEN-XT !
   ['] PLACE:UNIT-OPEN k EMIT-UNIT-OPEN-XT !
   ['] RETURN k EMIT-RETURN-XT !
   ['] PLACE:CLOSE k EMIT-CLOSE-XT !
   ['] SLOT-CLOSE k EMIT-SLOT-CLOSE-XT !
   ['] CB-STUB k EMIT-CB-STUB-XT !
   ['] PLACE:BYTES k EMIT-BYTES-XT !
   ['] PATCH k EMIT-PATCH-XT !
   ['] LIT k EMIT-LIT-XT !
   ['] ADDR-DATA k EMIT-ADDR-DATA-XT !
   ['] ADDR-CODE k EMIT-ADDR-CODE-XT !
   ['] ADDR-QUOT k EMIT-ADDR-QUOT-XT !
   ['] PUSH k EMIT-PUSH-XT !
   ['] PUSH-LIT k EMIT-PUSH-LIT-XT !
   ['] POP k EMIT-POP-XT !
   ['] SPILL k EMIT-SPILL-XT !
   ['] SPILL-LIT k EMIT-SPILL-LIT-XT !
   ['] FILL k EMIT-FILL-XT !
   ['] DSP+ k EMIT-DSP+-XT !
   ['] MOV k EMIT-MOV-XT !
   ['] BASE k EMIT-BASE-XT !
   ['] EXCHANGE k EMIT-EXCHANGE-XT !
   ['] PLACE:SLOTS k EMIT-SLOTS-XT !
   ['] SLOT-STORE k EMIT-SLOT-STORE-XT !
   ['] SLOT-LOAD k EMIT-SLOT-LOAD-XT !
   ['] PLACE:REC-CELLS! k EMIT-REC-CELLS-XT !
   ['] ENV-OPEN k EMIT-ENV-OPEN-XT !
   ['] REC-STORE k EMIT-REC-STORE-XT !
   ['] REC-QUOT k EMIT-REC-QUOT-XT !
   ['] ENV-LOAD k EMIT-ENV-LOAD-XT !
   ['] OP k EMIT-OP-XT !
   ['] OP-LIT k EMIT-OP-LIT-XT !
   ['] SHIFT k EMIT-SHIFT-XT !
   ['] SHIFT-LIT k EMIT-SHIFT-LIT-XT !
   ['] MULX k EMIT-MULX-XT !
   ['] MUL-LIT k EMIT-MUL-LIT-XT !
   ['] DIV-LIT k EMIT-DIV-LIT-XT !
   ['] MOD-LIT k EMIT-MOD-LIT-XT !
   ['] NEG k EMIT-NEG-XT !
   ['] NOT k EMIT-NOT-XT !
   ['] CMP k EMIT-CMP-XT !
   ['] CMP-LIT k EMIT-CMP-LIT-XT !
   ['] FLAG k EMIT-FLAG-XT !
   ['] FLAG-LIT k EMIT-FLAG-LIT-XT !
   ['] LOAD k EMIT-LOAD-XT !
   ['] STORE k EMIT-STORE-XT !
   ['] TOR k EMIT-TOR-XT !
   ['] RFROM k EMIT-RFROM-XT !
   ['] RFETCH k EMIT-RFETCH-XT !
   ['] CALL k EMIT-CALL-XT !
   ['] TAIL k EMIT-TAIL-XT !
   ['] CALL-SELF k EMIT-CALL-SELF-XT !
   ['] LABEL:IF, k EMIT-IF,-XT !
   ['] LABEL:ELSE, k EMIT-ELSE,-XT !
   ['] LABEL:THEN, k EMIT-THEN,-XT !
   ['] LABEL:BEGIN, k EMIT-BEGIN,-XT !
   ['] LABEL:WHILE, k EMIT-WHILE,-XT !
   ['] LABEL:REPEAT, k EMIT-REPEAT,-XT !
   ['] LABEL:UNTIL, k EMIT-UNTIL,-XT !
   ['] LABEL:IF-CC, k EMIT-IF-CC,-XT !
   ['] LABEL:WHILE-CC, k EMIT-WHILE-CC,-XT !
   ['] LABEL:UNTIL-CC, k EMIT-UNTIL-CC,-XT !
   ['] LABEL:AGAIN, k EMIT-AGAIN,-XT !
   ['] LABEL:CASE, k EMIT-CASE,-XT !
   ['] LABEL:OF, k EMIT-OF,-XT !
   ['] LABEL:OF-LIT k EMIT-OF-LIT-XT !
   ['] LABEL:ENDOF, k EMIT-ENDOF,-XT !
   ['] LABEL:ENDCASE, k EMIT-ENDCASE,-XT !
   ['] LOOP-PUSH k EMIT-LOOP-PUSH-XT !
   ['] LOOP-POP k EMIT-LOOP-POP-XT !
   ['] LOOP-I k EMIT-LOOP-I-XT !
   ['] LOOP-J k EMIT-LOOP-J-XT !
   ['] LOOP-OPEN k EMIT-LOOP-OPEN-XT !
   ['] LABEL:LEAVE, k EMIT-LEAVE,-XT !
   ['] LABEL:LOOP-NEXT k EMIT-LOOP-NEXT-XT !
   ['] LABEL:LOOP-STEP k EMIT-LOOP-STEP-XT !
   ['] LABEL:LOOP-CLOSE k EMIT-LOOP-CLOSE-XT !
   ['] IMM? k EMIT-IMM?-XT !
   ['] MARK k LABEL:MARK-XT !
   ['] FWD-JUMP k LABEL:FWD-JUMP-XT !
   ['] FWD-ZERO k LABEL:FWD-ZERO-XT !
   ['] FWD-NE k LABEL:FWD-NE-XT !
   ['] FWD-NE-LIT k LABEL:FWD-NE-LIT-XT !
   ['] FWD-GE k LABEL:FWD-GE-XT !
   ['] FWD-CC k LABEL:FWD-CC-XT !
   ['] BACK-JUMP k LABEL:BACK-JUMP-XT !
   ['] BACK-ZERO k LABEL:BACK-ZERO-XT !
   ['] BACK-CC k LABEL:BACK-CC-XT !
   ['] RESOLVE k LABEL:RESOLVE-XT !
   ['] ENTRY-EQ k LABEL:ENTRY-EQ-XT !
   ['] LOOP-NEXT k LABEL:LOOP-NEXT-XT !
   ['] LOOP-STEP k LABEL:LOOP-STEP-XT !
   ['] UNIT-RESET k PLACE:UNIT-RESET-XT !
   ['] PROLOGUE k PLACE:PROLOGUE-XT !
   ['] EPILOGUE k PLACE:EPILOGUE-XT !
   ['] SELF-PUT k PLACE:SELF-PUT-XT !
   ['] ADR-PUT k PLACE:ADR-PUT-XT !
   ['] BRANCH-LEN k PLACE:BRANCH-LEN-XT !
   ['] BRANCH-PUT k PLACE:BRANCH-PUT-XT ! ;

CTARGET-ARCH:AARCH64 ' FILL-SLOT EMIT-PROVIDE

;using
;using
;package
;package
