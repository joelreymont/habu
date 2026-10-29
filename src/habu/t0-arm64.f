\ t0-arm64.f - the arm64 emitter of the tier 0 compiler, package T0-A64.
\
\ It implements the operations of src/habu/t0-emit.f (package T0-EMIT), which
\ states what each operation does on every target; this file states only the
\ arm64 encodings and register choices. Every word here is private, and the
\ file ends by providing INSTALL, which stores each into its T0-EMIT cell, its
\ branch primitives into their src/habu/t0-label.f (package T0-LABEL) cells and
\ T0-LABEL's control words into T0-EMIT's, for CTARGET-ARCH:AARCH64.
\
\ THE CONTRACT A COMPILED BODY KEEPS. x19 is the data stack, which grows up: a
\ push is `str xR,[x19],#8` and a pop `ldr xR,[x19,#-8]!` (src/habu/jit.f
\ W-PUSHR, W-POPR). x20 is DATA and x19, x20, x26, x27 and x28 are the engine's
\ (src/habu/layout.f ENGINE-GPR:A64-MASK); compiled code never writes them
\ except x19 as the stack. x18 is Darwin's and never named (src/arch/arm64/asm.f
\ XREG? refuses it). x29 caches the innermost loop's index, as it does for the
\ JIT (src/habu/regalloc.f LIDX), and any callee may write it. The vregs are
\ the JIT's register pool in its order, x9..x15 then x25 x23 x24 x21 x22 x6 x7
\ x8 (src/habu/regalloc.f VRPACK, VRPACK2), the registers a body may clobber.
\ x16 and x17 are this file's own temporaries, a literal and an address, and
\ hold no value between two of its words (the JIT never pools them either).
\ A body is entered by `bl` or `blr` and leaves by `ret` through x30.
\
\ DIVISION. The seam has no divide (t0-emit.f DIVISION), so `sdiv` never
\ appears here and a zero divisor never reaches the hardware, which would
\ answer 0 instead of throwing.
\
\ THE SINK. A unit's body is emitted into RAW, at unit-relative offsets, and
\ CLOSE copies it into OUT, the whole emission BYTES reads, with the unit's
\ prologue in front and its epilogue before every `ret`. Each byte-buffer is
\ one src/lib/byte-buffer.f header; a word is appended little-endian.
\
\ THE FRAME is fixed at CLOSE, once the unit's size is known, so the body holds
\ no frame size to patch. Its slots are FRAME-CELLS cells rounded to 16 bytes,
\ slot k at sp + 8k, reserved by `sub sp,sp,#S` and given back by
\ `add sp,sp,#S` (in 4095-page chunks, then the remainder: a frame has no cap).
\ A unit with no slots and no call is a leaf and has neither. A unit with a
\ CALL or CALL-SELF (LINKED) also saves x30 above its slots:
\ `str x30,[sp,#-16]!` first and `ldr x30,[sp],#16` last, so sp keeps its
\ 16-byte alignment at every call; a TAIL leaves with x30 as the unit found it.
\ RETURN records the offset of its `ret` in EPI-AT and TAIL that of its `b`;
\ CLOSE inserts the epilogue there and moves every row of the unit by the
\ prologue and by the epilogues inserted at or before it.
\
\ THE BRANCHES. A control branch is a placeholder word in RAW and an entry of
\ the open unit's branch list: its raw site, its raw target once T0-LABEL
\ resolves it (a backward branch's at once), and its form, `b`, `b.cond` with
\ its condition or `cbz` with its register. CLOSE writes each, since the
\ epilogues it inserts move code: the site placed as a row is, and the target
\ counting only the epilogues before it, so a branch to a `ret` or tail `b`
\ lands on the epilogue in front of it. A branch never leaves its unit, so
\ placement leaves it alone; one its field cannot reach throws E-A64EMIT-REACH.
\
\ THE ROWS. A unit's address sites wait in PEND and its call and tail sites in
\ PCALL, at raw offsets, until CLOSE has laid the unit out, and CLOSE files them
\ into the seam (t0-emit.f THE ROWS) at their final offsets. A call is `bl 0`
\ and a tail `b 0` until PATCH writes the displacement. A data address is the
\ chain the JIT emits for one, recorded at its first word: inside the fixed
\ DATA mapping three words, `movz lsl32`, `movk lsl16`, `movk` (habu2.f
\ C-DATA-RAW, the shape src/habu/address-carrier.f CHAIN-SIZE reads back), and
\ elsewhere the four-lane `movz`, `movk lsl16`, `movk lsl32`, `movk lsl48`
\ (habu2.f C-ADDR-RAW), every lane emitted so the publisher can rewrite it in
\ place; a code address always takes the four lanes. A data address's row
\ carries the origin ADDR-DATA takes, a code address's -1. A quotation's
\ address is an `adr` relative to its own site, which QUOT-OPEN writes once the unit
\ begins, so it files no row and placement leaves it alone. `recurse` is a `bl`
\ to byte 0, which CLOSE writes, and files no row either.
\
\ THE PATCH SLOT. SLOT-CLOSE ends the unit with RETURN and CLOSE, which put
\ any epilogue in front of the `ret`, so the `ret` is the unit's last word.
\
\ THE CALLBACK STUB is tier 1's (src/compiler/native/emit.f WRITE-STUB):
\ x9 <- NCBSTUB's slot and x16 <- its thunk, each by the four-lane carrier,
\ the thunk's one ADDR-CODE row, then `br x16`. CB-STUB appends it to OUT after
\ the last unit is closed, so each unit's code is the same armed or not.
\
\ THE RETURN STACK is RSP-CELL cells deep from the base at
\ STACK-ABI:RETURN-BASE-CELL; slot d is base + 8d, as habu2.f J-TOR and J-RPOP
\ reach it.
\
\ THE INDEX CACHE. The frame cell stays the index's home, since `j` and the
\ runtime read it, and x29 is a copy written beside every store of it, as
\ habu2.f J-FRAME keeps it for the JIT. HELD says whether the code emitted
\ next may read x29: LOOP-PUSH and a loop head set it, and so do `i` and a
\ step once they load it. Every call clears it, since the callee may run a
\ loop, and so does every label a branch lands on, MARK and RESOLVE, since a
\ branch may come from code that wrote x29; LOOP-POP and the start of a unit
\ clear it too. While it is clear, `i`, `loop` and `+loop` load x29 from the
\ frame first. A refused token discards the whole emission, and OPEN begins
\ the next one clear.

require src/lib/errors.f
require src/lib/byte-buffer.f
require src/lib/le.f
require src/lib/span.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/arm64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f
require src/habu/t0-label.f

package T0-A64
using A64ASM

15 constant NVREG
16 constant TMP                       \ x16, a literal
17 constant ADR                       \ x17, an address
30 constant LR
29 constant IDX                       \ x29, the innermost index's cache
9 constant CB-ID                      \ x9, the stub's slot for its thunk
4 constant INSN                       \ the bytes of one instruction word
31 constant SP                        \ as a base or an add/sub operand

\ The machine register of each vreg; an index past it throws E-LAYOUT-BOUNDS.
NVREG TYPED-BUFFER REGS n
 9 0 REGS !  10 1 REGS !  11 2 REGS !  12 3 REGS !  13 4 REGS !
14 5 REGS !  15 6 REGS !  25 7 REGS !  23 8 REGS !  24 9 REGS !
21 10 REGS !  22 11 REGS !  6 12 REGS !  7 13 REGS !  8 14 REGS !

: REG ( T0-EMIT:vreg -- n ) T0-EMIT:VREG>N REGS @ ;

\ The arm64 condition of each T0-EMIT comparison.
6 TYPED-BUFFER CONDS n
C-EQ T0-EMIT:CC-EQ T0-EMIT:CC>N CONDS !
C-NE T0-EMIT:CC-NE T0-EMIT:CC>N CONDS !
C-LT T0-EMIT:CC-LT T0-EMIT:CC>N CONDS !
C-GE T0-EMIT:CC-GE T0-EMIT:CC>N CONDS !
C-LE T0-EMIT:CC-LE T0-EMIT:CC>N CONDS !
C-GT T0-EMIT:CC-GT T0-EMIT:CC>N CONDS !

\ The register form of each ALU operation, and the bitmask-immediate form of
\ the logical ones, OP-AND first.
5 TYPED-BUFFER ALU-RR [ n n n -- n ]
' ENC-ADD T0-EMIT:OP-ADD T0-EMIT:ALU>N ALU-RR !
' ENC-SUB T0-EMIT:OP-SUB T0-EMIT:ALU>N ALU-RR !
' ENC-AND T0-EMIT:OP-AND T0-EMIT:ALU>N ALU-RR !
' ENC-ORR T0-EMIT:OP-OR T0-EMIT:ALU>N ALU-RR !
' ENC-EOR T0-EMIT:OP-XOR T0-EMIT:ALU>N ALU-RR !

3 TYPED-BUFFER ALU-LOGI [ n n n -- n ]
' ENC-ANDI 0 ALU-LOGI !
' ENC-ORRI 1 ALU-LOGI !
' ENC-EORI 2 ALU-LOGI !

\ Each shifter by a register and by a count.
2 TYPED-BUFFER SH-RR [ n n n -- n ]
' ENC-LSLV T0-EMIT:SH-LEFT T0-EMIT:SHIFTER>N SH-RR !
' ENC-LSRV T0-EMIT:SH-RIGHT T0-EMIT:SHIFTER>N SH-RR !

2 TYPED-BUFFER SH-RI [ n n n -- n ]
' ENC-LSLI T0-EMIT:SH-LEFT T0-EMIT:SHIFTER>N SH-RI !
' ENC-LSRI T0-EMIT:SH-RIGHT T0-EMIT:SHIFTER>N SH-RI !

create RAW BUF:HDR-BYTES allot        \ the open unit's body
create OUT BUF:HDR-BYTES allot        \ the closed units, BYTES
create EPI BUF:HDR-BYTES allot        \ the open unit's epilogue, built at CLOSE
variable LIVE                         \ nonzero once the buffers hold storage
variable FRAME-CELLS                  \ machine-stack cells this unit uses
variable LINKED                       \ nonzero once this unit calls
variable HELD                         \ nonzero while x29 holds the index
variable NEPI
variable NPEND
variable NUNIT                        \ units begun in this emission
\ The open unit's `ret` and tail `b` offsets, ascending.
DYNAMIC-BUFFER EPI-AT n
DYNAMIC-BUFFER PEND-AT n              \ the open unit's address rows: raw first
DYNAMIC-BUFFER PEND-KIND n            \ word, kind, target, origin
DYNAMIC-BUFFER PEND-TO n
DYNAMIC-BUFFER PEND-ORIGIN n
variable NPCALL
DYNAMIC-BUFFER PCALL-AT n             \ the open unit's call and tail rows: raw
DYNAMIC-BUFFER PCALL-KIND n           \ first word, kind, target, loc
DYNAMIC-BUFFER PCALL-TO n
DYNAMIC-BUFFER PCALL-LOC n
variable NSELF
DYNAMIC-BUFFER SELF-AT n              \ the open unit's `recurse` sites, raw
DYNAMIC-BUFFER QUOT-SITE n            \ the `adr` in OUT that names each unit
variable NBR
DYNAMIC-BUFFER BR-AT n                \ the open unit's branches: raw site,
DYNAMIC-BUFFER BR-TO n                \ raw target or -1 while open, form,
DYNAMIC-BUFFER BR-FORM n              \ and the form's condition or register
DYNAMIC-BUFFER BR-ARG n

\ The forms of a branch.
0 constant FORM-B
1 constant FORM-BCOND
2 constant FORM-CBZ

4096 constant SINK-START-BYTES
4095 constant ADJ-MAX                 \ the widest add/sub immediate
26 constant B-BITS                    \ a b or bl field, in instructions
19 constant COND-BITS                 \ a b.cond or cbz field, in instructions
21 constant ADR-BITS                  \ an adr field, in bytes

: HERE-OFF ( -- n ) RAW BUF:LEN@ BUF:BLEN>N ;
: OUT-OFF ( -- n ) OUT BUF:LEN@ BUF:BLEN>N ;

\ Append one instruction word to buf, little-endian: one growth for its bytes.
: W32+ ( n ptr a -- ) {: w:n buf:ptr :}
   w  buf INSN BUF:N>BLEN BUF:GROW SPAN:$ drop  LE:U32! ;

: W ( n -- ) RAW W32+ ;

\ Overwrite the instruction word at byte at of OUT.
: W32! ( n n -- ) {: w:n at:n :}
   w  OUT BUF:SPAN$ drop at +  LE:U32! ;

\ Whether d fits a signed field of bits bits.
: SIGNED? ( n n -- bool ) {: d:n bits:n :}
   1 bits 1 - lshift {: half:n :}
   d half negate >= d half < and ;

\ The `bl` (link) or `b` that reaches bytes away; a distance no field holds
\ throws E-A64EMIT-REACH.
: BRANCH ( n bool -- n ) {: bytes:n link:bool :}
   bytes 3 and 0<> if E-A64EMIT-REACH throw then
   bytes 4 / {: d:n :}
   d B-BITS SIGNED? 0= if E-A64EMIT-REACH throw then
   link if d ENC-BL exit then
   d ENC-B ;

: HALF ( n n -- n ) 16 * rshift $FFFF and ;

\ How many of v's four halves equal h.
: HALVES ( n n -- n ) {: v:n h:n :}
   0 4 0 ?do v i HALF h = if 1 + then loop ;

\ One move-wide lane: the chain's first is MOVZ, or MOVN of the inverted half
\ in the inverted form, and the rest are MOVK.
: LANE ( bool n n n bool -- bool ) {: first:bool h:n hw:n r:n inv:bool :}
   first 0= if r h hw MOVKHW W false exit then
   inv if r h $FFFF xor hw MOVNHW W false exit then
   r h hw MOVZHW W false ;

\ Load the constant v into r with the shortest move-wide chain (src/habu/jit.f
\ LVMOVK): the inverted form when more halves are $FFFF than are zero, and
\ every half equal to the form's fill skipped.
: LIT-R ( n n -- ) {: v:n r:n :}
   v $FFFF HALVES v 0 HALVES > {: inv:bool :}
   inv if $FFFF else 0 then {: fill:n :}
   true 4 0 ?do v i HALF fill <> if v i HALF i r inv LANE then loop
   0= if exit then
   inv if r 0 0 MOVNHW W exit then
   r 0 0 MOVZHW W ;

\ Whether v is a nonzero count of 4 KiB pages an add/sub immediate holds.
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

\ Whether an unscaled or a writeback access reaches off: a signed nine bits.
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

\ Move sp by bytes into buf: 4095-page chunks, then the remainder.
: SP-PAGES ( n bool -- n )
   if SP SP rot ENC-ADDI-LSL12 exit then
   SP SP rot ENC-SUBI-LSL12 ;

: SP-ADJ ( n bool ptr a -- ) {: bytes:n add:bool buf:ptr :}
   bytes 12 rshift
   begin dup 0 > while
      dup ADJ-MAX min dup add SP-PAGES buf W32+ -
   repeat drop
   bytes ADJ-MAX and {: lo:n :}
   lo 0= if exit then
   add if SP SP lo ENC-ADDI buf W32+ exit then
   SP SP lo ENC-SUBI buf W32+ ;

: PEND+ ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   NPEND @ {: k:n :}
   k 1 + PEND-AT-RESERVE  k 1 + PEND-KIND-RESERVE  k 1 + PEND-TO-RESERVE
   k 1 + PEND-ORIGIN-RESERVE
   at k PEND-AT !  kind k PEND-KIND !  to k PEND-TO !  origin k PEND-ORIGIN !
   k 1 + NPEND ! ;

: PCALL+ ( n n n n -- ) {: at:n kind:n to:n loc:n :}
   NPCALL @ {: k:n :}
   k 1 + PCALL-AT-RESERVE  k 1 + PCALL-KIND-RESERVE
   k 1 + PCALL-TO-RESERVE  k 1 + PCALL-LOC-RESERVE
   at k PCALL-AT !  kind k PCALL-KIND !  to k PCALL-TO !  loc k PCALL-LOC !
   k 1 + NPCALL ! ;

: SELF+ ( n -- ) {: at:n :}
   NSELF @ {: k:n :}
   k 1 + SELF-AT-RESERVE
   at k SELF-AT !
   k 1 + NSELF ! ;

\ The epilogue goes in before raw offset at.
: EPI+ ( n -- ) {: at:n :}
   NEPI @ {: k:n :}
   k 1 + EPI-AT-RESERVE
   at k EPI-AT !
   k 1 + NEPI ! ;

\ The four-lane carrier into buf: every half, low first.
: CHAIN4-IN ( n n ptr a -- ) {: v:n r:n buf:ptr :}
   r v 0 HALF 0 MOVZHW buf W32+
   4 1 ?do r v i HALF i MOVKHW buf W32+ loop ;

: CHAIN4 ( n n -- ) RAW CHAIN4-IN ;

\ Whether v is inside the fixed DATA mapping and the mapping fits 48 bits, so
\ three halves, high first, carry it.
: DATA3? ( n -- bool ) {: v:n :}
   DATA-VA VA>N {: base:n :}
   base DATA-SIZE + $FFFFFFFFFFFF > if false exit then
   v base - {: off:n :}
   off 0 >= off DATA-SIZE <= and ;

: DATA-CHAIN ( n n -- ) {: v:n r:n :}
   v DATA3? 0= if v r CHAIN4 exit then
   r v 2 HALF 2 MOVZHW W
   r v 1 HALF 1 MOVKHW W
   r v 0 HALF 0 MOVKHW W ;

\ The return stack: x16 its depth, x17 the address of slot x16.
: RDEPTH ( -- ) TMP XREG-RBASE RSP-CELL ENC-LDR W ;
: RDEPTH! ( -- ) TMP XREG-RBASE RSP-CELL ENC-STR W ;

: RSLOT ( -- )
   ADR XREG-RBASE STACK-ABI:RETURN-BASE-CELL ENC-LDR W
   ADR ADR TMP 3 ENC-ADD-LSL W ;

\ Begin the next unit, with an empty body and frame and no call.
: UNIT-BEGIN ( -- )
   RAW BUF:CLEAR
   OUT-OFF T0-EMIT:UNIT+
   NUNIT @ 1 + NUNIT !
   0 FRAME-CELLS !  0 LINKED !  0 HELD !  0 NEPI !  0 NPEND !  0 NPCALL !
   0 NSELF !  0 NBR ! ;

\ How many epilogues CLOSE inserts at or before raw offset x.
: SITES<= ( n -- n ) {: x:n :}
   0 NEPI @ 0 ?do i EPI-AT @ x <= if 1 + then loop ;

\ How many it inserts strictly before x.
: SITES< ( n -- n ) {: x:n :}
   0 NEPI @ 0 ?do i EPI-AT @ x < if 1 + then loop ;

\ Where raw offset x of the open unit lands in OUT, and where a branch to x
\ lands: on the epilogue inserted at x, if any.
: PLACED ( n n n n -- n ) {: x:n base:n p:n e:n :}
   base x + p +  x SITES<= e * + ;

: LANDED ( n n n n -- n ) {: x:n base:n p:n e:n :}
   base x + p +  x SITES< e * + ;

\ Branch k's word, for its site at and target to in OUT.
: BR-WORD ( n n n -- n ) {: k:n at:n to:n :}
   k BR-FORM @ {: form:n :}
   form FORM-B = if to at - false BRANCH exit then
   to at - 4 / {: d:n :}
   d COND-BITS SIGNED? 0= if E-A64EMIT-REACH throw then
   form FORM-BCOND = if d k BR-ARG @ ENC-BCOND exit then
   k BR-ARG @ d ENC-CBZ ;

\ Copy RAW's bytes [from, to) to OUT.
: COPY ( n n -- ) {: from:n to:n :}
   RAW BUF:SPAN$ drop from +  to from - BUF:N>BLEN  OUT BUF:APPEND-SPAN ;


\ ---- the body ----------------------------------------------------------------
: OPEN ( -- )
   LIVE @ 0= if
      RAW SINK-START-BYTES BUF:N>BLEN BUF:INIT
      OUT SINK-START-BYTES BUF:N>BLEN BUF:INIT
      EPI SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 LIVE !
   then
   OUT BUF:CLEAR
   0 NUNIT !
   T0-LABEL:RESET
   UNIT-BEGIN ;

\ Point unit n's `adr`, in a closed unit of OUT, at the offset the unit begins
\ at; the first byte holds its register.
: QUOT-OPEN ( n -- ) {: n:n :}
   n NUNIT @ <> if T0-EMIT:E-T0-UNIT throw then
   n QUOT-SITE @ {: at:n :}
   OUT-OFF at - {: d:n :}
   d ADR-BITS SIGNED? 0= if E-A64EMIT-REACH throw then
   OUT BUF:SPAN$ drop at + c@ 31 and  d ENC-ADR  at W32!
   UNIT-BEGIN ;

: UNIT-OPEN ( -- ) UNIT-BEGIN ;

: RETURN ( -- ) HERE-OFF EPI+  ENC-RET W ;

\ A quotation's site is kept for QUOT-OPEN; every other address is a row.
: FILE-ADDR ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   kind T0-EMIT:ADDR-QUOT-KIND = if
      to 1 + QUOT-SITE-RESERVE  at to QUOT-SITE ! exit
   then
   at kind to origin T0-EMIT:ADDR-ROW+ ;

: CLOSE ( -- )
   FRAME-CELLS @ CELL * 15 + 15 invert and {: bytes:n :}
   OUT-OFF {: base:n :}
   LINKED @ if LR SP -16 ENC-STRPRE OUT W32+ then
   bytes false OUT SP-ADJ
   OUT-OFF base - {: p:n :}
   EPI BUF:CLEAR
   bytes true EPI SP-ADJ
   LINKED @ if LR SP 16 ENC-LDRPOST EPI W32+ then
   EPI BUF:LEN@ BUF:BLEN>N {: e:n :}
   0 NEPI @ 0 ?do
      i EPI-AT @ COPY
      EPI BUF:SPAN$ OUT BUF:APPEND-SPAN
      i EPI-AT @
   loop
   HERE-OFF COPY
   NPEND @ 0 ?do
      i PEND-AT @ base p e PLACED  i PEND-KIND @  i PEND-TO @  i PEND-ORIGIN @
      FILE-ADDR
   loop
   NPCALL @ 0 ?do
      i PCALL-AT @ base p e PLACED  i PCALL-KIND @  i PCALL-TO @  i PCALL-LOC @
      T0-EMIT:CALL-ROW+
   loop
   NSELF @ 0 ?do
      i SELF-AT @ base p e PLACED {: at:n :}
      at negate true BRANCH  at W32!
   loop
   NBR @ 0 ?do
      i BR-AT @ base p e PLACED {: at:n :}
      i  at  i BR-TO @ base p e LANDED  BR-WORD  at W32!
   loop ;

: SLOT-CLOSE ( -- )
   RETURN CLOSE
   OUT-OFF INSN - T0-EMIT:PATCH-SLOT! ;

\ Append the callback stub after the last unit and answer where it starts.
: CB-STUB ( -- n )
   OUT-OFF {: at:n :}
   NCBSTUB:SLOT@ CB-ID OUT CHAIN4-IN
   NCBSTUB:THUNK@ {: thunk:n :}
   OUT-OFF NEMIT:ADDR-CODE thunk -1 T0-EMIT:ADDR-ROW+
   thunk TMP OUT CHAIN4-IN
   TMP ENC-BR OUT W32+
   at ;

: BYTES ( -- ptr u8 n ) OUT BUF:SPAN$ BUF:BLEN>N ;

\ Every call row's `bl` or `b`, from its site placed at place to its target.
: PATCH ( n -- ) {: place:n :}
   T0-EMIT:CALL-ROWS 0 ?do
      i T0-EMIT:CALL-ROW {: at:n kind:n to:n :}
      to place at + -  kind NEMIT:CALL =  BRANCH  at W32!
   loop ;

\ ---- values ------------------------------------------------------------------
: LIT ( n T0-EMIT:vreg -- ) REG LIT-R ;

: ADDR-DATA ( n n T0-EMIT:vreg -- )
   {: v:n o:n d:T0-EMIT:vreg :}
   HERE-OFF NEMIT:ADDR-DATA v o PEND+
   v d REG DATA-CHAIN ;

: ADDR-CODE ( n T0-EMIT:vreg -- )
   {: v:n d:T0-EMIT:vreg :}
   HERE-OFF NEMIT:ADDR-CODE v -1 PEND+
   v d REG CHAIN4 ;

\ `adr rd, 0` until QUOT-OPEN n writes the distance to the unit.
: ADDR-QUOT ( n T0-EMIT:vreg -- )
   {: n:n d:T0-EMIT:vreg :}
   HERE-OFF T0-EMIT:ADDR-QUOT-KIND n -1 PEND+
   d REG 0 ENC-ADR W ;

: DSP+ ( n -- ) {: k:n :}  DSP DSP k CELL * ADD-IMM ;

\ Register r stored at the stack pointer, which then moves up k cells: one
\ post-index `str`. A push moves at most the virtual stack's 15 cells, within
\ the offset's reach.
: PUSH-R ( n n -- ) {: r:n k:n :}  r DSP k CELL * ENC-STRPOST W ;

: PUSH ( T0-EMIT:vreg n -- ) {: s:T0-EMIT:vreg k:n :}  s REG k PUSH-R ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v TMP LIT-R
   TMP k PUSH-R ;

\ The stack pointer moved down k cells, then the cell there loaded: one
\ pre-index `ldr` while the move fits its offset.
: POP ( T0-EMIT:vreg n -- ) {: d:T0-EMIT:vreg k:n :}
   k CELL * negate {: off:n :}
   off SIMM9? if d REG DSP off ENC-LDRPRE W exit then
   k negate DSP+
   d REG DSP 0 false ACCESS ;

: SPILL ( T0-EMIT:vreg n -- )
   {: s:T0-EMIT:vreg k:n :}
   s REG DSP k CELL * true XFER ;

\ The address first, since its fallback uses x16 too, then the value.
: SPILL-LIT ( n n -- ) {: v:n k:n :}
   DSP k CELL * ADDRESS {: b:n o:n :}
   v TMP LIT-R
   TMP b o true ACCESS ;

: FILL ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG DSP k CELL * false XFER ;

: MOV ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d REG s REG ENC-MOV W ;

\ Three moves through the temporary.
: EXCHANGE ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: a:T0-EMIT:vreg b:T0-EMIT:vreg :}
   TMP a REG ENC-MOV W
   a REG b REG ENC-MOV W
   b REG TMP ENC-MOV W ;

: SLOTS ( n -- ) {: k:n :}
   k FRAME-CELLS @ > if k FRAME-CELLS ! then ;

: SLOT-STORE ( T0-EMIT:vreg n -- )
   {: s:T0-EMIT:vreg k:n :}
   s REG SP k CELL * true XFER ;

: SLOT-LOAD ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG SP k CELL * false XFER ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:alu -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg o:T0-EMIT:alu :}
   d REG d REG s REG o T0-EMIT:ALU>N ALU-RR @ execute W ;

\ ADD and SUB take any v through ADD-IMM; a logical operation takes v as a
\ bitmask immediate when it is one, else from x16.
: OP-LIT ( T0-EMIT:vreg n T0-EMIT:alu -- )
   {: d:T0-EMIT:vreg v:n o:T0-EMIT:alu :}
   d REG {: r:n :}  o T0-EMIT:ALU>N {: k:n :}
   k T0-EMIT:OP-ADD T0-EMIT:ALU>N = if r r v ADD-IMM exit then
   k T0-EMIT:OP-SUB T0-EMIT:ALU>N = if r r v negate ADD-IMM exit then
   v LIMM? if
      r r v >LIMM  k T0-EMIT:OP-AND T0-EMIT:ALU>N - ALU-LOGI @ execute W exit
   then
   v TMP LIT-R
   r r TMP k ALU-RR @ execute W ;

\ The hardware masks a register count to six bits; SHIFT-LIT masks its own.
: SHIFT ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:shifter -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg o:T0-EMIT:shifter :}
   d REG d REG s REG o T0-EMIT:SHIFTER>N SH-RR @ execute W ;

: SHIFT-LIT ( T0-EMIT:vreg n T0-EMIT:shifter -- )
   {: d:T0-EMIT:vreg v:n o:T0-EMIT:shifter :}
   d REG d REG v 63 and o T0-EMIT:SHIFTER>N SH-RI @ execute W ;

: MULX ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d REG d REG s REG ENC-MUL W ;

\ arm64 multiplies registers only.
: MUL-LIT ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg v:n :}
   v TMP LIT-R
   d REG d REG TMP ENC-MUL W ;

: NEG ( T0-EMIT:vreg -- ) REG dup ENC-NEG W ;
: NOT ( T0-EMIT:vreg -- ) REG dup ENC-MVN W ;

\ csetm makes the full-cell flag of the condition.
: SET-FLAG ( T0-EMIT:vreg T0-EMIT:cc -- )
   {: d:T0-EMIT:vreg c:T0-EMIT:cc :}
   d REG c T0-EMIT:CC>N CONDS @ ENC-CSETM W ;

: CMP ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: a:T0-EMIT:vreg b:T0-EMIT:vreg :}
   a REG b REG ENC-CMP W ;

\ Compare register r with v: cmp for an imm12, cmn for a negated one, else
\ against x16.
: CMP-RI ( n n -- ) {: r:n v:n :}
   v negate {: nv:n :}
   v 0 >= v IMM12-LIM < and if r v ENC-CMPI W exit then
   nv 0 > nv IMM12-LIM < and if r nv ENC-CMNI W exit then
   v TMP LIT-R
   r TMP ENC-CMP W ;

: CMP-LIT ( T0-EMIT:vreg n -- ) {: a:T0-EMIT:vreg v:n :} a REG v CMP-RI ;

\ Every literal is admitted: OP-LIT, MUL-LIT, CMP-LIT and OF-LIT load one
\ no immediate field holds into x16.
: IMM? ( n -- bool ) drop true ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:width -- )
   {: d:T0-EMIT:vreg a:T0-EMIT:vreg w:T0-EMIT:width :}
   w T0-EMIT:WIDTH>N 1 = if d REG a REG 0 ENC-LDRB W exit then
   d REG a REG 0 ENC-LDR W ;

\ ---- the return stack --------------------------------------------------------
: TOR ( T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg :}
   RDEPTH RSLOT
   s REG ADR 0 ENC-STR W
   TMP TMP 1 ENC-ADDI W
   RDEPTH! ;

: RFROM ( T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg :}
   RDEPTH
   TMP TMP 1 ENC-SUBI W
   RDEPTH! RSLOT
   d REG ADR 0 ENC-LDR W ;

: RFETCH ( T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg :}
   RDEPTH RSLOT
   d REG ADR CELL negate ENC-LDUR W ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- ) {: to:n loc:n :}
   1 LINKED !  0 HELD !
   HERE-OFF NEMIT:CALL to loc PCALL+
   0 true BRANCH W ;

: TAIL ( n n -- ) {: to:n loc:n :}
   HERE-OFF EPI+
   HERE-OFF NEMIT:TAIL to loc PCALL+
   0 false BRANCH W ;

: CALL-SELF ( -- )
   1 LINKED !  0 HELD !
   HERE-OFF SELF+
   0 true BRANCH W ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives T0-LABEL emits through. A forward branch is its number
\ in the open unit's list, and a head a raw offset.

\ A branch of the form, with its condition or register, to raw offset to (-1
\ until RESOLVE): it emits a zero placeholder word and answers the branch's
\ number.
: BR+ ( n n n -- T0-LABEL:branch ) {: form:n arg:n to:n :}
   NBR @ {: k:n :}
   k 1 + BR-AT-RESERVE  k 1 + BR-TO-RESERVE
   k 1 + BR-FORM-RESERVE  k 1 + BR-ARG-RESERVE
   HERE-OFF k BR-AT !  to k BR-TO !  form k BR-FORM !  arg k BR-ARG !
   k 1 + NBR !
   0 W
   k T0-LABEL:>BRANCH ;

\ Bind the forward branch to here.
: RESOLVE ( T0-LABEL:branch -- )
   HERE-OFF swap T0-LABEL:BRANCH>N BR-TO !  0 HELD ! ;

: MARK ( -- T0-LABEL:head ) 0 HELD !  HERE-OFF T0-LABEL:>HEAD ;

: FWD-JUMP ( -- T0-LABEL:branch ) FORM-B 0 -1 BR+ ;
: FWD-BCOND ( n -- T0-LABEL:branch ) FORM-BCOND swap -1 BR+ ;
: FWD-ZERO ( T0-EMIT:vreg -- T0-LABEL:branch ) REG FORM-CBZ swap -1 BR+ ;

: FWD-NE ( T0-EMIT:vreg T0-EMIT:vreg -- T0-LABEL:branch )
   {: sel:T0-EMIT:vreg v:T0-EMIT:vreg :}
   sel REG v REG ENC-CMP W
   C-NE FWD-BCOND ;

: FWD-NE-LIT ( T0-EMIT:vreg n -- T0-LABEL:branch )
   {: sel:T0-EMIT:vreg v:n :}
   sel REG v CMP-RI
   C-NE FWD-BCOND ;

: FWD-GE ( T0-EMIT:vreg T0-EMIT:vreg -- T0-LABEL:branch )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg :}
   s REG l REG ENC-CMP W
   C-GE FWD-BCOND ;

: FWD-CC ( T0-EMIT:cc -- T0-LABEL:branch ) T0-EMIT:CC>N CONDS @ FWD-BCOND ;

\ The b.cond branch becomes b.eq.
: ENTRY-EQ ( T0-LABEL:branch -- ) T0-LABEL:BRANCH>N {: k:n :}  C-EQ k BR-ARG ! ;

: BACK-JUMP ( T0-LABEL:head -- ) T0-LABEL:HEAD>N FORM-B 0 rot BR+ drop ;

: BACK-BCOND ( n T0-LABEL:head -- )
   T0-LABEL:HEAD>N {: c:n m:n :}
   FORM-BCOND c m BR+ drop ;

: BACK-ZERO ( T0-EMIT:vreg T0-LABEL:head -- )
   T0-LABEL:HEAD>N {: c:T0-EMIT:vreg m:n :}
   FORM-CBZ c REG m BR+ drop ;

: BACK-CC ( T0-EMIT:cc T0-LABEL:head -- )
   {: c:T0-EMIT:cc m:T0-LABEL:head :}
   c T0-EMIT:CC>N CONDS @ m BACK-BCOND ;

\ ---- loops -------------------------------------------------------------------
\ x16 holds the loop stack's depth and x17 the address one frame past the
\ innermost, base + 16 * depth (habu2.f J-FRAME's layout).
: LDEPTH ( -- ) TMP XREG-RBASE LOOPSP-CELL ENC-LDR W ;
: LDEPTH! ( -- ) TMP XREG-RBASE LOOPSP-CELL ENC-STR W ;

: LTOP ( -- )
   ADR XREG-RBASE STACK-ABI:LOOP-BASE-CELL ENC-LDR W
   ADR ADR TMP 4 ENC-ADD-LSL W ;

\ The index and limit of the innermost frame, below LTOP's address.
-16 constant LIDX-OFF
-8 constant LLIM-OFF

\ The index of the frame outside the innermost into d, from memory.
: LOOP-J ( T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg :}
   LDEPTH LTOP
   d REG ADR LIDX-OFF 16 - false XFER ;

\ x17 the address past the innermost frame, and x29 its index unless HELD
\ says it holds it already.
: FRAME-IDX ( -- )
   LDEPTH LTOP
   HELD @ if exit then
   IDX ADR LIDX-OFF false XFER
   1 HELD ! ;

: LOOP-PUSH ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg :}
   LDEPTH
   TMP TMP 1 ENC-ADDI W
   LDEPTH! LTOP
   s REG ADR LIDX-OFF true XFER
   l REG ADR LLIM-OFF true XFER
   IDX s REG ENC-MOV W
   1 HELD ! ;

\ The head binds with x29 holding the index: the entry arrives from LOOP-PUSH
\ and every back edge from LOOP-NEXT or LOOP-STEP.
: LOOP-OPEN ( T0-EMIT:vreg T0-EMIT:vreg bool -- )
   T0-LABEL:LOOP-OPEN  1 HELD ! ;

: LOOP-POP ( -- )
   LDEPTH
   TMP TMP 1 ENC-SUBI W
   LDEPTH!
   0 HELD ! ;

: LOOP-I ( T0-EMIT:vreg -- )
   HELD @ 0= if FRAME-IDX then
   REG IDX ENC-MOV W ;

\ The step is x29's, so this target leaves the free vreg alone.
: LOOP-NEXT ( T0-EMIT:vreg T0-LABEL:head -- )
   nip {: m:T0-LABEL:head :}
   FRAME-IDX
   IDX IDX 1 ENC-ADDI W
   IDX ADR LIDX-OFF true XFER
   TMP ADR LLIM-OFF false XFER
   IDX TMP ENC-CMP W
   C-LT m BACK-BCOND ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative.
: LOOP-STEP ( T0-EMIT:vreg T0-EMIT:vreg T0-LABEL:head -- )
   {: s:T0-EMIT:vreg t:T0-EMIT:vreg m:T0-LABEL:head :}
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
\ Make this file's words, and T0-LABEL's over its branch primitives, the ones
\ T0-EMIT runs at slot k.
: INSTALL ( n -- )
   {: k:n :}
   NVREG k T0-EMIT:VREGS-N !
   ['] OPEN k T0-EMIT:OPEN-XT !
   ['] QUOT-OPEN k T0-EMIT:QUOT-OPEN-XT !
   ['] UNIT-OPEN k T0-EMIT:UNIT-OPEN-XT !
   ['] RETURN k T0-EMIT:RETURN-XT !
   ['] CLOSE k T0-EMIT:CLOSE-XT !
   ['] SLOT-CLOSE k T0-EMIT:SLOT-CLOSE-XT !
   ['] CB-STUB k T0-EMIT:CB-STUB-XT !
   ['] BYTES k T0-EMIT:BYTES-XT !
   ['] PATCH k T0-EMIT:PATCH-XT !
   ['] LIT k T0-EMIT:LIT-XT !
   ['] ADDR-DATA k T0-EMIT:ADDR-DATA-XT !
   ['] ADDR-CODE k T0-EMIT:ADDR-CODE-XT !
   ['] ADDR-QUOT k T0-EMIT:ADDR-QUOT-XT !
   ['] PUSH k T0-EMIT:PUSH-XT !
   ['] PUSH-LIT k T0-EMIT:PUSH-LIT-XT !
   ['] POP k T0-EMIT:POP-XT !
   ['] SPILL k T0-EMIT:SPILL-XT !
   ['] SPILL-LIT k T0-EMIT:SPILL-LIT-XT !
   ['] FILL k T0-EMIT:FILL-XT !
   ['] DSP+ k T0-EMIT:DSP+-XT !
   ['] MOV k T0-EMIT:MOV-XT !
   ['] EXCHANGE k T0-EMIT:EXCHANGE-XT !
   ['] SLOTS k T0-EMIT:SLOTS-XT !
   ['] SLOT-STORE k T0-EMIT:SLOT-STORE-XT !
   ['] SLOT-LOAD k T0-EMIT:SLOT-LOAD-XT !
   ['] OP k T0-EMIT:OP-XT !
   ['] OP-LIT k T0-EMIT:OP-LIT-XT !
   ['] SHIFT k T0-EMIT:SHIFT-XT !
   ['] SHIFT-LIT k T0-EMIT:SHIFT-LIT-XT !
   ['] MULX k T0-EMIT:MULX-XT !
   ['] MUL-LIT k T0-EMIT:MUL-LIT-XT !
   ['] NEG k T0-EMIT:NEG-XT !
   ['] NOT k T0-EMIT:NOT-XT !
   ['] CMP k T0-EMIT:CMP-XT !
   ['] CMP-LIT k T0-EMIT:CMP-LIT-XT !
   ['] SET-FLAG k T0-EMIT:SET-FLAG-XT !
   ['] LOAD k T0-EMIT:LOAD-XT !
   ['] TOR k T0-EMIT:TOR-XT !
   ['] RFROM k T0-EMIT:RFROM-XT !
   ['] RFETCH k T0-EMIT:RFETCH-XT !
   ['] CALL k T0-EMIT:CALL-XT !
   ['] TAIL k T0-EMIT:TAIL-XT !
   ['] CALL-SELF k T0-EMIT:CALL-SELF-XT !
   ['] T0-LABEL:IF, k T0-EMIT:IF,-XT !
   ['] T0-LABEL:ELSE, k T0-EMIT:ELSE,-XT !
   ['] T0-LABEL:THEN, k T0-EMIT:THEN,-XT !
   ['] T0-LABEL:BEGIN, k T0-EMIT:BEGIN,-XT !
   ['] T0-LABEL:WHILE, k T0-EMIT:WHILE,-XT !
   ['] T0-LABEL:REPEAT, k T0-EMIT:REPEAT,-XT !
   ['] T0-LABEL:UNTIL, k T0-EMIT:UNTIL,-XT !
   ['] T0-LABEL:IF-CC, k T0-EMIT:IF-CC,-XT !
   ['] T0-LABEL:WHILE-CC, k T0-EMIT:WHILE-CC,-XT !
   ['] T0-LABEL:UNTIL-CC, k T0-EMIT:UNTIL-CC,-XT !
   ['] T0-LABEL:AGAIN, k T0-EMIT:AGAIN,-XT !
   ['] T0-LABEL:CASE, k T0-EMIT:CASE,-XT !
   ['] T0-LABEL:OF, k T0-EMIT:OF,-XT !
   ['] T0-LABEL:OF-LIT k T0-EMIT:OF-LIT-XT !
   ['] T0-LABEL:ENDOF, k T0-EMIT:ENDOF,-XT !
   ['] T0-LABEL:ENDCASE, k T0-EMIT:ENDCASE,-XT !
   ['] LOOP-PUSH k T0-EMIT:LOOP-PUSH-XT !
   ['] LOOP-POP k T0-EMIT:LOOP-POP-XT !
   ['] LOOP-I k T0-EMIT:LOOP-I-XT !
   ['] LOOP-J k T0-EMIT:LOOP-J-XT !
   ['] LOOP-OPEN k T0-EMIT:LOOP-OPEN-XT !
   ['] T0-LABEL:LEAVE, k T0-EMIT:LEAVE,-XT !
   ['] T0-LABEL:LOOP-NEXT k T0-EMIT:LOOP-NEXT-XT !
   ['] T0-LABEL:LOOP-STEP k T0-EMIT:LOOP-STEP-XT !
   ['] T0-LABEL:LOOP-CLOSE k T0-EMIT:LOOP-CLOSE-XT !
   ['] IMM? k T0-EMIT:IMM?-XT !
   ['] MARK k T0-LABEL:MARK-XT !
   ['] FWD-JUMP k T0-LABEL:FWD-JUMP-XT !
   ['] FWD-ZERO k T0-LABEL:FWD-ZERO-XT !
   ['] FWD-NE k T0-LABEL:FWD-NE-XT !
   ['] FWD-NE-LIT k T0-LABEL:FWD-NE-LIT-XT !
   ['] FWD-GE k T0-LABEL:FWD-GE-XT !
   ['] FWD-CC k T0-LABEL:FWD-CC-XT !
   ['] BACK-JUMP k T0-LABEL:BACK-JUMP-XT !
   ['] BACK-ZERO k T0-LABEL:BACK-ZERO-XT !
   ['] BACK-CC k T0-LABEL:BACK-CC-XT !
   ['] RESOLVE k T0-LABEL:RESOLVE-XT !
   ['] ENTRY-EQ k T0-LABEL:ENTRY-EQ-XT !
   ['] LOOP-NEXT k T0-LABEL:LOOP-NEXT-XT !
   ['] LOOP-STEP k T0-LABEL:LOOP-STEP-XT ! ;

CTARGET-ARCH:AARCH64 ' INSTALL T0-EMIT:PROVIDE

;using
;package
