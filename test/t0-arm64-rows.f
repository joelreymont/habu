\ t0-arm64-rows.f - the rows the T0 arm64 emitter (src/habu/t0-arm64.f) files
\ for an install, through the seam src/habu/t0-emit.f: the does> patch slot of
\ a unit SLOT-CLOSE ends, a data address row's origin, the callback stub an
\ armed NCBSTUB appends, a fixed word folded into the body (a constant into
\ `cmp #imm`, a variable into its data address row) while a does> child stays
\ a call, the writeback transfers that move the data stack pointer with a
\ spill's or fill's last cell, a copy that shares its vreg until a load
\ writes a fresh one, a square that multiplies a shared vreg by itself, a
\ compare that branches on its condition codes, an `if` arm and a `while` loop
\ that keep their values in registers across the join and the back edge, a
\ value filled after a call into the register its loop's back edge expects,
\ stack words over one-cell values, and cells, cell+ and ptr-field, that call
\ nothing, return-stack pair moves that touch the depth cell once each, the
\ x86-64 stack moves that leave those codes alone, a divide by a literal (an
\ arm64 sdiv and msub in a leap-year test, the x86-64 idiv bytes with the
\ register it saves), and the refusals that keep the patch slot last and an
\ operation x86-64 does not provide from running.
\ Bodies are compiled by T0:COMPILE-TEXT or driven through the seam, sealed
\ into NEMIT, and checked by decoding the words NEMIT answers against the
\ assembler's encodings.

require src/lib/test.f
require src/arch/arm64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/compiler/native/string.f
require src/habu/t0-emit.f
require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0.f

package T0-ROWS-TEST
private

\ Where the placed seals put the emission. No placed body calls, so no branch
\ has to reach anything from it.
$40000 constant PLACE

\ The stub's slot and thunk, every half distinct, so a lane out of place shows.
$0001000200030004 constant CB-SLOT
$0000123456789ABC constant CB-THUNK

\ A DATA cell an ADDR body names, and the record ordinal its row files.
variable CELLV
7 constant ORDINAL

: V0 ( -- T0-EMIT:vreg ) 0 T0-EMIT:>VREG ;

\ The instruction word at byte at of the sealed emission.
: WORD@ ( n -- n ) {: at:n :}
   NEMIT:BYTES at + {: p:ptr :}
   0 4 0 ?do p i + c@ i 8 * lshift or loop ;

: HALF ( n n -- n ) 16 * rshift $FFFF and ;

\ The four-lane carrier of v into r starts at byte at.
: CHAIN4? ( n n n -- bool ) {: at:n r:n v:n :}
   at WORD@ r v 0 HALF 0 A64ASM:MOVZHW =
   4 1 ?do at i 4 * + WORD@ r v i HALF i A64ASM:MOVKHW = and loop ;

: ARCH-A64 ( -- ) CTARGET-ARCH:AARCH64 T0-EMIT:SELECT ;

\ An ADDR body, as the front end emits a fixed one: the DATA address, pushed,
\ and the return that is the does> patch slot.
: ADDR-BODY ( -- )
   T0-EMIT:OPEN
   CELLV NULL-PTR - ORDINAL V0 T0-EMIT:ADDR-DATA
   V0 1 T0-EMIT:PUSH
   T0-EMIT:SLOT-CLOSE ;

: PATCH-SLOT-CASE ( -- )
   ARCH-A64
   ADDR-BODY
   T0-EMIT:SEAL-UNPLACED
   s" the patch slot is the emission's last word" T-LABEL
   NEMIT:PATCH-SLOT NEMIT:SIZE 4 - T=
   s" the patch slot is a ret" T-LABEL
   NEMIT:PATCH-SLOT WORD@ A64ASM:ENC-RET T=
   s" the ADDR body is a leaf: its address chain is its first word" T-LABEL
   0 NEMIT:ADDR-SITE@ 0 T=
   s" the data address row files the ordinal ADDR-DATA took" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ ORDINAL T=
   NEMIT:CLEAR ;

\ The body of source text t has one string literal: under source capture its
\ address row files the captured row's origin and names that row's address.
: STRING-ROW ( ptr u8 n -- ) {: t:ptr u:n :}
   t u T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" a string body files one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   NSTR:SOURCE-COUNT 1- {: row:n :}
   s" under source capture the row files -(row+2)" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ row 2 + negate T=
   0 T0-EMIT:ADDR-ROW {: at:n kind:n to:n :}
   s" the captured source row starts at the address the row names" T-LABEL
   row NSTR:SOURCE@ drop NULL-PTR - to T=
   NEMIT:CLEAR ;

\ A plain literal interns from the text, a counted one from STR-BUF.
: ORIGIN-CASE ( -- )
   ARCH-A64
   NSTR:SOURCE-BEGIN
   S\" ORG ( -- ptr u8 n ) s\" abc\" " STRING-ROW
   S\" CORG ( -- ptr u8 ) c\" abc\" " STRING-ROW
   NSTR:SOURCE-END ;

\ The words a leaf body of source text t emits, x19 the data stack and x16 the
\ literal temporary, all its ret.
19 constant DS
16 constant LT

: BODY ( ptr u8 n -- ) T0:COMPILE-TEXT T0-EMIT:SEAL-UNPLACED ;

: WORDS= ( n -- ) {: n:n :}
   s" the body is its words and a ret" T-LABEL
   NEMIT:SIZE n 1+ 4 * T=
   s" the ret follows them" T-LABEL
   n 4 * WORD@ A64ASM:ENC-RET T= ;

\ One cell fills by a pre-index load and spills by a post-index store. A
\ multi-cell fill loads the upper cells at their offsets and pops with the
\ deepest; a multi-cell spill stores the upper cells at their offsets and
\ pushes with the bottom one.
: WRITEBACK-CASE ( -- )
   ARCH-A64
   s" INC ( n -- n ) 1+ " BODY
   s" a one-cell fill pops with ldr x9,[x19,#-8]!" T-LABEL
   0 WORD@ 9 DS -8 A64ASM:ENC-LDRPRE T=
   s" the add works on the filled vreg" T-LABEL
   4 WORD@ 9 9 1 A64ASM:ENC-ADDI T=
   s" a one-cell spill pushes with str x9,[x19],#8" T-LABEL
   8 WORD@ 9 DS 8 A64ASM:ENC-STRPOST T=
   3 WORDS=
   NEMIT:CLEAR
   s" VAL ( -- n ) 42 " BODY
   s" a literal spill loads the temporary" T-LABEL
   0 WORD@ LT 42 0 A64ASM:MOVZHW T=
   s" and pushes it with str x16,[x19],#8" T-LABEL
   4 WORD@ LT DS 8 A64ASM:ENC-STRPOST T=
   2 WORDS=
   NEMIT:CLEAR
   s" SWP ( n n -- n n ) swap " BODY
   s" a two-cell fill loads the top at -8" T-LABEL
   0 WORD@ 9 DS -8 A64ASM:ENC-LDUR T=
   s" and pops both with the second, ldr x10,[x19,#-16]!" T-LABEL
   4 WORD@ 10 DS -16 A64ASM:ENC-LDRPRE T=
   s" a two-cell spill stores the top at +8" T-LABEL
   8 WORD@ 10 DS 8 A64ASM:ENC-STR T=
   s" and pushes both with the bottom, str x9,[x19],#16" T-LABEL
   12 WORD@ 9 DS 16 A64ASM:ENC-STRPOST T=
   4 WORDS=
   NEMIT:CLEAR
   s" ROT3 ( n n n -- n n n ) rot " BODY
   s" a three-cell fill loads the top at -8" T-LABEL
   0 WORD@ 9 DS -8 A64ASM:ENC-LDUR T=
   s" the second at -16" T-LABEL
   4 WORD@ 10 DS -16 A64ASM:ENC-LDUR T=
   s" and pops all three with the third, ldr x11,[x19,#-24]!" T-LABEL
   8 WORD@ 11 DS -24 A64ASM:ENC-LDRPRE T=
   NEMIT:CLEAR ;

\ `dup` pushes a second entry holding the filled vreg, and `c@` loads the byte
\ through it into a fresh vreg: no mov copies the address.
: SHARE-CASE ( -- )
   ARCH-A64
   s" DC ( ptr u8 -- ptr u8 u8 ) dup c@ " BODY
   s" the address fills x9" T-LABEL
   0 WORD@ 9 DS -8 A64ASM:ENC-LDRPRE T=
   s" the byte loads through x9 into x10, no mov before it" T-LABEL
   4 WORD@ 10 9 0 A64ASM:ENC-LDRB T=
   s" the byte spills from x10" T-LABEL
   8 WORD@ 10 DS 8 A64ASM:ENC-STR T=
   s" and the address pushes from x9" T-LABEL
   12 WORD@ 9 DS 16 A64ASM:ENC-STRPOST T=
   4 WORDS=
   NEMIT:CLEAR
   s" SQ ( n -- n ) dup * " BODY
   s" the value fills x9" T-LABEL
   0 WORD@ 9 DS -8 A64ASM:ENC-LDRPRE T=
   s" `*` consumes the copy, so x9 multiplies by itself with no mov" T-LABEL
   4 WORD@ 9 9 9 A64ASM:ENC-MUL T=
   s" the square pushes from x9" T-LABEL
   8 WORD@ 9 DS 8 A64ASM:ENC-STRPOST T=
   3 WORDS=
   NEMIT:CLEAR ;

\ The defer an `is` body binds. Its address row names the defer's cell and files
\ the defer's record ordinal, which the unit object resolves the cell through.
\ Public: the body runs after the package closes and names it qualified.
public
defer ROW-HOOK ( -- n )
s" ROW-HOOK" XREF-FIND-CURRENT-INDEX XREF-REQUIRE-INDEX constant HOOK-IX
private

: IS-ORIGIN-CASE ( -- )
   ARCH-A64
   s" ISO ( n -- ) is T0-ROWS-TEST:ROW-HOOK " T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" an `is` body files one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   0 T0-EMIT:ADDR-ROW {: at:n kind:n to:n :}
   s" the row names the defer's cell" T-LABEL
   s" T0-ROWS-TEST:ROW-HOOK" NDICT:SPELL-DEFER-CELL to T=
   s" the row files the defer's record ordinal" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ HOOK-IX T=
   NEMIT:CLEAR ;

\ The fixed words a body names: a constant, a variable and a does> child,
\ whose clause replaced the address its created body pushed.
public
10 constant TEN
variable FOLDV
s" FOLDV" XREF-FIND-CURRENT-INDEX XREF-REQUIRE-INDEX constant FOLDV-IX
: KEEP ( n -- ) create , does> ( -- n ) @ ;
5 KEEP KID
private

\ Whether instruction word w is `cmp xN, #imm` for some register N the
\ assembler encodes: x18 is Darwin's and it refuses that one.
: CMPI-WORD? ( n n -- bool ) {: w:n imm:n :}
   false 31 0 ?do i 18 <> if i imm A64ASM:ENC-CMPI w = or then loop ;

\ Whether the sealed emission holds `cmp xN, #imm`.
: CMPI? ( n -- bool ) {: imm:n :}
   false NEMIT:SIZE 0 ?do i WORD@ imm CMPI-WORD? or 4 +loop ;

: FIXED-CASE ( -- )
   ARCH-A64
   s" FC ( n -- bool ) T0-ROWS-TEST:TEN = " T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" a constant folds: its body calls nothing" T-LABEL
   T0-EMIT:CALL-ROWS 0 T=
   s" and compares against it with cmp #imm" T-LABEL
   10 CMPI? TTRUE
   NEMIT:CLEAR
   s" FV ( -- n ) T0-ROWS-TEST:FOLDV @ " T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" a variable folds: its body calls nothing" T-LABEL
   T0-EMIT:CALL-ROWS 0 T=
   s" it files one data address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   0 NEMIT:ADDR-SITE-KIND@ NEMIT:ADDR-DATA T=
   0 T0-EMIT:ADDR-ROW {: at:n kind:n to:n :}
   s" naming the variable's cell" T-LABEL
   FOLDV NULL-PTR - to T=
   s" with the variable's record ordinal" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ FOLDV-IX T=
   NEMIT:CLEAR
   s" FK ( -- n ) T0-ROWS-TEST:KID " T0:COMPILE-TEXT
   T0-EMIT:SEAL-UNPLACED
   s" a does> child is called, not folded" T-LABEL
   T0-EMIT:CALL-ROWS 1 T=
   NEMIT:ADDR-SITES 0 T=
   0 T0-EMIT:CALL-ROW {: cat:n ckind:n cto:n :}
   s" the call reaches the child" T-LABEL
   cto s" T0-ROWS-TEST:KID" NDICT:CALL-TARGET T=
   NEMIT:CLEAR ;

\ A fixed VAL body sealed placed while NCBSTUB is armed.
: STUB-CASE ( -- )
   ARCH-A64
   CB-SLOT CB-THUNK NCBSTUB:SET
   s" VAL ( -- n ) 42 " T0:COMPILE-TEXT
   PLACE T0-EMIT:SEAL
   s" the stub is one more function, after the unit" T-LABEL
   NEMIT:FUNCTIONS 2 T=
   1 NEMIT:FUNCTION-OFFSET@ {: stub:n :}
   s" the stub is the emission's last nine words" T-LABEL
   stub NEMIT:SIZE 36 - T=
   s" the VAL unit ends in ret just before the stub" T-LABEL
   stub 4 - WORD@ A64ASM:ENC-RET T=
   s" the stub loads x9 with the slot, every lane" T-LABEL
   stub 9 CB-SLOT CHAIN4? TTRUE
   s" then x16 with the thunk, every lane" T-LABEL
   stub 16 + 16 CB-THUNK CHAIN4? TTRUE
   s" and branches through x16" T-LABEL
   stub 32 + WORD@ 16 A64ASM:ENC-BR T=
   s" the thunk carrier is the one address row" T-LABEL
   NEMIT:ADDR-SITES 1 T=
   s" at the thunk carrier" T-LABEL
   0 NEMIT:ADDR-SITE@ stub 16 + T=
   s" a code address" T-LABEL
   0 NEMIT:ADDR-SITE-KIND@ NEMIT:ADDR-CODE T=
   s" with no origin" T-LABEL
   0 NEMIT:ADDR-SITE-ORIGIN@ -1 T=
   s" the placed seal records the stub's address as the entry" T-LABEL
   NCBSTUB:ENTRY@ PLACE stub + T=
   NEMIT:CLEAR
   NCBSTUB:CLEAR ;

: SLOT-REFUSED-CASE ( -- )
   ARCH-A64
   s" a unit begun after the patch slot is refused" T-LABEL
   ADDR-BODY
   [: 1 T0-EMIT:QUOT-OPEN ;] T0-EMIT:E-T0-SLOT TTHROWSQ
   s" a callback stub after the patch slot is refused" T-LABEL
   ADDR-BODY
   CB-SLOT CB-THUNK NCBSTUB:SET
   [: T0-EMIT:SEAL-UNPLACED ;] T0-EMIT:E-T0-SLOT TTHROWSQ
   NCBSTUB:CLEAR ;

\ Whether no word of the sealed emission is a csetm flag or a cbz test of one.
: NO-FLAG? ( -- bool )
   0 0 A64ASM:ENC-CSETM $FFFF0FE0 and {: csetm:n :}
   0 0 A64ASM:ENC-CBZ $FF000000 and {: cbz:n :}
   true NEMIT:SIZE 4 / 0 ?do
      i 4 * WORD@ {: w:n :}
      w $FFFF0FE0 and csetm <>  w $FF000000 and cbz <> and and
   loop ;

\ A body with `0= while`, `>= until` and `IF` in capitals.
: LOOPS ( -- ptr u8 n )
   s" H ( n -- ) begin dup 0= while repeat begin dup 3 >= until 0< IF then " ;

\ A compare followed by `if` branches on its condition: cmp, then b.ge past
\ the arm, with no csetm flag and no cbz test of one. So does a compare with
\ zero before `while`, one before `until`, and one before `IF` in capitals.
: FUSE-CASE ( -- )
   ARCH-A64
   s" F ( n n -- n ) < if 1 else 2 then " BODY
   s" the compare is cmp x10,x9" T-LABEL
   8 WORD@ 10 9 A64ASM:ENC-CMP T=
   s" and its branch is b.ge, past the arm when < fails" T-LABEL
   12 WORD@ $FF00001F and  0 A64ASM:C-GE A64ASM:ENC-BCOND T=
   s" no word is a csetm or a cbz" T-LABEL
   NO-FLAG? TTRUE
   NEMIT:CLEAR
   LOOPS BODY
   s" 0= while, >= until and 0< IF: no word is a csetm or a cbz" T-LABEL
   NO-FLAG? TTRUE
   NEMIT:CLEAR ;

\ An `if` arm that swaps the compared values joins in registers: after the
\ compare nothing loads or stores through x19 until the result's push.
: JOIN-CASE ( -- )
   ARCH-A64
   s" MAX2 ( n n -- n ) over over < if swap then drop " BODY
   s" the compare is cmp x10,x9" T-LABEL
   8 WORD@ 10 9 A64ASM:ENC-CMP T=
   NEMIT:SIZE 8 - {: last:n :}
   s" the word before the ret pushes the result, str x10,[x19],#8" T-LABEL
   last WORD@ 10 DS 8 A64ASM:ENC-STRPOST T=
   s" no word between them loads or stores through x19" T-LABEL
   true last 12 ?do
      i WORD@ {: w:n :}
      w 25 rshift 5 and 4 =  w 5 rshift 31 and DS =  and 0= and
   4 +loop TTRUE
   NEMIT:CLEAR ;

\ The byte offset the word at byte at branches back to: a `b` or `b.cond`
\ with a negative offset; at itself for any other word.
: BACK-TARGET ( n -- n ) {: at:n :}
   at WORD@ {: w:n :}
   w $FC000000 and $14000000 =  w 25 rshift 1 and 1 =  and if
      w $3FFFFFF and 1 26 lshift - 4 * at + exit
   then
   w $FF000010 and $54000000 =  w 23 rshift 1 and 1 =  and if
      w 5 rshift $7FFFF and 1 19 lshift - 4 * at + exit
   then
   at ;

\ The body sealed last keeps its loop's values in registers around the back
\ edge: nothing between the loop head and the last branch back to it loads or
\ stores through x19.
: IN-REGS ( -- )
   0 0 NEMIT:SIZE 0 ?do
      i BACK-TARGET {: t:n :}
      t i <> if 2drop t i then
   4 +loop {: head:n back:n :}
   s" the body branches back to its loop head" T-LABEL
   back head > TTRUE
   s" no word between them loads or stores through x19" T-LABEL
   true back head ?do
      i WORD@ {: w:n :}
      w 25 rshift 5 and 4 =  w 5 rshift 31 and DS =  and 0= and
   4 +loop TTRUE
   NEMIT:CLEAR ;

\ A `while` loop over an address and a count, with a counter below them that
\ an `if` in the body bumps.
: LOOP-CASE ( -- )
   ARCH-A64
   s" CNT ( ptr u8 n -- n ptr u8 n ) 0 rot rot begin dup 0 > while over c@ 44 = if rot 1+ rot rot then swap 1+ swap 1- repeat " BODY
   IN-REGS ;

\ A do loop whose body swaps two values its head holds in registers.
: DO-LOOP-CASE ( -- )
   ARCH-A64
   s" DL ( n n -- n n ) 1+ swap 1+ swap 10 0 do swap 1+ loop " BODY
   IN-REGS ;

\ `i` in a straight loop body is one move from x29, the index's cache, and no
\ word loads the index from its frame cell. A join clears the cache: the first
\ `i` past it loads the index from its frame cell, and nothing else does.
: IDX-CASE ( -- )
   ARCH-A64
   s" IR ( n -- n ) 0 swap 0 do i + loop " BODY
   0 29 A64ASM:ENC-MOV {: mov:n :}
   0 17 -16 A64ASM:ENC-LDUR {: ldur:n :}
   s" i is one mov from x29" T-LABEL
   0 NEMIT:SIZE 0 ?do i WORD@ $FFFFFFE0 and mov = if 1+ then 4 +loop 1 T=
   s" no word loads the index from its frame cell" T-LABEL
   true NEMIT:SIZE 0 ?do i WORD@ $FFFFFFE0 and ldur <> and 4 +loop TTRUE
   NEMIT:CLEAR
   s" IR2 ( n -- n ) 0 swap 0 do i 0< if 1+ then i i + + loop " BODY
   s" only the first i past a join loads the index" T-LABEL
   0 NEMIT:SIZE 0 ?do i WORD@ $FFFFFFE0 and ldur = if 1+ then 4 +loop 1 T=
   NEMIT:CLEAR ;

\ -rot 2dup 2drop 2swap 2over 2>r 2r@ 2r> over one-cell values move entries
\ and call nothing.
: MOVE-CASE ( -- )
   ARCH-A64
   s" MV ( n n n n -- n n n n n n n n ) -rot 2dup 2drop 2swap 2over 2>r 2r@ 2r> " BODY
   s" the moved stack words call nothing" T-LABEL
   T0-EMIT:CALL-ROWS 0 T=
   NEMIT:CLEAR ;

\ cells, cell+ and ptr-field compute in registers and call nothing.
: CELLS-CASE ( -- )
   ARCH-A64
   s" CI ( n -- n ) cells cell+ " BODY
   s" cells and cell+ call nothing" T-LABEL
   T0-EMIT:CALL-ROWS 0 T=
   NEMIT:CLEAR
   s" PF ( ptr ptr u8 n -- ptr ptr u8 ) ptr-field " BODY
   s" ptr-field calls nothing" T-LABEL
   T0-EMIT:CALL-ROWS 0 T=
   NEMIT:CLEAR ;

\ How many words of the sealed emission equal w.
: COUNT-WORD ( n -- n ) {: w:n :}
   0 NEMIT:SIZE 0 ?do i WORD@ w = if 1+ then 4 +loop ;

\ The return stack's depth cell is read once and written once per `2>r` or
\ `2r>` over one-cell values, and `2r@` only reads it: x16 through the user
\ area's RSP-CELL.
: RS-CASE ( -- )
   ARCH-A64
   LT XREG-RBASE RSP-CELL A64ASM:ENC-LDR {: ld:n :}
   LT XREG-RBASE RSP-CELL A64ASM:ENC-STR {: st:n :}
   s" RS ( n n -- n n ) 2>r 2r> " BODY
   s" 2>r and 2r> each read the depth once" T-LABEL
   ld COUNT-WORD 2 T=
   s" and each write it once" T-LABEL
   st COUNT-WORD 2 T=
   NEMIT:CLEAR
   s" RS2 ( n n -- n n n n ) 2>r 2r@ 2r> " BODY
   s" 2r@ reads the depth once more" T-LABEL
   ld COUNT-WORD 3 T=
   s" and never writes it" T-LABEL
   st COUNT-WORD 2 T=
   NEMIT:CLEAR ;

\ A value filled after a call lands in the register the innermost structure
\ with a recorded layout keeps it in, as the JIT's NEED-ALLOC, steers a pull:
\ the body calls, and no register move follows the call. An entry filled
\ above it keeps off the registers that layout names for the entries below,
\ which SUM3 and SUM6 need; a do loop inside an `if` steers by the loop's
\ layout, not the `if`'s (SUM3); and an `else` after a first arm that leaves
\ records no layout, so the fill skips past it to the loop's (SUM6).
public
: STEP1 ( n -- n ) 1+ ;
private

: STEERED ( ptr u8 n -- )
   BODY
   0 0 A64ASM:ENC-MOV {: mov:n :}
   false 0 NEMIT:SIZE 0 ?do
      i WORD@ {: w:n :}
      over w $FFE0FFE0 and mov = and if 1+ then
      swap w $FC000000 and $94000000 = or swap
   4 +loop {: called:bool moves:n :}
   s" the body calls" T-LABEL
   called TTRUE
   s" no register move follows the call" T-LABEL
   moves 0 T=
   NEMIT:CLEAR ;

: STEER-CASE ( -- )
   ARCH-A64
   s" SUM1 ( n -- n ) 0 swap 0 do i T0-ROWS-TEST:STEP1 + loop " STEERED
   s" SUM3 ( n f -- n ) if 1+ 10 0 do i T0-ROWS-TEST:STEP1 + loop then " STEERED
   s" SUM6 ( n -- n ) 1+ 10 0 do i 5 = if leave else i T0-ROWS-TEST:STEP1 + then loop " STEERED ;

\ The x86-64 stack moves leave the condition codes alone: the stack pointer
\ moves by lea r12,[r12+8] and lea r12,[r12-8], and zero loads by mov ecx,0.
create X86-MOVES
$4D c, $8D c, $64 c, $24 c, $08 c,
$4D c, $8D c, $64 c, $24 c, $F8 c,
$B9 c, $00 c, $00 c, $00 c, $00 c,
15 constant X86-MOVES-LEN

: X86-FLAGS-CASE ( -- )
   CTARGET-ARCH:X86-64 T0-EMIT:SELECT
   T0-EMIT:OPEN
   1 T0-EMIT:DSP+  -1 T0-EMIT:DSP+  0 V0 T0-EMIT:LIT
   T0-EMIT:RETURN
   T0-EMIT:SEAL-UNPLACED
   s" x86-64 DSP+ is an lea and a zero literal a mov" T-LABEL
   NEMIT:BYTES NEMIT:SIZE X86-MOVES X86-MOVES-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   ARCH-A64 ;

\ An x86-64 compare followed by `if` branches on its condition: cmp rdx,rcx,
\ then jge rel32 past the arm.
create X86-FUSED
$48 c, $39 c, $CA c, $0F c, $8D c,
5 constant X86-FUSED-LEN

: X86-FUSE-CASE ( -- )
   CTARGET-ARCH:X86-64 T0-EMIT:SELECT
   s" F ( n n -- n ) < if 1 else 2 then " BODY
   s" x86-64 < if is cmp rdx,rcx then jge" T-LABEL
   NEMIT:BYTES NEMIT:SIZE X86-FUSED X86-FUSED-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   ARCH-A64 ;

\ x86-64 provides neither operation yet: each is refused, not run.
: X86-UNFILLED-CASE ( -- )
   CTARGET-ARCH:X86-64 T0-EMIT:SELECT
   s" x86-64 SLOT-CLOSE is refused as unfilled" T-LABEL
   T0-EMIT:OPEN
   [: T0-EMIT:SLOT-CLOSE ;] T0-EMIT:E-T0-UNFILLED TTHROWSQ
   s" an armed x86-64 seal is refused as unfilled" T-LABEL
   s" XVAL ( -- n ) 42 " T0:COMPILE-TEXT
   CB-SLOT CB-THUNK NCBSTUB:SET
   [: T0-EMIT:SEAL-UNPLACED ;] T0-EMIT:E-T0-UNFILLED TTHROWSQ
   NCBSTUB:CLEAR
   ARCH-A64 ;

\ Whether the sealed emission holds a word w with w and mask equal to want.
: HAS-WORD? ( n n -- bool ) {: mask:n want:n :}
   false NEMIT:SIZE 0 ?do i WORD@ mask and want = or 4 +loop ;

\ A leap-year test divides by its literals in place: sdiv and msub, and its one
\ call is `0<>`'s, none a divide row's.
: LEAP-CASE ( -- )
   ARCH-A64
   s" LEAP ( n -- bool ) dup 4 mod 0= over 100 mod 0<> and swap 400 mod 0= or "
   BODY
   s" mod by a literal is an sdiv" T-LABEL
   $FFE0FC00 $9AC00C00 HAS-WORD? TTRUE
   s" and an msub" T-LABEL
   $FFE08000 $9B008000 HAS-WORD? TTRUE
   s" its one call is 0<>, no divide row" T-LABEL
   T0-EMIT:CALL-ROWS 1 T=
   0 T0-EMIT:CALL-ROW {: cat:n ckind:n cto:n :}
   cto s" 0<>" NDICT:CALL-TARGET T=
   NEMIT:CLEAR ;

\ The x86-64 divide by a literal saves rdx, which idiv writes, unless the
\ destination is rdx, and then saves rcx for the divisor.
create X86-DIV-RCX                    \ rcx = rcx / 7
$52 c,  $48 c, $89 c, $C8 c,  $B9 c, $07 c, $00 c, $00 c, $00 c,
$48 c, $99 c,  $48 c, $F7 c, $F9 c,  $48 c, $89 c, $C1 c,  $5A c,
18 constant X86-DIV-RCX-LEN

create X86-DIV-RDX                    \ rdx = rdx / 7
$51 c,  $48 c, $89 c, $D0 c,  $B9 c, $07 c, $00 c, $00 c, $00 c,
$48 c, $99 c,  $48 c, $F7 c, $F9 c,  $48 c, $89 c, $C2 c,  $59 c,
18 constant X86-DIV-RDX-LEN

create X86-MOD-RDX                    \ rdx = rdx mod 400
$51 c,  $48 c, $89 c, $D0 c,  $B9 c, $90 c, $01 c, $00 c, $00 c,
$48 c, $99 c,  $48 c, $F7 c, $F9 c,  $59 c,
15 constant X86-MOD-RDX-LEN

create X86-MOD-R8                     \ r8 = r8 mod 7
$52 c,  $4C c, $89 c, $C0 c,  $41 c, $B8 c, $07 c, $00 c, $00 c, $00 c,
$48 c, $99 c,  $49 c, $F7 c, $F8 c,  $49 c, $89 c, $D0 c,  $5A c,
19 constant X86-MOD-R8-LEN

\ The bytes of one divide by a literal, sealed alone.
: X86-DIV ( [ T0-EMIT:vreg n -- ] n n -- )
   {: k:n v:n :}
   T0-EMIT:OPEN
   k T0-EMIT:>VREG v rot execute
   T0-EMIT:RETURN
   T0-EMIT:SEAL-UNPLACED ;

: X86-DIV-CASE ( -- )
   CTARGET-ARCH:X86-64 T0-EMIT:SELECT
   s" x86-64 rcx / 7 saves rdx and divides by rcx" T-LABEL
   [: T0-EMIT:DIV-LIT ;] 0 7 X86-DIV
   NEMIT:BYTES NEMIT:SIZE X86-DIV-RCX X86-DIV-RCX-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   s" x86-64 rdx / 7 saves rcx and divides by it" T-LABEL
   [: T0-EMIT:DIV-LIT ;] 1 7 X86-DIV
   NEMIT:BYTES NEMIT:SIZE X86-DIV-RDX X86-DIV-RDX-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   s" x86-64 rdx mod 400 leaves the remainder in rdx" T-LABEL
   [: T0-EMIT:MOD-LIT ;] 1 400 X86-DIV
   NEMIT:BYTES NEMIT:SIZE X86-MOD-RDX X86-MOD-RDX-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   s" x86-64 r8 mod 7 moves the remainder from rdx" T-LABEL
   [: T0-EMIT:MOD-LIT ;] 4 7 X86-DIV
   NEMIT:BYTES NEMIT:SIZE X86-MOD-R8 X86-MOD-R8-LEN CONTAINS? TTRUE
   NEMIT:CLEAR
   ARCH-A64 ;

public

: RUN ( -- )
   T-RESET
   PATCH-SLOT-CASE
   ORIGIN-CASE
   IS-ORIGIN-CASE
   STUB-CASE
   FIXED-CASE
   WRITEBACK-CASE
   SHARE-CASE
   SLOT-REFUSED-CASE
   FUSE-CASE
   JOIN-CASE
   LOOP-CASE
   DO-LOOP-CASE
   IDX-CASE
   MOVE-CASE
   CELLS-CASE
   RS-CASE
   STEER-CASE
   LEAP-CASE
   X86-FLAGS-CASE
   X86-DIV-CASE
   X86-FUSE-CASE
   X86-UNFILLED-CASE
   T-REPORT ;

;package

T0-ROWS-TEST:RUN
