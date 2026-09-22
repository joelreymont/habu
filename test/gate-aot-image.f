\ Read stripped code, excluding restored DATA and executable-segment padding.

require lib/errors.f
require lib/fs.f
require src/arch/arm64/asm.f
require tools/aot-call-report-lib.f      \ REPORT-JSON-BUFFER for CODE-REPORT
require tools/native-emit.f              \ SYS-EMIT-EXIT/SYS-EMIT-SVC for the built target
require test/gate-build-common.f

package AOT-IMAGE
using A64ASM

$18 constant ELF-ENTRY-OFF
$80000028 constant MACHO-LC-MAIN
variable ENTRY-OFF
variable ENTRY-COUNT
variable RX-END
variable DATA-OFF
variable DATA-COUNT
variable ROOT-CALL
create CODE-PATH FS-PATH-CAP allot

: IMAGE-CK ( bool -- )
   0= if E-BUILD-SOURCE throw then ;


: ELF-ENTRY ( -- n )
   0 ENTRY-COUNT !
   GB-ELF-PHNUM-OFF GB-U16-OFF 0 ?do
      i GB-ELF-PH-OFF {: off:n :}
      off GB-ELF-RX-LOAD? if
         ELF-ENTRY-OFF GB-U64-OFF
         off GB-ELF-PH-VADDR-OFF + GB-U64-OFF -
         off GB-ELF-PH-FILE-OFF + GB-U64-OFF + ENTRY-OFF !
         1 ENTRY-COUNT +!
      then
   loop
   ENTRY-COUNT @ 1 = IMAGE-CK
   ENTRY-OFF @ ;


: MACHO-ENTRY ( -- n )
   0 ENTRY-COUNT !
   GB-MH-SIZE
   GB-MH-NCMDS-OFF GB-U32-OFF 0 ?do
      dup GB-U32-OFF MACHO-LC-MAIN = if
         dup GB-LC-CMDSIZE-OFF + GB-U32-OFF $18 >= IMAGE-CK
         dup 8 + GB-U64-OFF ENTRY-OFF !
         1 ENTRY-COUNT +!
      then
      dup GB-LC-CMDSIZE-OFF + GB-U32-OFF +
   loop drop
   ENTRY-COUNT @ 1 = IMAGE-CK
   ENTRY-OFF @ GB-TEXT-OFF-V @ = IMAGE-CK
   ENTRY-OFF @ ;


\ ELF-HDR, / MACHO-CMDS, place the stripped startup at CODE-OFF. Check the
\ actual entry field, rather than treating the ELF RX header as instructions.
: CHECK-ENTRY ( -- n )
   HB-TARGET-LINUX? if ELF-ENTRY else MACHO-ENTRY then
   dup CODE-OFF = IMAGE-CK
   dup GB-TEXT-OFF-V @ >= IMAGE-CK
   dup RX-END @ < IMAGE-CK ;


: INSTR@ ( n -- n ) {: off:n :}
   off CODE-OFF >= off RX-END @ 4 - <= and IMAGE-CK
   off GB-U32-OFF ;


: INSTR= ( n n -- ) {: off:n want:n :}
   off INSTR@ want = IMAGE-CK ;


: WIDE-K? ( n n -- bool ) {: word:n reg:n :}
   word $FF80001F and $F2800000 reg or = ;


\ LIT64, emits MOVZ/MOVN followed by at most three MOVK lanes for this rd.
: SKIP-LITERAL ( n n -- n ) {: off:n reg:n :}
   off INSTR@ $FF80001F and {: opcode:n :}
   opcode $D2800000 reg or = opcode $92800000 reg or = or IMAGE-CK
   off 4 +
   3 0 ?do
      dup INSTR@ reg WIDE-K? 0= if unloop exit then
      4 +
   loop ;


\ Format authority: src/habu/aot-lib.f EMIT-DATA-COPY. The image is SPARSE: a u32
\ bitmap-byte length, then that many bitmap bytes (one bit a cell, low bit
\ first), then one unsigned LEB128 per present cell in cell order. Validate the
\ cursor setup that reads the bitmap length out of the image, the span-base
\ literal, and the complete bitmap + cell loops that follow the blob's address.
: CHECK-CURSORS ( n -- n ) {: off:n :}
   off      11 9 0 ENC-LDRW INSTR=
   off 4 +  9 9 4 ENC-ADDI INSTR=
   off 8 +  11 9 11 ENC-ADD INSTR=
   off 12 + 10 11 0 ENC-ADDI INSTR=
   off 16 + 13 SKIP-LITERAL ;


\ One inlined unsigned LEB128 decode, register for register: src/habu/aot-lib.f
\ EMIT-VGET, which is what makes a cell's value variable width. The registers
\ are arguments because the two loops that inline it hold their cursors in
\ different ones.
: CHECK-VGET ( n n n n n n -- n ) {: off:n acc:n cur:n b:n g:n sh:n :}
   off      acc 0 0 MOVZHW INSTR=
   off 4 +  sh 0 0 MOVZHW INSTR=
   off 8 +  b cur 0 ENC-LDRB INSTR=
   off 12 + cur cur 1 ENC-ADDI INSTR=
   off 16 + g b $7F >LIMM ENC-ANDI INSTR=
   off 20 + g g sh ENC-LSLV INSTR=
   off 24 + acc acc g ENC-ORR INSTR=
   off 28 + sh sh 7 ENC-ADDI INSTR=
   off 32 + g b $80 >LIMM ENC-ANDI INSTR=
   off 36 + g -7 ENC-CBNZ INSTR=
   off 40 + ;


: CHECK-COPY ( n -- )
   CHECK-CURSORS {: top:n :}
   top       9 11 ENC-CMP INSTR=
   top 4 +   26 C-CS ENC-BCOND INSTR=
   top 8 +   14 9 0 ENC-LDRB INSTR=
   top 12 +  9 9 1 ENC-ADDI INSTR=
   top 16 +  14 21 ENC-CBZ INSTR=
   top 20 +  12 8 0 MOVZHW INSTR=
   top 24 +  12 18 ENC-CBZ INSTR=
   top 28 +  21 14 1 >LIMM ENC-ANDI INSTR=
   top 32 +  21 12 ENC-CBZ INSTR=
   top 36 +  15 10 16 22 7 CHECK-VGET {: vend:n :}
   vend      15 13 0 ENC-STR INSTR=
   vend 4 +  14 14 1 ENC-LSRI INSTR=
   vend 8 +  13 13 8 ENC-ADDI INSTR=
   vend 12 + 12 12 1 ENC-SUBI INSTR=
   vend 16 + -17 ENC-B INSTR=
   vend 20 + -24 ENC-B INSTR=
   vend 24 + 13 13 64 ENC-ADDI INSTR=
   vend 28 + -26 ENC-B INSTR= ;


: ADR-REG? ( n n -- bool ) {: off:n reg:n :}
   off INSTR@ $9F00001F and $10000000 reg or = ;


: ADR-TARGET ( n -- n ) {: off:n :}
   off INSTR@ {: word:n :}
   word 29 rshift 3 and word 5 rshift $7FFFF and 2 lshift or
   $100000 xor $100000 - off + ;


\ One movz/movk lane, and the same word with that lane cleared: sf, opc, hw and
\ Rd say which instruction this is, the sixteen-bit immediate is what the pair
\ carries.
: MOVW-LANE ( n -- n )  5 rshift $FFFF and ;
: MOVW-SHAPE ( n -- n ) $FFE0001F and ;


\ THE DATA RESTORE, in the four words src/habu/aot-lib.f TEXT-ADR, emits: the
\ blob's byte offset from the code base in an LOFF, movz/movk pair, `adr x12` to
\ the code base itself (LTEXT, bound at text offset zero, which is this image's
\ entry), and the add that joins them. All four words are pinned because the
\ first one alone does not identify the sequence: EMIT-OWNED-CELLS opens a
\ LIT64, of a DATA offset with the same `movz x9`, and it is the `adr x12` and
\ the add that never follow it.
: DATA-RESTORE? ( n -- bool ) {: off:n :}
   off INSTR@ MOVW-SHAPE 9 0 0 MOVZHW = 0= if false exit then
   off 4 + INSTR@ MOVW-SHAPE 9 0 1 MOVKHW = 0= if false exit then
   off 8 + 12 ADR-REG? 0= if false exit then
   off 8 + ADR-TARGET CODE-OFF = 0= if false exit then
   off 12 + INSTR@ 9 12 9 ENC-ADD = ;


\ The pair's two lanes spell the blob's offset from text offset zero, so the
\ image's file offset for it is that offset past the entry.
: DATA-BLOB-OFF ( n -- n ) {: off:n :}
   off INSTR@ MOVW-LANE
   off 4 + INSTR@ MOVW-LANE 16 lshift or
   CODE-OFF + ;


: SCAN-STARTUP ( n -- )
   0 DATA-COUNT !
   begin
      dup INSTR@ BL? 0=
   while
      dup DATA-RESTORE? if
         dup 16 + CHECK-COPY
         dup DATA-BLOB-OFF DATA-OFF !
         1 DATA-COUNT +!
      then
      4 +
   repeat ROOT-CALL !
   DATA-COUNT @ 1 = IMAGE-CK ;


\ A seam publishes a syscall stencil as a byte string, because an x86_64 stencil
\ is five or two bytes; on the two targets this gate reads, each one is exactly
\ one instruction word.
: STENCIL-W ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 4 = IMAGE-CK
   a c@
   a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or
   a 3 + c@ 24 lshift or ;


: STARTUP-END ( -- n )
   \ The exit syscall tail is emitted even though a successful exit never
   \ returns to its Linux errno reconciliation instructions.
   ROOT-CALL @ 4 + 0 0 0 MOVZHW INSTR=
   ROOT-CALL @ 8 + SYS-EMIT-EXIT STENCIL-W INSTR=
   ROOT-CALL @ 12 + SYS-EMIT-SVC STENCIL-W INSTR=
   HB-TARGET-LINUX? if
      ROOT-CALL @ 16 + 16 $FFE 0 MOVNHW INSTR=
      ROOT-CALL @ 20 + 0 16 ENC-CMP INSTR=
      ROOT-CALL @ $18 + exit
   then
   ROOT-CALL @ $10 + ;


: CHECK-CODE-END ( -- )
   STARTUP-END {: end:n :}
   DATA-OFF @ end >= IMAGE-CK
   DATA-OFF @ RX-END @ < IMAGE-CK
   DATA-OFF @ 3 and 0= IMAGE-CK
   ROOT-CALL @ INSTR@ $3FFFFFF and $2000000 xor $2000000 - 4 *
   ROOT-CALL @ + {: root:n :}
   \ PLAN-BLOBS places closure record zero (the selected root) first.
   root end = root DATA-OFF @ < and IMAGE-CK ;

public

\ EMIT-DATA-BLOB places the validated startup's source address immediately
\ after all code. NSTR:WINDOW-OPEN guarantees these fixtures have a DATA copy.
: CODE-RANGE ( ptr u8 n -- n n )
   GB-EXEC-TEXT-RANGE {: off:n size:n :}
   off size GB-RANGE
   off size + RX-END !
   CHECK-ENTRY dup SCAN-STARTUP
   CHECK-CODE-END
   DATA-OFF @ over - ;


\ The existing report accepts raw bytes. Give it only the validated code span;
\ neither restored DATA nor ELF/Mach-O headers/padding are instruction sites.
: CODE-REPORT ( -- )
   GB-OUT$ CODE-RANGE {: off:n size:n :}
   s" hb-aot-code" CODE-PATH GT-PATH {: pathu:n :}
   CODE-PATH pathu off GB-ADDR size WRITE-ALL
   CODE-PATH pathu GB-REPORT-BUF GB-REPORT-CAP REPORT-JSON-BUFFER
   {: out:ptr outu:n :}
   GB-REPORT$ out outu WRITE-ALL ;

;using
;package
