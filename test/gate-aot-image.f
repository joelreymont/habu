\ Read stripped code, excluding restored DATA and executable-segment padding.

require lib/errors.f
require lib/fs.f
require src/arch/arm64/asm.f
require tools/aot-startup-shape.f        \ the startup's instruction shapes, named once
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


\ LIT64, emits MOVZ/MOVN followed by at most three MOVK lanes for this rd. Every
\ instruction shape this file reads is named in tools/aot-startup-shape.f, which
\ writes them with the emitter's own encoders; this file finds them in an image.
: SKIP-LITERAL ( n n -- n ) {: off:n reg:n :}
   off INSTR@ {: w:n :}
   w reg AOT-STARTUP-SHAPE:MOVZ-RD?  w reg AOT-STARTUP-SHAPE:MOVN-RD? or IMAGE-CK
   off 4 +
   3 0 ?do
      dup INSTR@ reg AOT-STARTUP-SHAPE:MOVK-RD? 0= if unloop exit then
      4 +
   loop ;


\ Format authority: src/habu/aot-lib.f EMIT-DATA-COPY. The image is SPARSE: a u32
\ of groups and a u32 of stored bitmap bytes, then the presence map (one bit a
\ group of 64 bitmap bytes, low bit first), then the groups that map says are
\ there (64 bytes each, one bit a cell), then one unsigned LEB128 per present
\ cell in cell order. Validate the cursor setup that derives the map, the stored
\ groups and the values out of those two counts, the span-base literal, and the
\ complete group + bitmap + cell loops that follow the blob's address.
: CHECK-CURSORS ( n -- n ) {: off:n :}
   off      11 9 0 ENC-LDRW INSTR=
   off 4 +  10 9 4 ENC-LDRW INSTR=
   off 8 +  9 9 8 ENC-ADDI INSTR=
   off 12 + 11 11 7 ENC-ADDI INSTR=
   off 16 + 11 11 3 ENC-LSRI INSTR=
   off 20 + 11 9 11 ENC-ADD INSTR=
   off 24 + 17 11 0 ENC-ADDI INSTR=
   off 28 + 10 17 10 ENC-ADD INSTR=
   off 32 + 13 SKIP-LITERAL ;


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


\ The outer loop walks the presence map a byte at a time and its eight bits one
\ at a time; a clear bit steps the destination one group's worth of DATA (4,096
\ bytes) and a set bit runs the 64-byte walk the group stored. The displacements
\ are the distance in instructions back to each loop head, so they say the shape
\ as surely as the opcodes do.
: CHECK-COPY ( n -- )
   CHECK-CURSORS {: top:n :}
   top       9 11 ENC-CMP INSTR=
   top 4 +   41 C-CS ENC-BCOND INSTR=
   top 8 +   23 9 0 ENC-LDRB INSTR=
   top 12 +  9 9 1 ENC-ADDI INSTR=
   top 16 +  24 8 0 MOVZHW INSTR=
   top 20 +  24 -5 ENC-CBZ INSTR=
   top 24 +  21 23 1 >LIMM ENC-ANDI INSTR=
   top 28 +  21 32 ENC-CBZ INSTR=
   top 32 +  25 64 0 MOVZHW INSTR=
   top 36 +  25 27 ENC-CBZ INSTR=
   top 40 +  14 17 0 ENC-LDRB INSTR=
   top 44 +  17 17 1 ENC-ADDI INSTR=
   top 48 +  25 25 1 ENC-SUBI INSTR=
   top 52 +  14 21 ENC-CBZ INSTR=
   top 56 +  12 8 0 MOVZHW INSTR=
   top 60 +  12 18 ENC-CBZ INSTR=
   top 64 +  21 14 1 >LIMM ENC-ANDI INSTR=
   top 68 +  21 12 ENC-CBZ INSTR=
   top 72 +  15 10 16 22 7 CHECK-VGET {: vend:n :}
   vend      15 13 0 ENC-STR INSTR=
   vend 4 +  14 14 1 ENC-LSRI INSTR=
   vend 8 +  13 13 8 ENC-ADDI INSTR=
   vend 12 + 12 12 1 ENC-SUBI INSTR=
   vend 16 + -17 ENC-B INSTR=
   vend 20 + -24 ENC-B INSTR=
   vend 24 + 13 13 64 ENC-ADDI INSTR=
   vend 28 + -26 ENC-B INSTR=
   vend 32 + 23 23 1 ENC-LSRI INSTR=
   vend 36 + 24 24 1 ENC-SUBI INSTR=
   vend 40 + -33 ENC-B INSTR=
   vend 44 + 21 4096 0 MOVZHW INSTR=
   vend 48 + 13 13 21 ENC-ADD INSTR=
   vend 52 + -5 ENC-B INSTR= ;


\ THE DATA RESTORE, in the four words src/habu/aot-lib.f TEXT-ADR, emits into x9
\ (AOT-STARTUP-SHAPE:TEXT-ADR-SEQ? names them). The four words are read before
\ the test rather than one at a time: SCAN-STARTUP stops at the root BL, which
\ the exit tail follows, so off+12 is inside the range INSTR@ admits at every
\ offset this walks.
: DATA-RESTORE? ( n -- bool ) {: off:n :}
   off INSTR@  off 4 + INSTR@  off 8 + INSTR@  off 12 + INSTR@
   off CODE-OFF 9 AOT-STARTUP-SHAPE:TEXT-ADR-SEQ? ;


\ The pair's two lanes spell the blob's offset from text offset zero, so the
\ image's file offset for it is that offset past the entry.
: DATA-BLOB-OFF ( n -- n ) {: off:n :}
   off INSTR@  off 4 + INSTR@ AOT-STARTUP-SHAPE:TEXT-ADR-OFFSET
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
