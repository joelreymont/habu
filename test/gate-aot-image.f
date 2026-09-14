\ Read stripped code, excluding restored DATA and executable-segment padding.
\ Load after gate-build-common.f and aot-call-report-lib.f.
require src/arch/arm64/asm.f

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


\ Format authority: src/habu/aot-lib.f EMIT-DATA-COPY. Validate the literal
\ destination/length registers and the complete byte-copy loop after ADR x9.
: CHECK-COPY ( n -- )
   4 + 10 SKIP-LITERAL 11 SKIP-LITERAL {: off:n :}
   off      12 0 0 MOVZHW INSTR=
   off 4 +  12 11 ENC-CMP INSTR=
   off 8 +  7 C-GE ENC-BCOND INSTR=
   off 12 + 13 9 12 ENC-ADD INSTR=
   off 16 + 13 13 0 ENC-LDRB INSTR=
   off 20 + 14 10 12 ENC-ADD INSTR=
   off 24 + 13 14 0 ENC-STRB INSTR=
   off 28 + 12 12 1 ENC-ADDI INSTR=
   off 32 + -7 ENC-B INSTR= ;


: ADR-X9? ( n -- bool )
   $9F00001F and $10000009 = ;


: ADR-TARGET ( n -- n ) {: off:n :}
   off INSTR@ {: word:n :}
   word 29 rshift 3 and word 5 rshift $7FFFF and 2 lshift or
   $100000 xor $100000 - off + ;


: SCAN-STARTUP ( n -- )
   0 DATA-COUNT !
   begin
      dup INSTR@ BL? 0=
   while
      dup INSTR@ ADR-X9? if
         dup CHECK-COPY
         dup ADR-TARGET DATA-OFF !
         1 DATA-COUNT +!
      then
      4 +
   repeat ROOT-CALL !
   DATA-COUNT @ 1 = IMAGE-CK ;


: STARTUP-END ( -- n )
   \ The exit syscall tail is emitted even though a successful exit never
   \ returns to its Linux errno reconciliation instructions.
   ROOT-CALL @ 4 + 0 0 0 MOVZHW INSTR=
   ROOT-CALL @ 8 + SYS-EMIT-EXIT INSTR=
   ROOT-CALL @ 12 + SYS-EMIT-SVC INSTR=
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
