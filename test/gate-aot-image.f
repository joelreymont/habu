\ Read stripped code, excluding restored DATA and executable-segment padding.

require lib/errors.f
require lib/fs.f
require tools/aot-startup-shape.f        \ the startup's instruction shapes, named once
require tools/aot-call-report-lib.f      \ REPORT-JSON-BUFFER for CODE-REPORT
require test/gate-build-common.f

package AOT-IMAGE

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
         dup DATA-BLOB-OFF DATA-OFF !
         1 DATA-COUNT +!
      then
      4 +
   repeat ROOT-CALL !
   DATA-COUNT @ 1 = IMAGE-CK ;


: CHECK-CODE-END ( -- )
   DATA-OFF @ RX-END @ < IMAGE-CK
   DATA-OFF @ 3 and 0= IMAGE-CK
   ROOT-CALL @ INSTR@ $3FFFFFF and $2000000 xor $2000000 - 4 *
   ROOT-CALL @ + {: root:n :}
   \ PLAN-BLOBS places closure record zero (the selected root) first.
   root DATA-OFF @ < IMAGE-CK ;

public

\ EMIT-DATA-BLOB places the startup's source address immediately
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

;package
