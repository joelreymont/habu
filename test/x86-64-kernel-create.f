\ Runtime CREATE dispatches through the captured handler. A defining word
\ calls it with an existing user stack and source; the call must return with
\ that stack intact. The emitted ELF remains in HB_TMP for peer replay.
require test/x86-64-boot-harness.f

package X64K-CREATE-TEST
using X64ASM
using X64CODE
using X64RT

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

: HANDLER, ( -- label )
   [: RAX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-RM
      RAX ASM-SINK ENC-INC
      RAX DATA-REG X64HARNESS:SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-MR ;]
   X64HARNESS:ROUTINE, ;

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false X64HARNESS:BOOT-OPEN,
   HANDLER, CREATEP-CELL X64HARNESS:LABEL-CELL!,
   41 X64HARNESS:PUSH,
   s" create" X64HARNESS:CALL-ROW,
   1 0 X64HARNESS:EXPECT-SCRATCH,
   41 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-kernel-create" TMP-PATH X64HARNESS:BOOT-CLOSE,
   X64HARNESS:DISPOSE
   T-REPORT ;

RUN

;using
;using
;using
;package
