\ x86-64-kernel-unit.f - prepared-code publication boundaries
\ in bootable x86-64 ELF images. Run with an ARM host engine; the printed
\ paths are repeatable peer artifacts, whose expected exit statuses are:
\ hb-x64-unit-publish 0,
\ hb-x64-unit-publish-count 83, hb-x64-unit-publish-wid 83.

require test/x86-64-boot-harness.f

package X64K-UNIT
using X64ASM
using X64CODE
using X64RT

DATA-START $20000 + constant RECORD-BUF
$54494E55 constant UNIT-NAME          \ "UNIT", little endian
42 constant ANSWER
6 constant CODE-LEN                  \ mov eax, imm32; ret
16 constant CODE-SLOT

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: CP-REG ( -- r64 ) ENGINE-GPR:X64-CP >R64 ;
: ROW, ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: PUSH, ( n -- ) X64HARNESS:PUSH, ;
: EXPECT, ( n -- ) X64HARNESS:EXPECT-POP, ;

\ A prepared record points at the free code slot. The producer has relocated
\ the code cell and inline name before handing it to the primitive.
: RECORD, ( n -- ) {: wid:n :}
   RAX CP-REG ASM-SINK ENC-MOV-RR
   RAX DATA-REG RECORD-BUF MEM-OFF ASM-SINK ENC-MOV-MR
   4 RECORD-BUF 16 + X64HARNESS:CELL!,
   UNIT-NAME RECORD-BUF 24 + X64HARNESS:CELL!,
   wid RECORD-BUF 40 + X64HARNESS:CELL!, ;

: CODE, ( -- label )
   [: 0 >R32 ANSWER >IMM32 ASM-SINK ENC-MOV32-RI32 ;]
   X64HARNESS:ROUTINE, ;

: PUBLISH, ( label n n -- ) {: code:label len:n count:n :}
   code 0 X64HARNESS:PUSH-LABEL,
   len PUSH,
   RECORD-BUF X64HARNESS:PUSH-DATA,
   count PUSH,
   s" native-unit-publish" ROW, ;

: PUBLISH-CASE, ( -- )
   0 RECORD,
   X64KERNEL:HIDX-BUILD,
   X64HARNESS:REST,
   CODE, CODE-LEN 1 PUBLISH,
   X64HARNESS:PUSH-CP,  DICT-SIZE CODE-SLOT + X64HARNESS:EXPECT-POP-REGION,
   ENGINE-GPR:X64-NDICT G-PUSH  1 EXPECT,
   X64KERNEL:REC-CODE X64HARNESS:PUSH-REGION-CELL,
   DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   X64KERNEL:REC-FLAGS X64HARNESS:PUSH-REGION-CELL,  4 EXPECT,
   X64KERNEL:REC-NAME X64HARNESS:PUSH-REGION-CELL,  UNIT-NAME EXPECT,
   RAX DBASE-REG DICT-SIZE MEM-OFF ASM-SINK ENC-LEA
   RAX ASM-SINK ENC-CALL-REG  0 G-PUSH  ANSWER EXPECT,
   s" UNIT" X64HARNESS:PUSH-TEXT,  0 PUSH,  s" search-wl" ROW,
   DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE X64HARNESS:PROBE,  X64HARNESS:FAULTED EXPECT,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: COUNT-CASE, ( -- )
   0 RECORD,  CODE, CODE-LEN DICT-CAP 1+ PUBLISH, ;

: WID-CASE, ( -- )
   OWNER-API-PUB-WID RECORD,
   1 SEAL-NDICT-CELL X64HARNESS:CELL!,
   CODE, CODE-LEN 1 PUBLISH, ;

: IMAGE ( [ -- ] ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   execute
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: PUBLISH-CASE, ;] s" hb-x64-unit-publish" TMP-PATH IMAGE
   [: COUNT-CASE, ;] s" hb-x64-unit-publish-count" TMP-PATH IMAGE
   [: WID-CASE, ;] s" hb-x64-unit-publish-wid" TMP-PATH IMAGE
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-UNIT:RUN
