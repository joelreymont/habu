\ A real x86-64 boot with both captured entry cells must enter Habu MAIN.
\ MAIN invokes the saved application and then reads one byte of stdin; entering
\ APP directly prints only its first line. Both emitted ELFs remain in HB_TMP
\ as hb-x64-main-entry and hb-x64-app-entry for direct replay.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require test/x86-64-peer-harness.f
require test/gate-common.f

package X64-MAIN-ENTRY-TEST
using X64ASM
using X64CODE
using X64RT

: RB ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

: BUILD ( bool ptr u8 n -- ) {: main?:bool path:ptr u:n :}
   X64HARNESS:INIT
   ASM-RESET
   LBL LBL {: floor:label code-end:label :}
   floor code-end X64BOOT:START,
   LBL LBL LBL {: app:label main:label msg:label :}
   RAX app MOVABS,
   RAX RB APP-ENTRY:XT-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   main? if
      RAX main MOVABS,
      RAX RB ENGINE-MAIN:XT-CELL MEM-OFF ASM-SINK ENC-MOV-MR
   then
   X64BOOT:ENTRY,
   app LBL,
   RDI 1 IMM,  RSI msg MOVABS,  RDX 4 IMM,  NR-WRITE SYS,
   ASM-SINK ENC-RET
   main LBL,
   RAX RB APP-ENTRY:XT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX ASM-SINK ENC-CALL-REG
   RSP 16 >IMM8 ASM-SINK ENC-SUB-RI8
   RDI 0 IMM,  RSI RSP ASM-SINK ENC-MOV-RR  RDX 1 IMM,  NR-READ SYS,
   RDI 1 IMM,  RSI RSP ASM-SINK ENC-MOV-RR  RDX 1 IMM,  NR-WRITE SYS,
   RSP 16 >IMM8 ASM-SINK ENC-ADD-RI8
   ASM-SINK ENC-RET
   msg LBL,  S\" APP\n" BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   floor LBL,  RDI 70 IMM,  NR-EXIT-GROUP SYS,
   code-end LBL,
   path u X64HARNESS:WRITE
   X64HARNESS:DISPOSE ;

: RUN ( -- )
   T-RESET
   s" hb-x64-main-entry" TMP-PATH {: path:ptr u:n :}
   true path u BUILD
   HB-TARGET-LINUX-X86-64? if
      GE-HB-RESET
      path u S\" M" GE-TIMEOUT-MS GE-RUN-STDIN
      s" captured MAIN and APP" GE-EXPECT-OK
      S\" APP\nM" s" captured MAIN and APP" GE-EXPECT-OUT
      s" " s" captured MAIN and APP" GE-EXPECT-ERR
   then
   s" hb-x64-app-entry" TMP-PATH {: app-path:ptr app-u:n :}
   false app-path app-u BUILD
   HB-TARGET-LINUX-X86-64? if
      GE-HB-RESET
      app-path app-u S\" M" GE-TIMEOUT-MS GE-RUN-STDIN
      s" APP without MAIN" GE-EXPECT-OK
      S\" APP\n" s" APP without MAIN" GE-EXPECT-OUT
      s" " s" APP without MAIN" GE-EXPECT-ERR
   then
   T-REPORT ;

RUN

;using
;using
;using
;package
