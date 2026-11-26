\ A real x86-64 boot with both captured entry cells must enter Habu MAIN.
\ MAIN invokes the saved application and then reads one byte of stdin; entering
\ APP directly prints only its first line. Both emitted ELFs remain in TMPDIR
\ as hb-x64-main-entry and hb-x64-app-entry for direct replay. The image has no
\ kernel, so a bare return stands in for the exit actions ENTRY, calls.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require test/x86-64-peer-harness.f
require src/lib/vector.f
require test/gate-common.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/lib/process.f

package X64-MAIN-ENTRY-TEST

: RB ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: IMM, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;

: BUILD ( bool str -- ) {: main?:bool path:str :}
   X64HARNESS:INIT
   X64CODE:ASM-RESET
   X64CODE:LBL X64CODE:LBL {: floor:label code-end:label :}
   floor code-end false X64BOOT:START,
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: app:label main:label msg:label :}
   X64ASM:RAX app X64CODE:MOVABS,
   X64ASM:RAX RB APP-ENTRY:XT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   main? if
      X64ASM:RAX main X64CODE:MOVABS,
      X64ASM:RAX RB ENGINE-MAIN:XT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   then
   X64CODE:LBL X64BOOT:LEXITHOOK !
   X64BOOT:ENTRY,
   app X64CODE:LBL,
   X64ASM:RDI 1 IMM,  X64ASM:RSI msg X64CODE:MOVABS,  X64ASM:RDX 4 IMM,  NR-WRITE SYS,
   X64CODE:ASM-SINK X64ASM:ENC-RET
   main X64CODE:LBL,
   X64ASM:RAX RB APP-ENTRY:XT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CALL-REG
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RDI 0 IMM,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX 1 IMM,  NR-READ SYS,
   X64ASM:RDI 1 IMM,  X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX 1 IMM,  NR-WRITE SYS,
   X64ASM:RSP 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64CODE:ASM-SINK X64ASM:ENC-RET
   X64BOOT:LEXITHOOK @ >LABEL X64CODE:LBL,  X64CODE:ASM-SINK X64ASM:ENC-RET
   msg X64CODE:LBL,  S\" APP\n" X64CODE:ASM-SINK VEC:APPEND
   floor X64CODE:LBL,  X64ASM:RDI 70 IMM,  NR-EXIT-GROUP SYS,
   code-end X64CODE:LBL,
   path X64HARNESS:WRITE
   X64HARNESS:DISPOSE ;

: RUN ( -- )
   T-RESET
   s" hb-x64-main-entry" TMP-PATH {: path:str :}
   true path BUILD
   HB-TARGET-LINUX-X86-64? if
      GE-HB-RESET
      path S\" M" PROCESS:WAIT-FOREVER GE-RUN-STDIN
      s" captured MAIN and APP" GE-EXPECT-OK
      S\" APP\nM" s" captured MAIN and APP" GE-EXPECT-OUT
      s" " s" captured MAIN and APP" GE-EXPECT-ERR
   then
   s" hb-x64-app-entry" TMP-PATH {: app-path:str :}
   false app-path BUILD
   HB-TARGET-LINUX-X86-64? if
      GE-HB-RESET
      app-path S\" M" PROCESS:WAIT-FOREVER GE-RUN-STDIN

      s" APP without MAIN" GE-EXPECT-OK
      S\" APP\n" s" APP without MAIN" GE-EXPECT-OUT
      s" " s" APP without MAIN" GE-EXPECT-ERR
   then
   T-REPORT ;

RUN

;package
