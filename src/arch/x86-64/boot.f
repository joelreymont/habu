\ Startup for the current linker's DATA runs and record image.
require src/core/exit-status.f
require src/habu/layout.f
require src/os/linux/layout.f
require src/habu/boot-x64.f
require src/os/linux-x86-64/sys.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/asm.f

package X64BOOT
private

\ Move each run [off][bytes] in [r9, last) from the carrier, rdi at its pages,
\ to DATA-VA + off; the syscall keeps rdi, rsi and r9. A failed move dies.
: MOVE-RUNS, ( label label -- )
   {: first:label last:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: more:label bad:label done:label :}
   X64ASM:RDI LINUX-LAYOUT:CARRIER-VA VA>N IMM,
   X64ASM:R9 first X64CODE:MOVABS,
   more X64CODE:LBL,
   X64ASM:RAX last X64CODE:MOVABS,
   X64ASM:R9 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR X64ASM:C-AE done X64CODE:JCC,
   X64ASM:R8 X64ASM:R9 X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RSI X64ASM:R9 8 X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX LINUX-LAYOUT:DATA-VA VA>N IMM,
   X64ASM:R8 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:R10 MREMAP-MOVE-FIXED IMM,
   NR-MREMAP SYS,
   X64ASM:RAX X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR X64ASM:C-NE bad X64CODE:JCC,
   X64ASM:RDI X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:R9 16 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   more X64CODE:JMP,
   bad S\" hb: cannot move the image's data into the fixed data region\n" EXIT-STATUS:SYSTEM FAIL,
   done X64CODE:LBL, ;

: COPY-RECORDS, ( label label -- )
   {: first:label last:label :}
   X64CODE:LBL X64CODE:LBL {: more:label done:label :}
   X64ASM:R8 first X64CODE:MOVABS, X64ASM:R9 last X64CODE:MOVABS,
   X64ASM:R10 DBASE-REG X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 DELTA-REG X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   more X64CODE:LBL,
   X64ASM:R8 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR X64ASM:C-AE done X64CODE:JCC,
   DREC 0 ?do
      X64ASM:RAX X64ASM:R8 i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
      X64ASM:RAX X64ASM:R10 i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   CELL +loop
   X64ASM:R8 DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   X64ASM:R10 DREC X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8
   ENGINE-GPR:X64-NDICT X64ASM:>R64 X64CODE:ASM-SINK X64ASM:ENC-INC
   more X64CODE:JMP,
   done X64CODE:LBL, ;

public

\ Both images use fixed ELF addresses. The linker has already relocated
\ their code cells. The returned entry is bound after the kernel is emitted.
: LINK-START, ( bool label label label label -- label label )
   {: engine:bool image:label end:label records:label records-end:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: go:label crash:label rest:label stub:label trap:label :}
   OPEN, CODE-REGION, DATA-REGION,
   image end MOVE-RUNS,
   engine if records records-end COPY-RECORDS, then
   X64ASM:RAX LINUX-LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,  ALIAS-DELTA,
   crash rest stub SETTLE,
   engine if TRAMPOLINES, trap rest TRAP, then
   go X64CODE:JMP,
   crash rest stub STUBS,
   engine if trap crash BP-HANDLER, then
   go stub ;

;package
