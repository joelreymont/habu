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
using X64ASM
using X64CODE

\ Move each run [off][bytes] in [r9, last) from the carrier, rdi at its pages,
\ to DATA-VA + off; the syscall keeps rdi, rsi and r9. A failed move dies.
: MOVE-RUNS, ( label label -- )
   {: first:label last:label :}
   LBL LBL LBL {: more:label bad:label done:label :}
   RDI LINUX-LAYOUT:CARRIER-VA VA>N IMM,
   R9 first MOVABS,
   more LBL,
   RAX last MOVABS,
   R9 RAX ASM-SINK ENC-CMP-RR C-AE done JCC,
   R8 R9 MEM-AT ASM-SINK ENC-MOV-RM
   RSI R9 8 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX LINUX-LAYOUT:DATA-VA VA>N IMM,
   R8 RAX ASM-SINK ENC-ADD-RR
   RDX RSI ASM-SINK ENC-MOV-RR
   R10 MREMAP-MOVE-FIXED IMM,
   NR-MREMAP SYS,
   RAX R8 ASM-SINK ENC-CMP-RR C-NE bad JCC,
   RDI RSI ASM-SINK ENC-ADD-RR
   R9 16 >IMM8 ASM-SINK ENC-ADD-RI8
   more JMP,
   bad S\" hb: cannot move the image's data into the fixed data region\n" EXIT-STATUS:SYSTEM FAIL,
   done LBL, ;

: COPY-RECORDS, ( label label -- )
   {: first:label last:label :}
   LBL LBL {: more:label done:label :}
   R8 first MOVABS, R9 last MOVABS,
   R10 DBASE-REG ASM-SINK ENC-MOV-RR  R10 DELTA-REG ASM-SINK ENC-ADD-RR
   more LBL,
   R8 R9 ASM-SINK ENC-CMP-RR C-AE done JCC,
   DREC 0 ?do
      RAX R8 i MEM-OFF ASM-SINK ENC-MOV-RM
      RAX R10 i MEM-OFF ASM-SINK ENC-MOV-MR
   CELL +loop
   R8 DREC >IMM8 ASM-SINK ENC-ADD-RI8
   R10 DREC >IMM8 ASM-SINK ENC-ADD-RI8
   ENGINE-GPR:X64-NDICT >R64 ASM-SINK ENC-INC
   more JMP,
   done LBL, ;

public

\ Both images use fixed ELF addresses. The linker has already relocated
\ their code cells. The returned entry is bound after the kernel is emitted.
: LINK-START, ( bool label label label label -- label label )
   {: engine:bool image:label end:label records:label records-end:label :}
   LBL LBL LBL LBL LBL {: go:label crash:label rest:label stub:label trap:label :}
   OPEN, CODE-REGION, DATA-REGION,
   image end MOVE-RUNS,
   engine if records records-end COPY-RECORDS, then
   RAX LINUX-LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,  ALIAS-DELTA,
   crash rest stub SETTLE,
   engine if TRAMPOLINES, trap rest TRAP, then
   go JMP,
   crash rest stub STUBS,
   engine if trap crash BP-HANDLER, then
   go stub ;

;using
;using
;package
