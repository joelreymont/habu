\ Startup for the current linker's DATA runs and record image.
require src/habu/boot-x64.f

package X64BOOT
private
using X64ASM
using X64CODE

\ Each DATA run is an offset, a cell count, then the cells.
: COPY-RUNS, ( label label -- )
   {: first:label last:label :}
   LBL LBL LBL {: more:label each:label done:label :}
   R8 first MOVABS, R9 last MOVABS,
   more LBL,
   R8 R9 ASM-SINK ENC-CMP-RR C-AE done JCC,
   R10 R8 MEM-AT ASM-SINK ENC-MOV-RM
   RCX R8 8 MEM-OFF ASM-SINK ENC-MOV-RM
   R11 X64LAYOUT:DATA-VA VA>N IMM,
   R10 R11 ASM-SINK ENC-ADD-RR
   R8 16 >IMM8 ASM-SINK ENC-ADD-RI8
   each LBL,
   RCX RCX ASM-SINK ENC-TEST-RR C-E more JCC,
   RAX R8 MEM-AT ASM-SINK ENC-MOV-RM
   RAX R10 MEM-AT ASM-SINK ENC-MOV-MR
   R8 8 >IMM8 ASM-SINK ENC-ADD-RI8
   R10 8 >IMM8 ASM-SINK ENC-ADD-RI8
   RCX ASM-SINK ENC-DEC each JMP,
   done LBL, ;

: COPY-RECORDS, ( label label -- )
   {: first:label last:label :}
   LBL LBL {: more:label done:label :}
   R8 first MOVABS, R9 last MOVABS,
   R10 DBASE-REG ASM-SINK ENC-MOV-RR
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

: PROTECT-REGION, ( -- )
   RDI DBASE-REG ASM-SINK ENC-MOV-RR
   RSI REGION IMM, RDX PROT-RX IMM,
   NR-MPROTECT SYS, C-B REGION-BAD @ >LABEL JCC, ;

public

\ Both images use fixed ELF addresses. The linker has already relocated
\ their code cells. The returned entry is bound after the kernel is emitted.
: LINK-START, ( bool label label label label -- label label )
   {: engine:bool image:label end:label records:label records-end:label :}
   LBL LBL LBL LBL {: go:label crash:label rest:label stub:label :}
   LBL SCALAR-ROW-LABEL !
   OPEN, CODE-REGION, DATA-REGION,
   image end COPY-RUNS,
   engine if records records-end COPY-RECORDS, then
   RAX X64LAYOUT:DATA-VA VA>N IMM,
   false DATA-INIT,
   crash rest stub SETTLE,
   PROTECT-REGION,
   go JMP,
   REGION-BAD @ >LABEL s\" hb: cannot map or protect code region\n" MAP-FAIL-RC FAIL,
   crash rest stub STUBS,
   SCALAR-MARKER,
   go stub ;

;using
;using
;package
