\ Decode a fixed-address x86-64 snapshot heap in boot code. The writer places
\ raw bytes or the cell grid after the immutable text footer, then zero padding
\ and the trailer at the end of RX. Nothing in DATA above DATA-START is mapped
\ from the file; that range is zero-backed before this decoder runs.
require lib/byte-buffer.f
require src/habu/cell-grid.f
require src/habu/snapshot-format.f
require src/habu/layout.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/target-layout.f

package X64SNAP
using X64ASM
using X64CODE
private

: SINK ( -- ptr u8 ) ASM-SINK ;
: HEAP-VA ( -- n ) X64LAYOUT:DATA-VA VA>N DATA-START + ;
: MOV, ( r64 r64 -- ) SINK ENC-MOV-RR ;
: LIT, ( r64 n -- ) >IMM64 SINK ENC-MOV-RI64 ;
: ADD, ( r64 r64 -- ) SINK ENC-ADD-RR ;
: SUB, ( r64 r64 -- ) SINK ENC-SUB-RR ;
: CMP, ( r64 r64 -- ) SINK ENC-CMP-RR ;
: CMPI, ( r64 n -- ) >IMM32 SINK ENC-CMP-RI32 ;
: TEST, ( r64 r64 -- ) SINK ENC-TEST-RR ;
: ADDI, ( r64 n -- ) >IMM32 SINK ENC-ADD-RI32 ;
: SUBI, ( r64 n -- ) >IMM32 SINK ENC-SUB-RI32 ;
: ANDI, ( r64 n -- ) >IMM32 SINK ENC-AND-RI32 ;
: OR, ( r64 r64 -- ) SINK ENC-OR-RR ;
: SHL, ( r64 n -- ) >IMM8 SINK ENC-SHL-RI8 ;
: SHR, ( r64 n -- ) >IMM8 SINK ENC-SHR-RI8 ;
: LD, ( r64 r64 n -- ) MEM-OFF SINK ENC-MOV-RM ;
: ST, ( r64 r64 n -- ) MEM-OFF SINK ENC-MOV-MR ;
: LB, ( r64 r64 n -- ) MEM-OFF SINK ENC-MOVZX-8-RM ;
: SB, ( r64 r64 n -- ) {: sr:r64 base:r64 off:n :}
   sr R64>N >R8 base off MEM-OFF SINK ENC-MOV8-MR ;
: ZERO, ( r64 -- ) {: r:r64 :}
   r R64>N >R32 r R64>N >R32 SINK ENC-XOR32-RR ;

\ R8=heap start, R9=trailer start, R10=exact saved DP, R11=heap form.
\ The caller may use every register except RBP, R12 and RSP after the fallthrough.
\ A bad branch first restores this emitter's small kernel-stack frame.
public
: DECODE, ( label -- ) {: bad:label :}
   LBL LBL LBL LBL LBL LBL
   {: fail:label done:label raw:label grid:label pad:label padloop:label :}
   LBL LBL LBL LBL LBL LBL
   {: maploop:label mapbits:label mapnext:label mapdone:label mapclean:label lastok:label :}
   LBL LBL LBL LBL LBL LBL LBL LBL
   {: grouploop:label groupnext:label present:label byteloop:label bitloop:label
      bitnext:label groupdone:label absent:label :}
   LBL LBL LBL LBL LBL LBL LBL
   {: vloop:label vend:label vclean:label full:label tail:label tailloop:label putdone:label :}
   RSP 48 SUBI,
   R8 R9 CMP,  C-A fail JCC,
   RAX HEAP-VA LIT,
   R10 RAX CMP,  C-B fail JCC,
   RAX X64LAYOUT:DATA-VA VA>N X64LAYOUT:DATA-SIZE + LIT,
   R10 RAX CMP,  C-A fail JCC,
   R11 SNAPSHOT-FORMAT:HEAP-RAW CMPI,  C-E raw JCC,
   R11 SNAPSHOT-FORMAT:HEAP-GRID CMPI,  C-E grid JCC,
   fail JMP,

   \ Raw bytes are exactly [DATA-START, DP); the remaining RX bytes are pad.
   raw LBL,
   RAX R10 MOV,  RDI HEAP-VA LIT,  RAX RDI SUB,
   RCX R9 MOV,  RCX R8 SUB,
   RAX RCX CMP,  C-A fail JCC,
   RCX RAX MOV,  RSI R8 MOV,
   $F3 SINK BUF:APPEND-BYTE  $A4 SINK BUF:APPEND-BYTE
   pad JMP,

   \ Grid framing: G groups, S bitmap bytes, ceil(G/8) presence bytes.
   grid LBL,
   RAX R9 MOV,  RAX R8 SUB,
   RAX SNAPSHOT-FORMAT:GRID-FRAME CMPI,  C-B fail JCC,
   R15 R8 0 LD,  RDX R8 8 LD,
   R15 RSP 0 ST,
   RCX R10 MOV,  RAX HEAP-VA LIT,  RCX RAX SUB,
   RCX CELL-GRID:GROUP-SPAN 1- ADDI,
   RCX 12 SHR,
   R15 RCX CMP,  C-A fail JCC,
   R13 R8 MOV,  R13 SNAPSHOT-FORMAT:GRID-FRAME ADDI,
   R13 RSP 8 ST,
   R14 R15 MOV,  R14 7 ADDI,  R14 3 SHR,  R14 R13 ADD,
   R14 R9 CMP,  C-A fail JCC,

   \ Count all present groups and refuse bits above G and a missing final group.
   RBX R13 MOV,  RAX ZERO,
   maploop LBL,
      RBX R14 CMP,  C-AE mapdone JCC,
      R11 RBX 0 LB,  RBX 1 ADDI,
      RCX 8 LIT,
   mapbits LBL,
      R8 R11 MOV,  R8 1 ANDI,
      R8 R8 TEST,  C-E mapnext JCC,
      RAX SINK ENC-INC
   mapnext LBL,
      R11 1 SHR,
      RCX SINK ENC-DEC
      C-NE mapbits JCC,
      maploop JMP,
   mapdone LBL,
   RAX RSP 40 ST,
   RCX R15 MOV,  RCX 7 ANDI,
   RCX RCX TEST,  C-E mapclean JCC,
      R11 R14 -1 LB,  R11 SINK ENC-SHR-CL
      R11 R11 TEST,  C-NE fail JCC,
   mapclean LBL,
   R15 R15 TEST,  C-E lastok JCC,
      RAX R15 MOV,  RAX 1 SUBI,  RAX 3 SHR,  RAX R13 ADD,
      R11 RAX 0 LB,
      RCX R15 MOV,  RCX 1 SUBI,  RCX 7 ANDI,
      R11 SINK ENC-SHR-CL
      R11 1 ANDI,  R11 R11 TEST,  C-E fail JCC,
   lastok LBL,
   RAX RSP 40 LD,
   RAX 6 SHL,  RAX RDX CMP,  C-NE fail JCC,
   RAX R9 MOV,  RAX R14 SUB,
   RDX RAX CMP,  C-A fail JCC,
   RSI R14 MOV,  RSI RDX ADD,  RDX R14 MOV,
   RDI HEAP-VA LIT,
   R15 ZERO,  R15 RSP 16 ST,

   \ Each present group has 64 bitmap bytes. Its set bits name nonzero cells.
   grouploop LBL,
      R15 RSP 16 LD,
      RAX RSP 0 LD,
      R15 RAX CMP,  C-AE groupdone JCC,
      RAX R15 MOV,  RAX 3 SHR,
      R13 RSP 8 LD,  RAX R13 ADD,
      R11 RAX 0 LB,
      RCX R15 MOV,  RCX 7 ANDI,
      R11 SINK ENC-SHR-CL
      R11 1 ANDI,  R11 R11 TEST,  C-E absent JCC,
   present LBL,
      R14 RDI MOV,
      RAX ZERO,  RAX RSP 32 ST,
      RAX CELL-GRID:GROUP-BYTES LIT,  RAX RSP 24 ST,
   byteloop LBL,
      R11 RDX 0 LB,  RDX 1 ADDI,
      RAX RSP 32 LD,  RAX R11 OR,  RAX RSP 32 ST,
      RBX 8 LIT,
   bitloop LBL,
      RAX R11 MOV,  RAX 1 ANDI,  RAX RAX TEST,
      C-E bitnext JCC,
      RAX ZERO,  RCX ZERO,
   vloop LBL,
      RSI R9 CMP,  C-AE fail JCC,
      R13 RSI 0 LB,  RSI 1 ADDI,
      R8 R13 MOV,  R8 $7F ANDI,  R8 SINK ENC-SHL-CL
      RAX R8 OR,
      R8 R13 MOV,  R8 $80 ANDI,  R8 R8 TEST,
      C-E vend JCC,
      RCX 7 ADDI,  RCX 63 CMPI,  C-A fail JCC,
      vloop JMP,
   vend LBL,
      R13 R13 TEST,  C-E fail JCC,
      RCX 63 CMPI,  C-NE vclean JCC,
      R13 1 CMPI,  C-A fail JCC,
   vclean LBL,
      R8 R14 MOV,  R8 CELL-GRID:CELL-BYTES ADDI,
      R8 R10 CMP,  C-BE full JCC,
      R14 R10 CMP,  C-AE fail JCC,
      R13 R10 MOV,  R13 R14 SUB,
      RCX R13 MOV,  RCX 3 SHL,
      R8 RAX MOV,  R8 SINK ENC-SHR-CL
      R8 R8 TEST,  C-NE fail JCC,
      RCX R13 MOV,  R13 R14 MOV,
   tailloop LBL,
      RAX R13 0 SB,
      R13 1 ADDI,  RAX 8 SHR,
      RCX SINK ENC-DEC
      C-NE tailloop JCC,
      putdone JMP,
   full LBL,
      RAX R14 0 ST,
   putdone LBL,
   bitnext LBL,
      R11 1 SHR,  R14 CELL-GRID:CELL-BYTES ADDI,
      RBX SINK ENC-DEC
      C-NE bitloop JCC,
      RAX RSP 24 LD,  RAX SINK ENC-DEC  RAX RSP 24 ST,
      RAX RAX TEST,  C-NE byteloop JCC,
      RAX RSP 32 LD,  RAX RAX TEST,  C-E fail JCC,
   absent LBL,
      RDI CELL-GRID:GROUP-SPAN ADDI,
   groupnext LBL,
      RAX RSP 16 LD,  RAX 1 ADDI,  RAX RSP 16 ST,
      grouploop JMP,
   groupdone LBL,

   \ Both forms finish with fewer than one page of zero bytes before trailer.
   pad LBL,
   RAX R9 MOV,  RAX RSI SUB,
   RAX PROT-PAGE-MAX CMPI,  C-AE fail JCC,
   padloop LBL,
      RSI R9 CMP,  C-AE done JCC,
      RAX RSI 0 LB,  RAX RAX TEST,  C-NE fail JCC,
      RSI 1 ADDI,  padloop JMP,
   fail LBL,  RSP 48 ADDI,  bad JMP,
   done LBL,  RSP 48 ADDI, ;

;using
;using
;package
