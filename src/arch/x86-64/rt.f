\ rt.f - X64RT, x86-64 runtime emitters appending to X64CODE's ASM-SINK: twins
\ of src/habu/rt.f G-PUSH, G-POP, G-OUT, G-PRINT9, G-PRINTU9 and G-EMITC, and
\ habu1.f SYS-PUSH. Moves
\ take x86-64 register numbers, as the seam files
\ proc-watch.f and proc-control.f name them. The seam loads globally here, so a
\ file loading it into a private wordlist (test/x86-64-peer-harness.f) follows.

require src/lib/byte-buffer.f
require src/lib/string.f
require src/core/cell.f
require src/core/engine-error.f
require src/habu/layout.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f

package X64RT
using X64ASM
using X64CODE

\ ENGINE-GPR:X64-DSTACK by name, not the target-selected ENGINE-GPR:DSTACK: an
\ ARM64 engine cross-builds x86-64 with this file. X64M:DSTACK-GPR, which
\ src/compiler/native/emit-x64.f reads, is the same declaration, so emitted and
\ compiled code share one stack.
: DSP ( -- r64 )  ENGINE-GPR:X64-DSTACK >R64 ;
: DATA-REG ( -- r64 )  ENGINE-GPR:X64-RBASE >R64 ;

\ mov r32, imm32, which zero-extends: a descriptor, a count or a byte.
: MOV32, ( r64 n -- ) {: r:r64 v:n :}
   r R64>N >R32 v >IMM32 ASM-SINK ENC-MOV32-RI32 ;

: PUT-BYTE, ( r64 -- ) {: r:r64 :}
   RSI ASM-SINK ENC-DEC
   r R64>N >R8 RSI MEM-AT ASM-SINK ENC-MOV8-MR ;

\ The printers' frame: 32 bytes of machine stack, written downward from its
\ top, which holds any cell's digits with its sign and the newline.
32 constant PRINT-BYTES

public

\ The stack pointer stands just past the top cell and the stack grows upward,
\ as XDS does on ARM64 and as the compiler's own crossings assume
\ (emit-x64.f PUT-DMOVE, PUT-DSTORE): a push stores at the pointer and then
\ advances it, a pop retreats it and then loads. x86-64 has no write-back
\ addressing, so each is two instructions where ARM64's is one.
: G-PUSH ( n -- ) {: r:n :}
   r >R64  DSP MEM-AT  ASM-SINK ENC-MOV-MR
   DSP  CELL >IMM8  ASM-SINK ENC-ADD-RI8 ;

: G-POP ( n -- ) {: r:n :}
   DSP  CELL >IMM8  ASM-SINK ENC-SUB-RI8
   r >R64  DSP MEM-AT  ASM-SINK ENC-MOV-RM ;

\ The label of the output funnel's device arm (GENIO-OUT), the twin of habu2.f's
\ LGENIOOUT; X64KERNEL:HELPERS, makes it and emits the arm (src/habu/kernel-x64.f).
\ Entered with rdi the OUT-CELL index, rsi the span and rdx its length.
variable LGENIOOUT

\ Write the span rsi, rdx to the current output device: the twin of G-OUT.
\ THE TERMINAL PATH PAYS ONE LOAD AND ONE COMPARE: OUT-CELL zero writes fd 1
\ in place, and any other index leaves for LGENIOOUT with the index in rdi,
\ where the write would have put its descriptor. It clobbers rax rcx rdx rsi
\ rdi r8-r11 and no VM register, since the device arm calls a compiled xt.
: G-OUT ( -- )
   LBL LBL {: dev:label done:label :}
   RDI DATA-REG GENIO-ABI:OUT-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RDI RDI ASM-SINK ENC-TEST-RR  C-NE dev JCC,
   RDI 1 MOV32,  NR-WRITE SYS,
   done JMP,
   dev LBL,
   LGENIOOUT @ >LABEL CALL,
   done LBL, ;

\ Write rax's unsigned decimal digits downward from rsi, leaving rsi at the
\ first. Zero writes "0": the loop divides once before its test. It clobbers
\ rax rcx rdx.
: DIGITS, ( -- )
   LBL {: loop:label :}
   RCX 10 MOV32,
   loop LBL,
   RDX ZERO-REG,  RCX ASM-SINK ENC-DIV
   RDX [char] 0 >IMM8 ASM-SINK ENC-ADD-RI8  RDX PUT-BYTE,
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE loop JCC, ;

private

\ Open the printers' frame with rsi at its top and the newline below it.
: PRINT-OPEN, ( -- )
   RSP PRINT-BYTES >IMM8 ASM-SINK ENC-SUB-RI8
   RSI RSP PRINT-BYTES MEM-OFF ASM-SINK ENC-LEA
   RCX STR-LF MOV32,  RCX PUT-BYTE, ;

\ Write the frame from rsi to its top and close it.
: PRINT-CLOSE, ( -- )
   RDX RSP PRINT-BYTES MEM-OFF ASM-SINK ENC-LEA
   RDX RSI ASM-SINK ENC-SUB-RR
   G-OUT
   RSP PRINT-BYTES >IMM8 ASM-SINK ENC-ADD-RI8 ;

public

\ Print rax as signed decimal and a newline: the twin of G-PRINT9. THE DIGIT
\ LOOP DIVIDES UNSIGNED, so MIN-N, whose negation is MIN-N again, reads as
\ its magnitude 2^63. It clobbers rax rcx rdx rsi rdi r8-r11.
: G-PRINT9 ( -- )
   LBL LBL {: pos:label done:label :}
   PRINT-OPEN,
   R8 ZERO-REG,
   RAX RAX ASM-SINK ENC-TEST-RR  C-GE pos JCC,
   R8 1 MOV32,  RAX ASM-SINK ENC-NEG
   pos LBL,
   DIGITS,
   R8 R8 ASM-SINK ENC-TEST-RR  C-E done JCC,
   RCX [char] - MOV32,  RCX PUT-BYTE,
   done LBL,
   PRINT-CLOSE, ;

\ Print rax as unsigned decimal and a newline: the twin of G-PRINTU9.
: G-PRINTU9 ( -- )
   PRINT-OPEN,  DIGITS,  PRINT-CLOSE, ;

\ Write the byte in al to the output device: the twin of G-EMITC. The byte
\ goes to the machine stack first, because a device takes a span.
: G-EMITC ( -- )
   RSP CELL 2 * >IMM8 ASM-SINK ENC-SUB-RI8
   0 >R8 RSP MEM-AT ASM-SINK ENC-MOV8-MR
   RSI RSP ASM-SINK ENC-MOV-RR  RDX 1 MOV32,
   G-OUT
   RSP CELL 2 * >IMM8 ASM-SINK ENC-ADD-RI8 ;

\ Push rax, or -1 when the carry is set: the twin of habu1.f SYS-PUSH, which
\ pushes x0 or -1. It follows src/os/linux-x86-64/sys.f SYS,, which leaves CF
\ set on error. A mov leaves the flags alone, so the -1 is staged in rcx after
\ the compare and selected without a branch; rcx is free, since `syscall`
\ clobbers it.
: SYS-PUSH ( -- )
   RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   C-B RAX RCX ASM-SINK ENC-CMOVCC
   0 G-PUSH ;

\ Only filesystem calls update the running task's cause. SYS, leaves CF set for
\ Linux's negative errno; the pushed result is still -1 on error.
: FS-SYS-PUSH ( -- )
   LBL LBL {: failed:label done:label :}
   C-B failed JCC,
   RCX ZERO-REG,  done JMP,
   failed LBL,
   RCX RAX ASM-SINK ENC-MOV-RR
   RCX ASM-SINK ENC-NEG
   RAX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   done LBL,
   RCX DATA-REG FS-ABI:ERRNO-OFF MEM-OFF ASM-SINK ENC-MOV-MR
   0 G-PUSH ;

\ An x86-64 stencil is a byte string of whole instructions
\ (src/os/linux-x86-64/sys.f), so it is appended as it stands where ARM64's
\ consumer reassembles four-byte words.
: EMIT-STENCIL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN ;

;using
;using
;package
