split-str
\ rt.f - X64RT, x86-64 runtime emitters appending to X64CODE's ASM-SINK: twins
\ of src/habu/rt.f G-PUSH, G-POP, G-OUT, G-PRINT9, G-PRINTU9 and G-EMITC, and
\ habu1.f SYS-PUSH. Moves
\ take x86-64 register numbers, as the seam files
\ proc-watch.f and proc-control.f name them. The seam loads globally here, so a
\ file loading it into a private wordlist (test/x86-64-peer-harness.f) follows.

require src/lib/vector.f
require src/lib/string.f
require src/core/cell.f
require src/core/engine-error.f
require src/habu/layout.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/sys.f

package X64RT

\ ENGINE-GPR:X64-DSTACK by name, not the target-selected ENGINE-GPR:DSTACK: an
\ ARM64 engine cross-builds x86-64 with this file.
: DSP ( -- X64ASM:r64 )  ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;
: DATA-REG ( -- X64ASM:r64 )  ENGINE-GPR:X64-RBASE X64ASM:>R64 ;

\ mov r32, imm32, which zero-extends: a descriptor, a count or a byte.
: MOV32, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 v:n :}
   r X64ASM:R64>N X64ASM:>R32 v X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV32-RI32 ;

: PUT-BYTE, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-DEC
   r X64ASM:R64>N X64ASM:>R8 X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR ;

\ The printers' frame: 32 bytes of machine stack, written downward from its
\ top, which holds any cell's digits with its sign and the newline.
32 constant PRINT-BYTES

public

\ The stack pointer stands just past the top cell and the stack grows upward,
\ as XDS does on ARM64: a push stores at the pointer and then advances it, a
\ pop retreats it and then loads. x86-64 has no write-back addressing, so each
\ is two instructions where ARM64's is one.
: G-PUSH ( n -- ) {: r:n :}
   r X64ASM:>R64  DSP X64ASM:MEM-AT  X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   DSP  CELL X64ASM:>IMM8  X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

: G-POP ( n -- ) {: r:n :}
   DSP  CELL X64ASM:>IMM8  X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   r X64ASM:>R64  DSP X64ASM:MEM-AT  X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;

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
   X64CODE:LBL X64CODE:LBL {: dev:label done:label :}
   X64ASM:RDI DATA-REG GENIO-ABI:OUT-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RDI X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE dev X64CODE:JCC,
   X64ASM:RDI 1 MOV32,  NR-WRITE SYS,
   done X64CODE:JMP,
   dev X64CODE:LBL,
   LGENIOOUT @ >LABEL X64CODE:CALL,
   done X64CODE:LBL, ;

\ Write rax's unsigned decimal digits downward from rsi, leaving rsi at the
\ first. Zero writes "0": the loop divides once before its test. It clobbers
\ rax rcx rdx.
: DIGITS, ( -- )
   X64CODE:LBL {: loop:label :}
   X64ASM:RCX 10 MOV32,
   loop X64CODE:LBL,
   X64ASM:RDX ZERO-REG,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DIV
   X64ASM:RDX [char] 0 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8  X64ASM:RDX PUT-BYTE,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-NE loop X64CODE:JCC, ;

private

\ Open the printers' frame with rsi at its top and the newline below it.
: PRINT-OPEN, ( -- )
   X64ASM:RSP PRINT-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   X64ASM:RSI X64ASM:RSP PRINT-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX STR:LF MOV32,  X64ASM:RCX PUT-BYTE, ;

\ Write the frame from rsi to its top and close it.
: PRINT-CLOSE, ( -- )
   X64ASM:RDX X64ASM:RSP PRINT-BYTES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   G-OUT
   X64ASM:RSP PRINT-BYTES X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

public

\ Print rax as signed decimal and a newline: the twin of G-PRINT9. THE DIGIT
\ LOOP DIVIDES UNSIGNED, so MIN-N, whose negation is MIN-N again, reads as
\ its magnitude 2^63. It clobbers rax rcx rdx rsi rdi r8-r11.
: G-PRINT9 ( -- )
   X64CODE:LBL X64CODE:LBL {: pos:label done:label :}
   PRINT-OPEN,
   X64ASM:R8 ZERO-REG,
   X64ASM:RAX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-GE pos X64CODE:JCC,
   X64ASM:R8 1 MOV32,  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NEG
   pos X64CODE:LBL,
   DIGITS,
   X64ASM:R8 X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-TEST-RR  X64ASM:C-E done X64CODE:JCC,
   X64ASM:RCX [char] - MOV32,  X64ASM:RCX PUT-BYTE,
   done X64CODE:LBL,
   PRINT-CLOSE, ;

\ Print rax as unsigned decimal and a newline: the twin of G-PRINTU9.
: G-PRINTU9 ( -- )
   PRINT-OPEN,  DIGITS,  PRINT-CLOSE, ;

\ Write the byte in al to the output device: the twin of G-EMITC. The byte
\ goes to the machine stack first, because a device takes a span.
: G-EMITC ( -- )
   X64ASM:RSP CELL 2 * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI8
   0 X64ASM:>R8 X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
   X64ASM:RSI X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:RDX 1 MOV32,
   G-OUT
   X64ASM:RSP CELL 2 * X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI8 ;

\ Push rax, or -1 when the carry is set: the twin of habu1.f SYS-PUSH, which
\ pushes x0 or -1. It follows src/os/linux-x86-64/sys.f SYS,, which leaves CF
\ set on error. A mov leaves the flags alone, so the -1 is staged in rcx after
\ the compare and selected without a branch; rcx is free, since `syscall`
\ clobbers it.
: SYS-PUSH ( -- )
   X64ASM:RCX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:C-B X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMOVCC
   0 G-PUSH ;

\ Only filesystem calls update the running task's cause. SYS, leaves CF set for
\ Linux's negative errno; the pushed result is still -1 on error.
: FS-SYS-PUSH ( -- )
   X64CODE:LBL X64CODE:LBL {: failed:label done:label :}
   X64ASM:C-B failed X64CODE:JCC,
   X64ASM:RCX ZERO-REG,  done X64CODE:JMP,
   failed X64CODE:LBL,
   X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-NEG
   X64ASM:RAX -1 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   done X64CODE:LBL,
   X64ASM:RCX DATA-REG FS-ABI:ERRNO-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR
   0 G-PUSH ;

\ An x86-64 stencil is a byte string of whole instructions
\ (src/os/linux-x86-64/sys.f), so it is appended as it stands where ARM64's
\ consumer reassembles four-byte words.
: EMIT-STENCIL ( str -- ) {: a:^ u:n :}
   a u X64CODE:ASM-SINK VEC:APPEND ;

;package
