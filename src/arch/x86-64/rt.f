\ rt.f - x86-64 runtime emitters, package X64RT: the engine's data-stack push
\ and pop, and the consumer of the seam's syscall stencils. They are the twins of
\ src/habu/rt.f G-PUSH / G-POP and src/habu/jit.f C-EMIT-STENCIL, and they
\ append to the current code stream, X64CODE's ASM-SINK.
\
\ The seam files src/os/linux-x86-64/proc-watch.f and proc-control.f name their
\ registers by x86-64 number - 7 rdi, 6 rsi, 2 rdx, 0 rax - so these moves take
\ the number and not an X64ASM register.
\
\ The data-stack register is stated once, as layout.f's ENGINE-GPR:X64-DSTACK:
\ the moves below emit through it, and so does every pass that addresses the
\ caller's stack (src/arch/x86-64/machine.f X64M:DSTACK-GPR, read by
\ src/compiler/native/emit-x64.f DSTACK), so code these moves emit and code the
\ compiler emits share one stack by construction. ARM64 states its register
\ twice, in mnem.f's XDS and layout.f, and src/habu/rt.f DSTACK-AGREE keeps the
\ two together; x86-64 has no second statement to keep. It is read by name and
\ not through the target-selected ENGINE-GPR:DSTACK: this file runs on the ARM64
\ engine that cross-builds x86-64, where DSTACK is ARM64's.

require lib/byte-buffer.f
require src/core/cell.f
require src/habu/layout.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64RT
using X64ASM
using X64CODE

: DSP ( -- r64 )  ENGINE-GPR:X64-DSTACK >R64 ;

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

\ An x86-64 stencil is a byte string of whole instructions
\ (src/os/linux-x86-64/sys.f), so it is appended as it stands where ARM64's
\ consumer reassembles four-byte words. BUF:N>BLEN refuses a negative length.
: EMIT-STENCIL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN ;

;using
;using
;package
