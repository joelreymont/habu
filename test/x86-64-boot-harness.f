\ x86-64-boot-harness.f - the booted harness: an x86-64 image that boots as the
\ engine does, carries the whole kernel and runs one case against the DATA the
\ boot mapped. An image is staged in this order:
\
\    BOOT-OPEN,  case  BOOT-CLOSE,
\
\ BOOT-OPEN, emits X64BOOT's `_start`, a jump over the kernel X64KERNEL:KERNEL,
\ emits next, and the case's entry past it; BOOT-CLOSE, exits 0 and writes the
\ image. A case stages cells, arms DATA cells, calls kernel rows by name and
\ checks a popped cell, the data stack's depth, the machine stack's balance
\ and a scratch cell. The checks exit FIRST-CASE upward in the order they are
\ staged, as the peer harness numbers its case checks; a negative image
\ (BOOT-OPEN, given true) expects a wrong answer from its first check, so it
\ exits FIRST-CASE. A row that ends the process ends the image with its own
\ status.
\
\ The peer harness's own entry (test/x86-64-peer-harness.f OPEN,) cannot run a
\ kernel body: its rbp is a 1 KiB window of the machine stack, CASE1, resets
\ r12 to rbp and CLOSE, pins r13-r15, where a body reads DATA through rbp and
\ moves those registers.
\
\ boot-x64.f and kernel-x64.f load the x86-64 seam globally, so they come
\ before the peer harness, which would otherwise load it into its own private
\ wordlist.
require src/habu/boot-x64.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require test/x86-64-peer-harness.f

package X64HARNESS
using X64ASM
using X64CODE
using X64RT

\ Scratch cells past the heap floor, where nothing a booted image runs
\ allocates.
DATA-START $10000 + constant BOOT-SCRATCH-OFF
variable ENTRY-CELL

: ENTRY-LBL ( -- label ) ENTRY-CELL @ >LABEL ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;

public

\ Begin an image: the boot, then the kernel with fresh labels and a fresh
\ registry, since labels die with the stream the last image linked and
\ registry rows do not.
: BOOT-OPEN, ( bool -- ) {: negative:bool :}
   ASM-RESET
   LBL EXIT-CELL !  LBL ENTRY-CELL !
   FIRST-CASE CASE-NEXT !
   negative if FIRST-CASE else 0 then WRONG-AT !
   X64BOOT:START,
   ENTRY-LBL JMP,
   ENGINE-PRIMS:RESET
   X64KERNEL:KERNEL,
   ENTRY-LBL LBL, ;

: PUSH, ( n -- ) {: v:n :}  RAX v IMM  0 G-PUSH ;

\ Push a string's address and length. Its bytes sit in the text behind a jump.
: PUSH-TEXT, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   past LBL,
   RAX text MOVABS,  0 G-PUSH
   u PUSH, ;

\ Push the address of the scratch cell n bytes in.
: PUSH-SCRATCH, ( n -- ) {: off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-LEA
   0 G-PUSH ;

\ Store n into the DATA cell at an offset: arm the friend latch, a live task or
\ the exit hook before a row reads it.
: CELL!, ( n n -- ) {: v:n off:n :}
   RAX v IMM
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Call the kernel row registered under the name.
: CALL-ROW, ( ptr u8 n -- ) X64KERNEL:ENTRY-LABEL CALL, ;

\ Pop a cell and check it.
: EXPECT-POP, ( n -- ) {: want:n :}  0 G-POP  want EXPECT, ;

\ Check the data stack holds n cells above the base the boot published.
: EXPECT-DEPTH, ( n -- ) {: want:n :}
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   want CELL * EXPECT, ;

\ Check rsp stands where the kernel left it, at argc: one cell below the
\ argument vector the boot published.
: EXPECT-BALANCED, ( -- )
   RAX DATA-REG ARGV-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RSP ASM-SINK ENC-SUB-RR
   CELL EXPECT, ;

\ Check the scratch cell at an offset holds n.
: EXPECT-SCRATCH, ( n n -- ) {: want:n off:n :}
   RAX DATA-REG BOOT-SCRATCH-OFF off + MEM-OFF ASM-SINK ENC-MOV-RM
   want EXPECT, ;

\ End an image: exit 0 when every check held, then write it.
: BOOT-CLOSE, ( ptr u8 n -- )
   RDI ZERO-REG,
   WRITE-ELF ;

;using
;using
;using
;package
