\ x86-64-kernel-ffi.f - the FFI rows of the x86-64 kernel
\ (src/habu/kernel-x64.f FFI,) in the booted harness, cross-built for an x86-64
\ peer. Every image seals the friend latch first, so each guard a row calls
\ walks its whole span test and clobbers every scratch register. Each image is
\ one case, and the peer that runs it must see its status:
\
\    hb-x64-kernel-ffi             0  ffi-call on libc getpid, found by DLSYM,,
\                                     answers what the getpid row does; a SysV
\                                     stub the image carries answers its
\                                     argument places, rsp & 15 and al, called
\                                     by ffi-call (8 cells), ffi-call-n (3 and 9
\                                     cells) and ffi-call-bounded (9), each
\                                     also a cell deeper but for ffi-call-n's 3
\    hb-x64-kernel-ffi-negative   21  the same, expecting the wrong difference
\                                     of the two pids
\    hb-x64-kernel-ffi-armed      83  ffi-call-bounded whose ninth argument and
\                                     its extent name a band cell
\    hb-x64-kernel-ffi-call-armed 83  ffi-call whose eighth argument is a band
\                                     cell's address
\
\ No libc symbol takes nine integer arguments, so the stub stands in, as
\ lib/ffi-test.f FFI-T-SUM10 does on ARM64. The host checks each image's ELF
\ header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-FFI
using X64ASM
using X64CODE
using X64RT

\ Scratch the harness hands out (PUSH-SCRATCH,): the argument cells, then the
\ writable extents ffi-call-bounded reads.
$40 constant ARGS
$100 constant EXTS
9 constant PLACES                       \ the most argument cells a case passes
\ SysV aligns rsp to 16 at the call, so the stub's entry finds the return
\ address pushed below that.
8 constant ENTRY-RSP-LOW

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

\ Call a row with the machine stack a cell deeper than ROW leaves it, so the
\ trampoline's alignment takes its other case.
: SHIFTED-ROW ( ptr u8 n -- )
   RAX ASM-SINK ENC-PUSH  ROW  RCX ASM-SINK ENC-POP ;

: SUB, ( -- )                           \ ( x y -- x-y )
   1 G-POP  0 G-POP  RAX RCX ASM-SINK ENC-SUB-RR  0 G-PUSH ;

: PUSH-XT, ( label -- ) {: at:label :}  RAX at MOVABS,  0 G-PUSH ;

\ Store n into the scratch cell at an offset.
: SCRATCH!, ( n n -- ) {: v:n off:n :}
   off AT,  1 G-POP
   RAX v >IMM64 ASM-SINK ENC-MOV-RI64
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ Store the address of the band cell TIER-PROV:N-CELL into the scratch cell at
\ an offset.
: BAND!, ( n -- ) {: off:n :}
   off AT,  1 G-POP
   RAX DATA-REG TIER-PROV:N-CELL MEM-OFF ASM-SINK ENC-LEA
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ Argument cell i holds i + 1, and every extent is a cell.
: ARGS, ( -- )
   PLACES 0 ?do
      i 1+ ARGS i CELL * + SCRATCH!,
      CELL EXTS i CELL * + SCRATCH!,
   loop ;

\ ---- the stub ----------------------------------------------------------------
\ A SysV function of six register and n stack integer arguments. It folds each
\ argument place in order into rax as one hex digit, rax * 16 + the place, then
\ rsp & 15 at its entry as one more, then al at its entry as a byte, so a place
\ out of order, a misaligned call or a dirty al changes its answer.
: FOLD-REG, ( r64 -- ) {: r:r64 :}
   RAX 4 >IMM8 ASM-SINK ENC-SHL-RI8  RAX r ASM-SINK ENC-ADD-RR ;

: FOLD-MEM, ( n -- ) {: off:n :}
   RAX 4 >IMM8 ASM-SINK ENC-SHL-RI8  RAX RSP off MEM-OFF ASM-SINK ENC-ADD-RM ;

: STUB, ( n -- ) {: cells:n :}
   R11 0 >R8 ASM-SINK ENC-MOVZX-8-RR
   RAX RDI ASM-SINK ENC-MOV-RR
   RSI FOLD-REG,  RDX FOLD-REG,  RCX FOLD-REG,  R8 FOLD-REG,  R9 FOLD-REG,
   cells 0 ?do  i 1+ CELL * FOLD-MEM,  loop
   R10 RSP ASM-SINK ENC-MOV-RR  R10 15 >IMM8 ASM-SINK ENC-AND-RI8  R10 FOLD-REG,
   RAX 8 >IMM8 ASM-SINK ENC-SHL-RI8  RAX R11 ASM-SINK ENC-ADD-RR ;

\ What the stub of n places answers when argument cell i holds i + 1.
: STUB-ANSWER ( n -- n ) {: places:n :}
   0  places 0 ?do  16 *  i 1+ +  loop
   16 * ENTRY-RSP-LOW +  256 * ;

: STUB8 ( -- label ) [: 2 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB9 ( -- label ) [: 3 STUB, ;] X64HARNESS:ROUTINE, ;

\ ---- the cases ---------------------------------------------------------------
\ ffi-call on getpid, the pointer from DLSYM, with rsi naming a NUL-terminated
\ copy behind a jump, less the getpid row's answer.
: GETPID, ( -- )
   LBL LBL {: name:label past:label :}
   past JMP,
   name LBL,  s" getpid" BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN  0 ASM-SINK BUF:APPEND-BYTE
   past LBL,
   ARGS AT,  0 N,
   RSI name MOVABS,  X64KERNEL:DLSYM,  0 G-PUSH
   s" ffi-call" ROW
   s" getpid" ROW  SUB,  0 WANT ;

\ ffi-call passes eight cells whatever nargs is, ffi-call-n max(nargs, 8).
: STUB-CALLS, ( label label -- ) {: s8:label s9:label :}
   8 STUB-ANSWER {: want8:n :}
   PLACES STUB-ANSWER {: want9:n :}
   ARGS AT, 8 N, s8 PUSH-XT, s" ffi-call" ROW  want8 WANT
   ARGS AT, 8 N, s8 PUSH-XT, s" ffi-call" SHIFTED-ROW  want8 WANT
   ARGS AT, 3 N, s8 PUSH-XT, s" ffi-call-n" ROW  want8 WANT
   ARGS AT, PLACES N, s9 PUSH-XT, s" ffi-call-n" ROW  want9 WANT
   ARGS AT, PLACES N, s9 PUSH-XT, s" ffi-call-n" SHIFTED-ROW  want9 WANT
   ARGS AT, EXTS AT, PLACES N, s9 PUSH-XT, s" ffi-call-bounded" ROW  want9 WANT
   ARGS AT, EXTS AT, PLACES N, s9 PUSH-XT, s" ffi-call-bounded" SHIFTED-ROW
   want9 WANT ;

\ Ten checks, the harness's whole budget.
: CALLS-CASE ( -- )
   STUB8 STUB9 {: s8:label s9:label :}
   GETPID,
   s8 s9 STUB-CALLS, ;

\ The last argument and its extent name the band cell, so the guard reaches it
\ only by keeping its index across eight guards that pass.
: BOUNDED-ARMED-CASE ( -- )
   STUB9 {: s9:label :}
   ARGS PLACES 1- CELL * + BAND!,
   ARGS AT, EXTS AT, PLACES N, s9 PUSH-XT, s" ffi-call-bounded" ROW
   PLACES STUB-ANSWER WANT ;

: CALL-ARMED-CASE ( -- )
   STUB8 {: s8:label :}
   ARGS 7 CELL * + BAND!,
   ARGS AT, 8 N, s8 PUSH-XT, s" ffi-call" ROW
   8 STUB-ANSWER WANT ;

\ An image: the sealed latch and the argument cells, the case, then the stack
\ checks every case ends with.
: BUILD ( [ -- ] bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   SEAL,  ARGS,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: CALLS-CASE ;] false s" hb-x64-kernel-ffi" TMP-PATH BUILD
   [: CALLS-CASE ;] true s" hb-x64-kernel-ffi-negative" TMP-PATH BUILD
   [: BOUNDED-ARMED-CASE ;] false s" hb-x64-kernel-ffi-armed" TMP-PATH BUILD
   [: CALL-ARMED-CASE ;] false s" hb-x64-kernel-ffi-call-armed" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-FFI:RUN
