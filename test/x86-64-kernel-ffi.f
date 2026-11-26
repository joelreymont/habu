\ x86-64-kernel-ffi.f - the FFI rows of src/habu/kernel-x64.f FFI, in the booted
\ harness, cross-built for an x86-64 peer: one case per image. No libc symbol
\ takes nine integer arguments, so a stub stands in.
require test/x86-64-boot-harness.f
require src/lib/vector.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-FFI

\ Scratch the harness hands out (PUSH-SCRATCH,): the argument cells, then the
\ ABI rows' fpbuf and stackbuf and snprintf's buffer.
$40 constant ARGS
$180 constant FPS
$1C0 constant STK
$200 constant OUT
9 constant PLACES                       \ the most argument cells a case passes
6 constant REG-PLACES                   \ rdi rsi rdx rcx r8 r9
3 constant STACK-PLACES                 \ PLACES less REG-PLACES
8 constant FLOATS                       \ xmm0..7
8 constant ABI-AL                       \ al the ABI rows set: xmm0..7's bound
$F constant JUNK                        \ the ABI rows' argbuf[6..9): no place
\ SysV aligns rsp to 16 at the call, so the stub's entry finds the return
\ address pushed below that.
8 constant ENTRY-RSP-LOW

: ROW ( str -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

\ Call a row with the machine stack a cell deeper than ROW leaves it, so the
\ trampoline's alignment takes its other case.
: SHIFTED-ROW ( str -- )
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  ROW  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-POP ;

: SUB, ( -- )                           \ ( x y -- x-y )
   1 X64RT:G-POP  0 X64RT:G-POP  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  0 X64RT:G-PUSH ;

: PUSH-XT, ( label -- ) {: at:label :}  X64ASM:RAX at X64CODE:MOVABS,  0 X64RT:G-PUSH ;

: SCRATCH!, ( n n -- ) {: v:n off:n :}
   off AT,  1 X64RT:G-POP
   X64ASM:RAX v X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ Argument cell i holds i + 1.
: ARGS, ( -- )
   PLACES 0 ?do  i 1+ ARGS i CELL * + SCRATCH!,  loop ;

: SCRATCH-AT!, ( n n -- ) {: at:n off:n :}
   off AT,  at AT,  0 X64RT:G-POP  1 X64RT:G-POP
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: LABEL!, ( label n -- ) {: at:label off:n :}
   off AT,  1 X64RT:G-POP  X64ASM:RAX at X64CODE:MOVABS,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ The ABI rows' cells: fpbuf cell i holds i + 1 and stackbuf the nine-place
\ stub's last three places, which argbuf's cells past the registers must not
\ stand in for.
: ABI-ARGS, ( -- )
   FLOATS 0 ?do  i 1+ FPS i CELL * + SCRATCH!,  loop
   STACK-PLACES 0 ?do
      REG-PLACES i + 1+ STK i CELL * + SCRATCH!,
      JUNK ARGS REG-PLACES i + CELL * + SCRATCH!,
   loop ;

\ Push ffi-call-abi's arguments below the function: nstack stackbuf cells.
: ABI, ( n -- ) {: nstack:n :}
   ARGS AT, FPS AT, STK AT, nstack N, ;

\ A NUL-terminated copy of a string behind a jump, and its label.
: C-TEXT, ( str -- label ) {: s:str :}
   X64CODE:LBL X64CODE:LBL {: text:label past:label :}
   past X64CODE:JMP,
   text X64CODE:LBL,  s X64CODE:ASM-SINK VEC:APPEND  0 X64CODE:ASM-SINK VEC:PUSH c!
   past X64CODE:LBL,
   text ;

: LIBC, ( str -- )
   C-TEXT, {: name:label :}
   X64ASM:RSI name X64CODE:MOVABS,  X64KERNEL:DLSYM,  0 X64RT:G-PUSH ;

\ A SysV function of six register and n stack integer arguments. It folds each
\ argument place in order into rax as one hex digit, rax * 16 + the place, then
\ rsp & 15 at its entry as one more, then al at its entry as a byte, so a place
\ out of order, a misaligned call or a dirty al changes its answer.
: FOLD-REG, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX r X64CODE:ASM-SINK X64ASM:ENC-ADD-RR ;

: FOLD-MEM, ( n -- ) {: off:n :}
   X64ASM:RAX 4 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX X64ASM:RSP off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM ;

: STUB, ( n -- ) {: cells:n :}
   X64ASM:R11 0 X64ASM:>R8 X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RR
   X64ASM:RAX X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   X64ASM:RSI FOLD-REG,  X64ASM:RDX FOLD-REG,  X64ASM:RCX FOLD-REG,  X64ASM:R8 FOLD-REG,  X64ASM:R9 FOLD-REG,
   cells 0 ?do  i 1+ CELL * FOLD-MEM,  loop
   X64ASM:R10 X64ASM:RSP X64CODE:ASM-SINK X64ASM:ENC-MOV-RR  X64ASM:R10 15 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8  X64ASM:R10 FOLD-REG,
   X64ASM:RAX 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR ;

\ What the stub of n places answers when argument cell i holds i + 1.
: STUB-ANSWER ( n -- n ) {: places:n :}
   0  places 0 ?do  16 *  i 1+ +  loop
   16 * ENTRY-RSP-LOW +  256 * ;

: STUB6 ( -- label ) [: 0 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB7 ( -- label ) [: 1 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB8 ( -- label ) [: 2 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB9 ( -- label ) [: 3 STUB, ;] X64HARNESS:ROUTINE, ;

\ A SysV function of eight double arguments. It folds the bits of xmm0..7 in
\ order into rax as one hex digit each, then al at its entry as a byte, and
\ answers the fold in xmm0 and its complement in rax, so a register out of
\ order, a dirty al or the wrong answer register changes what a row pushes.
: FSTUB, ( -- )
   X64ASM:R11 0 X64ASM:>R8 X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RR
   X64ASM:RAX X64ASM:XMM0 X64CODE:ASM-SINK X64ASM:ENC-MOVQ-RX
   FLOATS 1 ?do  X64ASM:RCX i X64ASM:>XMM X64CODE:ASM-SINK X64ASM:ENC-MOVQ-RX  X64ASM:RCX FOLD-REG,  loop
   X64ASM:RAX 8 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-SHL-RI8  X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:XMM0 X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-NOT ;

\ The float stub's fold when fpbuf cell i holds i + 1, through an ABI row.
: FSTUB-ANSWER ( -- n )
   0  FLOATS 0 ?do  16 *  i 1+ +  loop
   256 * ABI-AL + ;

: FSTUB ( -- label ) [: FSTUB, ;] X64HARNESS:ROUTINE, ;

\ ffi-call on getpid, the pointer from DLSYM,, less the getpid row's answer.
: GETPID, ( -- )
   ARGS AT,  0 N,  s" getpid" LIBC,
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
   ARGS AT, PLACES N, s9 PUSH-XT, s" ffi-call-n" SHIFTED-ROW  want9 WANT ;

\ Cells 1..k, 1 the deepest, and the stub: ffi-call-k's operands.
: K-ARGS, ( label n -- ) {: s:label k:n :}
   k 0 ?do  i 1+ N,  loop  s PUSH-XT, ;

\ ffi-call-0 on getpid, less the getpid row's answer.
: GETPID-0, ( -- )
   s" getpid" LIBC,  s" ffi-call-0" ROW
   s" getpid" ROW  SUB,  0 WANT ;

\ ffi-call-6 passes registers only; ffi-call-7 and -8 one and two
\ machine-stack cells, at both rsp parities.
: K-CALLS, ( label label label -- ) {: s6:label s7:label s8:label :}
   6 STUB-ANSWER 7 STUB-ANSWER 8 STUB-ANSWER {: w6:n w7:n w8:n :}
   s6 6 K-ARGS, s" ffi-call-6" ROW  w6 WANT
   s7 7 K-ARGS, s" ffi-call-7" ROW  w7 WANT
   s7 7 K-ARGS, s" ffi-call-7" SHIFTED-ROW  w7 WANT
   s8 8 K-ARGS, s" ffi-call-8" ROW  w8 WANT
   s8 8 K-ARGS, s" ffi-call-8" SHIFTED-ROW  w8 WANT ;

: CALLS-CASE ( -- )
   STUB8 STUB9 {: s8:label s9:label :}
   GETPID,
   s8 s9 STUB-CALLS, ;

: K-CASE ( -- )
   STUB6 STUB7 STUB8 {: s6:label s7:label s8:label :}
   GETPID-0,
   s6 s7 s8 K-CALLS, ;

\ The ABI rows: the float stub through both, whose plain row answers rax and
\ -r row xmm0; the nine-place stub with three stackbuf cells, at both rsp
\ parities; the stub of register places with nstack -1, which passes nothing
\ on the stack.
: ABI-CASE ( -- )
   FSTUB STUB6 STUB9 {: f:label s6:label s9:label :}
   FSTUB-ANSWER {: fwant:n :}
   PLACES STUB-ANSWER ABI-AL + {: want9:n :}
   REG-PLACES STUB-ANSWER ABI-AL + {: want6:n :}
   ABI-ARGS,
   0 ABI, f PUSH-XT, s" ffi-call-abi" ROW  fwant invert WANT
   0 ABI, f PUSH-XT, s" ffi-call-abi-r" ROW  fwant WANT
   STACK-PLACES ABI, s9 PUSH-XT, s" ffi-call-abi" ROW  want9 WANT
   STACK-PLACES ABI, s9 PUSH-XT, s" ffi-call-abi" SHIFTED-ROW  want9 WANT
   -1 ABI, s6 PUSH-XT, s" ffi-call-abi" ROW  want6 WANT ;

\ snprintf(buf, 8, "%.1f %.1f", 1.5, 2.5): the doubles' bits, what it answers,
\ and the eight bytes it leaves in buf, the text and its NUL, as a
\ little-endian cell.
$3FF8000000000000 constant ONE-HALF       \ 1.5
$4004000000000000 constant TWO-HALF       \ 2.5
8 constant OUT-BYTES
7 constant PRINTED
$00302E3020302E30 constant ZEROS-TEXT     \ "0.0 0.0"
$00352E3220352E31 constant FLOATS-TEXT    \ "1.5 2.5"
64 constant ZEROED-CELLS

\ Zero the cells below rsp, where snprintf's register save area will sit: it
\ stores xmm0..7 there only when al is nonzero, and va_arg reads the doubles
\ back from it.
: ZERO-BELOW, ( -- )
   X64ASM:RAX ZERO-REG,
   ZEROED-CELLS 0 ?do  X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-PUSH  loop
   X64ASM:RSP ZEROED-CELLS CELL * X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32 ;

: XMM-BITS, ( X64ASM:xmm n -- ) {: x:X64ASM:xmm v:n :}
   X64ASM:RAX v X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64  x X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOVQ-XR ;

\ The control first: xmm0 and xmm1 loaded and ffi-call-n's al of 0, so
\ snprintf reads its doubles from the zeroed slots. Then ffi-call-abi, whose al
\ of 8 makes it save them.
: SNPRINTF-CASE ( -- )
   s" %.1f %.1f" C-TEXT, {: fmt:label :}
   OUT ARGS SCRATCH-AT!,
   OUT-BYTES ARGS CELL + SCRATCH!,
   fmt ARGS 2 CELL * + LABEL!,
   ONE-HALF FPS SCRATCH!,  TWO-HALF FPS CELL + SCRATCH!,
   ARGS AT, 3 N, s" snprintf" LIBC,
   ZERO-BELOW,  X64ASM:XMM0 ONE-HALF XMM-BITS,  X64ASM:XMM1 TWO-HALF XMM-BITS,
   s" ffi-call-n" ROW  PRINTED WANT
   ZEROS-TEXT OUT X64HARNESS:EXPECT-SCRATCH,
   0 ABI, s" snprintf" LIBC, s" ffi-call-abi" ROW  PRINTED WANT
   FLOATS-TEXT OUT X64HARNESS:EXPECT-SCRATCH, ;

: BUILD ( [ -- ] bool str -- ) {: negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   ARGS,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   [: CALLS-CASE ;] false s" hb-x64-kernel-ffi" TMP-PATH BUILD
   [: CALLS-CASE ;] true s" hb-x64-kernel-ffi-negative" TMP-PATH BUILD
   [: K-CASE ;] false s" hb-x64-kernel-ffi-k" TMP-PATH BUILD
   [: ABI-CASE ;] false s" hb-x64-kernel-ffi-abi" TMP-PATH BUILD
   [: SNPRINTF-CASE ;] false s" hb-x64-kernel-ffi-snprintf" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-FFI:RUN
