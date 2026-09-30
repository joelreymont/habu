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
\    hb-x64-kernel-ffi-abi         0  the ABI-planned rows: a stub folding
\                                     xmm0..7 and al answers the fold in xmm0
\                                     and its complement in rax, called by all
\                                     four; the nine-place stub called by
\                                     ffi-call-abi and ffi-call-abi-bounded
\                                     with its last three places in stackbuf
\                                     and junk in argbuf[6..9), the second a
\                                     cell deeper; a stub of register places
\                                     called by both with nstack -1
\    hb-x64-kernel-ffi-snprintf    0  libc snprintf(buf, 8, "%.1f %.1f", 1.5,
\                                     2.5) over zeroed stack: through
\                                     ffi-call-n, whose al is 0, it prints
\                                     0.0 0.0; through ffi-call-abi-bounded,
\                                     whose al is 8, 1.5 2.5
\    hb-x64-kernel-ffi-abi-armed  83  ffi-call-abi-bounded whose third stack
\                                     cell and its stkext entry name a band
\                                     cell, the regext entry of the same index
\                                     0
\    hb-x64-kernel-ffi-sret-armed 83  ffi-call-abi with sret set and argbuf[8]
\                                     a band cell's address
\    hb-x64-kernel-ffi-regext-armed
\                                 83  ffi-call-abi-bounded whose fifth register
\                                     slot and its regext entry name a band
\                                     cell, the stack cells clean
\    hb-x64-kernel-ffi-nint-armed 83  ffi-call-abi, sret 0, whose sixth
\                                     argument, below nint, is a band cell's
\                                     address
\
\ No libc symbol takes nine integer arguments, so the stub stands in, as
\ lib/ffi-test.f FFI-T-SUM10 does on ARM64. The host checks each image's ELF
\ header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-FFI
using X64ASM
using X64CODE
using X64RT

\ Scratch the harness hands out (PUSH-SCRATCH,): the argument cells, the
\ writable extents ffi-call-bounded reads, a stack extent table apart from
\ them, then the ABI rows' fpbuf and stackbuf and snprintf's buffer.
$40 constant ARGS
$100 constant EXTS
$160 constant STK-EXTS
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

\ Store the address of the scratch cell at the first offset into the one at
\ the second.
: SCRATCH-AT!, ( n n -- ) {: at:n off:n :}
   off AT,  at AT,  0 G-POP  1 G-POP
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ Store a label's address into the scratch cell at an offset.
: LABEL!, ( label n -- ) {: at:label off:n :}
   off AT,  1 G-POP  RAX at MOVABS,
   RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

\ The ABI rows' cells: fpbuf cell i holds i + 1 and stackbuf the nine-place
\ stub's last three places, which argbuf's cells past the registers must not
\ stand in for.
: ABI-ARGS, ( -- )
   FLOATS 0 ?do  i 1+ FPS i CELL * + SCRATCH!,  loop
   STACK-PLACES 0 ?do
      REG-PLACES i + 1+ STK i CELL * + SCRATCH!,
      JUNK ARGS REG-PLACES i + CELL * + SCRATCH!,
   loop ;

\ Push ffi-call-abi's arguments below the function: nstack stackbuf cells,
\ the register places live, no sret.
: ABI, ( n -- ) {: nstack:n :}
   ARGS AT, FPS AT, STK AT, nstack N, REG-PLACES N, 0 N, ;

\ And ffi-call-abi-bounded's, every extent a cell.
: BOUNDED, ( n -- ) {: nstack:n :}
   ARGS AT, FPS AT, STK AT, EXTS AT, EXTS AT, nstack N, ;

\ A NUL-terminated copy of a string behind a jump, and its label.
: C-TEXT, ( ptr u8 n -- label ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,  a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN  0 ASM-SINK BUF:APPEND-BYTE
   past LBL,
   text ;

\ Push the address of the libc function of a name, found by DLSYM,.
: LIBC, ( ptr u8 n -- )
   C-TEXT, {: name:label :}
   RSI name MOVABS,  X64KERNEL:DLSYM,  0 G-PUSH ;

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

: STUB6 ( -- label ) [: 0 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB8 ( -- label ) [: 2 STUB, ;] X64HARNESS:ROUTINE, ;
: STUB9 ( -- label ) [: 3 STUB, ;] X64HARNESS:ROUTINE, ;

\ A SysV function of eight double arguments. It folds the bits of xmm0..7 in
\ order into rax as one hex digit each, then al at its entry as a byte, and
\ answers the fold in xmm0 and its complement in rax, so a register out of
\ order, a dirty al or the wrong answer register changes what a row pushes.
: FSTUB, ( -- )
   R11 0 >R8 ASM-SINK ENC-MOVZX-8-RR
   RAX XMM0 ASM-SINK ENC-MOVQ-RX
   FLOATS 1 ?do  RCX i >XMM ASM-SINK ENC-MOVQ-RX  RCX FOLD-REG,  loop
   RAX 8 >IMM8 ASM-SINK ENC-SHL-RI8  RAX R11 ASM-SINK ENC-ADD-RR
   XMM0 RAX ASM-SINK ENC-MOVQ-XR
   RAX ASM-SINK ENC-NOT ;

\ The float stub's fold when fpbuf cell i holds i + 1, through an ABI row.
: FSTUB-ANSWER ( -- n )
   0  FLOATS 0 ?do  16 *  i 1+ +  loop
   256 * ABI-AL + ;

: FSTUB ( -- label ) [: FSTUB, ;] X64HARNESS:ROUTINE, ;

\ ---- the cases ---------------------------------------------------------------
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

\ The ABI rows: the float stub through all four, whose plain rows answer rax
\ and -r rows xmm0; the nine-place stub with three stackbuf cells, at both
\ rsp parities; the stub of register places with nstack -1, which passes
\ nothing on the stack. Ten checks with the stack checks, the harness's whole
\ budget.
: ABI-CASE ( -- )
   FSTUB STUB6 STUB9 {: f:label s6:label s9:label :}
   FSTUB-ANSWER {: fwant:n :}
   PLACES STUB-ANSWER ABI-AL + {: want9:n :}
   REG-PLACES STUB-ANSWER ABI-AL + {: want6:n :}
   ABI-ARGS,
   0 ABI, f PUSH-XT, s" ffi-call-abi" ROW  fwant invert WANT
   0 ABI, f PUSH-XT, s" ffi-call-abi-r" ROW  fwant WANT
   0 BOUNDED, f PUSH-XT, s" ffi-call-abi-bounded" ROW  fwant invert WANT
   0 BOUNDED, f PUSH-XT, s" ffi-call-abi-r-bounded" ROW  fwant WANT
   STACK-PLACES ABI, s9 PUSH-XT, s" ffi-call-abi" ROW  want9 WANT
   STACK-PLACES BOUNDED, s9 PUSH-XT, s" ffi-call-abi-bounded" SHIFTED-ROW
   want9 WANT
   -1 ABI, s6 PUSH-XT, s" ffi-call-abi" ROW  want6 WANT
   -1 BOUNDED, s6 PUSH-XT, s" ffi-call-abi-bounded" ROW  want6 WANT ;

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
   RAX ZERO-REG,
   ZEROED-CELLS 0 ?do  RAX ASM-SINK ENC-PUSH  loop
   RSP ZEROED-CELLS CELL * >IMM32 ASM-SINK ENC-ADD-RI32 ;

\ Load a double's bits into an XMM register.
: XMM-BITS, ( xmm n -- ) {: x:xmm v:n :}
   RAX v >IMM64 ASM-SINK ENC-MOV-RI64  x RAX ASM-SINK ENC-MOVQ-XR ;

\ The control first: xmm0 and xmm1 loaded and ffi-call-n's al of 0, so
\ snprintf reads its doubles from the zeroed slots. Then ffi-call-abi-bounded,
\ whose al of 8 makes it save them.
: SNPRINTF-CASE ( -- )
   s" %.1f %.1f" C-TEXT, {: fmt:label :}
   OUT ARGS SCRATCH-AT!,
   OUT-BYTES ARGS CELL + SCRATCH!,
   fmt ARGS 2 CELL * + LABEL!,
   ONE-HALF FPS SCRATCH!,  TWO-HALF FPS CELL + SCRATCH!,
   ARGS AT, 3 N, s" snprintf" LIBC,
   ZERO-BELOW,  XMM0 ONE-HALF XMM-BITS,  XMM1 TWO-HALF XMM-BITS,
   s" ffi-call-n" ROW  PRINTED WANT
   ZEROS-TEXT OUT X64HARNESS:EXPECT-SCRATCH,
   0 BOUNDED, s" snprintf" LIBC, s" ffi-call-abi-bounded" ROW  PRINTED WANT
   FLOATS-TEXT OUT X64HARNESS:EXPECT-SCRATCH, ;

\ The third stack cell and its stkext entry name the band cell, so the guard
\ reaches it only past the nine register guards and two stack guards that
\ pass. regext's entry of the same index is 0, so a stack guard that read
\ regext would pass it, as habu1.f BFFI-CALL-ABI-BOUNDED-CORE says the two
\ tables differ.
: ABI-ARMED-CASE ( -- )
   STUB9 {: s9:label :}
   ABI-ARGS,
   STK 2 CELL * + BAND!,
   0 EXTS 2 CELL * + SCRATCH!,
   STACK-PLACES 0 ?do  CELL STK-EXTS i CELL * + SCRATCH!,  loop
   ARGS AT, FPS AT, STK AT, EXTS AT, STK-EXTS AT, STACK-PLACES N,
   s9 PUSH-XT, s" ffi-call-abi-bounded" ROW
   PLACES STUB-ANSWER ABI-AL + WANT ;

\ The fifth register slot and its regext entry name the band cell, and the
\ stack cells are clean, so only the register guard refuses it.
: REGEXT-ARMED-CASE ( -- )
   STUB9 {: s9:label :}
   ABI-ARGS,
   ARGS 4 CELL * + BAND!,
   STACK-PLACES BOUNDED, s9 PUSH-XT, s" ffi-call-abi-bounded" ROW
   PLACES STUB-ANSWER ABI-AL + WANT ;

\ The sixth argument, the last below nint, names the band cell, and sret is
\ 0, so only the nint guard refuses it.
: NINT-ARMED-CASE ( -- )
   STUB9 {: s9:label :}
   ABI-ARGS,
   ARGS REG-PLACES 1- CELL * + BAND!,
   STACK-PLACES ABI, s9 PUSH-XT, s" ffi-call-abi" ROW
   PLACES STUB-ANSWER ABI-AL + WANT ;

\ argbuf[8] names the band cell, past the live register places: only the
\ sret guard reaches it.
: SRET-ARMED-CASE ( -- )
   STUB9 {: s9:label :}
   ABI-ARGS,
   ARGS PLACES 1- CELL * + BAND!,
   ARGS AT, FPS AT, STK AT, STACK-PLACES N, REG-PLACES N, 1 N,
   s9 PUSH-XT, s" ffi-call-abi" ROW
   PLACES STUB-ANSWER ABI-AL + WANT ;

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
   [: ABI-CASE ;] false s" hb-x64-kernel-ffi-abi" TMP-PATH BUILD
   [: SNPRINTF-CASE ;] false s" hb-x64-kernel-ffi-snprintf" TMP-PATH BUILD
   [: ABI-ARMED-CASE ;] false s" hb-x64-kernel-ffi-abi-armed" TMP-PATH BUILD
   [: SRET-ARMED-CASE ;] false s" hb-x64-kernel-ffi-sret-armed" TMP-PATH BUILD
   [: REGEXT-ARMED-CASE ;] false s" hb-x64-kernel-ffi-regext-armed" TMP-PATH BUILD
   [: NINT-ARMED-CASE ;] false s" hb-x64-kernel-ffi-nint-armed" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-FFI:RUN
