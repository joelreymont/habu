\ t0-x64.f - the x86-64 emitter of the tier 0 compiler, part X86-64 of package
\ COMP.
\
\ Implements the EMIT- operations src/habu/t0-emit.f states for every target;
\ this file holds only x86-64 encodings and register choices. FILL-SLOT
\ stores its words into the EMIT- and src/habu/t0-label.f (LABEL) cells for
\ CTARGET-ARCH:X86-64.
\
\ PLACE (src/habu/t0-place.f) places the units, and a `ret` and a tail `jmp`
\ take an epilogue.
\
\ FLAGS says whether the condition codes hold a compare no word has read yet.
\ CMP and CMP-LIT set it; FWD-CC and BACK-CC, which read the codes, clear it,
\ and so does a unit's start. A word that reads its own compare's codes leaves
\ it alone. While it is clear a zero is loaded by an xor, which writes the
\ codes; while it is set, by a mov, which does not.
\
\ The frame cell stays the loop index's home (`j` and the runtime read it); r12
\ is a copy written beside every store of it. HELD says whether the code
\ emitted next may read r12: a loop head sets it, and so do `i` and a step once
\ they load it. Every call clears it, since the callee may run a loop, and so
\ does every label a branch lands on (MARK and RESOLVE), since a branch may
\ come from code that wrote r12; LOOP-POP and a unit's start clear it too.
\ While it is clear, `i`, `loop` and `+loop` load r12 from the frame first.
\
\ AHEAD says by how many cells the data stack's top lies above rbx, negative
\ below: x86-64 has no writeback, so a push or pop moves AHEAD and SETTLE emits
\ the sum as one lea, which keeps the codes, where rbx must be exact: a call, a
\ return, a loop head, a backward branch and `?do`'s entry, whose head follows.
\ A forward branch carries its AHEAD to its label (BR-AHEAD): code falling into
\ the label moves rbx to it, and code after a jump or return (GONE) adopts it,
\ so every arrival at a label has one AHEAD. CMP and CMP-LIT settle before their
\ compare, since the branch that reads it may be a backward one and the CPU
\ fuses the two only when they are adjacent.
\
\ A byte displacement reaches 16 cells below rbx to 15 above; an access past it
\ first moves rbx by the whole reach toward it, by a lea too, when the access
\ is then within reach: a disp32 costs three bytes more at each access, the lea
\ four once, and a run's accesses lie near one another. A copy between cells
\ too far apart for one reach would move rbx back and forth, so the access
\ after one that moved it does not.
\ Cells above rbx may hold live values until SETTLE, so nothing asynchronous may
\ write there.
\
\ A literal divisor v from 3 to 2^31 - 1, no power of two, divides by a multiply
\ (Granlund and Montgomery, "Division by invariant integers using
\ multiplication", 1994). With m = floor(2^(64+s) / v) + 1, the quotient of x is
\ floor(x m / 2^(64+s)), plus 1 when x is negative, for every cell x when
\ m v - 2^(64+s) is at most 2^(s+1). RECIPROCAL answers the m of the least such
\ s; some s below 31 is one, and its m is below 2^64.

require src/lib/error.f
require src/lib/vector.f
require src/habu/layout.f
require src/os/linux/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f
require src/habu/t0-label.f
require src/habu/t0-place.f
require src/compiler/native/relax.f

package COMP
package X86-64

EXPORT BOOT:data-base

8 constant NVREG

\ An immediate is a sign-extended imm32.
: IMM? ( n -- bool ) {: v:n :}
   v -2147483648 >= v 2147483647 <= and ;

\ A sign-extended imm8.
: SHORT? ( n -- bool ) {: v:n :}
   v -128 >= v 127 <= and ;

: COND ( cc -- X64ASM:condition )
   {: c:cc :}
   c CC>N {: k:n :}
   k 0 = if X64ASM:C-E exit then
   k 1 = if X64ASM:C-NE exit then
   k 2 = if X64ASM:C-L exit then
   k 3 = if X64ASM:C-GE exit then
   k 4 = if X64ASM:C-LE exit then
   k 5 = if X64ASM:C-G exit then
   X64ASM:E-OPERAND throw ;

variable FLAGS                        \ nonzero while the condition codes are held
variable HELD                         \ nonzero while r12 holds the index
variable AHEAD
variable STEPPED                      \ nonzero when the last access moved rbx
variable GONE                         \ nonzero while the code emitted next is unreachable
DYNAMIC-BUFFER BR-AHEAD n             \ the AHEAD each forward branch carries

0 constant FORM-JMP
1 constant FORM-JCC

: SINK ( -- ^VEC:vector<u8> ) PLACE:RAW @ ;

\ A body's contract: rbp is DATA, rbx the data stack pointer, r12 the loop
\ index, which any callee may write, and r13-r15 the other engine registers;
\ compiled code writes rbx as the stack and r12, and none of the others. The
\ vregs are rcx rdx rsi rdi r8-r11; rax is this file's temporary and holds no
\ value between two of its words. A body is entered by `call`, leaves by `ret`
\ and saves nothing; its frame on the machine stack is its own cells rounded to
\ 16.
: DSP ( -- X64ASM:r64 ) ENGINE-GPR:X64-DSTACK X64ASM:>R64 ;
: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: IDX ( -- X64ASM:r64 ) ENGINE-GPR:X64-INDEX X64ASM:>R64 ;

: REG ( vreg -- X64ASM:r64 )
   {: v:vreg :}
   v VREG>N {: k:n :}
   k 0 < k NVREG >= or if X64ASM:E-OPERAND throw then
   k 0 = if X64ASM:RCX exit then
   k 1 = if X64ASM:RDX exit then
   k 2 = if X64ASM:RSI exit then
   k 3 = if X64ASM:RDI exit then
   k 4 + X64ASM:>R64 ;

\ The low k bytes of v into OUT at off.
: PUT ( n n n -- ) {: v:n off:n k:n :}
   PLACE:OUT @ off VEC:AT {: p:^ :}
   k 0 ?do v i 8 * rshift $FF and p i + c! loop ;

: REL32! ( n n -- )
   {: d:n off:n :}
   d -2147483648 < d 2147483647 > or if E-X64EMIT-REACH throw then
   d off 4 PUT ;

\ Load the constant n into r with the shortest form that holds it and keeps
\ held condition codes.
: LIT-IN ( n X64ASM:r64 ^VEC:vector<u8> -- ) {: v:n r:X64ASM:r64 buf:^ :}
   r X64ASM:R64>N X64ASM:>R32 {: r4:X64ASM:r32 :}
   v 0 =  FLAGS @ 0=  and if r4 r4 buf X64ASM:ENC-XOR32-RR exit then
   v 0 >= v $FFFFFFFF <= and if r4 v X64ASM:>IMM32 buf X64ASM:ENC-MOV32-RI32 exit then
   v IMM? if r v X64ASM:>IMM32 buf X64ASM:ENC-MOV-RI32 exit then
   r v X64ASM:>IMM64 buf X64ASM:ENC-MOV-RI64 ;

: LIT-R ( n X64ASM:r64 -- ) SINK LIT-IN ;

\ The site loads 0 until its value is written. A build's data has one offset from
\ data-base on every host, and below DATA-LOW its address fits `mov r32, imm32`.
: ADDR ( n n vreg n -- )
   {: v:n o:n d:vreg kind:n :}
   PLACE:HERE-OFF kind v o PLACE:PEND+
   v data-base PTR>N - {: off:n :}
   kind NEMIT:ADDR-CODE =  off 0 >= off LINUX-LAYOUT:DATA-LOW < and  or if
      d REG X64ASM:R64>N X64ASM:>R32 0 X64ASM:>IMM32 SINK X64ASM:ENC-MOV32-RI32 exit
   then
   d REG 0 X64ASM:>IMM64 SINK X64ASM:ENC-MOV-RI64 ;

\ r, holding the return stack depth, becomes the address one cell past its top.
: RTOP ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r 3 X64ASM:>IMM8 SINK X64ASM:ENC-SHL-RI8
   r DATA-REG STACK-ABI:RETURN-BASE-CELL X64ASM:MEM-OFF SINK X64ASM:ENC-ADD-RM ;

: RDEPTH ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r DATA-REG RSP-CELL X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM ;

: RDEPTH! ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r DATA-REG RSP-CELL X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR ;

\ ---- the data stack pointer --------------------------------------------------
\ Move rbx up k cells; the stack's top stays.
: RBX+ ( n -- ) {: k:n :}
   k 0 = if exit then
   DSP DSP k CELL * X64ASM:MEM-OFF SINK X64ASM:ENC-LEA
   k negate AHEAD +! ;

: SETTLE ( -- ) AHEAD @ RBX+ ;

: DSP+ ( n -- ) AHEAD +! ;

\ The cells above rbx a byte displacement reaches.
-16 constant REACH-LOW
15 constant REACH-HIGH

: REACHED? ( n -- bool ) {: k:n :}
   k REACH-LOW >= k REACH-HIGH <= and ;

\ Whether an access that is past reach moves rbx by s to the cell k above it.
: STEP? ( n n -- bool ) {: k:n s:n :}
   k s - REACHED?  STEPPED @ 0=  and ;

\ Bring the cell k cells above rbx in reach where that pays.
: REACH ( n -- ) {: k:n :}
   k REACHED? if 0 STEPPED ! exit then
   k 0 < if REACH-LOW else REACH-HIGH then {: s:n :}
   k s STEP? if s RBX+ 1 else 0 then STEPPED ! ;

\ The cell k cells above the stack's top.
: TOP-MEM ( n -- X64ASM:mem ) {: k:n :}
   AHEAD @ k + REACH
   DSP AHEAD @ k + CELL * X64ASM:MEM-OFF ;

\ ---- PLACE's hooks -----------------------------------------------------------
: UNIT-RESET ( -- ) 0 FLAGS !  0 HELD !  0 AHEAD !  0 STEPPED !  0 GONE ! ;

: PROLOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   bytes 0= if exit then
   bytes SHORT? if X64ASM:RSP bytes X64ASM:>IMM8 buf X64ASM:ENC-SUB-RI8 exit then
   X64ASM:RSP bytes X64ASM:>IMM32 buf X64ASM:ENC-SUB-RI32 ;

: EPILOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   bytes 0= if exit then
   bytes SHORT? if X64ASM:RSP bytes X64ASM:>IMM8 buf X64ASM:ENC-ADD-RI8 exit then
   X64ASM:RSP bytes X64ASM:>IMM32 buf X64ASM:ENC-ADD-RI32 ;

\ The call is E8 rel32, and the address lea its REX, opcode, ModRM and disp32.
: SELF-PUT ( n -- ) {: at:n :}  at 5 + negate  at 1 + REL32! ;

: ADR-PUT ( n n -- ) {: at:n to:n :}  to at 7 + -  at 3 + REL32! ;

\ Branch k's rel8 opcode.
: OPCODE ( n -- n ) {: k:n :}
   k PLACE:BR-FORM @ FORM-JMP = if $EB exit then
   $70 k PLACE:BR-ARG @ + ;

: BRANCH-LEN ( n n -- n ) {: k:n d:n :}  k OPCODE d X64ASM:BRANCH-GROWTH ;

: BRANCH-PUT ( n n n -- ) {: k:n at:n to:n :}
   k RELAX:BR-LEN @ {: growth:n :}
   PLACE:OUT @ at VEC:AT  k OPCODE  growth  to at - X64ASM:BRANCH-HOLE - growth -
   X64ASM:BRANCH-PUT ;

\ ---- the body ----------------------------------------------------------------
\ A reading quotation receives its record in rdi.
\
\ The rows (t0-emit.f "THE ROWS"): a call or tail site is E8 or E9 rel32, zero
\ until PATCH measures it from its placed end; an address site is a mov r,
\ imm whose first byte is at the row's offset (ADDR). A quotation's
\ RIP-relative lea names its unit inside the emission and needs no row.
: RETURN ( -- ) SETTLE PLACE:EPI+  SINK X64ASM:ENC-RET  1 GONE ! ;

\ The return is the kernel's does> patch slot, `jmp rel32` and the `ret` it
\ falls through to, filed at the jump's offset.
: SLOT-CLOSE ( -- )
   SETTLE PLACE:EPI+
   0 X64ASM:>REL SINK X64ASM:ENC-JMP-REL32
   SINK X64ASM:ENC-RET
   PLACE:CLOSE
   PLACE:OUT-OFF 6 - EMIT-PATCH-SLOT! ;

\ The stub after the last unit, answering its offset: r10 its slot, rax the
\ thunk, which writes rax first (src/habu/kernel-x64.f CALLBACK-ENTRY-BODY).
: CB-STUB ( -- n )
   PLACE:OUT-OFF {: at:n :}
   NCBSTUB:SLOT@ X64ASM:R10 PLACE:OUT @ LIT-IN
   NCBSTUB:THUNK@ {: thunk:n :}
   PLACE:OUT-OFF NEMIT:ADDR-CODE thunk -1 EMIT-ADDR-ROW+
   X64ASM:RAX X64ASM:R64>N X64ASM:>R32 0 X64ASM:>IMM32 PLACE:OUT @ X64ASM:ENC-MOV32-RI32
   X64ASM:RAX PLACE:OUT @ X64ASM:ENC-JMP-REG
   at ;

\ A call's rel32 is measured from the end of its five bytes, placed at.
: PATCH-CALL ( n n n -- ) {: at:n off:n to:n :}
   to at off + 5 + - off 1+ REL32! ;

: PATCH ( n -- ) {: at:n :}
   EMIT-CALL-ROWS 0 ?do
      i EMIT-CALL-ROW {: off:n kind:n to:n :}
      at off to PATCH-CALL
   loop
   EMIT-ADDR-ROWS 0 ?do
      i EMIT-ADDR-ROW {: off:n kind:n to:n :}
      to  PLACE:OUT @ off VEC:AT  X64ASM:MOV-IMM!
   loop ;

\ ---- values ------------------------------------------------------------------
: LIT ( n vreg -- )
   {: v:n d:vreg :}
   v d REG LIT-R ;

: ADDR-DATA ( n n vreg -- ) NEMIT:ADDR-DATA ADDR ;

: ADDR-CODE ( n vreg -- )
   {: v:n d:vreg :}
   v -1 d NEMIT:ADDR-CODE ADDR ;

\ `lea r, [rip]` until ADR-PUT writes the distance to unit n.
: ADDR-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   PLACE:HERE-OFF ADDR-QUOT-KIND n -1 PLACE:PEND+
   d REG 0 X64ASM:MEM-RIP SINK X64ASM:ENC-LEA ;

: PUSH ( vreg n -- ) {: s:vreg k:n :}
   s REG 0 TOP-MEM SINK X64ASM:ENC-MOV-MR
   k DSP+ ;

\ Store the constant n at m, keeping held condition codes.
: LIT-M ( n X64ASM:mem -- ) {: v:n m:X64ASM:mem :}
   v IMM? if v X64ASM:>IMM32 m SINK X64ASM:ENC-MOV-MI32 exit then
   v X64ASM:RAX LIT-R
   X64ASM:RAX m SINK X64ASM:ENC-MOV-MR ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v 0 TOP-MEM LIT-M
   k DSP+ ;

: POP ( vreg n -- ) {: d:vreg k:n :}
   k negate DSP+
   d REG 0 TOP-MEM SINK X64ASM:ENC-MOV-RM ;

: SPILL ( vreg n -- )
   {: s:vreg k:n :}
   s REG k TOP-MEM SINK X64ASM:ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v k TOP-MEM LIT-M ;

: FILL ( vreg n -- )
   {: d:vreg k:n :}
   d REG k TOP-MEM SINK X64ASM:ENC-MOV-RM ;

: MOV ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG SINK X64ASM:ENC-MOV-RR ;

: BASE ( vreg -- ) REG DATA-REG SINK X64ASM:ENC-MOV-RR ;

: EXCHANGE ( vreg vreg -- )
   {: a:vreg b:vreg :}
   a REG b REG SINK X64ASM:ENC-XCHG-RR ;

: SLOT-STORE ( vreg n -- )
   {: s:vreg k:n :}
   s REG X64ASM:RSP k PLACE:SLOT-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR ;

: SLOT-LOAD ( vreg n -- )
   {: d:vreg k:n :}
   d REG X64ASM:RSP k PLACE:SLOT-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM ;

: ENV-OPEN ( -- )
   PLACE:BODY-EMPTY 1 PLACE:ENV !
   X64ASM:RDI X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-MOV-MR ;

: REC-STORE ( vreg n -- )
   {: s:vreg n:n :}
   s REG X64ASM:RSP n PLACE:REC-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR ;

: REC-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   d REG X64ASM:RSP n PLACE:REC-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-LEA
   $8000000000000000 X64ASM:RAX LIT-R
   d REG X64ASM:RAX SINK X64ASM:ENC-OR-RR ;

: ENV-LOAD ( vreg n -- )
   {: d:vreg n:n :}
   PLACE:ENV @ 0= if E-T0-UNIT throw then
   d REG {: r:X64ASM:r64 :}
   r X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM
   r r n CELL * X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( vreg vreg alu -- )
   {: d:vreg s:vreg o:alu :}
   d REG {: dr:X64ASM:r64 :}  s REG {: sr:X64ASM:r64 :}  o ALU>N {: k:n :}
   k 0 = if dr sr SINK X64ASM:ENC-ADD-RR exit then
   k 1 = if dr sr SINK X64ASM:ENC-SUB-RR exit then
   k 2 = if dr sr SINK X64ASM:ENC-AND-RR exit then
   k 3 = if dr sr SINK X64ASM:ENC-OR-RR exit then
   k 4 = if dr sr SINK X64ASM:ENC-XOR-RR exit then
   X64ASM:E-OPERAND throw ;

: OP-RI8 ( X64ASM:r64 X64ASM:imm8 alu -- )
   {: dr:X64ASM:r64 iv:X64ASM:imm8 o:alu :}
   o ALU>N {: k:n :}
   k 0 = if dr iv SINK X64ASM:ENC-ADD-RI8 exit then
   k 1 = if dr iv SINK X64ASM:ENC-SUB-RI8 exit then
   k 2 = if dr iv SINK X64ASM:ENC-AND-RI8 exit then
   k 3 = if dr iv SINK X64ASM:ENC-OR-RI8 exit then
   k 4 = if dr iv SINK X64ASM:ENC-XOR-RI8 exit then
   X64ASM:E-OPERAND throw ;

: OP-RI32 ( X64ASM:r64 X64ASM:imm32 alu -- )
   {: dr:X64ASM:r64 iv:X64ASM:imm32 o:alu :}
   o ALU>N {: k:n :}
   k 0 = if dr iv SINK X64ASM:ENC-ADD-RI32 exit then
   k 1 = if dr iv SINK X64ASM:ENC-SUB-RI32 exit then
   k 2 = if dr iv SINK X64ASM:ENC-AND-RI32 exit then
   k 3 = if dr iv SINK X64ASM:ENC-OR-RI32 exit then
   k 4 = if dr iv SINK X64ASM:ENC-XOR-RI32 exit then
   X64ASM:E-OPERAND throw ;

: OP-LIT ( vreg n alu -- )
   {: d:vreg v:n o:alu :}
   v SHORT? if d REG v X64ASM:>IMM8 o OP-RI8 exit then
   d REG v X64ASM:>IMM32 o OP-RI32 ;

\ A shift by a register takes its count in cl, and cl is vreg 0's low byte;
\ the hardware masks the count to six bits. SHIFT-LIT masks its count here.
: SHIFT-CL ( X64ASM:r64 shifter -- )
   {: r:X64ASM:r64 o:shifter :}
   o SHIFTER>N 0 = if r SINK X64ASM:ENC-SHL-CL exit then
   r SINK X64ASM:ENC-SHR-CL ;

: SHIFT ( vreg vreg shifter -- )
   {: d:vreg s:vreg o:shifter :}
   s VREG>N 0 = if d REG o SHIFT-CL exit then
   X64ASM:RAX X64ASM:RCX SINK X64ASM:ENC-MOV-RR
   X64ASM:RCX s REG SINK X64ASM:ENC-MOV-RR
   d VREG>N 0 = if
      X64ASM:RAX o SHIFT-CL
      X64ASM:RCX X64ASM:RAX SINK X64ASM:ENC-MOV-RR exit
   then
   d REG o SHIFT-CL
   X64ASM:RCX X64ASM:RAX SINK X64ASM:ENC-MOV-RR ;

: SHIFT-LIT ( vreg n shifter -- )
   {: d:vreg v:n o:shifter :}
   v 63 and X64ASM:>IMM8 {: c:X64ASM:imm8 :}
   o SHIFTER>N 0 = if d REG c SINK X64ASM:ENC-SHL-RI8 exit then
   d REG c SINK X64ASM:ENC-SHR-RI8 ;

: MULX ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG SINK X64ASM:ENC-IMUL-RR ;

: MUL-R ( X64ASM:r64 n -- )
   {: r:X64ASM:r64 v:n :}
   v SHORT? if r r v X64ASM:>IMM8 SINK X64ASM:ENC-IMUL-RRI8 exit then
   r r v X64ASM:>IMM32 SINK X64ASM:ENC-IMUL-RRI32 ;

: MUL-LIT ( vreg n -- )
   {: d:vreg v:n :}
   d REG v MUL-R ;

\ idiv and the one-operand imul write rax and rdx, and rdx is a vreg, so it is
\ saved on the machine stack unless d is rdx; then rcx stands in for d and is
\ saved instead. The saved register is the only one restored, so every vreg
\ but d keeps its value.
: IN-RDX? ( X64ASM:r64 -- bool ) X64ASM:R64>N X64ASM:RDX X64ASM:R64>N = ;
: STAND-IN ( X64ASM:r64 -- X64ASM:r64 ) dup IN-RDX? if drop X64ASM:RCX then ;
: SAVED ( X64ASM:r64 -- X64ASM:r64 ) IN-RDX? if X64ASM:RCX else X64ASM:RDX then ;

: MOV-R ( X64ASM:r64 X64ASM:r64 -- )
   {: d:X64ASM:r64 s:X64ASM:r64 :}
   d X64ASM:R64>N s X64ASM:R64>N <> if d s SINK X64ASM:ENC-MOV-RR then ;

\ The divisor goes into d or its stand-in, which cqo does not write, once the
\ dividend is in rax.
: DIVIDE ( X64ASM:r64 n bool -- )
   {: r:X64ASM:r64 v:n rem:bool :}
   r STAND-IN {: w:X64ASM:r64 :}
   r SAVED SINK X64ASM:ENC-PUSH
   X64ASM:RAX r SINK X64ASM:ENC-MOV-RR
   v w LIT-R
   SINK X64ASM:ENC-CQO
   w SINK X64ASM:ENC-IDIV
   r rem if X64ASM:RDX else X64ASM:RAX then MOV-R
   r SAVED SINK X64ASM:ENC-POP ;

\ rax becomes what makes dropping the low k bits of r + rax round toward zero,
\ 2^k - 1 when r is negative, else 0, and is added to r; k is 1 to 62.
: BIAS ( X64ASM:r64 n -- )
   {: r:X64ASM:r64 k:n :}
   X64ASM:RAX r SINK X64ASM:ENC-MOV-RR
   k 1 > if X64ASM:RAX 63 X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8 then
   X64ASM:RAX 64 k - X64ASM:>IMM8 SINK X64ASM:ENC-SHR-RI8
   r X64ASM:RAX SINK X64ASM:ENC-ADD-RR ;

: LOW-BITS ( vreg n -- )
   {: d:vreg k:n :}
   1 k lshift 1- {: mask:n :}
   mask IMM? if d mask OP-AND OP-LIT exit then
   d 64 k - SH-LEFT SHIFT-LIT
   d 64 k - SH-RIGHT SHIFT-LIT ;

: DIV-POW2 ( vreg n -- )
   {: d:vreg k:n :}
   k 0= if exit then
   d REG k BIAS
   d REG k X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8 ;

: MOD-POW2 ( vreg n -- )
   {: d:vreg k:n :}
   k 0= if 0 d LIT exit then
   d REG k BIAS
   d k LOW-BITS
   d REG X64ASM:RAX SINK X64ASM:ENC-SUB-RR ;

: RECIP? ( n -- bool ) {: v:n :}  v 0 > v $80000000 < and ;

\ The quotient and remainder of 2^(64+s) by v, which is above 2^s, in two digits
\ of 32 bits: no dividend here passes 2^63.
: POW-DIV ( n n -- n n )
   {: s:n v:n :}
   1 s 32 + lshift v /mod {: r:n hi:n :}
   r 32 lshift v /mod {: rem:n lo:n :}
   hi 32 lshift lo or rem ;

: RECIPROCAL ( n -- n n ) {: v:n :}
   31 0 ?do
      i v POW-DIV v swap - 2 i lshift <= if 1+ i unloop exit then
      drop
   loop
   X64ASM:E-OPERAND throw ;

\ rdx becomes floor(r m / 2^(64+s)); imul reads an m past 2^63 as m - 2^64, so
\ r is added back.
: HIGH-MUL ( X64ASM:r64 n n -- )
   {: r:X64ASM:r64 m:n s:n :}
   m X64ASM:RAX LIT-R
   r SINK X64ASM:ENC-IMUL1
   m 0 < if X64ASM:RDX r SINK X64ASM:ENC-ADD-RR then
   s 0 > if X64ASM:RDX s X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8 then ;

\ r becomes the quotient, rdx plus r's sign bit.
: RECIP-QUOT ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r 63 X64ASM:>IMM8 SINK X64ASM:ENC-SHR-RI8
   r X64ASM:RDX SINK X64ASM:ENC-ADD-RR ;

: RECIP-DIV ( X64ASM:r64 n bool -- )
   {: r:X64ASM:r64 v:n rem:bool :}
   r STAND-IN {: w:X64ASM:r64 :}
   r SAVED SINK X64ASM:ENC-PUSH
   w r MOV-R
   w v RECIPROCAL HIGH-MUL
   rem if
      X64ASM:RAX w SINK X64ASM:ENC-MOV-RR
      X64ASM:RAX RECIP-QUOT
      X64ASM:RAX v MUL-R
      w X64ASM:RAX SINK X64ASM:ENC-SUB-RR
   else
      w RECIP-QUOT
   then
   r w MOV-R
   r SAVED SINK X64ASM:ENC-POP ;

: DIVIDE-LIT ( vreg n bool -- )
   {: d:vreg v:n rem:bool :}
   v POW2-EXP {: k:n :}
   k 0 >= if d k rem if MOD-POW2 else DIV-POW2 then exit then
   d REG v rem v RECIP? if RECIP-DIV else DIVIDE then ;

: DIV-LIT ( vreg n -- ) false DIVIDE-LIT ;
: MOD-LIT ( vreg n -- ) true DIVIDE-LIT ;

: NEG ( vreg -- ) REG SINK X64ASM:ENC-NEG ;
: NOT ( vreg -- ) REG SINK X64ASM:ENC-NOT ;

: CMP ( vreg vreg -- )
   {: a:vreg b:vreg :}
   SETTLE
   1 FLAGS !
   a REG b REG SINK X64ASM:ENC-CMP-RR ;

\ `test r, r` sets every signed condition as `cmp r, 0` does.
: CMP-R ( X64ASM:r64 n -- )
   {: r:X64ASM:r64 v:n :}
   v 0 = if r r SINK X64ASM:ENC-TEST-RR exit then
   v SHORT? if r v X64ASM:>IMM8 SINK X64ASM:ENC-CMP-RI8 exit then
   r v X64ASM:>IMM32 SINK X64ASM:ENC-CMP-RI32 ;

: CMP-LIT ( vreg n -- )
   {: a:vreg v:n :}
   SETTLE
   1 FLAGS !
   a REG v CMP-R ;

: SAME? ( vreg vreg -- bool ) VREG>N swap VREG>N = ;

: ZERO ( vreg -- ) REG X64ASM:R64>N X64ASM:>R32 dup SINK X64ASM:ENC-XOR32-RR ;

\ -1 in d when the codes hold a borrow, else 0.
: BORROW ( vreg -- ) REG dup SINK X64ASM:ENC-SBB-RR ;

\ Zeroes, before the compare, the register FLAG-END sets a byte of: d, or rax
\ when via, since d is then an operand of the compare.
: FLAG-START ( vreg bool -- )
   {: d:vreg via:bool :}
   via if X64ASM:RAX else d REG then
   X64ASM:R64>N X64ASM:>R32 dup SINK X64ASM:ENC-XOR32-RR ;

\ d's flag from the codes of a compare after FLAG-START. Through rax the byte
\ is 1 when c fails, so rax - 1 is the flag.
: FLAG-END ( vreg cc bool -- )
   {: d:vreg c:cc via:bool :}
   d REG {: r:X64ASM:r64 :}
   via if
      c CC-NOT COND 0 X64ASM:>R8 SINK X64ASM:ENC-SETCC
      r X64ASM:RAX -1 X64ASM:MEM-OFF SINK X64ASM:ENC-LEA exit
   then
   c COND r X64ASM:R64>N X64ASM:>R8 SINK X64ASM:ENC-SETCC
   r SINK X64ASM:ENC-NEG ;

: FLAG ( vreg vreg vreg cc -- )
   {: d:vreg a:vreg b:vreg c:cc :}
   d a SAME? d b SAME? or {: via:bool :}
   d via FLAG-START
   a REG b REG SINK X64ASM:ENC-CMP-RR
   d c via FLAG-END ;

\ -1 in d when a is negative, else 0.
: SIGN ( vreg vreg -- )
   {: d:vreg a:vreg :}
   d a SAME? 0= if d a MOV then
   d REG 63 X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8 ;

\ An unsigned compare with 1, or of 0 with a, borrows only when the flag holds.
: ZERO-EQ ( vreg vreg -- )
   {: d:vreg a:vreg :}
   a REG 1 X64ASM:>IMM8 SINK X64ASM:ENC-CMP-RI8  d BORROW ;

: ZERO-NE ( vreg vreg -- )
   {: d:vreg a:vreg :}
   d a SAME? if d NEG else d ZERO  d REG a REG SINK X64ASM:ENC-CMP-RR then
   d BORROW ;

: FLAG-LIT ( vreg vreg n cc -- )
   {: d:vreg a:vreg v:n c:cc :}
   v 0= if
      c CC>N {: k:n :}
      k CC-LT CC>N = if d a SIGN exit then
      k CC-GE CC>N = if d a SIGN  d NOT exit then
      k CC-EQ CC>N = if d a ZERO-EQ exit then
      k CC-NE CC>N = if d a ZERO-NE exit then
   then
   d a SAME? {: via:bool :}
   d via FLAG-START
   a REG v CMP-R
   d c via FLAG-END ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( vreg vreg width -- )
   {: d:vreg a:vreg w:width :}
   w WIDTH>N {: n:n :}
   n 1 = if d REG a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOVZX-8-RM exit then
   n 2 = if d REG a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOVZX-16-RM exit then
   n 4 = if d REG X64ASM:R64>N X64ASM:>R32 a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-RM exit then
   d REG a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM ;

: STORE ( vreg vreg width -- )
   {: s:vreg a:vreg w:width :}
   s REG {: r:X64ASM:r64 :}
   w WIDTH>N {: n:n :}
   n 1 = if r X64ASM:R64>N X64ASM:>R8 a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV8-MR exit then
   n 2 = if r X64ASM:R64>N X64ASM:>R16 a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV16-MR exit then
   n 4 = if r X64ASM:R64>N X64ASM:>R32 a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MR exit then
   r a REG X64ASM:MEM-AT SINK X64ASM:ENC-MOV-MR ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- ) {: to:n loc:n :}
   SETTLE
   0 HELD !
   PLACE:HERE-OFF NEMIT:CALL to loc PLACE:PCALL+
   0 X64ASM:>REL SINK X64ASM:ENC-CALL-REL32 ;

: TAIL ( n n -- ) {: to:n loc:n :}
   SETTLE PLACE:EPI+
   PLACE:HERE-OFF NEMIT:TAIL to loc PLACE:PCALL+
   0 X64ASM:>REL SINK X64ASM:ENC-JMP-REL32
   1 GONE ! ;

: CALL-SELF ( -- )
   SETTLE
   0 HELD !
   PLACE:HERE-OFF PLACE:SELF+
   0 X64ASM:>REL SINK X64ASM:ENC-CALL-REL32 ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives LABEL emits through. A forward branch is its number
\ in the open unit's list, and a head a raw offset.

\ A branch of the form, with its condition or 0, to a raw offset or -1.
: BRANCH ( n n n -- LABEL:branch ) {: form:n arg:n to:n :}
   to 0 >= if SETTLE then
   form arg to X64ASM:BRANCH-HOLE PLACE:BR+ {: b:LABEL:branch :}
   b LABEL:BRANCH>N {: k:n :}
   k 1 + BR-AHEAD-RESERVE
   AHEAD @ k BR-AHEAD !
   b ;

: RESOLVE ( LABEL:branch -- ) {: b:LABEL:branch :}
   b LABEL:BRANCH>N BR-AHEAD @ {: a:n :}
   GONE @ if a AHEAD ! else AHEAD @ a - RBX+ then
   0 GONE !
   b PLACE:RESOLVE  0 HELD ! ;

: MARK ( -- LABEL:head )
   GONE @ if 0 AHEAD ! else SETTLE then
   0 HELD !  0 GONE !  PLACE:MARK ;

: FWD-JUMP ( -- LABEL:branch ) FORM-JMP 0 -1 BRANCH  1 GONE ! ;

: FWD-JCC ( X64ASM:condition -- LABEL:branch ) {: c:X64ASM:condition :}
   FORM-JCC c X64ASM:CONDITION>N -1 BRANCH ;

: TEST ( vreg -- )
   {: c:vreg :}
   c REG c REG SINK X64ASM:ENC-TEST-RR ;

: FWD-ZERO ( vreg -- LABEL:branch ) TEST X64ASM:C-E FWD-JCC ;

: FWD-NE ( vreg vreg -- LABEL:branch )
   {: sel:vreg v:vreg :}
   sel REG v REG SINK X64ASM:ENC-CMP-RR
   X64ASM:C-NE FWD-JCC ;

: FWD-NE-LIT ( vreg n -- LABEL:branch )
   {: sel:vreg v:n :}
   sel REG v CMP-R
   X64ASM:C-NE FWD-JCC ;

: FWD-GE ( vreg vreg -- LABEL:branch )
   {: s:vreg l:vreg :}
   SETTLE
   s REG l REG SINK X64ASM:ENC-CMP-RR
   X64ASM:C-GE FWD-JCC ;

: FWD-CC ( cc -- LABEL:branch )
   0 FLAGS !
   COND FWD-JCC ;

\ The branch becomes je.
: ENTRY-EQ ( LABEL:branch -- ) LABEL:BRANCH>N {: k:n :}
   X64ASM:C-E X64ASM:CONDITION>N k PLACE:BR-ARG ! ;

: BACK-JUMP ( LABEL:head -- ) LABEL:HEAD>N FORM-JMP 0 rot BRANCH drop  1 GONE ! ;

: BACK-JCC ( X64ASM:condition LABEL:head -- )
   LABEL:HEAD>N {: c:X64ASM:condition m:n :}
   FORM-JCC c X64ASM:CONDITION>N m BRANCH drop ;

: BACK-ZERO ( vreg LABEL:head -- )
   {: c:vreg m:LABEL:head :}
   SETTLE c TEST X64ASM:C-E m BACK-JCC ;

: BACK-CC ( cc LABEL:head -- )
   {: c:cc m:LABEL:head :}
   0 FLAGS !
   c COND m BACK-JCC ;

\ ---- the return stack --------------------------------------------------------
\ Each goes through rax. Vreg i of p moved to or from the slot off bytes above
\ rax.
: RS-XFER ( ^vreg n n bool -- ) {: p:^ i:n off:n st:bool :}
   p i VREG-AT REG X64ASM:RAX off X64ASM:MEM-OFF SINK
   st if X64ASM:ENC-MOV-MR exit then X64ASM:ENC-MOV-RM ;

: RDEPTH+ ( n -- ) {: k:n :} X64ASM:RAX X64ASM:RAX k X64ASM:MEM-OFF SINK X64ASM:ENC-LEA ;

: TOR ( ^vreg n -- ) {: p:^ n:n :}
   X64ASM:RAX RDEPTH
   n RDEPTH+
   X64ASM:RAX RDEPTH!
   X64ASM:RAX RTOP
   n 0 ?do p i i n - CELL * true RS-XFER loop ;

: RFROM ( ^vreg n -- ) {: p:^ n:n :}
   X64ASM:RAX RDEPTH
   n negate RDEPTH+
   X64ASM:RAX RDEPTH!
   X64ASM:RAX RTOP
   n 0 ?do p i i CELL * false RS-XFER loop ;

: RFETCH ( ^vreg n n -- ) {: p:^ n:n k:n :}
   X64ASM:RAX RDEPTH
   X64ASM:RAX RTOP
   n 0 ?do p i i n - k - CELL * false RS-XFER loop ;


\ ---- loops ---------------------------------------------------------------------
: LTOP ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r DATA-REG LOOPSP-CELL X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM ;

: LTOP! ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   r DATA-REG LOOPSP-CELL X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR ;

\ The index and limit of the innermost frame, below LTOP's address.
-16 constant LIDX-OFF
-8 constant LLIM-OFF

: LOOP-J ( vreg -- )
   {: d:vreg :}
   X64ASM:RAX LTOP
   d REG X64ASM:RAX LIDX-OFF STACK-ABI:LOOP-FRAME-BYTES - X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM ;

\ rax the address past the innermost frame, and r12 its index unless HELD
\ says it holds it already.
: FRAME-IDX ( -- )
   X64ASM:RAX LTOP
   HELD @ if exit then
   IDX X64ASM:RAX LIDX-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM
   1 HELD ! ;

: LOOP-PUSH ( vreg vreg -- )
   {: s:vreg l:vreg :}
   X64ASM:RAX LTOP
   X64ASM:RAX STACK-ABI:LOOP-FRAME-BYTES X64ASM:>IMM8 SINK X64ASM:ENC-ADD-RI8
   X64ASM:RAX LTOP!
   s REG X64ASM:RAX LIDX-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR
   l REG X64ASM:RAX LLIM-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR
   IDX s REG SINK X64ASM:ENC-MOV-RR ;

\ The head binds with r12 holding the index: the entry arrives from LOOP-PUSH
\ and every back edge from LOOP-NEXT or LOOP-STEP.
: LOOP-OPEN ( vreg vreg bool -- )
   LABEL:LOOP-OPEN  1 HELD ! ;

: LOOP-POP ( -- )
   DATA-REG LOOPSP-CELL X64ASM:MEM-OFF
   STACK-ABI:LOOP-FRAME-BYTES negate X64ASM:>IMM8 SINK X64ASM:ENC-ADD-MI8
   0 HELD ! ;

: LOOP-I ( vreg -- )
   HELD @ 0= if FRAME-IDX then
   REG IDX SINK X64ASM:ENC-MOV-RR ;

\ The step is r12's, so this target leaves the free vreg alone.
: LOOP-NEXT ( vreg LABEL:head -- )
   nip {: m:LABEL:head :}
   SETTLE
   FRAME-IDX
   IDX SINK X64ASM:ENC-INC
   IDX X64ASM:RAX LIDX-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR
   IDX X64ASM:RAX LLIM-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-CMP-RM
   X64ASM:C-L m BACK-JCC ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative.
: LOOP-STEP ( vreg vreg LABEL:head -- )
   {: s:vreg t:vreg m:LABEL:head :}
   s REG {: sr:X64ASM:r64 :}  t REG {: r:X64ASM:r64 :}
   SETTLE
   FRAME-IDX
   r IDX SINK X64ASM:ENC-MOV-RR
   r X64ASM:RAX LLIM-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-SUB-RM
   IDX sr SINK X64ASM:ENC-ADD-RR
   IDX X64ASM:RAX LIDX-OFF X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MR
   X64ASM:RAX r SINK X64ASM:ENC-MOV-RR
   X64ASM:RAX sr SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX r SINK X64ASM:ENC-XOR-RR
   sr r SINK X64ASM:ENC-XOR-RR
   X64ASM:RAX sr SINK X64ASM:ENC-AND-RR
   X64ASM:C-NS m BACK-JCC ;

\ ---- the seam ----------------------------------------------------------------
\ Make this file's words, and LABEL's over its branch primitives, the ones the
\ EMIT- operations run at slot k.
: FILL-SLOT ( n -- )
   {: k:n :}
   NVREG k EMIT-VREGS-N !
   ['] PLACE:OPEN k EMIT-OPEN-XT !
   ['] PLACE:QUOT-OPEN k EMIT-QUOT-OPEN-XT !
   ['] PLACE:UNIT-OPEN k EMIT-UNIT-OPEN-XT !
   ['] RETURN k EMIT-RETURN-XT !
   ['] PLACE:CLOSE k EMIT-CLOSE-XT !
   ['] SLOT-CLOSE k EMIT-SLOT-CLOSE-XT !
   ['] CB-STUB k EMIT-CB-STUB-XT !
   ['] PLACE:BYTES k EMIT-BYTES-XT !
   ['] PATCH k EMIT-PATCH-XT !
   ['] LIT k EMIT-LIT-XT !
   ['] ADDR-DATA k EMIT-ADDR-DATA-XT !
   ['] ADDR-CODE k EMIT-ADDR-CODE-XT !
   ['] ADDR-QUOT k EMIT-ADDR-QUOT-XT !
   ['] PUSH k EMIT-PUSH-XT !
   ['] PUSH-LIT k EMIT-PUSH-LIT-XT !
   ['] POP k EMIT-POP-XT !
   ['] SPILL k EMIT-SPILL-XT !
   ['] SPILL-LIT k EMIT-SPILL-LIT-XT !
   ['] FILL k EMIT-FILL-XT !
   ['] DSP+ k EMIT-DSP+-XT !
   ['] MOV k EMIT-MOV-XT !
   ['] BASE k EMIT-BASE-XT !
   ['] EXCHANGE k EMIT-EXCHANGE-XT !
   ['] PLACE:SLOTS k EMIT-SLOTS-XT !
   ['] SLOT-STORE k EMIT-SLOT-STORE-XT !
   ['] SLOT-LOAD k EMIT-SLOT-LOAD-XT !
   ['] PLACE:REC-CELLS! k EMIT-REC-CELLS-XT !
   ['] ENV-OPEN k EMIT-ENV-OPEN-XT !
   ['] REC-STORE k EMIT-REC-STORE-XT !
   ['] REC-QUOT k EMIT-REC-QUOT-XT !
   ['] ENV-LOAD k EMIT-ENV-LOAD-XT !
   ['] OP k EMIT-OP-XT !
   ['] OP-LIT k EMIT-OP-LIT-XT !
   ['] SHIFT k EMIT-SHIFT-XT !
   ['] SHIFT-LIT k EMIT-SHIFT-LIT-XT !
   ['] MULX k EMIT-MULX-XT !
   ['] MUL-LIT k EMIT-MUL-LIT-XT !
   ['] DIV-LIT k EMIT-DIV-LIT-XT !
   ['] MOD-LIT k EMIT-MOD-LIT-XT !
   ['] NEG k EMIT-NEG-XT !
   ['] NOT k EMIT-NOT-XT !
   ['] CMP k EMIT-CMP-XT !
   ['] CMP-LIT k EMIT-CMP-LIT-XT !
   ['] FLAG k EMIT-FLAG-XT !
   ['] FLAG-LIT k EMIT-FLAG-LIT-XT !
   ['] LOAD k EMIT-LOAD-XT !
   ['] STORE k EMIT-STORE-XT !
   ['] CALL k EMIT-CALL-XT !
   ['] TAIL k EMIT-TAIL-XT !
   ['] CALL-SELF k EMIT-CALL-SELF-XT !
   ['] LABEL:IF, k EMIT-IF,-XT !
   ['] LABEL:ELSE, k EMIT-ELSE,-XT !
   ['] LABEL:THEN, k EMIT-THEN,-XT !
   ['] LABEL:BEGIN, k EMIT-BEGIN,-XT !
   ['] LABEL:WHILE, k EMIT-WHILE,-XT !
   ['] LABEL:REPEAT, k EMIT-REPEAT,-XT !
   ['] LABEL:UNTIL, k EMIT-UNTIL,-XT !
   ['] LABEL:IF-CC, k EMIT-IF-CC,-XT !
   ['] LABEL:WHILE-CC, k EMIT-WHILE-CC,-XT !
   ['] LABEL:UNTIL-CC, k EMIT-UNTIL-CC,-XT !
   ['] LABEL:AGAIN, k EMIT-AGAIN,-XT !
   ['] LABEL:CASE, k EMIT-CASE,-XT !
   ['] LABEL:OF, k EMIT-OF,-XT !
   ['] LABEL:OF-LIT k EMIT-OF-LIT-XT !
   ['] LABEL:ENDOF, k EMIT-ENDOF,-XT !
   ['] LABEL:ENDCASE, k EMIT-ENDCASE,-XT !
   ['] TOR k EMIT-TOR-XT !
   ['] RFROM k EMIT-RFROM-XT !
   ['] RFETCH k EMIT-RFETCH-XT !
   ['] LOOP-PUSH k EMIT-LOOP-PUSH-XT !
   ['] LOOP-POP k EMIT-LOOP-POP-XT !
   ['] LOOP-I k EMIT-LOOP-I-XT !
   ['] LOOP-J k EMIT-LOOP-J-XT !
   ['] LOOP-OPEN k EMIT-LOOP-OPEN-XT !
   ['] LABEL:LEAVE, k EMIT-LEAVE,-XT !
   ['] LABEL:LOOP-NEXT k EMIT-LOOP-NEXT-XT !
   ['] LABEL:LOOP-STEP k EMIT-LOOP-STEP-XT !
   ['] LABEL:LOOP-CLOSE k EMIT-LOOP-CLOSE-XT !
   ['] IMM? k EMIT-IMM?-XT !
   ['] MARK k LABEL:MARK-XT !
   ['] FWD-JUMP k LABEL:FWD-JUMP-XT !
   ['] FWD-ZERO k LABEL:FWD-ZERO-XT !
   ['] FWD-NE k LABEL:FWD-NE-XT !
   ['] FWD-NE-LIT k LABEL:FWD-NE-LIT-XT !
   ['] FWD-GE k LABEL:FWD-GE-XT !
   ['] FWD-CC k LABEL:FWD-CC-XT !
   ['] BACK-JUMP k LABEL:BACK-JUMP-XT !
   ['] BACK-ZERO k LABEL:BACK-ZERO-XT !
   ['] BACK-CC k LABEL:BACK-CC-XT !
   ['] RESOLVE k LABEL:RESOLVE-XT !
   ['] ENTRY-EQ k LABEL:ENTRY-EQ-XT !
   ['] LOOP-NEXT k LABEL:LOOP-NEXT-XT !
   ['] LOOP-STEP k LABEL:LOOP-STEP-XT !
   ['] UNIT-RESET k PLACE:UNIT-RESET-XT !
   ['] PROLOGUE k PLACE:PROLOGUE-XT !
   ['] EPILOGUE k PLACE:EPILOGUE-XT !
   ['] SELF-PUT k PLACE:SELF-PUT-XT !
   ['] ADR-PUT k PLACE:ADR-PUT-XT !
   ['] BRANCH-LEN k PLACE:BRANCH-LEN-XT !
   ['] BRANCH-PUT k PLACE:BRANCH-PUT-XT ! ;

CTARGET-ARCH:X86-64 ' FILL-SLOT EMIT-PROVIDE

;package
;package
