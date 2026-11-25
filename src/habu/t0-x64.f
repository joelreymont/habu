split-str
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
\ the sum as one lea, which keeps the codes, wherever control arrives or
\ leaves. A word that compares and branches settles before its compare, since
\ the CPU fuses the two only when they are adjacent. Cells above rbx may hold
\ live values until SETTLE, so nothing asynchronous may write there.

require src/lib/error.f
require src/lib/vector.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f
require src/habu/t0-label.f
require src/habu/t0-place.f

package COMP
package X86-64
using X64ASM
using PLACE

8 constant NVREG

\ An immediate is a sign-extended imm32.
: IMM? ( n -- bool ) {: v:n :}
   v -2147483648 >= v 2147483647 <= and ;

\ A sign-extended imm8.
: SHORT? ( n -- bool ) {: v:n :}
   v -128 >= v 127 <= and ;

: COND ( cc -- condition )
   {: c:cc :}
   c CC>N {: k:n :}
   k 0 = if C-E exit then
   k 1 = if C-NE exit then
   k 2 = if C-L exit then
   k 3 = if C-GE exit then
   k 4 = if C-LE exit then
   k 5 = if C-G exit then
   E-OPERAND throw ;

variable FLAGS                        \ nonzero while the condition codes are held
variable HELD                         \ nonzero while r12 holds the index
variable AHEAD

0 constant FORM-JMP
1 constant FORM-JCC

\ A branch's placeholder is its rel8 form; its rel32 form is so much longer.
2 constant HOLE
3 constant JMP-GROWTH
4 constant JCC-GROWTH

: SINK ( -- ^VEC:vector<u8> ) RAW @ ;

\ A body's contract: rbp is DATA, rbx the data stack pointer, r12 the loop
\ index, which any callee may write, and r13-r15 the other engine registers;
\ compiled code writes rbx as the stack and r12, and none of the others. The
\ vregs are rcx rdx rsi rdi r8-r11; rax is this file's temporary and holds no
\ value between two of its words. A body is entered by `call`, leaves by `ret`
\ and saves nothing; its frame on the machine stack is its own cells rounded to
\ 16.
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: IDX ( -- r64 ) ENGINE-GPR:X64-INDEX >R64 ;

: REG ( vreg -- r64 )
   {: v:vreg :}
   v VREG>N {: k:n :}
   k 0 < k NVREG >= or if E-OPERAND throw then
   k 0 = if RCX exit then
   k 1 = if RDX exit then
   k 2 = if RSI exit then
   k 3 = if RDI exit then
   k 4 + >R64 ;

\ The low k bytes of v into OUT at off.
: PUT ( n n n -- ) {: v:n off:n k:n :}
   OUT @ off VEC:AT {: p:^ :}
   k 0 ?do v i 8 * rshift $FF and p i + c! loop ;

: REL32! ( n n -- )
   {: d:n off:n :}
   d -2147483648 < d 2147483647 > or if E-X64EMIT-REACH throw then
   d off 4 PUT ;

\ Load the constant n into r with the shortest form that holds it and keeps
\ held condition codes.
: LIT-IN ( n r64 ^VEC:vector<u8> -- ) {: v:n r:r64 buf:^ :}
   r R64>N >R32 {: r4:r32 :}
   v 0 =  FLAGS @ 0=  and if r4 r4 buf ENC-XOR32-RR exit then
   v 0 >= v $FFFFFFFF <= and if r4 v >IMM32 buf ENC-MOV32-RI32 exit then
   v IMM? if r v >IMM32 buf ENC-MOV-RI32 exit then
   r v >IMM64 buf ENC-MOV-RI64 ;

: LIT-R ( n r64 -- ) SINK LIT-IN ;

: ADDR ( n n vreg n -- )
   {: v:n o:n d:vreg kind:n :}
   HERE-OFF kind v o PEND+
   d REG v >IMM64 SINK ENC-MOV-RI64 ;

\ r, holding the return stack depth, becomes the address one cell past its top.
: RTOP ( r64 -- ) {: r:r64 :}
   r 3 >IMM8 SINK ENC-SHL-RI8
   r DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF SINK ENC-ADD-RM ;

: RDEPTH ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-RM ;

: RDEPTH! ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-MR ;

\ ---- the data stack pointer --------------------------------------------------
: SETTLE ( -- )
   AHEAD @ {: k:n :}
   k 0 = if exit then
   DSP DSP k CELL * MEM-OFF SINK ENC-LEA
   0 AHEAD ! ;

: DSP+ ( n -- ) AHEAD +! ;

\ The cell k cells above the stack's top.
: TOP-MEM ( n -- mem ) {: k:n :}  DSP AHEAD @ k + CELL * MEM-OFF ;

\ ---- PLACE's hooks -----------------------------------------------------------
: UNIT-RESET ( -- ) 0 FLAGS !  0 HELD !  0 AHEAD ! ;

: PROLOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   bytes 0= if exit then
   bytes SHORT? if RSP bytes >IMM8 buf ENC-SUB-RI8 exit then
   RSP bytes >IMM32 buf ENC-SUB-RI32 ;

: EPILOGUE ( n ^VEC:vector<u8> -- ) {: bytes:n buf:^ :}
   bytes 0= if exit then
   bytes SHORT? if RSP bytes >IMM8 buf ENC-ADD-RI8 exit then
   RSP bytes >IMM32 buf ENC-ADD-RI32 ;

\ The call is E8 rel32, and the address lea its REX, opcode, ModRM and disp32.
: SELF-PUT ( n -- ) {: at:n :}  at 5 + negate  at 1 + REL32! ;

: ADR-PUT ( n n -- ) {: at:n to:n :}  to at 7 + -  at 3 + REL32! ;

: BRANCH-LEN ( n n -- n ) {: k:n d:n :}
   d HOLE - SHORT? if 0 exit then
   k BR-FORM @ FORM-JMP = if JMP-GROWTH exit then
   JCC-GROWTH ;

\ EB or 70+cc, rel8; else E9 or 0F 80+cc, rel32.
: BRANCH-PUT ( n n n -- ) {: k:n at:n to:n :}
   at HOLE + k BR-LEN @ + {: end:n :}
   to end - {: d:n :}
   k BR-FORM @ FORM-JMP = {: jmp:bool :}
   k BR-LEN @ 0= if
      jmp if $EB else $70 k BR-ARG @ + then at 1 PUT
      d at 1 + 1 PUT exit
   then
   jmp if $E9 at 1 PUT else $800F k BR-ARG @ 8 lshift + at 2 PUT then
   d end 4 - REL32! ;

\ ---- the body ----------------------------------------------------------------
\ A reading quotation receives its record in rdi.
\
\ The rows (t0-emit.f "THE ROWS"): a call or tail site is E8 or E9 rel32, zero
\ until PATCH measures it from its placed end; an address site is movabs r64,
\ imm64, whose imm64 starts two bytes past the row's offset. A quotation's
\ RIP-relative lea names its unit inside the emission and needs no row.
: RETURN ( -- ) SETTLE EPI+  SINK ENC-RET ;

\ The return is the kernel's does> patch slot, `jmp rel32` and the `ret` it
\ falls through to, filed at the jump's offset.
: SLOT-CLOSE ( -- )
   SETTLE EPI+
   0 >REL SINK ENC-JMP-REL32
   SINK ENC-RET
   PLACE:CLOSE
   OUT-OFF 6 - EMIT-PATCH-SLOT! ;

\ The stub after the last unit, answering its offset: r10 its slot, rax the
\ thunk, which writes rax first (src/habu/kernel-x64.f CALLBACK-ENTRY-BODY).
: CB-STUB ( -- n )
   OUT-OFF {: at:n :}
   NCBSTUB:SLOT@ R10 OUT @ LIT-IN
   NCBSTUB:THUNK@ {: thunk:n :}
   OUT-OFF NEMIT:ADDR-CODE thunk -1 EMIT-ADDR-ROW+
   RAX thunk >IMM64 OUT @ ENC-MOV-RI64
   RAX OUT @ ENC-JMP-REG
   at ;

\ A call's rel32 is measured from the end of its five bytes, placed at.
: PATCH-CALL ( n n n -- ) {: at:n off:n to:n :}
   to at off + 5 + - off 1+ REL32! ;

: PATCH ( n -- ) {: at:n :}
   EMIT-CALL-ROWS 0 ?do
      i EMIT-CALL-ROW {: off:n kind:n to:n :}
      at off to PATCH-CALL
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
   HERE-OFF ADDR-QUOT-KIND n -1 PEND+
   d REG 0 MEM-RIP SINK ENC-LEA ;

: PUSH ( vreg n -- ) {: s:vreg k:n :}
   s REG 0 TOP-MEM SINK ENC-MOV-MR
   k DSP+ ;

\ Store the constant n at m, keeping held condition codes.
: LIT-M ( n mem -- ) {: v:n m:mem :}
   v IMM? if v >IMM32 m SINK ENC-MOV-MI32 exit then
   v RAX LIT-R
   RAX m SINK ENC-MOV-MR ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v 0 TOP-MEM LIT-M
   k DSP+ ;

: POP ( vreg n -- ) {: d:vreg k:n :}
   k negate DSP+
   d REG 0 TOP-MEM SINK ENC-MOV-RM ;

: SPILL ( vreg n -- )
   {: s:vreg k:n :}
   s REG k TOP-MEM SINK ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v k TOP-MEM LIT-M ;

: FILL ( vreg n -- )
   {: d:vreg k:n :}
   d REG k TOP-MEM SINK ENC-MOV-RM ;

: MOV ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG SINK ENC-MOV-RR ;

: EXCHANGE ( vreg vreg -- )
   {: a:vreg b:vreg :}
   a REG b REG SINK ENC-XCHG-RR ;

: SLOT-STORE ( vreg n -- )
   {: s:vreg k:n :}
   s REG RSP k SLOT-OFF MEM-OFF SINK ENC-MOV-MR ;

: SLOT-LOAD ( vreg n -- )
   {: d:vreg k:n :}
   d REG RSP k SLOT-OFF MEM-OFF SINK ENC-MOV-RM ;

: ENV-OPEN ( -- )
   BODY-EMPTY 1 ENV !
   RDI RSP MEM-AT SINK ENC-MOV-MR ;

: REC-STORE ( vreg n -- )
   {: s:vreg n:n :}
   s REG RSP n REC-OFF MEM-OFF SINK ENC-MOV-MR ;

: REC-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   d REG RSP n REC-OFF MEM-OFF SINK ENC-LEA
   $8000000000000000 RAX LIT-R
   d REG RAX SINK ENC-OR-RR ;

: ENV-LOAD ( vreg n -- )
   {: d:vreg n:n :}
   ENV @ 0= if E-T0-UNIT throw then
   d REG {: r:r64 :}
   r RSP MEM-AT SINK ENC-MOV-RM
   r r n CELL * MEM-OFF SINK ENC-MOV-RM ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( vreg vreg alu -- )
   {: d:vreg s:vreg o:alu :}
   d REG {: dr:r64 :}  s REG {: sr:r64 :}  o ALU>N {: k:n :}
   k 0 = if dr sr SINK ENC-ADD-RR exit then
   k 1 = if dr sr SINK ENC-SUB-RR exit then
   k 2 = if dr sr SINK ENC-AND-RR exit then
   k 3 = if dr sr SINK ENC-OR-RR exit then
   k 4 = if dr sr SINK ENC-XOR-RR exit then
   E-OPERAND throw ;

: OP-RI8 ( r64 imm8 alu -- )
   {: dr:r64 iv:imm8 o:alu :}
   o ALU>N {: k:n :}
   k 0 = if dr iv SINK ENC-ADD-RI8 exit then
   k 1 = if dr iv SINK ENC-SUB-RI8 exit then
   k 2 = if dr iv SINK ENC-AND-RI8 exit then
   k 3 = if dr iv SINK ENC-OR-RI8 exit then
   k 4 = if dr iv SINK ENC-XOR-RI8 exit then
   E-OPERAND throw ;

: OP-RI32 ( r64 imm32 alu -- )
   {: dr:r64 iv:imm32 o:alu :}
   o ALU>N {: k:n :}
   k 0 = if dr iv SINK ENC-ADD-RI32 exit then
   k 1 = if dr iv SINK ENC-SUB-RI32 exit then
   k 2 = if dr iv SINK ENC-AND-RI32 exit then
   k 3 = if dr iv SINK ENC-OR-RI32 exit then
   k 4 = if dr iv SINK ENC-XOR-RI32 exit then
   E-OPERAND throw ;

: OP-LIT ( vreg n alu -- )
   {: d:vreg v:n o:alu :}
   v SHORT? if d REG v >IMM8 o OP-RI8 exit then
   d REG v >IMM32 o OP-RI32 ;

\ A shift by a register takes its count in cl, and cl is vreg 0's low byte;
\ the hardware masks the count to six bits. SHIFT-LIT masks its count here.
: SHIFT-CL ( r64 shifter -- )
   {: r:r64 o:shifter :}
   o SHIFTER>N 0 = if r SINK ENC-SHL-CL exit then
   r SINK ENC-SHR-CL ;

: SHIFT ( vreg vreg shifter -- )
   {: d:vreg s:vreg o:shifter :}
   s VREG>N 0 = if d REG o SHIFT-CL exit then
   RAX RCX SINK ENC-MOV-RR
   RCX s REG SINK ENC-MOV-RR
   d VREG>N 0 = if
      RAX o SHIFT-CL
      RCX RAX SINK ENC-MOV-RR exit
   then
   d REG o SHIFT-CL
   RCX RAX SINK ENC-MOV-RR ;

: SHIFT-LIT ( vreg n shifter -- )
   {: d:vreg v:n o:shifter :}
   v 63 and >IMM8 {: c:imm8 :}
   o SHIFTER>N 0 = if d REG c SINK ENC-SHL-RI8 exit then
   d REG c SINK ENC-SHR-RI8 ;

: MULX ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG SINK ENC-IMUL-RR ;

: MUL-LIT ( vreg n -- )
   {: d:vreg v:n :}
   v SHORT? if d REG d REG v >IMM8 SINK ENC-IMUL-RRI8 exit then
   d REG d REG v >IMM32 SINK ENC-IMUL-RRI32 ;

\ idiv divides rdx:rax and writes rax and rdx, and rdx is a vreg, so it is
\ saved on the machine stack unless d is rdx. The divisor goes into d once
\ the dividend is in rax; when d is rdx, which cqo writes, into rcx, saved
\ instead. The saved register is the only one restored, so every vreg but d
\ keeps its value.
: DIVIDE ( vreg n bool -- )
   {: d:vreg v:n rem:bool :}
   d REG {: dr:r64 :}
   dr R64>N RDX R64>N = {: in-rdx:bool :}
   in-rdx if RCX else dr then {: sr:r64 :}
   in-rdx if RCX else RDX then {: saved:r64 :}
   saved SINK ENC-PUSH
   RAX dr SINK ENC-MOV-RR
   v sr LIT-R
   SINK ENC-CQO
   sr SINK ENC-IDIV
   rem if RDX else RAX then {: res:r64 :}
   res R64>N dr R64>N <> if dr res SINK ENC-MOV-RR then
   saved SINK ENC-POP ;

: DIV-LIT ( vreg n -- ) false DIVIDE ;
: MOD-LIT ( vreg n -- ) true DIVIDE ;

: NEG ( vreg -- ) REG SINK ENC-NEG ;
: NOT ( vreg -- ) REG SINK ENC-NOT ;

: CMP ( vreg vreg -- )
   {: a:vreg b:vreg :}
   SETTLE
   1 FLAGS !
   a REG b REG SINK ENC-CMP-RR ;

\ `test r, r` sets every signed condition as `cmp r, 0` does.
: CMP-R ( r64 n -- )
   {: r:r64 v:n :}
   v 0 = if r r SINK ENC-TEST-RR exit then
   v SHORT? if r v >IMM8 SINK ENC-CMP-RI8 exit then
   r v >IMM32 SINK ENC-CMP-RI32 ;

: CMP-LIT ( vreg n -- )
   {: a:vreg v:n :}
   SETTLE
   1 FLAGS !
   a REG v CMP-R ;

: SAME? ( vreg vreg -- bool ) VREG>N swap VREG>N = ;

: ZERO ( vreg -- ) REG R64>N >R32 dup SINK ENC-XOR32-RR ;

\ -1 in d when the codes hold a borrow, else 0.
: BORROW ( vreg -- ) REG dup SINK ENC-SBB-RR ;

\ d's flag from the codes of a compare; d is zero unless via, when d is an
\ operand and the byte goes through al.
: FLAG-END ( vreg cc bool -- )
   {: d:vreg c:cc via:bool :}
   d REG {: r:r64 :}
   via if
      c COND 0 >R8 SINK ENC-SETCC
      r R64>N >R32 0 >R8 SINK ENC-MOVZX32-8-RR
   else
      c COND r R64>N >R8 SINK ENC-SETCC
   then
   r SINK ENC-NEG ;

: FLAG ( vreg vreg vreg cc -- )
   {: d:vreg a:vreg b:vreg c:cc :}
   d a SAME? d b SAME? or {: via:bool :}
   via 0= if d ZERO then
   a REG b REG SINK ENC-CMP-RR
   d c via FLAG-END ;

\ -1 in d when a is negative, else 0.
: SIGN ( vreg vreg -- )
   {: d:vreg a:vreg :}
   d a SAME? 0= if d a MOV then
   d REG 63 >IMM8 SINK ENC-SAR-RI8 ;

\ An unsigned compare with 1, or of 0 with a, borrows only when the flag holds.
: ZERO-EQ ( vreg vreg -- )
   {: d:vreg a:vreg :}
   a REG 1 >IMM8 SINK ENC-CMP-RI8  d BORROW ;

: ZERO-NE ( vreg vreg -- )
   {: d:vreg a:vreg :}
   d a SAME? if d NEG else d ZERO  d REG a REG SINK ENC-CMP-RR then
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
   via 0= if d ZERO then
   a REG v CMP-R
   d c via FLAG-END ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( vreg vreg width -- )
   {: d:vreg a:vreg w:width :}
   w WIDTH>N 1 = if d REG a REG MEM-AT SINK ENC-MOVZX-8-RM exit then
   d REG a REG MEM-AT SINK ENC-MOV-RM ;

: STORE ( vreg vreg width -- )
   {: s:vreg a:vreg w:width :}
   s REG {: r:r64 :}
   w WIDTH>N 1 = if r R64>N >R8 a REG MEM-AT SINK ENC-MOV8-MR exit then
   r a REG MEM-AT SINK ENC-MOV-MR ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- ) {: to:n loc:n :}
   SETTLE
   0 HELD !
   HERE-OFF NEMIT:CALL to loc PCALL+
   0 >REL SINK ENC-CALL-REL32 ;

: TAIL ( n n -- ) {: to:n loc:n :}
   SETTLE EPI+
   HERE-OFF NEMIT:TAIL to loc PCALL+
   0 >REL SINK ENC-JMP-REL32 ;

: CALL-SELF ( -- )
   SETTLE
   0 HELD !
   HERE-OFF SELF+
   0 >REL SINK ENC-CALL-REL32 ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives LABEL emits through. A forward branch is its number
\ in the open unit's list, and a head a raw offset.

\ A branch of the form, with its condition or 0, to a raw offset or -1.
: BRANCH ( n n n -- LABEL:branch ) SETTLE HOLE BR+ ;

: RESOLVE ( LABEL:branch -- ) SETTLE PLACE:RESOLVE  0 HELD ! ;

: MARK ( -- LABEL:head ) SETTLE 0 HELD !  PLACE:MARK ;

: FWD-JUMP ( -- LABEL:branch ) FORM-JMP 0 -1 BRANCH ;

: FWD-JCC ( condition -- LABEL:branch ) {: c:condition :}
   FORM-JCC c CONDITION>N -1 BRANCH ;

: TEST ( vreg -- )
   {: c:vreg :}
   c REG c REG SINK ENC-TEST-RR ;

: FWD-ZERO ( vreg -- LABEL:branch ) SETTLE TEST C-E FWD-JCC ;

: FWD-NE ( vreg vreg -- LABEL:branch )
   {: sel:vreg v:vreg :}
   SETTLE
   sel REG v REG SINK ENC-CMP-RR
   C-NE FWD-JCC ;

: FWD-NE-LIT ( vreg n -- LABEL:branch )
   {: sel:vreg v:n :}
   SETTLE
   sel REG v CMP-R
   C-NE FWD-JCC ;

: FWD-GE ( vreg vreg -- LABEL:branch )
   {: s:vreg l:vreg :}
   SETTLE
   s REG l REG SINK ENC-CMP-RR
   C-GE FWD-JCC ;

: FWD-CC ( cc -- LABEL:branch )
   0 FLAGS !
   COND FWD-JCC ;

\ The branch becomes je.
: ENTRY-EQ ( LABEL:branch -- ) LABEL:BRANCH>N {: k:n :}
   C-E CONDITION>N k BR-ARG ! ;

: BACK-JUMP ( LABEL:head -- ) LABEL:HEAD>N FORM-JMP 0 rot BRANCH drop ;

: BACK-JCC ( condition LABEL:head -- )
   LABEL:HEAD>N {: c:condition m:n :}
   FORM-JCC c CONDITION>N m BRANCH drop ;

: BACK-ZERO ( vreg LABEL:head -- )
   {: c:vreg m:LABEL:head :}
   SETTLE c TEST C-E m BACK-JCC ;

: BACK-CC ( cc LABEL:head -- )
   {: c:cc m:LABEL:head :}
   0 FLAGS !
   c COND m BACK-JCC ;

\ ---- the return stack --------------------------------------------------------
\ Each goes through rax. Vreg i of p moved to or from the slot off bytes above
\ rax.
: RS-XFER ( ^vreg n n bool -- ) {: p:^ i:n off:n st:bool :}
   p i VREG-AT REG RAX off MEM-OFF SINK
   st if ENC-MOV-MR exit then ENC-MOV-RM ;

: RDEPTH+ ( n -- ) {: k:n :} RAX RAX k MEM-OFF SINK ENC-LEA ;

: TOR ( ^vreg n -- ) {: p:^ n:n :}
   RAX RDEPTH
   n RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i n - CELL * true RS-XFER loop ;

: RFROM ( ^vreg n -- ) {: p:^ n:n :}
   RAX RDEPTH
   n negate RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i CELL * false RS-XFER loop ;

: RFETCH ( ^vreg n n -- ) {: p:^ n:n k:n :}
   RAX RDEPTH
   RAX RTOP
   n 0 ?do p i i n - k - CELL * false RS-XFER loop ;


\ ---- loops ---------------------------------------------------------------------
: LDEPTH ( r64 -- ) {: r:r64 :}
   r DATA-REG LOOPSP-CELL MEM-OFF SINK ENC-MOV-RM ;

: LDEPTH! ( r64 -- ) {: r:r64 :}
   r DATA-REG LOOPSP-CELL MEM-OFF SINK ENC-MOV-MR ;

\ r, holding the depth, becomes the address one frame past the innermost.
: LTOP ( r64 -- ) {: r:r64 :}
   r 4 >IMM8 SINK ENC-SHL-RI8
   r DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF SINK ENC-ADD-RM ;

\ The index and limit of the innermost frame, below LTOP's address.
-16 constant LIDX-OFF
-8 constant LLIM-OFF

: LOOP-J ( vreg -- )
   {: d:vreg :}
   RAX LDEPTH
   RAX LTOP
   d REG RAX LIDX-OFF 16 - MEM-OFF SINK ENC-MOV-RM ;

\ rax the address past the innermost frame, and r12 its index unless HELD
\ says it holds it already.
: FRAME-IDX ( -- )
   RAX LDEPTH
   RAX LTOP
   HELD @ if exit then
   IDX RAX LIDX-OFF MEM-OFF SINK ENC-MOV-RM
   1 HELD ! ;

: LOOP-PUSH ( vreg vreg -- )
   {: s:vreg l:vreg :}
   RAX LDEPTH
   RAX SINK ENC-INC
   RAX LDEPTH!
   RAX LTOP
   s REG RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   l REG RAX LLIM-OFF MEM-OFF SINK ENC-MOV-MR
   IDX s REG SINK ENC-MOV-RR ;

\ The head binds with r12 holding the index: the entry arrives from LOOP-PUSH
\ and every back edge from LOOP-NEXT or LOOP-STEP.
: LOOP-OPEN ( vreg vreg bool -- )
   LABEL:LOOP-OPEN  1 HELD ! ;

: LOOP-POP ( -- )
   RAX LDEPTH
   RAX SINK ENC-DEC
   RAX LDEPTH!
   0 HELD ! ;

: LOOP-I ( vreg -- )
   HELD @ 0= if FRAME-IDX then
   REG IDX SINK ENC-MOV-RR ;

\ The step is r12's, so this target leaves the free vreg alone.
: LOOP-NEXT ( vreg LABEL:head -- )
   nip {: m:LABEL:head :}
   SETTLE
   FRAME-IDX
   IDX SINK ENC-INC
   IDX RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   IDX RAX LLIM-OFF MEM-OFF SINK ENC-CMP-RM
   C-L m BACK-JCC ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative.
: LOOP-STEP ( vreg vreg LABEL:head -- )
   {: s:vreg t:vreg m:LABEL:head :}
   s REG {: sr:r64 :}  t REG {: r:r64 :}
   SETTLE
   FRAME-IDX
   r IDX SINK ENC-MOV-RR
   r RAX LLIM-OFF MEM-OFF SINK ENC-SUB-RM
   IDX sr SINK ENC-ADD-RR
   IDX RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   RAX r SINK ENC-MOV-RR
   RAX sr SINK ENC-ADD-RR
   RAX r SINK ENC-XOR-RR
   sr r SINK ENC-XOR-RR
   RAX sr SINK ENC-AND-RR
   C-NS m BACK-JCC ;

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

;using
;using
;package
;package
