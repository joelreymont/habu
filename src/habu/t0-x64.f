\ t0-x64.f - the x86-64 emitter of the tier 0 compiler, part X86-64 of package
\ COMP.
\
\ Implements the EMIT- operations src/habu/t0-emit.f states for every target;
\ this file holds only x86-64 encodings and register choices. FILL-SLOT
\ stores its words into the EMIT- and src/habu/t0-label.f (LABEL) cells for
\ CTARGET-ARCH:X86-64.

require src/lib/error.f
require src/lib/byte-buffer.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f
require src/habu/t0-label.f

package COMP
package X86-64
using X64ASM

8 constant NVREG

\ An immediate is a sign-extended imm32.
: IMM? ( n -- bool ) {: v:n :}
   v -2147483648 >= v 2147483647 <= and ;

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

create SINK BUF:HDR-BYTES SCRATCH-ALLOT
SCRATCH-VARIABLE SINK-LIVE            \ nonzero once SINK holds storage
variable FRAME-CELLS                  \ machine-stack cells this body uses
variable NFRAME
variable NUNIT                        \ units begun in this emission
DYNAMIC-BUFFER FRAME-AT n             \ offset of each frame imm32 field
DYNAMIC-BUFFER UNIT-REF n             \ the address site that names each unit

4096 constant SINK-START-BYTES

: HERE-OFF ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;

\ A body's contract (src/habu/kernel-x64.f "A BODY'S CONTRACT"): rbp is DATA,
\ r12 the data stack pointer, rbx and r13-r15 the other engine registers;
\ compiled code writes none of them except r12 as the stack
\ (src/arch/x86-64/rt.f G-PUSH, G-POP). The vregs are rcx rdx rsi rdi r8-r11;
\ rax is this file's temporary and holds no value between two of its words. A
\ body is entered by `call`, leaves by `ret` and saves nothing; its frame on
\ the machine stack is its own cells rounded to 16.
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

: REG ( vreg -- r64 )
   {: v:vreg :}
   v VREG>N {: k:n :}
   k 0 < k NVREG >= or if E-OPERAND throw then
   k 0 = if RCX exit then
   k 1 = if RDX exit then
   k 2 = if RSI exit then
   k 3 = if RDI exit then
   k 4 + >R64 ;

: PUT ( n n n -- ) {: v:n off:n k:n :}
   SINK BUF:SPAN$ drop off + {: p:ptr :}
   k 0 ?do v i 8 * rshift $FF and p i + c! loop ;

\ A forward rel32 at the end of what was just emitted: its field's offset.
: FWD-SITE ( -- LABEL:branch ) HERE-OFF 4 - LABEL:>BRANCH ;

\ Load the constant n into r with the shortest form that holds it and leaves
\ the condition codes alone: zero is a mov, not an xor.
: LIT-R ( n r64 -- ) {: v:n r:r64 :}
   v 0 >= v $FFFFFFFF <= and if r R64>N >R32 v >IMM32 SINK ENC-MOV32-RI32 exit then
   v IMM? if r v >IMM32 SINK ENC-MOV-RI32 exit then
   r v >IMM64 SINK ENC-MOV-RI64 ;

: FRAME-SITE ( -- )
   HERE-OFF 4 - {: at:n :}
   NFRAME @ {: k:n :}
   k 1 + FRAME-AT-RESERVE
   at k FRAME-AT !
   k 1 + NFRAME ! ;

: UNFRAME ( -- )
   RSP 0 >IMM32 SINK ENC-ADD-RI32  FRAME-SITE ;

: ADDR-R ( n n r64 n -- )
   {: v:n o:n r:r64 kind:n :}
   HERE-OFF kind v o EMIT-ADDR-ROW+
   r v >IMM64 SINK ENC-MOV-RI64 ;

: ADDR ( n n vreg n -- )
   {: v:n o:n d:vreg kind:n :}
   v o d REG kind ADDR-R ;

\ r, holding the return stack depth, becomes the address one cell past its top.
: RTOP ( r64 -- ) {: r:r64 :}
   r 3 >IMM8 SINK ENC-SHL-RI8
   r DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF SINK ENC-ADD-RM ;

: RDEPTH ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-RM ;

: RDEPTH! ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-MR ;

: REL-TO ( n -- rel ) HERE-OFF - >REL ;

: UNIT-BEGIN ( -- )
   HERE-OFF EMIT-UNIT+
   NUNIT @ 1 + NUNIT !
   0 FRAME-CELLS !  0 NFRAME !
   RSP 0 >IMM32 SINK ENC-SUB-RI32  FRAME-SITE ;


\ ---- the body ----------------------------------------------------------------
\ The frame: each unit's start emits `sub rsp, imm32` and each way out emits
\ `add rsp, imm32`, each field recorded in FRAME-AT; CLOSE writes the unit's
\ frame size into every one. Slot k is the cell at rsp + 8k.
\
\ The rows (t0-emit.f "THE ROWS"): a call or tail site is E8 or E9 rel32, zero
\ until PATCH measures it from its placed end; an address site is movabs r64,
\ imm64, whose imm64 starts two bytes past the row's offset. A quotation's
\ imm64 holds its unit's offset until PATCH adds the placement. Every branch is
\ rel32.
: OPEN ( -- )
   SINK-LIVE @ 0= if
      SINK SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 SINK-LIVE !
   then
   SINK BUF:CLEAR
   0 NUNIT !
   LABEL:RESET
   UNIT-BEGIN ;

: QUOT-OPEN ( n -- ) {: n:n :}
   n NUNIT @ <> if E-T0-UNIT throw then
   HERE-OFF n UNIT-REF @ 2 + 8 PUT
   UNIT-BEGIN ;

: UNIT-OPEN ( -- ) UNIT-BEGIN ;

: RETURN ( -- )
   UNFRAME  SINK ENC-RET ;

: CLOSE ( -- )
   FRAME-CELLS @ CELL * 15 + 15 invert and {: bytes:n :}
   NFRAME @ 0 ?do bytes i FRAME-AT @ 4 PUT loop ;

\ The return is the kernel's does> patch slot, filed at the jump's offset.
: SLOT-CLOSE ( -- )
   UNFRAME
   HERE-OFF EMIT-PATCH-SLOT!
   0 >REL SINK ENC-JMP-REL32
   SINK ENC-RET
   CLOSE ;

\ The stub after the last unit, answering its offset: r10 its slot, rax the
\ thunk, which writes rax first (src/habu/kernel-x64.f CALLBACK-ENTRY-BODY).
: CB-STUB ( -- n )
   HERE-OFF {: at:n :}
   NCBSTUB:SLOT@ R10 LIT-R
   NCBSTUB:THUNK@ -1 RAX NEMIT:ADDR-CODE ADDR-R
   RAX SINK ENC-JMP-REG
   at ;

: BYTES ( -- ptr u8 n ) SINK BUF:SPAN$ BUF:BLEN>N ;

\ A call's rel32 is measured from the end of its five bytes, placed at.
: PATCH-CALL ( n n n -- ) {: at:n off:n to:n :}
   to at off + 5 + - {: d:n :}
   d -2147483648 < d 2147483647 > or if E-X64EMIT-REACH throw then
   d off 1 + 4 PUT ;

: PATCH ( n -- ) {: at:n :}
   EMIT-CALL-ROWS 0 ?do
      i EMIT-CALL-ROW {: off:n kind:n to:n :}
      at off to PATCH-CALL
   loop
   EMIT-ADDR-ROWS 0 ?do
      i EMIT-ADDR-ROW {: off:n kind:n to:n :}
      kind ADDR-QUOT-KIND = if at to EMIT-UNIT + off 2 + 8 PUT then
   loop ;

\ ---- values ------------------------------------------------------------------
: LIT ( n vreg -- )
   {: v:n d:vreg :}
   v d REG LIT-R ;

: ADDR-DATA ( n n vreg -- ) NEMIT:ADDR-DATA ADDR ;

: ADDR-CODE ( n vreg -- )
   {: v:n d:vreg :}
   v -1 d NEMIT:ADDR-CODE ADDR ;

\ The site's imm64 holds 0 until QUOT-OPEN writes unit n's offset.
: ADDR-QUOT ( n vreg -- )
   {: n:n d:vreg :}
   n 1 + UNIT-REF-RESERVE
   HERE-OFF n UNIT-REF !
   HERE-OFF ADDR-QUOT-KIND n -1 EMIT-ADDR-ROW+
   d REG 0 >IMM64 SINK ENC-MOV-RI64 ;

\ The stack pointer moved by k cells: an lea, which leaves the condition codes
\ alone, as EMIT-CMP requires.
: DSP+ ( n -- ) {: k:n :}
   k 0 = if exit then
   DSP DSP k CELL * MEM-OFF SINK ENC-LEA ;

\ x86-64 has no writeback: a push stores and then moves the pointer, and a pop
\ moves it and then loads, as G-PUSH and G-POP do.
: PUSH ( vreg n -- ) {: s:vreg k:n :}
   s REG DSP MEM-AT SINK ENC-MOV-MR
   k DSP+ ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP MEM-AT SINK ENC-MOV-MR
   k DSP+ ;

: POP ( vreg n -- ) {: d:vreg k:n :}
   k negate DSP+
   d REG DSP MEM-AT SINK ENC-MOV-RM ;

: SPILL ( vreg n -- )
   {: s:vreg k:n :}
   s REG DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: FILL ( vreg n -- )
   {: d:vreg k:n :}
   d REG DSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

: MOV ( vreg vreg -- )
   {: d:vreg s:vreg :}
   d REG s REG SINK ENC-MOV-RR ;

: EXCHANGE ( vreg vreg -- )
   {: a:vreg b:vreg :}
   a REG b REG SINK ENC-XCHG-RR ;

: SLOTS ( n -- ) {: k:n :}
   k FRAME-CELLS @ > if k FRAME-CELLS ! then ;

: SLOT-STORE ( vreg n -- )
   {: s:vreg k:n :}
   s REG RSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SLOT-LOAD ( vreg n -- )
   {: d:vreg k:n :}
   d REG RSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

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

: OP-LIT ( vreg n alu -- )
   {: d:vreg v:n o:alu :}
   d REG {: dr:r64 :}  v >IMM32 {: iv:imm32 :}  o ALU>N {: k:n :}
   k 0 = if dr iv SINK ENC-ADD-RI32 exit then
   k 1 = if dr iv SINK ENC-SUB-RI32 exit then
   k 2 = if dr iv SINK ENC-AND-RI32 exit then
   k 3 = if dr iv SINK ENC-OR-RI32 exit then
   k 4 = if dr iv SINK ENC-XOR-RI32 exit then
   E-OPERAND throw ;

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

: SET-FLAG ( vreg cc -- )
   {: d:vreg c:cc :}
   c COND 0 >R8 SINK ENC-SETCC
   d REG 0 >R8 SINK ENC-MOVZX-8-RR
   d REG SINK ENC-NEG ;

: CMP ( vreg vreg -- )
   {: a:vreg b:vreg :}
   a REG b REG SINK ENC-CMP-RR ;

: CMP-LIT ( vreg n -- )
   {: a:vreg v:n :}
   a REG v >IMM32 SINK ENC-CMP-RI32 ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( vreg vreg width -- )
   {: d:vreg a:vreg w:width :}
   w WIDTH>N 1 = if d REG a REG MEM-AT SINK ENC-MOVZX-8-RM exit then
   d REG a REG MEM-AT SINK ENC-MOV-RM ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- ) {: to:n loc:n :}
   HERE-OFF NEMIT:CALL to loc EMIT-CALL-ROW+
   0 >REL SINK ENC-CALL-REL32 ;

: TAIL ( n n -- ) {: to:n loc:n :}
   UNFRAME
   HERE-OFF NEMIT:TAIL to loc EMIT-CALL-ROW+
   0 >REL SINK ENC-JMP-REL32 ;

: CALL-SELF ( -- )
   0 HERE-OFF 5 + - >REL SINK ENC-CALL-REL32 ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives LABEL emits through. A forward branch is the offset
\ of its rel32 field, waiting for its target, and a head is an offset.

: RESOLVE ( LABEL:branch -- ) LABEL:BRANCH>N {: at:n :}
   HERE-OFF at 4 + - at 4 PUT ;

: MARK ( -- LABEL:head ) HERE-OFF LABEL:>HEAD ;

: FWD-JUMP ( -- LABEL:branch )
   0 >REL SINK ENC-JMP-REL32  FWD-SITE ;

: FWD-JCC ( condition -- LABEL:branch ) {: c:condition :}
   c 0 >REL SINK ENC-JCC-REL32  FWD-SITE ;

: TEST ( vreg -- )
   {: c:vreg :}
   c REG c REG SINK ENC-TEST-RR ;

: FWD-ZERO ( vreg -- LABEL:branch ) TEST C-E FWD-JCC ;

: FWD-NE ( vreg vreg -- LABEL:branch )
   {: sel:vreg v:vreg :}
   sel REG v REG SINK ENC-CMP-RR
   C-NE FWD-JCC ;

: FWD-NE-LIT ( vreg n -- LABEL:branch )
   {: sel:vreg v:n :}
   sel v CMP-LIT
   C-NE FWD-JCC ;

: FWD-GE ( vreg vreg -- LABEL:branch )
   {: s:vreg l:vreg :}
   s REG l REG SINK ENC-CMP-RR
   C-GE FWD-JCC ;

: FWD-CC ( cc -- LABEL:branch ) COND FWD-JCC ;

\ The branch's jcc becomes je: its opcode byte is just before the field.
: ENTRY-EQ ( LABEL:branch -- ) LABEL:BRANCH>N {: at:n :}
   C-E CONDITION>N $80 +  SINK BUF:SPAN$ drop at 1 - + c! ;

\ A backward rel is measured from the end of the instruction, five bytes for
\ jmp rel32 and six for jcc rel32.
: BACK-JUMP ( LABEL:head -- ) LABEL:HEAD>N {: m:n :}
   m 5 - REL-TO SINK ENC-JMP-REL32 ;

: BACK-JCC ( condition LABEL:head -- )
   LABEL:HEAD>N {: c:condition m:n :}
   c m 6 - REL-TO SINK ENC-JCC-REL32 ;

: BACK-ZERO ( vreg LABEL:head -- )
   {: c:vreg m:LABEL:head :}
   c TEST C-E m BACK-JCC ;

: BACK-CC ( cc LABEL:head -- )
   {: c:cc m:LABEL:head :}
   c COND m BACK-JCC ;

\ ---- the return stack --------------------------------------------------------
\ Each goes through rax. Vreg i of p moved to or from the slot off bytes above
\ rax.
: RS-XFER ( ptr vreg n n bool -- ) {: p:ptr i:n off:n st:bool :}
   p i VREG-AT REG RAX off MEM-OFF SINK
   st if ENC-MOV-MR exit then ENC-MOV-RM ;

: RDEPTH+ ( n -- ) {: k:n :} RAX RAX k MEM-OFF SINK ENC-LEA ;

: TOR ( ptr vreg n -- ) {: p:ptr n:n :}
   RAX RDEPTH
   n RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i n - CELL * true RS-XFER loop ;

: RFROM ( ptr vreg n -- ) {: p:ptr n:n :}
   RAX RDEPTH
   n negate RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i CELL * false RS-XFER loop ;

: RFETCH ( ptr vreg n n -- ) {: p:ptr n:n k:n :}
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

\ The index of the frame k levels out of the innermost into d.
: LINDEX ( vreg n -- )
   {: d:vreg k:n :}
   d REG {: r:r64 :}
   r LDEPTH
   r LTOP
   r r LIDX-OFF k 16 * - MEM-OFF SINK ENC-MOV-RM ;


: LOOP-PUSH ( vreg vreg -- )
   {: s:vreg l:vreg :}
   RAX LDEPTH
   RAX SINK ENC-INC
   RAX LDEPTH!
   RAX LTOP
   s REG RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   l REG RAX LLIM-OFF MEM-OFF SINK ENC-MOV-MR ;

: LOOP-POP ( -- )
   RAX LDEPTH
   RAX SINK ENC-DEC
   RAX LDEPTH! ;

: LOOP-I ( vreg -- ) 0 LINDEX ;
: LOOP-J ( vreg -- ) 1 LINDEX ;

: LOOP-NEXT ( vreg LABEL:head -- )
   {: t:vreg m:LABEL:head :}
   t REG {: r:r64 :}
   RAX LDEPTH
   RAX LTOP
   r RAX LIDX-OFF MEM-OFF SINK ENC-MOV-RM
   r SINK ENC-INC
   r RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   r RAX LLIM-OFF MEM-OFF SINK ENC-CMP-RM
   C-L m BACK-JCC ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative.
: LOOP-STEP ( vreg vreg LABEL:head -- )
   {: s:vreg t:vreg m:LABEL:head :}
   s REG {: sr:r64 :}  t REG {: r:r64 :}
   RAX LDEPTH
   RAX LTOP
   r RAX LIDX-OFF MEM-OFF SINK ENC-MOV-RM
   r RAX LLIM-OFF MEM-OFF SINK ENC-SUB-RM
   sr RAX LIDX-OFF MEM-OFF SINK ENC-ADD-MR
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
   ['] OPEN k EMIT-OPEN-XT !
   ['] QUOT-OPEN k EMIT-QUOT-OPEN-XT !
   ['] UNIT-OPEN k EMIT-UNIT-OPEN-XT !
   ['] RETURN k EMIT-RETURN-XT !
   ['] CLOSE k EMIT-CLOSE-XT !
   ['] SLOT-CLOSE k EMIT-SLOT-CLOSE-XT !
   ['] CB-STUB k EMIT-CB-STUB-XT !
   ['] BYTES k EMIT-BYTES-XT !
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
   ['] SLOTS k EMIT-SLOTS-XT !
   ['] SLOT-STORE k EMIT-SLOT-STORE-XT !
   ['] SLOT-LOAD k EMIT-SLOT-LOAD-XT !
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
   ['] SET-FLAG k EMIT-SET-FLAG-XT !
   ['] LOAD k EMIT-LOAD-XT !
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
   ['] LABEL:LOOP-OPEN k EMIT-LOOP-OPEN-XT !
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
   ['] LOOP-STEP k LABEL:LOOP-STEP-XT ! ;

CTARGET-ARCH:X86-64 ' FILL-SLOT EMIT-PROVIDE

;using
;package
;package
