\ t0-x64.f - the x86-64 emitter of the tier 0 compiler, package T0-X64.
\
\ It implements the operations of src/habu/t0-emit.f (package T0-EMIT), which
\ states what each operation does on every target; this file states only the
\ x86-64 encodings and register choices. Every word here is private, and the
\ file ends by providing INSTALL, which stores each into its T0-EMIT cell, its
\ branch primitives into their src/habu/t0-label.f (package T0-LABEL) cells and
\ T0-LABEL's control words into T0-EMIT's, for CTARGET-ARCH:X86-64.
\
\ THE CONTRACT A COMPILED BODY KEEPS (src/habu/kernel-x64.f "A BODY'S
\ CONTRACT"). rbp is DATA, r12 the data stack pointer, rbx and r13-r15 the
\ other engine registers; compiled code never writes them except r12 as the
\ stack (src/arch/x86-64/rt.f G-PUSH, G-POP). The vregs are rcx rdx rsi rdi
\ r8-r11; rax is this file's own temporary and holds no value between two of
\ its words. A body is entered by `call` and leaves by `ret`; nothing is
\ saved, and its frame on the machine stack is only its own cells, rounded to
\ 16 (src/arch/x86-64/abi.f FRAME-FOR).
\
\ THE SINK is one src/lib/byte-buffer.f header, as src/arch/x86-64/icode.f
\ keeps for the kernel.
\
\ THE FRAME. OPEN and QUOT-OPEN emit `sub rsp, imm32` and each way out (RETURN,
\ TAIL) emits `add rsp, imm32`, each with a zero field recorded in FRAME-AT.
\ CLOSE writes the unit's frame size, FRAME-CELLS rounded to 16 bytes, into
\ every field the unit recorded, so rsp keeps its alignment at every call.
\ Slot k is the cell at rsp + 8k.
\
\ THE ROWS are filed into the seam as each site is emitted (t0-emit.f THE
\ ROWS). A call or tail site is E8 or E9 rel32 with the field left zero until
\ PATCH measures it from its placed end; an address site is movabs r64, imm64,
\ whose imm64 starts two bytes past the row's offset. A quotation's imm64
\ holds the unit's offset, which QUOT-OPEN writes, until PATCH adds the
\ placement. Every branch is rel32.

require src/lib/errors.f
require src/lib/byte-buffer.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/habu/t0-emit.f
require src/habu/t0-label.f

package T0-X64
using X64ASM

8 constant NVREG                      \ rcx rdx rsi rdi r8-r11

\ An immediate is a sign-extended imm32.
: IMM? ( n -- bool ) {: v:n :}
   v -2147483648 >= v 2147483647 <= and ;

\ The x86 condition number of each T0-EMIT comparison, CC-EQ .. CC-GT.
: COND ( T0-EMIT:cc -- condition )
   {: c:T0-EMIT:cc :}
   c T0-EMIT:CC>N {: k:n :}
   k 0 = if C-E exit then
   k 1 = if C-NE exit then
   k 2 = if C-L exit then
   k 3 = if C-GE exit then
   k 4 = if C-LE exit then
   k 5 = if C-G exit then
   E-OPERAND throw ;

create SINK BUF:HDR-BYTES allot
variable SINK-LIVE                    \ nonzero once SINK holds storage
variable FRAME-CELLS                  \ machine-stack cells this body uses
variable NFRAME
variable NUNIT                        \ units begun in this emission
DYNAMIC-BUFFER FRAME-AT n             \ offset of each frame imm32 field
DYNAMIC-BUFFER UNIT-REF n             \ the address site that names each unit

4096 constant SINK-START-BYTES

: HERE-OFF ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;

: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

\ vreg 0..3 are rcx rdx rsi rdi, 4..7 are r8..r11.
: REG ( T0-EMIT:vreg -- r64 )
   {: v:T0-EMIT:vreg :}
   v T0-EMIT:VREG>N {: k:n :}
   k 0 < k NVREG >= or if E-OPERAND throw then
   k 0 = if RCX exit then
   k 1 = if RDX exit then
   k 2 = if RSI exit then
   k 3 = if RDI exit then
   k 4 + >R64 ;

\ Write the low k bytes of v at offset off of the sink, little-endian.
: PUT ( n n n -- ) {: v:n off:n k:n :}
   SINK BUF:SPAN$ drop off + {: p:ptr :}
   k 0 ?do v i 8 * rshift $FF and p i + c! loop ;

\ A forward rel32 at the end of what was just emitted: its field's offset.
: FWD-SITE ( -- T0-LABEL:branch ) HERE-OFF 4 - T0-LABEL:>BRANCH ;

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

: ADDR ( n n T0-EMIT:vreg n -- )
   {: v:n o:n d:T0-EMIT:vreg kind:n :}
   HERE-OFF kind v o T0-EMIT:ADDR-ROW+
   d REG v >IMM64 SINK ENC-MOV-RI64 ;

\ r, holding the return stack depth, becomes the address one cell past its top.
: RTOP ( r64 -- ) {: r:r64 :}
   r 3 >IMM8 SINK ENC-SHL-RI8
   r DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF SINK ENC-ADD-RM ;

: RDEPTH ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-RM ;

: RDEPTH! ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-MR ;

: REL-TO ( n -- rel ) HERE-OFF - >REL ;

\ Begin the next unit at the current offset, with an empty frame.
: UNIT-BEGIN ( -- )
   HERE-OFF T0-EMIT:UNIT+
   NUNIT @ 1 + NUNIT !
   0 FRAME-CELLS !  0 NFRAME !
   RSP 0 >IMM32 SINK ENC-SUB-RI32  FRAME-SITE ;


\ ---- the body ----------------------------------------------------------------
: OPEN ( -- )
   SINK-LIVE @ 0= if
      SINK SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 SINK-LIVE !
   then
   SINK BUF:CLEAR
   0 NUNIT !
   T0-LABEL:RESET
   UNIT-BEGIN ;

: QUOT-OPEN ( n -- ) {: n:n :}
   n NUNIT @ <> if T0-EMIT:E-T0-UNIT throw then
   HERE-OFF n UNIT-REF @ 2 + 8 PUT
   UNIT-BEGIN ;

: RETURN ( -- )
   UNFRAME  SINK ENC-RET ;

: CLOSE ( -- )
   FRAME-CELLS @ CELL * 15 + 15 invert and {: bytes:n :}
   NFRAME @ 0 ?do bytes i FRAME-AT @ 4 PUT loop ;

: BYTES ( -- ptr u8 n ) SINK BUF:SPAN$ BUF:BLEN>N ;

\ A call's rel32 is measured from the end of its five bytes, placed at.
: PATCH-CALL ( n n n -- ) {: at:n off:n to:n :}
   to at off + 5 + - {: d:n :}
   d -2147483648 < d 2147483647 > or if E-X64EMIT-REACH throw then
   d off 1 + 4 PUT ;

: PATCH ( n -- ) {: at:n :}
   T0-EMIT:CALL-ROWS 0 ?do
      i T0-EMIT:CALL-ROW {: off:n kind:n to:n :}
      at off to PATCH-CALL
   loop
   T0-EMIT:ADDR-ROWS 0 ?do
      i T0-EMIT:ADDR-ROW {: off:n kind:n to:n :}
      kind T0-EMIT:ADDR-QUOT-KIND = if at to T0-EMIT:UNIT + off 2 + 8 PUT then
   loop ;

\ ---- values ------------------------------------------------------------------
: LIT ( n T0-EMIT:vreg -- )
   {: v:n d:T0-EMIT:vreg :}
   v d REG LIT-R ;

: ADDR-DATA ( n n T0-EMIT:vreg -- ) NEMIT:ADDR-DATA ADDR ;

: ADDR-CODE ( n T0-EMIT:vreg -- )
   {: v:n d:T0-EMIT:vreg :}
   v -1 d NEMIT:ADDR-CODE ADDR ;

\ The site's imm64 holds 0 until QUOT-OPEN writes unit n's offset.
: ADDR-QUOT ( n T0-EMIT:vreg -- )
   {: n:n d:T0-EMIT:vreg :}
   n 1 + UNIT-REF-RESERVE
   HERE-OFF n UNIT-REF !
   HERE-OFF T0-EMIT:ADDR-QUOT-KIND n -1 T0-EMIT:ADDR-ROW+
   d REG 0 >IMM64 SINK ENC-MOV-RI64 ;

\ The stack pointer moved by k cells: an lea, which leaves the condition codes
\ alone, as T0-EMIT:CMP requires.
: DSP+ ( n -- ) {: k:n :}
   k 0 = if exit then
   DSP DSP k CELL * MEM-OFF SINK ENC-LEA ;

\ x86-64 has no writeback: a push stores and then moves the pointer, and a pop
\ moves it and then loads, as G-PUSH and G-POP do.
: PUSH ( T0-EMIT:vreg n -- ) {: s:T0-EMIT:vreg k:n :}
   s REG DSP MEM-AT SINK ENC-MOV-MR
   k DSP+ ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP MEM-AT SINK ENC-MOV-MR
   k DSP+ ;

: POP ( T0-EMIT:vreg n -- ) {: d:T0-EMIT:vreg k:n :}
   k negate DSP+
   d REG DSP MEM-AT SINK ENC-MOV-RM ;

: SPILL ( T0-EMIT:vreg n -- )
   {: s:T0-EMIT:vreg k:n :}
   s REG DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: FILL ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG DSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

: MOV ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d REG s REG SINK ENC-MOV-RR ;

: EXCHANGE ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: a:T0-EMIT:vreg b:T0-EMIT:vreg :}
   a REG b REG SINK ENC-XCHG-RR ;

: SLOTS ( n -- ) {: k:n :}
   k FRAME-CELLS @ > if k FRAME-CELLS ! then ;

: SLOT-STORE ( T0-EMIT:vreg n -- )
   {: s:T0-EMIT:vreg k:n :}
   s REG RSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SLOT-LOAD ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG RSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:alu -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg o:T0-EMIT:alu :}
   d REG {: dr:r64 :}  s REG {: sr:r64 :}  o T0-EMIT:ALU>N {: k:n :}
   k 0 = if dr sr SINK ENC-ADD-RR exit then
   k 1 = if dr sr SINK ENC-SUB-RR exit then
   k 2 = if dr sr SINK ENC-AND-RR exit then
   k 3 = if dr sr SINK ENC-OR-RR exit then
   k 4 = if dr sr SINK ENC-XOR-RR exit then
   E-OPERAND throw ;

: OP-LIT ( T0-EMIT:vreg n T0-EMIT:alu -- )
   {: d:T0-EMIT:vreg v:n o:T0-EMIT:alu :}
   d REG {: dr:r64 :}  v >IMM32 {: iv:imm32 :}  o T0-EMIT:ALU>N {: k:n :}
   k 0 = if dr iv SINK ENC-ADD-RI32 exit then
   k 1 = if dr iv SINK ENC-SUB-RI32 exit then
   k 2 = if dr iv SINK ENC-AND-RI32 exit then
   k 3 = if dr iv SINK ENC-OR-RI32 exit then
   k 4 = if dr iv SINK ENC-XOR-RI32 exit then
   E-OPERAND throw ;

\ A shift by a register takes its count in cl, and cl is vreg 0's low byte;
\ the hardware masks the count to six bits. SHIFT-LIT masks its count here.
: SHIFT-CL ( r64 T0-EMIT:shifter -- )
   {: r:r64 o:T0-EMIT:shifter :}
   o T0-EMIT:SHIFTER>N 0 = if r SINK ENC-SHL-CL exit then
   r SINK ENC-SHR-CL ;

: SHIFT ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:shifter -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg o:T0-EMIT:shifter :}
   s T0-EMIT:VREG>N 0 = if d REG o SHIFT-CL exit then
   RAX RCX SINK ENC-MOV-RR
   RCX s REG SINK ENC-MOV-RR
   d T0-EMIT:VREG>N 0 = if
      RAX o SHIFT-CL
      RCX RAX SINK ENC-MOV-RR exit
   then
   d REG o SHIFT-CL
   RCX RAX SINK ENC-MOV-RR ;

: SHIFT-LIT ( T0-EMIT:vreg n T0-EMIT:shifter -- )
   {: d:T0-EMIT:vreg v:n o:T0-EMIT:shifter :}
   v 63 and >IMM8 {: c:imm8 :}
   o T0-EMIT:SHIFTER>N 0 = if d REG c SINK ENC-SHL-RI8 exit then
   d REG c SINK ENC-SHR-RI8 ;

: MULX ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d REG s REG SINK ENC-IMUL-RR ;

: MUL-LIT ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg v:n :}
   d REG d REG v >IMM32 SINK ENC-IMUL-RRI32 ;

\ idiv divides rdx:rax and writes rax and rdx, and rdx is a vreg, so it is
\ saved on the machine stack unless d is rdx. The divisor goes into d once
\ the dividend is in rax; when d is rdx, which cqo writes, into rcx, saved
\ instead. The saved register is the only one restored, so every vreg but d
\ keeps its value.
: DIVIDE ( T0-EMIT:vreg n bool -- )
   {: d:T0-EMIT:vreg v:n rem:bool :}
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

: DIV-LIT ( T0-EMIT:vreg n -- ) false DIVIDE ;
: MOD-LIT ( T0-EMIT:vreg n -- ) true DIVIDE ;

: NEG ( T0-EMIT:vreg -- ) REG SINK ENC-NEG ;
: NOT ( T0-EMIT:vreg -- ) REG SINK ENC-NOT ;

\ setcc, then zero-extend and negate the byte into a flag.
: SET-FLAG ( T0-EMIT:vreg T0-EMIT:cc -- )
   {: d:T0-EMIT:vreg c:T0-EMIT:cc :}
   c COND 0 >R8 SINK ENC-SETCC
   d REG 0 >R8 SINK ENC-MOVZX-8-RR
   d REG SINK ENC-NEG ;

: CMP ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: a:T0-EMIT:vreg b:T0-EMIT:vreg :}
   a REG b REG SINK ENC-CMP-RR ;

: CMP-LIT ( T0-EMIT:vreg n -- )
   {: a:T0-EMIT:vreg v:n :}
   a REG v >IMM32 SINK ENC-CMP-RI32 ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:width -- )
   {: d:T0-EMIT:vreg a:T0-EMIT:vreg w:T0-EMIT:width :}
   w T0-EMIT:WIDTH>N 1 = if d REG a REG MEM-AT SINK ENC-MOVZX-8-RM exit then
   d REG a REG MEM-AT SINK ENC-MOV-RM ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n n -- ) {: to:n loc:n :}
   HERE-OFF NEMIT:CALL to loc T0-EMIT:CALL-ROW+
   0 >REL SINK ENC-CALL-REL32 ;

: TAIL ( n n -- ) {: to:n loc:n :}
   UNFRAME
   HERE-OFF NEMIT:TAIL to loc T0-EMIT:CALL-ROW+
   0 >REL SINK ENC-JMP-REL32 ;

: CALL-SELF ( -- )
   0 HERE-OFF 5 + - >REL SINK ENC-CALL-REL32 ;

\ ---- control -----------------------------------------------------------------
\ The branch primitives T0-LABEL emits through. A forward branch is the offset
\ of its rel32 field, waiting for its target, and a head is an offset.

\ Bind the branch's forward field to here.
: RESOLVE ( T0-LABEL:branch -- ) T0-LABEL:BRANCH>N {: at:n :}
   HERE-OFF at 4 + - at 4 PUT ;

: MARK ( -- T0-LABEL:head ) HERE-OFF T0-LABEL:>HEAD ;

: FWD-JUMP ( -- T0-LABEL:branch )
   0 >REL SINK ENC-JMP-REL32  FWD-SITE ;

: FWD-JCC ( condition -- T0-LABEL:branch ) {: c:condition :}
   c 0 >REL SINK ENC-JCC-REL32  FWD-SITE ;

: TEST ( T0-EMIT:vreg -- )
   {: c:T0-EMIT:vreg :}
   c REG c REG SINK ENC-TEST-RR ;

: FWD-ZERO ( T0-EMIT:vreg -- T0-LABEL:branch ) TEST C-E FWD-JCC ;

: FWD-NE ( T0-EMIT:vreg T0-EMIT:vreg -- T0-LABEL:branch )
   {: sel:T0-EMIT:vreg v:T0-EMIT:vreg :}
   sel REG v REG SINK ENC-CMP-RR
   C-NE FWD-JCC ;

: FWD-NE-LIT ( T0-EMIT:vreg n -- T0-LABEL:branch )
   {: sel:T0-EMIT:vreg v:n :}
   sel v CMP-LIT
   C-NE FWD-JCC ;

: FWD-GE ( T0-EMIT:vreg T0-EMIT:vreg -- T0-LABEL:branch )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg :}
   s REG l REG SINK ENC-CMP-RR
   C-GE FWD-JCC ;

: FWD-CC ( T0-EMIT:cc -- T0-LABEL:branch ) COND FWD-JCC ;

\ The branch's jcc becomes je: its opcode byte is just before the field.
: ENTRY-EQ ( T0-LABEL:branch -- ) T0-LABEL:BRANCH>N {: at:n :}
   C-E CONDITION>N $80 +  SINK BUF:SPAN$ drop at 1 - + c! ;

\ A backward rel is measured from the end of the instruction, five bytes for
\ jmp rel32 and six for jcc rel32.
: BACK-JUMP ( T0-LABEL:head -- ) T0-LABEL:HEAD>N {: m:n :}
   m 5 - REL-TO SINK ENC-JMP-REL32 ;

: BACK-JCC ( condition T0-LABEL:head -- )
   T0-LABEL:HEAD>N {: c:condition m:n :}
   c m 6 - REL-TO SINK ENC-JCC-REL32 ;

: BACK-ZERO ( T0-EMIT:vreg T0-LABEL:head -- )
   {: c:T0-EMIT:vreg m:T0-LABEL:head :}
   c TEST C-E m BACK-JCC ;

: BACK-CC ( T0-EMIT:cc T0-LABEL:head -- )
   {: c:T0-EMIT:cc m:T0-LABEL:head :}
   c COND m BACK-JCC ;

\ ---- the return stack --------------------------------------------------------
\ Each goes through rax. Vreg i of p moved to or from the slot off bytes above
\ rax.
: RS-XFER ( ptr T0-EMIT:vreg n n bool -- ) {: p:ptr i:n off:n st:bool :}
   p i T0-EMIT:VREG-AT REG RAX off MEM-OFF SINK
   st if ENC-MOV-MR exit then ENC-MOV-RM ;

\ The depth in rax moved by k.
: RDEPTH+ ( n -- ) {: k:n :} RAX RAX k MEM-OFF SINK ENC-LEA ;

: TOR ( ptr T0-EMIT:vreg n -- ) {: p:ptr n:n :}
   RAX RDEPTH
   n RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i n - CELL * true RS-XFER loop ;

: RFROM ( ptr T0-EMIT:vreg n -- ) {: p:ptr n:n :}
   RAX RDEPTH
   n negate RDEPTH+
   RAX RDEPTH!
   RAX RTOP
   n 0 ?do p i i CELL * false RS-XFER loop ;

: RFETCH ( ptr T0-EMIT:vreg n n -- ) {: p:ptr n:n k:n :}
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
: LINDEX ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG {: r:r64 :}
   r LDEPTH
   r LTOP
   r r LIDX-OFF k 16 * - MEM-OFF SINK ENC-MOV-RM ;


: LOOP-PUSH ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg :}
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

: LOOP-I ( T0-EMIT:vreg -- ) 0 LINDEX ;
: LOOP-J ( T0-EMIT:vreg -- ) 1 LINDEX ;

: LOOP-NEXT ( T0-EMIT:vreg T0-LABEL:head -- )
   {: t:T0-EMIT:vreg m:T0-LABEL:head :}
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
: LOOP-STEP ( T0-EMIT:vreg T0-EMIT:vreg T0-LABEL:head -- )
   {: s:T0-EMIT:vreg t:T0-EMIT:vreg m:T0-LABEL:head :}
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
\ Make this file's words, and T0-LABEL's over its branch primitives, the ones
\ T0-EMIT runs at slot k.
: INSTALL ( n -- )
   {: k:n :}
   NVREG k T0-EMIT:VREGS-N !
   ['] OPEN k T0-EMIT:OPEN-XT !
   ['] QUOT-OPEN k T0-EMIT:QUOT-OPEN-XT !
   ['] RETURN k T0-EMIT:RETURN-XT !
   ['] CLOSE k T0-EMIT:CLOSE-XT !
   ['] BYTES k T0-EMIT:BYTES-XT !
   ['] PATCH k T0-EMIT:PATCH-XT !
   ['] LIT k T0-EMIT:LIT-XT !
   ['] ADDR-DATA k T0-EMIT:ADDR-DATA-XT !
   ['] ADDR-CODE k T0-EMIT:ADDR-CODE-XT !
   ['] ADDR-QUOT k T0-EMIT:ADDR-QUOT-XT !
   ['] PUSH k T0-EMIT:PUSH-XT !
   ['] PUSH-LIT k T0-EMIT:PUSH-LIT-XT !
   ['] POP k T0-EMIT:POP-XT !
   ['] SPILL k T0-EMIT:SPILL-XT !
   ['] SPILL-LIT k T0-EMIT:SPILL-LIT-XT !
   ['] FILL k T0-EMIT:FILL-XT !
   ['] DSP+ k T0-EMIT:DSP+-XT !
   ['] MOV k T0-EMIT:MOV-XT !
   ['] EXCHANGE k T0-EMIT:EXCHANGE-XT !
   ['] SLOTS k T0-EMIT:SLOTS-XT !
   ['] SLOT-STORE k T0-EMIT:SLOT-STORE-XT !
   ['] SLOT-LOAD k T0-EMIT:SLOT-LOAD-XT !
   ['] OP k T0-EMIT:OP-XT !
   ['] OP-LIT k T0-EMIT:OP-LIT-XT !
   ['] SHIFT k T0-EMIT:SHIFT-XT !
   ['] SHIFT-LIT k T0-EMIT:SHIFT-LIT-XT !
   ['] MULX k T0-EMIT:MULX-XT !
   ['] MUL-LIT k T0-EMIT:MUL-LIT-XT !
   ['] DIV-LIT k T0-EMIT:DIV-LIT-XT !
   ['] MOD-LIT k T0-EMIT:MOD-LIT-XT !
   ['] NEG k T0-EMIT:NEG-XT !
   ['] NOT k T0-EMIT:NOT-XT !
   ['] CMP k T0-EMIT:CMP-XT !
   ['] CMP-LIT k T0-EMIT:CMP-LIT-XT !
   ['] SET-FLAG k T0-EMIT:SET-FLAG-XT !
   ['] LOAD k T0-EMIT:LOAD-XT !
   ['] CALL k T0-EMIT:CALL-XT !
   ['] TAIL k T0-EMIT:TAIL-XT !
   ['] CALL-SELF k T0-EMIT:CALL-SELF-XT !
   ['] T0-LABEL:IF, k T0-EMIT:IF,-XT !
   ['] T0-LABEL:ELSE, k T0-EMIT:ELSE,-XT !
   ['] T0-LABEL:THEN, k T0-EMIT:THEN,-XT !
   ['] T0-LABEL:BEGIN, k T0-EMIT:BEGIN,-XT !
   ['] T0-LABEL:WHILE, k T0-EMIT:WHILE,-XT !
   ['] T0-LABEL:REPEAT, k T0-EMIT:REPEAT,-XT !
   ['] T0-LABEL:UNTIL, k T0-EMIT:UNTIL,-XT !
   ['] T0-LABEL:IF-CC, k T0-EMIT:IF-CC,-XT !
   ['] T0-LABEL:WHILE-CC, k T0-EMIT:WHILE-CC,-XT !
   ['] T0-LABEL:UNTIL-CC, k T0-EMIT:UNTIL-CC,-XT !
   ['] T0-LABEL:AGAIN, k T0-EMIT:AGAIN,-XT !
   ['] T0-LABEL:CASE, k T0-EMIT:CASE,-XT !
   ['] T0-LABEL:OF, k T0-EMIT:OF,-XT !
   ['] T0-LABEL:OF-LIT k T0-EMIT:OF-LIT-XT !
   ['] T0-LABEL:ENDOF, k T0-EMIT:ENDOF,-XT !
   ['] T0-LABEL:ENDCASE, k T0-EMIT:ENDCASE,-XT !
   ['] TOR k T0-EMIT:TOR-XT !
   ['] RFROM k T0-EMIT:RFROM-XT !
   ['] RFETCH k T0-EMIT:RFETCH-XT !
   ['] LOOP-PUSH k T0-EMIT:LOOP-PUSH-XT !
   ['] LOOP-POP k T0-EMIT:LOOP-POP-XT !
   ['] LOOP-I k T0-EMIT:LOOP-I-XT !
   ['] LOOP-J k T0-EMIT:LOOP-J-XT !
   ['] T0-LABEL:LOOP-OPEN k T0-EMIT:LOOP-OPEN-XT !
   ['] T0-LABEL:LEAVE, k T0-EMIT:LEAVE,-XT !
   ['] T0-LABEL:LOOP-NEXT k T0-EMIT:LOOP-NEXT-XT !
   ['] T0-LABEL:LOOP-STEP k T0-EMIT:LOOP-STEP-XT !
   ['] T0-LABEL:LOOP-CLOSE k T0-EMIT:LOOP-CLOSE-XT !
   ['] IMM? k T0-EMIT:IMM?-XT !
   ['] MARK k T0-LABEL:MARK-XT !
   ['] FWD-JUMP k T0-LABEL:FWD-JUMP-XT !
   ['] FWD-ZERO k T0-LABEL:FWD-ZERO-XT !
   ['] FWD-NE k T0-LABEL:FWD-NE-XT !
   ['] FWD-NE-LIT k T0-LABEL:FWD-NE-LIT-XT !
   ['] FWD-GE k T0-LABEL:FWD-GE-XT !
   ['] FWD-CC k T0-LABEL:FWD-CC-XT !
   ['] BACK-JUMP k T0-LABEL:BACK-JUMP-XT !
   ['] BACK-ZERO k T0-LABEL:BACK-ZERO-XT !
   ['] BACK-CC k T0-LABEL:BACK-CC-XT !
   ['] RESOLVE k T0-LABEL:RESOLVE-XT !
   ['] ENTRY-EQ k T0-LABEL:ENTRY-EQ-XT !
   ['] LOOP-NEXT k T0-LABEL:LOOP-NEXT-XT !
   ['] LOOP-STEP k T0-LABEL:LOOP-STEP-XT ! ;

CTARGET-ARCH:X86-64 ' INSTALL T0-EMIT:PROVIDE

;using
;package
