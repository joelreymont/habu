\ t0-x64.f - the x86-64 emitter of the tier 0 compiler, package T0-X64.
\
\ It implements the operations of src/habu/t0-emit.f (package T0-EMIT), which
\ states what each operation does on every target; this file states only the
\ x86-64 encodings and register choices. Every word here is private, and the
\ file ends by providing INSTALL, which stores each into its T0-EMIT cell, for
\ CTARGET-ARCH:X86-64.
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
\ THE ROWS. A call or tail site is E8 or E9 rel32 with the field left zero; an
\ address site is movabs r64, imm64, whose imm64 starts two bytes past the
\ row's offset. A quotation's imm64 holds the unit's offset until the
\ publisher adds the placement. Every branch is rel32.

require src/lib/byte-buffer.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f
require src/compiler/target.f
require src/habu/t0-emit.f

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
variable NCALL
variable NADDR
variable NUNIT                        \ units begun in this emission
DYNAMIC-BUFFER FRAME-AT n             \ offset of each frame imm32 field
DYNAMIC-BUFFER CALL-AT n              \ call row: first byte, kind, target
DYNAMIC-BUFFER CALL-KIND-AT n
DYNAMIC-BUFFER CALL-TO n
DYNAMIC-BUFFER ADDR-AT n              \ address row: first byte, kind, value
DYNAMIC-BUFFER ADDR-KIND-AT n
DYNAMIC-BUFFER ADDR-TO n
DYNAMIC-BUFFER UNIT-REF n             \ the address row that names each unit
DYNAMIC-BUFFER CF-KIND n              \ the open structures, innermost last,
DYNAMIC-BUFFER CF-AT n                \ and each one's offset
variable CF-N
DYNAMIC-BUFFER LV-HEAD n              \ the open loop levels: the head's offset,
DYNAMIC-BUFFER LV-ENTRY n             \ `?do`'s entry field or -1,
DYNAMIC-BUFFER LV-FIRST n             \ and the level's first field in LEAVES
variable LV-N
DYNAMIC-BUFFER LEAVES n               \ the open levels' `leave` fields, innermost last
variable LEAVE-N

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
: FWD-SITE ( -- n ) HERE-OFF 4 - ;

\ Load the constant n into r with the shortest form that holds it.
: LIT-R ( n r64 -- ) {: v:n r:r64 :}
   v 0 = if r R64>N >R32 r R64>N >R32 SINK ENC-XOR32-RR exit then
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

: CALL-ROW+ ( n n n -- ) {: at:n kind:n to:n :}
   NCALL @ {: k:n :}
   k 1 + CALL-AT-RESERVE  k 1 + CALL-KIND-AT-RESERVE  k 1 + CALL-TO-RESERVE
   at k CALL-AT !  kind k CALL-KIND-AT !  to k CALL-TO !
   k 1 + NCALL ! ;

: ADDR-ROW+ ( n n n -- ) {: at:n kind:n to:n :}
   NADDR @ {: k:n :}
   k 1 + ADDR-AT-RESERVE  k 1 + ADDR-KIND-AT-RESERVE  k 1 + ADDR-TO-RESERVE
   at k ADDR-AT !  kind k ADDR-KIND-AT !  to k ADDR-TO !
   k 1 + NADDR ! ;

: ADDR ( n T0-EMIT:vreg n -- )
   {: v:n d:T0-EMIT:vreg kind:n :}
   HERE-OFF kind v ADDR-ROW+
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
   0 NCALL !  0 NADDR !  0 NUNIT !
   0 CF-N !  0 LV-N !  0 LEAVE-N !
   UNIT-BEGIN ;

: QUOT-OPEN ( n -- ) {: n:n :}
   n NUNIT @ <> if T0-EMIT:E-T0-UNIT throw then
   n UNIT-REF @ {: row:n :}
   HERE-OFF row ADDR-TO !
   HERE-OFF row ADDR-AT @ 2 + 8 PUT
   UNIT-BEGIN ;

: RETURN ( -- )
   UNFRAME  SINK ENC-RET ;

: CLOSE ( -- )
   FRAME-CELLS @ CELL * 15 + 15 invert and {: bytes:n :}
   NFRAME @ 0 ?do bytes i FRAME-AT @ 4 PUT loop ;

: CODE$ ( -- ptr u8 n ) SINK BUF:SPAN$ BUF:BLEN>N ;

: CALL-ROWS ( -- n ) NCALL @ ;
: CALL-ROW ( n -- n n n ) {: k:n :}  k CALL-AT @  k CALL-KIND-AT @  k CALL-TO @ ;
: ADDR-ROWS ( -- n ) NADDR @ ;
: ADDR-ROW ( n -- n n n ) {: k:n :}  k ADDR-AT @  k ADDR-KIND-AT @  k ADDR-TO @ ;

\ ---- values ------------------------------------------------------------------
: LIT ( n T0-EMIT:vreg -- )
   {: v:n d:T0-EMIT:vreg :}
   v d REG LIT-R ;

: ADDR-DATA ( n T0-EMIT:vreg -- ) T0-EMIT:ADDR-DATA-KIND ADDR ;
: ADDR-CODE ( n T0-EMIT:vreg -- ) T0-EMIT:ADDR-CODE-KIND ADDR ;

: ADDR-QUOT ( n T0-EMIT:vreg -- )
   {: n:n d:T0-EMIT:vreg :}
   n 1 + UNIT-REF-RESERVE
   NADDR @ n UNIT-REF !
   0 d T0-EMIT:ADDR-QUOT-KIND ADDR ;

: PUSH ( T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg :}
   s REG DSP MEM-AT SINK ENC-MOV-MR
   DSP CELL >IMM8 SINK ENC-ADD-RI8 ;

: SPILL ( T0-EMIT:vreg n -- )
   {: s:T0-EMIT:vreg k:n :}
   s REG DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: FILL ( T0-EMIT:vreg n -- )
   {: d:T0-EMIT:vreg k:n :}
   d REG DSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

: DSP+ ( n -- ) {: k:n :}
   k 0 = if exit then
   k 0 > if DSP k CELL * >IMM32 SINK ENC-ADD-RI32 exit then
   DSP k negate CELL * >IMM32 SINK ENC-SUB-RI32 ;

: MOV ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d REG s REG SINK ENC-MOV-RR ;

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

: NEG ( T0-EMIT:vreg -- ) REG SINK ENC-NEG ;
: NOT ( T0-EMIT:vreg -- ) REG SINK ENC-NOT ;

\ setcc, then zero-extend and negate the byte into a flag.
: SET-FLAG ( T0-EMIT:vreg T0-EMIT:cc -- )
   {: d:T0-EMIT:vreg c:T0-EMIT:cc :}
   c COND 0 >R8 SINK ENC-SETCC
   d REG 0 >R8 SINK ENC-MOVZX-8-RR
   d REG SINK ENC-NEG ;

: CMP-SET ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:cc -- )
   {: d:T0-EMIT:vreg s:T0-EMIT:vreg c:T0-EMIT:cc :}
   d REG s REG SINK ENC-CMP-RR
   d c SET-FLAG ;

: CMP-SET-LIT ( T0-EMIT:vreg n T0-EMIT:cc -- )
   {: d:T0-EMIT:vreg v:n c:T0-EMIT:cc :}
   d REG v >IMM32 SINK ENC-CMP-RI32
   d c SET-FLAG ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( T0-EMIT:vreg T0-EMIT:vreg T0-EMIT:width -- )
   {: d:T0-EMIT:vreg a:T0-EMIT:vreg w:T0-EMIT:width :}
   w T0-EMIT:WIDTH>N 1 = if d REG a REG MEM-AT SINK ENC-MOVZX-8-RM exit then
   d REG a REG MEM-AT SINK ENC-MOV-RM ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n -- ) {: to:n :}
   HERE-OFF T0-EMIT:CALL-KIND to CALL-ROW+
   0 >REL SINK ENC-CALL-REL32 ;

: TAIL ( n -- ) {: to:n :}
   UNFRAME
   HERE-OFF T0-EMIT:TAIL-KIND to CALL-ROW+
   0 >REL SINK ENC-JMP-REL32 ;

: CALL-SELF ( -- )
   0 HERE-OFF 5 + - >REL SINK ENC-CALL-REL32 ;

\ ---- control -----------------------------------------------------------------
\ An open structure is a POSITION, the offset of a forward branch's rel32 field
\ waiting for its target or of a loop head, or the start of a case beneath its
\ `endof` exits. Which of the two a position is follows from the word that pops
\ it, so only the case start is marked.
0 constant POSITION
1 constant CASE-START

: CF-PUSH ( n n -- ) {: kind:n at:n :}
   CF-N @ {: k:n :}
   k 1 + CF-KIND-RESERVE  k 1 + CF-AT-RESERVE
   kind k CF-KIND !  at k CF-AT !
   k 1 + CF-N ! ;

: CF-POP ( -- n )
   CF-N @ 1 - {: k:n :}
   k CF-N !
   k CF-AT @ ;

: CF-TOP-KIND ( -- n ) CF-N @ 1 - CF-KIND @ ;

: AT+ ( n -- ) POSITION swap CF-PUSH ;

\ Bind the forward field at offset n to here.
: RESOLVE ( n -- ) {: at:n :}
   HERE-OFF at 4 + - at 4 PUT ;

: FWD-JMP ( -- n )
   0 >REL SINK ENC-JMP-REL32  FWD-SITE ;

: FWD-JCC ( condition -- n ) {: c:condition :}
   c 0 >REL SINK ENC-JCC-REL32  FWD-SITE ;

: TEST ( T0-EMIT:vreg -- )
   {: c:T0-EMIT:vreg :}
   c REG c REG SINK ENC-TEST-RR ;

\ A backward rel is measured from the end of the instruction, five bytes for
\ jmp rel32 and six for jcc rel32.
: BACK-JMP ( n -- ) {: m:n :}
   m 5 - REL-TO SINK ENC-JMP-REL32 ;

: BACK-JCC ( condition n -- ) {: c:condition m:n :}
   c m 6 - REL-TO SINK ENC-JCC-REL32 ;


: IF, ( T0-EMIT:vreg -- ) TEST C-E FWD-JCC AT+ ;

: ELSE, ( -- ) FWD-JMP CF-POP RESOLVE AT+ ;

: THEN, ( -- ) CF-POP RESOLVE ;

: BEGIN, ( -- ) HERE-OFF AT+ ;

: WHILE, ( T0-EMIT:vreg -- )
   TEST C-E FWD-JCC {: s:n :}
   CF-POP {: m:n :}
   s AT+
   m AT+ ;

: REPEAT, ( -- ) CF-POP BACK-JMP CF-POP RESOLVE ;

: UNTIL, ( T0-EMIT:vreg -- ) TEST C-E CF-POP BACK-JCC ;

: AGAIN, ( -- ) CF-POP BACK-JMP ;

: CASE, ( -- ) CASE-START 0 CF-PUSH ;

: OF, ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: sel:T0-EMIT:vreg v:T0-EMIT:vreg :}
   sel REG v REG SINK ENC-CMP-RR
   C-NE FWD-JCC AT+ ;

: OF-LIT ( T0-EMIT:vreg n -- )
   {: sel:T0-EMIT:vreg v:n :}
   sel REG v >IMM32 SINK ENC-CMP-RI32
   C-NE FWD-JCC AT+ ;

: ENDOF, ( -- ) ELSE, ;

: ENDCASE, ( -- )
   begin CF-TOP-KIND CASE-START <> while CF-POP RESOLVE repeat
   CF-POP drop ;

\ ---- the return stack --------------------------------------------------------
\ TOR goes through rax; RFROM and RFETCH through the destination alone.
: TOR ( T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg :}
   RAX RDEPTH
   RAX SINK ENC-INC
   RAX RDEPTH!
   RAX RTOP
   s REG RAX CELL negate MEM-OFF SINK ENC-MOV-MR ;

: RFROM ( T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg :}
   d REG {: r:r64 :}
   r RDEPTH
   r SINK ENC-DEC
   r RDEPTH!
   r RTOP
   r r MEM-AT SINK ENC-MOV-RM ;

: RFETCH ( T0-EMIT:vreg -- )
   {: d:T0-EMIT:vreg :}
   d REG {: r:r64 :}
   r RDEPTH
   r RTOP
   r r CELL negate MEM-OFF SINK ENC-MOV-RM ;


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

\ The innermost level and its head.
: LV ( -- n ) LV-N @ 1 - ;
: HEAD ( -- n ) LV LV-HEAD @ ;

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

: LOOP-OPEN ( T0-EMIT:vreg T0-EMIT:vreg bool -- )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg entry:bool :}
   entry if
      s REG l REG SINK ENC-CMP-RR
      C-GE FWD-JCC
   else -1 then {: at:n :}
   LV-N @ {: k:n :}
   k 1 + LV-HEAD-RESERVE  k 1 + LV-ENTRY-RESERVE  k 1 + LV-FIRST-RESERVE
   HERE-OFF k LV-HEAD !
   at k LV-ENTRY !
   LEAVE-N @ k LV-FIRST !
   k 1 + LV-N ! ;

: LEAVE, ( -- )
   LEAVE-N @ {: n:n :}
   n 1 + LEAVES-RESERVE
   FWD-JMP n LEAVES !
   n 1 + LEAVE-N ! ;

: LOOP-NEXT ( T0-EMIT:vreg -- )
   {: t:T0-EMIT:vreg :}
   t REG {: r:r64 :}
   RAX LDEPTH
   RAX LTOP
   r RAX LIDX-OFF MEM-OFF SINK ENC-MOV-RM
   r SINK ENC-INC
   r RAX LIDX-OFF MEM-OFF SINK ENC-MOV-MR
   r RAX LLIM-OFF MEM-OFF SINK ENC-CMP-RM
   C-L HEAD BACK-JCC ;

\ With old = index - limit before the step, the index crossed the boundary
\ when (old xor old + s) and (old xor s) is negative. The level's `?do` entry
\ jge becomes je.
: LOOP-STEP ( T0-EMIT:vreg T0-EMIT:vreg -- )
   {: s:T0-EMIT:vreg t:T0-EMIT:vreg :}
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
   C-NS HEAD BACK-JCC
   LV LV-ENTRY @ {: at:n :}
   at 0 >= if C-E CONDITION>N $80 +  SINK BUF:SPAN$ drop at 1 - + c! then ;

: LOOP-CLOSE ( -- )
   LV {: k:n :}
   LEAVE-N @ k LV-FIRST @ ?do i LEAVES @ RESOLVE loop
   k LV-ENTRY @ {: at:n :}
   at 0 >= if at RESOLVE then
   k LV-FIRST @ LEAVE-N !
   k LV-N !
   LOOP-POP ;

\ ---- the seam ----------------------------------------------------------------
\ Make this file's words the ones T0-EMIT runs at slot k.
: INSTALL ( n -- )
   {: k:n :}
   NVREG k T0-EMIT:VREGS-N !
   ['] OPEN k T0-EMIT:OPEN-XT !
   ['] QUOT-OPEN k T0-EMIT:QUOT-OPEN-XT !
   ['] RETURN k T0-EMIT:RETURN-XT !
   ['] CLOSE k T0-EMIT:CLOSE-XT !
   ['] CODE$ k T0-EMIT:CODE$-XT !
   ['] CALL-ROWS k T0-EMIT:CALL-ROWS-XT !
   ['] CALL-ROW k T0-EMIT:CALL-ROW-XT !
   ['] ADDR-ROWS k T0-EMIT:ADDR-ROWS-XT !
   ['] ADDR-ROW k T0-EMIT:ADDR-ROW-XT !
   ['] LIT k T0-EMIT:LIT-XT !
   ['] ADDR-DATA k T0-EMIT:ADDR-DATA-XT !
   ['] ADDR-CODE k T0-EMIT:ADDR-CODE-XT !
   ['] ADDR-QUOT k T0-EMIT:ADDR-QUOT-XT !
   ['] PUSH k T0-EMIT:PUSH-XT !
   ['] SPILL k T0-EMIT:SPILL-XT !
   ['] SPILL-LIT k T0-EMIT:SPILL-LIT-XT !
   ['] FILL k T0-EMIT:FILL-XT !
   ['] DSP+ k T0-EMIT:DSP+-XT !
   ['] MOV k T0-EMIT:MOV-XT !
   ['] SLOTS k T0-EMIT:SLOTS-XT !
   ['] SLOT-STORE k T0-EMIT:SLOT-STORE-XT !
   ['] SLOT-LOAD k T0-EMIT:SLOT-LOAD-XT !
   ['] OP k T0-EMIT:OP-XT !
   ['] OP-LIT k T0-EMIT:OP-LIT-XT !
   ['] SHIFT k T0-EMIT:SHIFT-XT !
   ['] SHIFT-LIT k T0-EMIT:SHIFT-LIT-XT !
   ['] MULX k T0-EMIT:MULX-XT !
   ['] MUL-LIT k T0-EMIT:MUL-LIT-XT !
   ['] NEG k T0-EMIT:NEG-XT !
   ['] NOT k T0-EMIT:NOT-XT !
   ['] CMP-SET k T0-EMIT:CMP-SET-XT !
   ['] CMP-SET-LIT k T0-EMIT:CMP-SET-LIT-XT !
   ['] LOAD k T0-EMIT:LOAD-XT !
   ['] CALL k T0-EMIT:CALL-XT !
   ['] TAIL k T0-EMIT:TAIL-XT !
   ['] CALL-SELF k T0-EMIT:CALL-SELF-XT !
   ['] IF, k T0-EMIT:IF,-XT !
   ['] ELSE, k T0-EMIT:ELSE,-XT !
   ['] THEN, k T0-EMIT:THEN,-XT !
   ['] BEGIN, k T0-EMIT:BEGIN,-XT !
   ['] WHILE, k T0-EMIT:WHILE,-XT !
   ['] REPEAT, k T0-EMIT:REPEAT,-XT !
   ['] UNTIL, k T0-EMIT:UNTIL,-XT !
   ['] AGAIN, k T0-EMIT:AGAIN,-XT !
   ['] CASE, k T0-EMIT:CASE,-XT !
   ['] OF, k T0-EMIT:OF,-XT !
   ['] OF-LIT k T0-EMIT:OF-LIT-XT !
   ['] ENDOF, k T0-EMIT:ENDOF,-XT !
   ['] ENDCASE, k T0-EMIT:ENDCASE,-XT !
   ['] TOR k T0-EMIT:TOR-XT !
   ['] RFROM k T0-EMIT:RFROM-XT !
   ['] RFETCH k T0-EMIT:RFETCH-XT !
   ['] LOOP-PUSH k T0-EMIT:LOOP-PUSH-XT !
   ['] LOOP-POP k T0-EMIT:LOOP-POP-XT !
   ['] LOOP-I k T0-EMIT:LOOP-I-XT !
   ['] LOOP-J k T0-EMIT:LOOP-J-XT !
   ['] LOOP-OPEN k T0-EMIT:LOOP-OPEN-XT !
   ['] LEAVE, k T0-EMIT:LEAVE,-XT !
   ['] LOOP-NEXT k T0-EMIT:LOOP-NEXT-XT !
   ['] LOOP-STEP k T0-EMIT:LOOP-STEP-XT !
   ['] LOOP-CLOSE k T0-EMIT:LOOP-CLOSE-XT !
   ['] IMM? k T0-EMIT:IMM?-XT ! ;

CTARGET-ARCH:X86-64 ' INSTALL T0-EMIT:PROVIDE

;using
;package
