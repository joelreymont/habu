\ t0-x64.f - the x86-64 emitter of the tier 0 compiler, package T0-EMIT.
\
\ src/habu/t0.f (package T0) walks a definition's body text and drives these
\ words; every target's emitter file defines the same T0-EMIT words and T0
\ loads after one of them. The front end names values by virtual register
\ (vreg 0..VREGS-1) and never by machine register, so it stays the same for
\ every target.
\
\ THE CONTRACT A COMPILED BODY KEEPS (src/habu/kernel-x64.f "A BODY'S
\ CONTRACT"). rbp is DATA, r12 the data stack pointer, rbx and r13-r15 the
\ other engine registers; compiled code never writes them except r12 as the
\ stack. The data stack grows up: a push stores at r12 and adds a cell, a pop
\ subtracts a cell and loads (src/arch/x86-64/rt.f G-PUSH, G-POP). The vregs
\ are rcx rdx rsi rdi r8-r11; rax is this file's own temporary and holds no
\ value between two of its words. A body is entered by `call` and leaves by
\ `ret`; nothing is saved, and its frame on the machine stack is only its own
\ cells, rounded to 16 (src/arch/x86-64/abi.f FRAME-FOR).
\
\ THE SINK. One src/lib/byte-buffer.f header, as src/arch/x86-64/icode.f keeps
\ for the kernel. OPEN empties it and CLOSE leaves the finished body in it for
\ CODE$ to read; the next OPEN begins the next body.
\
\ THE FRAME. OPEN emits `sub rsp, imm32` and each way out (RETURN, TAIL) emits
\ `add rsp, imm32`, each with a zero field recorded in FRAME-AT. CLOSE emits
\ nothing: the front end ends the body with RETURN or TAIL, and CLOSE writes
\ the frame size, FRAME-CELLS rounded to 16 bytes, into every field.
\
\ THE ROWS. A call or tail site (E8 or E9 rel32, the field left zero) and a
\ code or data address site (movabs r64, imm64) are recorded with the offset of
\ the instruction's first byte, its kind and its target, for the publisher to
\ place and relocate: CALL-ROWS / CALL-ROW and ADDR-ROWS / ADDR-ROW read them.
\ Branches inside the body are resolved here: JMP, JZ and JNZ answer a site
\ that PATCH binds to the current offset, LABEL answers a mark that JMP-BACK
\ and JZ-BACK branch to. Every branch is rel32.
\
\ DIVISION. `/`, `mod` and `/mod` are calls of the kernel rows, which screen a
\ zero divisor and MIN-N -1; this file has no divide.

require src/lib/type/deftype.f
require src/lib/byte-buffer.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/x86-64/asm.f

package T0-EMIT
using X64ASM
public

DEFTYPE VREG       \ a virtual register, 0 .. VREGS-1
DEFTYPE ALU        \ a two-operand operation OP applies
DEFTYPE SHIFTER    \ a shift SHIFT applies
DEFTYPE CC         \ a comparison CMP-SET answers
DEFTYPE WIDTH      \ the width LOAD and STORE move
DEFTYPE MARK       \ a backward branch target, from LABEL
DEFTYPE SITE       \ a forward branch, from JMP, JZ or JNZ

8 constant VREGS

: OP-ADD ( -- alu ) 0 >ALU ;
: OP-SUB ( -- alu ) 1 >ALU ;
: OP-AND ( -- alu ) 2 >ALU ;
: OP-OR  ( -- alu ) 3 >ALU ;
: OP-XOR ( -- alu ) 4 >ALU ;

: SH-LEFT  ( -- shifter ) 0 >SHIFTER ;
: SH-RIGHT ( -- shifter ) 1 >SHIFTER ;

\ Each comparison is its x86 condition number.
: CC-EQ  ( -- cc )  4 >CC ;
: CC-NE  ( -- cc )  5 >CC ;
: CC-LT  ( -- cc ) 12 >CC ;
: CC-GE  ( -- cc ) 13 >CC ;
: CC-LE  ( -- cc ) 14 >CC ;
: CC-GT  ( -- cc ) 15 >CC ;

: CELL-W ( -- width ) 8 >WIDTH ;
: BYTE-W ( -- width ) 1 >WIDTH ;

\ The comparison that answers the same with its operands exchanged.
: CC-SWAP ( cc -- cc ) {: c:cc :}
   c CC>N {: k:n :}
   k 12 = if CC-GT exit then
   k 15 = if CC-LT exit then
   k 13 = if CC-LE exit then
   k 14 = if CC-GE exit then
   c ;

\ Whether OP-LIT, MUL-LIT and CMP-SET-LIT take n as their immediate.
: IMM? ( n -- bool ) {: v:n :}
   v -2147483648 >= v 2147483647 <= and ;

\ The kinds of a call row and of an address row.
0 constant CALL-KIND
1 constant TAIL-KIND
1 constant ADDR-DATA-KIND
2 constant ADDR-CODE-KIND

private

create SINK BUF:HDR-BYTES allot
variable SINK-LIVE                    \ nonzero once SINK holds storage
variable FRAME-CELLS                  \ machine-stack cells this body uses
variable NFRAME
variable NCALL
variable NADDR
DYNAMIC-BUFFER FRAME-AT n             \ offset of each frame imm32 field
DYNAMIC-BUFFER CALL-AT n              \ call row: first byte, kind, target
DYNAMIC-BUFFER CALL-KIND-AT n
DYNAMIC-BUFFER CALL-TO n
DYNAMIC-BUFFER ADDR-AT n              \ address row: first byte, kind, value
DYNAMIC-BUFFER ADDR-KIND-AT n
DYNAMIC-BUFFER ADDR-TO n

4096 constant SINK-START-BYTES

: HERE-OFF ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;

: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;

\ vreg 0..3 are rcx rdx rsi rdi, 4..7 are r8..r11.
: REG ( vreg -- r64 ) {: v:vreg :}
   v VREG>N {: k:n :}
   k 0 < k VREGS >= or if E-OPERAND throw then
   k 0 = if RCX exit then
   k 1 = if RDX exit then
   k 2 = if RSI exit then
   k 3 = if RDI exit then
   k 4 + >R64 ;

\ Write the low four bytes of v at offset off of the sink, little-endian.
: PUT32 ( n n -- ) {: v:n off:n :}
   SINK BUF:SPAN$ drop off + {: p:ptr :}
   4 0 ?do v i 8 * rshift $FF and p i + c! loop ;

\ A forward rel32 at the end of what was just emitted: its field's offset.
: FWD-SITE ( -- site ) HERE-OFF 4 - >SITE ;

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

: ADDR ( n vreg n -- ) {: v:n d:vreg kind:n :}
   HERE-OFF kind v ADDR-ROW+
   d REG v >IMM64 SINK ENC-MOV-RI64 ;

\ The return stack is RSP-CELL cells deep from STACK-ABI:RETURN-BASE-CELL
\ (src/habu/kernel-x64.f RSTACK,): r becomes the address one cell past its top.
: RTOP ( r64 -- ) {: r:r64 :}
   r 3 >IMM8 SINK ENC-SHL-RI8
   r DATA-REG STACK-ABI:RETURN-BASE-CELL MEM-OFF SINK ENC-ADD-RM ;

: RDEPTH ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-RM ;

: RDEPTH! ( r64 -- ) {: r:r64 :}
   r DATA-REG RSP-CELL MEM-OFF SINK ENC-MOV-MR ;

: REL-TO ( n -- rel ) HERE-OFF - >REL ;

public

\ ---- the body ----------------------------------------------------------------
: OPEN ( -- )
   SINK-LIVE @ 0= if
      SINK SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 SINK-LIVE !
   then
   SINK BUF:CLEAR
   0 FRAME-CELLS !  0 NFRAME !  0 NCALL !  0 NADDR !
   RSP 0 >IMM32 SINK ENC-SUB-RI32  FRAME-SITE ;

\ Leave the word from the middle of its body.
: RETURN ( -- )
   UNFRAME  SINK ENC-RET ;

: CLOSE ( -- )
   FRAME-CELLS @ CELL * 15 + 15 invert and {: bytes:n :}
   NFRAME @ 0 ?do bytes i FRAME-AT @ PUT32 loop ;

: CODE$ ( -- ptr u8 n ) SINK BUF:SPAN$ BUF:BLEN>N ;

: CALL-ROWS ( -- n ) NCALL @ ;
: CALL-ROW ( n -- n n n ) {: k:n :}  k CALL-AT @  k CALL-KIND-AT @  k CALL-TO @ ;
: ADDR-ROWS ( -- n ) NADDR @ ;
: ADDR-ROW ( n -- n n n ) {: k:n :}  k ADDR-AT @  k ADDR-KIND-AT @  k ADDR-TO @ ;

\ ---- values ------------------------------------------------------------------
: LIT ( n vreg -- ) {: v:n d:vreg :}  v d REG LIT-R ;

: ADDR-DATA ( n vreg -- ) ADDR-DATA-KIND ADDR ;
: ADDR-CODE ( n vreg -- ) ADDR-CODE-KIND ADDR ;

\ The data stack: PUSH and POP move one cell; SPILL, SPILL-LIT and FILL address
\ the cell k cells above the stack pointer without moving it, and DSP+ moves
\ it by k cells.
: PUSH ( vreg -- ) {: s:vreg :}
   s REG DSP MEM-AT SINK ENC-MOV-MR
   DSP CELL >IMM8 SINK ENC-ADD-RI8 ;

: POP ( vreg -- ) {: d:vreg :}
   DSP CELL >IMM8 SINK ENC-SUB-RI8
   d REG DSP MEM-AT SINK ENC-MOV-RM ;

: SPILL ( vreg n -- ) {: s:vreg k:n :}
   s REG DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   v RAX LIT-R
   RAX DSP k CELL * MEM-OFF SINK ENC-MOV-MR ;

: FILL ( vreg n -- ) {: d:vreg k:n :}
   d REG DSP k CELL * MEM-OFF SINK ENC-MOV-RM ;

: DSP+ ( n -- ) {: k:n :}
   k 0 = if exit then
   k 0 > if DSP k CELL * >IMM32 SINK ENC-ADD-RI32 exit then
   DSP k negate CELL * >IMM32 SINK ENC-SUB-RI32 ;

: MOV ( vreg vreg -- ) {: d:vreg s:vreg :}
   d REG s REG SINK ENC-MOV-RR ;

\ ---- arithmetic --------------------------------------------------------------
: OP ( vreg vreg alu -- ) {: d:vreg s:vreg o:alu :}
   d REG {: dr:r64 :}  s REG {: sr:r64 :}  o ALU>N {: k:n :}
   k 0 = if dr sr SINK ENC-ADD-RR exit then
   k 1 = if dr sr SINK ENC-SUB-RR exit then
   k 2 = if dr sr SINK ENC-AND-RR exit then
   k 3 = if dr sr SINK ENC-OR-RR exit then
   k 4 = if dr sr SINK ENC-XOR-RR exit then
   E-OPERAND throw ;

\ n must answer IMM? true.
: OP-LIT ( vreg n alu -- ) {: d:vreg v:n o:alu :}
   d REG {: dr:r64 :}  v >IMM32 {: iv:imm32 :}  o ALU>N {: k:n :}
   k 0 = if dr iv SINK ENC-ADD-RI32 exit then
   k 1 = if dr iv SINK ENC-SUB-RI32 exit then
   k 2 = if dr iv SINK ENC-AND-RI32 exit then
   k 3 = if dr iv SINK ENC-OR-RI32 exit then
   k 4 = if dr iv SINK ENC-XOR-RI32 exit then
   E-OPERAND throw ;

\ A shift by a register takes its count in cl, and cl is vreg 0's low byte.
\ The hardware masks the count to six bits, which is Habu's `lshift` and
\ `rshift` (a count of 64 shifts by nothing); `rshift` is logical.
: SHIFT-CL ( r64 shifter -- ) {: r:r64 o:shifter :}
   o SHIFTER>N 0 = if r SINK ENC-SHL-CL exit then
   r SINK ENC-SHR-CL ;

: SHIFT ( vreg vreg shifter -- ) {: d:vreg s:vreg o:shifter :}
   s VREG>N 0 = if d REG o SHIFT-CL exit then
   RAX RCX SINK ENC-MOV-RR
   RCX s REG SINK ENC-MOV-RR
   d VREG>N 0 = if
      RAX o SHIFT-CL
      RCX RAX SINK ENC-MOV-RR exit
   then
   d REG o SHIFT-CL
   RCX RAX SINK ENC-MOV-RR ;

: SHIFT-LIT ( vreg n shifter -- ) {: d:vreg v:n o:shifter :}
   v 63 and >IMM8 {: c:imm8 :}
   o SHIFTER>N 0 = if d REG c SINK ENC-SHL-RI8 exit then
   d REG c SINK ENC-SHR-RI8 ;

: MULX ( vreg vreg -- ) {: d:vreg s:vreg :}
   d REG s REG SINK ENC-IMUL-RR ;

\ n must answer IMM? true.
: MUL-LIT ( vreg n -- ) {: d:vreg v:n :}
   d REG d REG v >IMM32 SINK ENC-IMUL-RRI32 ;

: NEG ( vreg -- ) REG SINK ENC-NEG ;
: NOT ( vreg -- ) REG SINK ENC-NOT ;

\ d becomes -1 when d c s holds, else 0: a Habu flag.
: SET-FLAG ( vreg cc -- ) {: d:vreg c:cc :}
   c CC>N >CONDITION 0 >R8 SINK ENC-SETCC
   d REG 0 >R8 SINK ENC-MOVZX-8-RR
   d REG SINK ENC-NEG ;

: CMP-SET ( vreg vreg cc -- ) {: d:vreg s:vreg c:cc :}
   d REG s REG SINK ENC-CMP-RR
   d c SET-FLAG ;

\ n must answer IMM? true.
: CMP-SET-LIT ( vreg n cc -- ) {: d:vreg v:n c:cc :}
   d REG v >IMM32 SINK ENC-CMP-RI32
   d c SET-FLAG ;

\ ---- memory ------------------------------------------------------------------
\ LOAD reads the cell or the zero-extended byte at address a into d; STORE
\ writes the cell or the low byte of s at address a. Neither screens the
\ address, as the kernel's `@` and `c@` rows do not: `!` and `c!` screen a span
\ and stay calls.
: LOAD ( vreg vreg width -- ) {: d:vreg a:vreg w:width :}
   w WIDTH>N 1 = if d REG a REG MEM-AT SINK ENC-MOVZX-8-RM exit then
   d REG a REG MEM-AT SINK ENC-MOV-RM ;

: STORE ( vreg vreg width -- ) {: s:vreg a:vreg w:width :}
   w WIDTH>N 1 = if s REG R64>N >R8 a REG MEM-AT SINK ENC-MOV8-MR exit then
   s REG a REG MEM-AT SINK ENC-MOV-MR ;

\ ---- calls -------------------------------------------------------------------
: CALL ( n -- ) {: to:n :}
   HERE-OFF CALL-KIND to CALL-ROW+
   0 >REL SINK ENC-CALL-REL32 ;

: TAIL ( n -- ) {: to:n :}
   UNFRAME
   HERE-OFF TAIL-KIND to CALL-ROW+
   0 >REL SINK ENC-JMP-REL32 ;

\ `recurse`: a call of byte 0 of this body.
: CALL-SELF ( -- )
   0 HERE-OFF 5 + - >REL SINK ENC-CALL-REL32 ;

\ ---- branches ----------------------------------------------------------------
: LABEL ( -- mark ) HERE-OFF >MARK ;

: JMP ( -- site )
   0 >REL SINK ENC-JMP-REL32  FWD-SITE ;

: JZ ( vreg -- site ) {: c:vreg :}
   c REG c REG SINK ENC-TEST-RR
   C-E 0 >REL SINK ENC-JCC-REL32  FWD-SITE ;

: JNZ ( vreg -- site ) {: c:vreg :}
   c REG c REG SINK ENC-TEST-RR
   C-NE 0 >REL SINK ENC-JCC-REL32  FWD-SITE ;

\ A backward rel is measured from the end of the instruction, five bytes for
\ jmp rel32 and six for jcc rel32.
: JMP-BACK ( mark -- ) {: m:mark :}
   m MARK>N 5 - REL-TO SINK ENC-JMP-REL32 ;

: JZ-BACK ( vreg mark -- ) {: c:vreg m:mark :}
   c REG c REG SINK ENC-TEST-RR
   C-E m MARK>N 6 - REL-TO SINK ENC-JCC-REL32 ;

: PATCH ( site -- ) {: s:site :}
   s SITE>N {: at:n :}
   HERE-OFF at 4 + - at PUT32 ;

\ ---- the return stack --------------------------------------------------------
\ >r, r> and r@ move a cell between a vreg and the return stack through rax or
\ the destination alone.
: TOR ( vreg -- ) {: s:vreg :}
   RAX RDEPTH
   RAX SINK ENC-INC
   RAX RDEPTH!
   RAX RTOP
   s REG RAX CELL negate MEM-OFF SINK ENC-MOV-MR ;

: RFROM ( vreg -- ) {: d:vreg :}
   d REG {: r:r64 :}
   r RDEPTH
   r SINK ENC-DEC
   r RDEPTH!
   r RTOP
   r r MEM-AT SINK ENC-MOV-RM ;

: RFETCH ( vreg -- ) {: d:vreg :}
   d REG {: r:r64 :}
   r RDEPTH
   r RTOP
   r r CELL negate MEM-OFF SINK ENC-MOV-RM ;

;using
;package
