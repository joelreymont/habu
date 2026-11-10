\ t0-wasm.f - the Wasm generator of the tier 0 compiler, part WASM of package
\ COMP.
\
\ Implements the straight-line and control EMIT- operations src/habu/t0-emit.f
\ states for every target; this file holds only Wasm encodings and local
\ choices. FILL-SLOT stores its words into the EMIT- cells for
\ CTARGET-ARCH:WASM. Later work: the quotation and does> units (QUOT-OPEN,
\ UNIT-OPEN, ADDR-QUOT), the callback stub, CALL-SELF, the return stack, the DO
\ loops and `leave`, which stay unset.

require src/lib/error.f
require src/lib/byte-buffer.f
require src/lib/span.f
require src/core/engine-error.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/habu/t0-emit.f

package COMP
package WASM

15 constant NVREG

\ The exit status a Wasm checked-memory fault records, the value and meaning of
\ tier 1's (src/arch/wasm/select.f CRASH-RC).
134 constant CRASH-RC

\ ---- the Wasm bytes ------------------------------------------------------------
$00 constant OP-UNREACHABLE
$02 constant OP-BLOCK
$03 constant OP-LOOP
$04 constant OP-IF
$05 constant OP-ELSE
$0B constant OP-END
$0C constant OP-BR
$0D constant OP-BR-IF
$01 constant OP-NOP
$0F constant OP-RETURN
$10 constant OP-CALL
$1A constant OP-DROP
$20 constant OP-LOCAL-GET
$21 constant OP-LOCAL-SET
$28 constant OP-I32-LOAD
$29 constant OP-I64-LOAD
$31 constant OP-I64-LOAD8-U
$36 constant OP-I32-STORE
$37 constant OP-I64-STORE
$41 constant OP-I32-CONST
$42 constant OP-I64-CONST
$45 constant OP-I32-EQZ
$4B constant OP-I32-GT-U
$50 constant OP-I64-EQZ
$51 constant OP-I64-EQ
$52 constant OP-I64-NE
$53 constant OP-I64-LT-S
$54 constant OP-I64-LT-U
$55 constant OP-I64-GT-S
$56 constant OP-I64-GT-U
$57 constant OP-I64-LE-S
$59 constant OP-I64-GE-S
$6A constant OP-I32-ADD
$72 constant OP-I32-OR
$7C constant OP-I64-ADD
$7D constant OP-I64-SUB
$7E constant OP-I64-MUL
$7F constant OP-I64-DIV-S
$81 constant OP-I64-REM-S
$83 constant OP-I64-AND
$84 constant OP-I64-OR
$85 constant OP-I64-XOR
$86 constant OP-I64-SHL
$88 constant OP-I64-SHR-U
$A7 constant OP-I32-WRAP-I64
$AD constant OP-I64-EXTEND-I32-U
$7F constant I32
$7E constant I64
$40 constant EMPTY-TYPE              \ a block, loop or if taking and leaving nothing
2 constant ALIGN-32                  \ a load's or store's alignment exponent
3 constant ALIGN-64
8 constant CELL-BYTES

\ The locals: ctx, the unit's parameter; SP, the data stack pointer; the
\ vregs; CA and CB, the operands of the last compare; then frame slot k.
0 constant L-CTX
1 constant L-SP
2 constant L-VREG
17 constant L-CA
18 constant L-CB
19 constant L-SLOT
17 constant FIXED-I64                \ the i64 locals before the frame

\ ---- the sink ------------------------------------------------------------------
create SINK BUF:HDR-BYTES SCRATCH-ALLOT
SCRATCH-VARIABLE SINK-LIVE            \ nonzero once SINK holds storage
variable FRAME-CELLS                  \ frame slots this unit uses
variable COUNT-AT                     \ the unit's padded i64 count field
DYNAMIC-BUFFER CF-KIND n              \ the open blocks' kinds, innermost last
variable CF-N

4096 constant SINK-START-BYTES
10 constant LEB-MOST
LEB-MOST BUFFER: LEB-ROOM             \ a shortest LEB before it is appended

: HERE-OFF ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;

: B, ( n -- ) SINK BUF:APPEND-BYTE ;

: SCRATCH ( -- SPAN:span<u8> ) LEB-ROOM LEB-MOST SPAN:MAKE ;

: LEB, ( n -- ) {: u:n :}
   LEB-ROOM u BUF:N>BLEN SINK BUF:APPEND-SPAN ;

: U32, ( n -- ) SCRATCH WLEB:U32! LEB, ;
: S32, ( n -- ) SCRATCH WLEB:S32! LEB, ;
: S64, ( n -- ) SCRATCH WLEB:S64! LEB, ;

: PAD32, ( n -- ) SINK 5 BUF:N>BLEN BUF:GROW WLEB:U32-PAD! drop ;
: PAD64, ( n -- ) SINK 10 BUF:N>BLEN BUF:GROW WLEB:S64-PAD! drop ;

: GET, ( n -- ) OP-LOCAL-GET B, U32, ;
: SET, ( n -- ) OP-LOCAL-SET B, U32, ;
: I32, ( n -- ) OP-I32-CONST B, S32, ;
: I64, ( n -- ) OP-I64-CONST B, S64, ;
: OPENER, ( n -- ) B, EMPTY-TYPE B, ;
: END, ( -- ) OP-END B, ;

\ A load or a store: the opcode, its alignment's exponent, its offset.
: MEM, ( n n n -- ) {: o:n al:n off:n :}
   o B,  al U32,  off U32, ;

: REG ( vreg -- n )
   VREG>N {: k:n :}
   k 0 < k NVREG >= or if E-T0-OPERAND throw then
   k L-VREG + ;

: GET-REG ( vreg -- ) REG GET, ;
: SET-REG ( vreg -- ) REG SET, ;

\ SP is loaded from the context at a unit's start and after each call; SYNC
\ is the only write of the context's top.
: SP@, ( -- )
   L-CTX GET,  OP-I32-LOAD ALIGN-32 WPROF:CTX-STACK-TOP MEM,  L-SP SET, ;

: SYNC ( -- )
   L-CTX GET,  L-SP GET,  OP-I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM, ;

\ The address of the cell k cells above SP; k stays out of the memory offset,
\ which is unsigned.
: CELL-AT, ( n -- ) {: k:n :}
   L-SP GET,
   k 0= if exit then
   k CELL-BYTES * I32,  OP-I32-ADD B, ;

: LD64, ( -- ) OP-I64-LOAD ALIGN-64 0 MEM, ;
: ST64, ( -- ) OP-I64-STORE ALIGN-64 0 MEM, ;

\ A fault is fatal, never a status (src/arch/wasm/select.f THE TRAP): the
\ caller writes the i64 address between FAULT-KIND, and FAULT-END,.
: FAULT-KIND, ( n -- ) {: kind:n :}
   L-CTX GET,  kind I32,  OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
   L-CTX GET, ;

: FAULT-END, ( -- )
   OP-I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM,  OP-UNREACHABLE B, ;

\ SP past the stack region records STACK-BOUNDS with SP as its address. The
\ check follows the stores of the cells the move covers, so a failed one has
\ written at most that move's cells past the region before the module ends;
\ tier 1 checks before the stores (src/arch/wasm/select.f ROOM).
: ROOM ( -- )
   L-SP GET,  WPROF:STACK-END I32,  OP-I32-GT-U B,  OP-IF OPENER,
   ENGINE-ERROR:STACK-BOUNDS FAULT-KIND,  L-SP GET,  OP-I64-EXTEND-I32-U B,
   FAULT-END,  END, ;

\ The checked pointer of tier 1 (src/arch/wasm/select.f THE CHECKED ACCESS):
\ its high 32 bits are zero and it is at or past WPROF:CTX-BASE, else the
\ fault records CRASH-RC with the pointer.
: CHECKED ( vreg -- ) {: a:vreg :}
   a GET-REG  WPROF:CTX-BASE I64,  OP-I64-LT-U B,
   a GET-REG  $FFFFFFFF I64,  OP-I64-GT-U B,
   OP-I32-OR B,  OP-IF OPENER,
   CRASH-RC FAULT-KIND,  a GET-REG  FAULT-END,  END, ;

\ Every unit is a function of type (i32 ctx) -> (i32 status) as WLINK takes a
\ body: one i32 local, SP, then FIXED-I64 + FRAME-CELLS i64 locals, a padded
\ count CLOSE writes.
: UNIT-BEGIN ( -- )
   HERE-OFF EMIT-UNIT+
   0 FRAME-CELLS !
   2 B,  1 B,  I32 B,
   HERE-OFF COUNT-AT !  FIXED-I64 PAD32,  I64 B,
   SP@, ;


\ ---- the body ----------------------------------------------------------------
\ The rows (t0-emit.f "THE ROWS"): a call or tail site is `call` and a padded
\ five-byte u32 function index, zero here, whose field starts one byte past the
\ row's offset; an address site is `i64.const` and a padded ten-byte s64
\ holding the host value, whose field starts one byte past the row's offset.
\ The does> patch slot is six bytes: `drop; i32.const 0; nop; nop; nop` (no
\ clause, status 0), or `call` and a padded five-byte function index.
: OPEN ( -- )
   SINK-LIVE @ 0= if
      SINK SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 SINK-LIVE !
   then
   SINK BUF:CLEAR
   0 CF-N !
   UNIT-BEGIN ;

\ Status 0 is a return (src/arch/wasm/select.f THE STATUS).
: RETURN ( -- )
   SYNC  0 I32,  OP-RETURN B, ;

\ Every path of a unit leaves by RETURN or TAIL, so the closing `unreachable`
\ makes the fall-through valid whatever precedes it.
: CLOSE ( -- )
   FIXED-I64 FRAME-CELLS @ +  SINK BUF:SPAN  COUNT-AT @  WLEB:U32-PATCH
   OP-UNREACHABLE B,  END, ;


\ The return is the does> patch slot over the context: a clause patched in
\ runs with it, and its status is the unit's.
: SLOT-CLOSE ( -- )
   SYNC  L-CTX GET,
   HERE-OFF EMIT-PATCH-SLOT!
   OP-DROP B,  0 I32,  OP-NOP B,  OP-NOP B,  OP-NOP B,
   OP-RETURN B,
   CLOSE ;

: BYTES ( -- ptr u8 n ) SINK BUF:SPAN$ BUF:BLEN>N ;

\ A Wasm body is never placed at a host address.
: PATCH ( n -- ) drop E-COMP-STATE throw ;

: IMM? ( n -- bool ) drop true ;

\ ---- values ------------------------------------------------------------------
: LIT ( n vreg -- ) {: v:n d:vreg :}
   v I64,  d SET-REG ;

: ADDR ( n n vreg n -- ) {: v:n o:n d:vreg kind:n :}
   HERE-OFF kind v o EMIT-ADDR-ROW+
   OP-I64-CONST B,  v PAD64,  d SET-REG ;

: ADDR-DATA ( n n vreg -- ) NEMIT:ADDR-DATA ADDR ;

: ADDR-CODE ( n vreg -- ) {: v:n d:vreg :}
   v -1 d NEMIT:ADDR-CODE ADDR ;

: SPILL ( vreg n -- ) {: s:vreg k:n :}
   k CELL-AT,  s GET-REG  ST64, ;

: SPILL-LIT ( n n -- ) {: v:n k:n :}
   k CELL-AT,  v I64,  ST64, ;

: FILL ( vreg n -- ) {: d:vreg k:n :}
   k CELL-AT,  LD64,  d SET-REG ;

: DSP+ ( n -- ) {: k:n :}
   k 0= if exit then
   k CELL-AT,  L-SP SET,
   k 0 > if ROOM then ;

: PUSH ( vreg n -- ) {: s:vreg k:n :}
   s 0 SPILL  k DSP+ ;

: PUSH-LIT ( n n -- ) {: v:n k:n :}
   v 0 SPILL-LIT  k DSP+ ;

: POP ( vreg n -- ) {: d:vreg k:n :}
   k negate DSP+  d 0 FILL ;

: MOV ( vreg vreg -- ) {: d:vreg s:vreg :}
   s GET-REG  d SET-REG ;

: EXCHANGE ( vreg vreg -- ) {: a:vreg b:vreg :}
   a GET-REG  b GET-REG  a SET-REG  b SET-REG ;

: SLOTS ( n -- ) {: k:n :}
   k FRAME-CELLS @ > if k FRAME-CELLS ! then ;

: SLOT-STORE ( vreg n -- ) {: s:vreg k:n :}
   s GET-REG  k L-SLOT + SET, ;

: SLOT-LOAD ( vreg n -- ) {: d:vreg k:n :}
   k L-SLOT + GET,  d SET-REG ;

\ ---- arithmetic --------------------------------------------------------------
: ALU-OP ( alu -- n )
   ALU>N {: k:n :}
   k 0 = if OP-I64-ADD exit then
   k 1 = if OP-I64-SUB exit then
   k 2 = if OP-I64-AND exit then
   k 3 = if OP-I64-OR exit then
   k 4 = if OP-I64-XOR exit then
   E-T0-OPERAND throw ;

\ Wasm masks a shift count to six bits.
: SHIFT-OP ( shifter -- n )
   SHIFTER>N {: k:n :}
   k 0 = if OP-I64-SHL exit then
   k 1 = if OP-I64-SHR-U exit then
   E-T0-OPERAND throw ;

\ The opcode of each condition, which the branches reuse.
: COND ( cc -- n )
   CC>N {: k:n :}
   k 0 = if OP-I64-EQ exit then
   k 1 = if OP-I64-NE exit then
   k 2 = if OP-I64-LT-S exit then
   k 3 = if OP-I64-GE-S exit then
   k 4 = if OP-I64-LE-S exit then
   k 5 = if OP-I64-GT-S exit then
   E-T0-OPERAND throw ;

\ d becomes d o s, or d o v.
: REG-OP ( vreg vreg n -- ) {: d:vreg s:vreg o:n :}
   d GET-REG  s GET-REG  o B,  d SET-REG ;

: LIT-OP ( vreg n n -- ) {: d:vreg v:n o:n :}
   d GET-REG  v I64,  o B,  d SET-REG ;

: OP ( vreg vreg alu -- ) ALU-OP REG-OP ;
: OP-LIT ( vreg n alu -- ) ALU-OP LIT-OP ;
: SHIFT ( vreg vreg shifter -- ) SHIFT-OP REG-OP ;

: SHIFT-LIT ( vreg n shifter -- ) {: d:vreg v:n o:shifter :}
   d  v 63 and  o SHIFT-OP LIT-OP ;

: MULX ( vreg vreg -- ) OP-I64-MUL REG-OP ;
: MUL-LIT ( vreg n -- ) OP-I64-MUL LIT-OP ;

\ v is neither 0 nor -1, so neither traps; both truncate toward zero.
: DIV-LIT ( vreg n -- ) OP-I64-DIV-S LIT-OP ;
: MOD-LIT ( vreg n -- ) OP-I64-REM-S LIT-OP ;

: NEG ( vreg -- ) {: d:vreg :}
   0 I64,  d GET-REG  OP-I64-SUB B,  d SET-REG ;

: NOT ( vreg -- ) -1 OP-I64-XOR LIT-OP ;

\ The compare copies its operands into CA and CB: the moves t0-emit.f allows
\ before its reader may overwrite the vregs, and none of them writes CA or CB.
: CMP ( vreg vreg -- ) {: a:vreg b:vreg :}
   a GET-REG  L-CA SET,  b GET-REG  L-CB SET, ;

: CMP-LIT ( vreg n -- ) {: a:vreg v:n :}
   a GET-REG  L-CA SET,  v I64,  L-CB SET, ;

\ 0 minus the zero-extended i32 condition: -1 when it holds, else 0.
: SET-FLAG ( vreg cc -- ) {: d:vreg c:cc :}
   c COND {: o:n :}
   0 I64,  L-CA GET,  L-CB GET,  o B,  OP-I64-EXTEND-I32-U B,  OP-I64-SUB B,
   d SET-REG ;

\ ---- memory ------------------------------------------------------------------
: LOAD ( vreg vreg width -- ) {: d:vreg a:vreg w:width :}
   a CHECKED
   a GET-REG  OP-I32-WRAP-I64 B,
   w WIDTH>N 1 = if OP-I64-LOAD8-U 0 0 MEM, else LD64, then
   d SET-REG ;

\ ---- calls -------------------------------------------------------------------
: CALL-SITE ( n n n -- ) {: kind:n to:n loc:n :}
   SYNC  L-CTX GET,
   HERE-OFF kind to loc EMIT-CALL-ROW+
   OP-CALL B,  0 PAD32, ;

\ A nonzero status propagates the throw with the context's top where the
\ throw left it.
: CALL ( n n -- ) {: to:n loc:n :}
   NEMIT:CALL to loc CALL-SITE
   OP-IF OPENER,  1 I32,  OP-RETURN B,  END,
   SP@, ;

\ The callee's status is this unit's.
: TAIL ( n n -- ) {: to:n loc:n :}
   NEMIT:TAIL to loc CALL-SITE
   OP-RETURN B, ;

\ ---- control -----------------------------------------------------------------
\ CONTROL. A Wasm branch names an enclosing block by depth, so every open source
\ structure is one open block of the empty type, an entry in CF-KIND; a
\ branch's depth is the count of entries above its target. A `begin` is a
\ `loop` and a `while` an `if` inside it: `begin A while B repeat` is
\ `loop A <flag> if B br 1 end end`, and `begin A while B while C repeat D then`
\ is `loop A <flag> if B <flag> if C br 2 end D end end`. A back branch makes
\ its LOOP a LOOP-DONE, whose `end` CLOSE-DONE writes once it is innermost.
0 constant CF-IF
1 constant CF-LOOP
2 constant CF-LOOP-DONE
3 constant CF-CASE

: CF-PUSH ( n -- ) {: kind:n :}
   CF-N @ {: k:n :}
   k 1 + CF-KIND-RESERVE
   kind k CF-KIND !
   k 1 + CF-N ! ;

: CF-POP ( -- ) CF-N @ 1 - CF-N ! ;

\ The index of the innermost open entry of kind.
: CF-FIND ( n -- n ) {: kind:n :}
   CF-N @ 1 -
   begin dup 0 >= if dup CF-KIND @ kind <> else false then while 1 - repeat
   dup 0 < if E-T0-OPERAND throw then ;

: DEPTH-OF ( n -- n ) CF-FIND  CF-N @ 1 -  swap - ;

: MARK-DONE ( -- ) CF-LOOP CF-FIND {: k:n :}  CF-LOOP-DONE k CF-KIND ! ;

: CLOSE-DONE ( -- )
   begin CF-N @ 0 > if CF-N @ 1 - CF-KIND @ CF-LOOP-DONE = else false then
   while END, CF-POP repeat ;

: BR, ( n -- ) OP-BR B, U32, ;
: BR-IF, ( n -- ) OP-BR-IF B, U32, ;
: ARM, ( -- ) OP-IF OPENER,  CF-IF CF-PUSH ;

\ Condition c of the last compare, as an i32.
: TEST, ( cc -- ) {: c:cc :}
   L-CA GET,  L-CB GET,  c COND B, ;

: IF, ( vreg -- ) GET-REG  OP-I64-EQZ B,  OP-I32-EQZ B,  ARM, ;
: IF-CC, ( cc -- ) TEST,  ARM, ;
: ELSE, ( -- ) OP-ELSE B, ;
: THEN, ( -- ) END,  CF-POP  CLOSE-DONE ;
: BEGIN, ( -- ) OP-LOOP OPENER,  CF-LOOP CF-PUSH ;

: REPEAT, ( -- )
   CF-LOOP DEPTH-OF BR,  END,  CF-POP  MARK-DONE  CLOSE-DONE ;

: UNTIL, ( vreg -- )
   GET-REG  OP-I64-EQZ B,  CF-LOOP DEPTH-OF BR-IF,  MARK-DONE  CLOSE-DONE ;

: UNTIL-CC, ( cc -- )
   CC-NOT TEST,  CF-LOOP DEPTH-OF BR-IF,  MARK-DONE  CLOSE-DONE ;

: AGAIN, ( -- ) CF-LOOP DEPTH-OF BR,  MARK-DONE  CLOSE-DONE ;
: CASE, ( -- ) OP-BLOCK OPENER,  CF-CASE CF-PUSH ;

: OF, ( vreg vreg -- ) {: s:vreg v:vreg :}
   s GET-REG  v GET-REG  OP-I64-EQ B,  ARM, ;

: OF-LIT ( vreg n -- ) {: s:vreg v:n :}
   s GET-REG  v I64,  OP-I64-EQ B,  ARM, ;

: ENDOF, ( -- ) CF-CASE DEPTH-OF BR,  END,  CF-POP  CLOSE-DONE ;
: ENDCASE, ( -- ) END,  CF-POP  CLOSE-DONE ;

\ ---- the seam ----------------------------------------------------------------
: FILL-SLOT ( n -- )
   {: k:n :}
   NVREG k EMIT-VREGS-N !
   ['] OPEN k EMIT-OPEN-XT !
   ['] RETURN k EMIT-RETURN-XT !
   ['] CLOSE k EMIT-CLOSE-XT !
   ['] SLOT-CLOSE k EMIT-SLOT-CLOSE-XT !
   ['] BYTES k EMIT-BYTES-XT !
   ['] PATCH k EMIT-PATCH-XT !
   ['] LIT k EMIT-LIT-XT !
   ['] ADDR-DATA k EMIT-ADDR-DATA-XT !
   ['] ADDR-CODE k EMIT-ADDR-CODE-XT !
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
   ['] IMM? k EMIT-IMM?-XT !
   ['] LOAD k EMIT-LOAD-XT !
   ['] CALL k EMIT-CALL-XT !
   ['] TAIL k EMIT-TAIL-XT !
   ['] IF, k EMIT-IF,-XT !
   ['] ELSE, k EMIT-ELSE,-XT !
   ['] THEN, k EMIT-THEN,-XT !
   ['] BEGIN, k EMIT-BEGIN,-XT !
   ['] IF, k EMIT-WHILE,-XT !
   ['] REPEAT, k EMIT-REPEAT,-XT !
   ['] UNTIL, k EMIT-UNTIL,-XT !
   ['] IF-CC, k EMIT-IF-CC,-XT !
   ['] IF-CC, k EMIT-WHILE-CC,-XT !
   ['] UNTIL-CC, k EMIT-UNTIL-CC,-XT !
   ['] AGAIN, k EMIT-AGAIN,-XT !
   ['] CASE, k EMIT-CASE,-XT !
   ['] OF, k EMIT-OF,-XT !
   ['] OF-LIT k EMIT-OF-LIT-XT !
   ['] ENDOF, k EMIT-ENDOF,-XT !
   ['] ENDCASE, k EMIT-ENDCASE,-XT ! ;

CTARGET-ARCH:WASM ' FILL-SLOT EMIT-PROVIDE

;package
;package
