\ t0-wasm.f - the Wasm generator of the tier 0 compiler, part WASM of package
\ COMP.
\
\ Implements the straight-line, control, return-stack and loop EMIT- operations;
\ this file holds only Wasm encodings and local choices. The callback stub cell
\ stays unset.
\
\ THE ROWS. A call or tail site is `call` and a padded five-byte u32 function
\ index, zero here; an address site is `i64.const` and a padded ten-byte s64
\ holding the host value; each field starts one byte past the row's offset. A
\ Wasm body is never placed at a host address, so a site naming a function of
\ its own emission, a quotation's address or a self call, holds that function's
\ offset from byte 0; no host address is that small. The does> patch slot is six
\ bytes: `drop; i32.const 0; nop; nop; nop` (no clause, status 0), or `call` and
\ a padded five-byte function index.
\
\ CONTROL. A Wasm branch names an enclosing block by depth, so every open source
\ structure is one open block of the empty type, an entry in CF-KIND; a
\ branch's depth is the count of entries above its target. A `begin` is a
\ `loop` and a `while` an `if` inside it: `begin A while B repeat` is
\ `loop A <flag> if B br 1 end end`, and `begin A while B while C repeat D then`
\ is `loop A <flag> if B <flag> if C br 2 end D end end`. A back branch makes
\ its LOOP a LOOP-DONE, whose `end` CLOSE-DONE writes once it is innermost.
\ A DO level is a `block` around a `loop`: `leave` and the `?do` entry branch to
\ the block, and LOOP-CLOSE pops the frame after both ends.
\
\ RECORDS. A unit's frame slots are Wasm locals with no address, so its reading
\ quotations' records take cells on the return stack at the unit's entry, and
\ each return gives them back; a throw does not, and catch restores the depth.

require src/lib/error.f
require src/lib/byte-buffer.f
require src/lib/span.f
require src/core/engine-error.f
require src/habu/layout.f
require src/habu/stack-abi.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/arch/wasm/code.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/habu/t0-emit.f

package COMP
package WASM
using WCODE

15 constant NVREG

\ ---- the Wasm bytes ------------------------------------------------------------
8 constant CELL-BYTES

\ Locals: ctx (the unit's parameter), SP (the data stack pointer), AT (a return
\ or loop word's address), ENV (the record a reading quotation was called with),
\ REC (this unit's records), the vregs, CA and CB (the last compare), frame slot k.
0 constant L-CTX
1 constant L-SP
2 constant L-AT
3 constant L-ENV
4 constant L-REC
5 constant L-VREG
20 constant L-CA
21 constant L-CB
22 constant L-SLOT
17 constant FIXED-I64                \ the i64 locals before the frame

\ ---- the sink ------------------------------------------------------------------
create SINK BUF:HDR-BYTES SCRATCH-ALLOT
SCRATCH-VARIABLE SINK-LIVE            \ nonzero once SINK holds storage
variable FRAME-CELLS
variable COUNT-AT                     \ the unit's padded i64 count field
variable REC-CELLS
variable ENV                          \ nonzero once ENV-OPEN ran
variable BODY-AT                      \ HERE-OFF where the unit's body begins
DYNAMIC-BUFFER CF-KIND n              \ the open blocks' kinds, innermost last
DYNAMIC-BUFFER CF-ENTRY n             \ at a DO-HEAD: its `?do` compare's offset, or -1
variable CF-N
variable NUNIT                        \ units begun in this emission
DYNAMIC-BUFFER UNIT-REF n             \ each unit's address field offset

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

\ A stack's depth is an i64 in the DATA cell c native code keeps it in, the
\ return stack's in cells, the loop's in frames.
: DEPTH, ( n -- ) I32,  LD64, ;

: DEPTH+ ( n n -- ) {: c:n k:n :}
   c I32,  c DEPTH,  k I64,  OP-I64-ADD B,  ST64, ;

\ A fault records its kind and the i64 address the caller writes between
\ FAULT-KIND, and FAULT-ADDR,; FAULT-END, then traps, as a stack-bounds fault.
: FAULT-KIND, ( n -- ) {: kind:n :}
   L-CTX GET,  kind I32,  OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
   L-CTX GET, ;

: FAULT-ADDR, ( -- )
   OP-I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM, ;

: FAULT-END, ( -- )  FAULT-ADDR,  OP-UNREACHABLE B, ;

\ The check follows the move's stores, so a failed one has written at most that
\ move's cells past it.
: ROOM ( -- )
   L-SP GET,  WPROF:STACK-END I32,  OP-I32-GT-U B,  OP-IF OPENER,
   ENGINE-ERROR:STACK-BOUNDS FAULT-KIND,  L-SP GET,  OP-I64-EXTEND-I32-U B,
   FAULT-END,  END, ;

\ A failed check throws ENGINE-ERROR:MEMORY-FAULT, the context's top at SP.
: CHECKED ( vreg -- ) {: a:vreg :}
   a GET-REG  WPROF:CTX-BASE I64,  OP-I64-LT-U B,
   a GET-REG  $FFFFFFFF I64,  OP-I64-GT-U B,
   OP-I32-OR B,  OP-IF OPENER,
   WPROF:CRASH-RC FAULT-KIND,  a GET-REG  FAULT-ADDR,
   SYNC  L-CTX GET,  ENGINE-ERROR:MEMORY-FAULT I64,
   OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
   1 I32,  OP-RETURN B,  END, ;

\ A unit is a function (i32 ctx) -> (i32 status): four i32 locals, SP, AT, ENV
\ and REC, then FIXED-I64 + FRAME-CELLS i64 locals, a padded count CLOSE writes.
: UNIT-BEGIN ( -- )
   HERE-OFF EMIT-UNIT+
   NUNIT @ 1 + NUNIT !
   0 FRAME-CELLS !  0 REC-CELLS !  0 ENV !
   2 B,  4 B,  I32 B,
   HERE-OFF COUNT-AT !  FIXED-I64 PAD32,  I64 B,
   SP@,
   HERE-OFF BODY-AT ! ;


\ ---- the body ----------------------------------------------------------------
: OPEN ( -- )
   SINK-LIVE @ 0= if
      SINK SINK-START-BYTES BUF:N>BLEN BUF:INIT
      1 SINK-LIVE !
   then
   SINK BUF:CLEAR
   0 CF-N !
   0 NUNIT !
   UNIT-BEGIN ;

\ CF-N is 0: the unit before closed every structure it opened.
: QUOT-OPEN ( n -- ) {: n:n :}
   n NUNIT @ <> if E-T0-UNIT throw then
   HERE-OFF  SINK BUF:SPAN  n UNIT-REF @  WLEB:S64-PATCH
   UNIT-BEGIN ;

: UNIT-OPEN ( -- ) UNIT-BEGIN ;

: REC-RELEASE ( -- )
   REC-CELLS @ 0= if exit then
   RDEPTH-AT REC-CELLS @ negate DEPTH+ ;

\ Status 0 is a return.
: RETURN ( -- )
   REC-RELEASE  SYNC  0 I32,  OP-RETURN B, ;

\ Every path of a unit leaves by RETURN or TAIL, so the closing `unreachable`
\ makes the fall-through valid whatever precedes it.
: CLOSE ( -- )
   FIXED-I64 FRAME-CELLS @ +  SINK BUF:SPAN  COUNT-AT @  WLEB:U32-PATCH
   OP-UNREACHABLE B,  END, ;


\ The return is the does> patch slot over the context: a clause patched in
\ runs with it, and its status is the unit's.
: SLOT-CLOSE ( -- )
   REC-RELEASE  SYNC  L-CTX GET,
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

\ The site's field holds 0 until QUOT-OPEN writes unit n's offset.
: ADDR-QUOT ( n vreg -- ) {: n:n d:vreg :}
   n 1 + UNIT-REF-RESERVE
   HERE-OFF ADDR-QUOT-KIND n -1 EMIT-ADDR-ROW+
   OP-I64-CONST B,  HERE-OFF n UNIT-REF !  0 PAD64,  d SET-REG ;

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

: COND ( cc -- n )
   CC>N {: k:n :}
   k 0 = if OP-I64-EQ exit then
   k 1 = if OP-I64-NE exit then
   k 2 = if OP-I64-LT-S exit then
   k 3 = if OP-I64-GE-S exit then
   k 4 = if OP-I64-LE-S exit then
   k 5 = if OP-I64-GT-S exit then
   E-T0-OPERAND throw ;

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

\ The compare copies its operands into CA and CB: the moves before its reader
\ may overwrite the vregs, and none of them writes CA or CB.
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

\ `recurse` calls function offset 0, unit 0.
: CALL-SELF ( -- ) 0 0 CALL ;

\ The callee's status is this unit's.
: TAIL ( n n -- ) {: to:n loc:n :}
   NEMIT:TAIL to loc CALL-SITE
   OP-RETURN B, ;

\ ---- control -----------------------------------------------------------------
0 constant CF-IF
1 constant CF-LOOP
2 constant CF-LOOP-DONE
3 constant CF-CASE
4 constant CF-DO
5 constant CF-DO-HEAD

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

\ ---- the return stack ----------------------------------------------------------
\ One task: the stacks are WPROF's fixed regions.

\ AT becomes base plus the depth at c, the depth shifted left by sh.
: TOP, ( n n n -- ) {: c:n base:n sh:n :}
   c DEPTH,  OP-I32-WRAP-I64 B,  sh I32,  OP-I32-SHL B,
   base I32,  OP-I32-ADD B,  L-AT SET, ;

: RTOP ( -- ) RDEPTH-AT WPROF:RETURN-BASE 3 TOP, ;

\ AT moves down k bytes; a memory offset is unsigned.
: AT-, ( n -- ) {: k:n :}
   L-AT GET,  k I32,  OP-I32-SUB B,  L-AT SET, ;

: AT-STORE ( vreg n -- ) {: s:vreg off:n :}
   L-AT GET,  s GET-REG  OP-I64-STORE ALIGN-64 off MEM, ;

: AT-LOAD ( vreg n -- ) {: d:vreg off:n :}
   L-AT GET,  OP-I64-LOAD ALIGN-64 off MEM,  d SET-REG ;

\ Wasm memory has no guard page, so a push checks before its stores.
: BOUND ( n -- ) {: most:n :}
   L-AT GET,  most I32,  OP-I32-GT-U B,  OP-IF OPENER,
   ENGINE-ERROR:STACK-BOUNDS FAULT-KIND,  L-AT GET,  OP-I64-EXTEND-I32-U B,
   FAULT-END,  END, ;

: TOR ( ptr vreg n -- ) {: p:ptr n:n :}
   RTOP
   WPROF:RETURN-BASE STACK-ABI:RETURN-BYTES +  n CELL-BYTES * -  BOUND
   n 0 ?do  p i VREG-AT  i CELL-BYTES * AT-STORE  loop
   RDEPTH-AT n DEPTH+ ;

: RFROM ( ptr vreg n -- ) {: p:ptr n:n :}
   RDEPTH-AT n negate DEPTH+
   RTOP
   n 0 ?do  p i VREG-AT  i CELL-BYTES * AT-LOAD  loop ;

: RFETCH ( ptr vreg n n -- ) {: p:ptr n:n k:n :}
   RTOP  n k + CELL-BYTES * AT-,
   n 0 ?do  p i VREG-AT  i CELL-BYTES * AT-LOAD  loop ;

\ ---- records -------------------------------------------------------------------
: BODY-EMPTY ( -- ) HERE-OFF BODY-AT @ <> if E-T0-UNIT throw then ;

\ TOR's entry without its stores: REC is the first of n cells taken.
: REC-CELLS! ( n -- ) {: n:n :}
   BODY-EMPTY
   n REC-CELLS !
   n 0<> if
      RTOP
      WPROF:RETURN-BASE STACK-ABI:RETURN-BYTES +  n CELL-BYTES * -  BOUND
      L-AT GET,  L-REC SET,
      RDEPTH-AT n DEPTH+
   then
   HERE-OFF BODY-AT ! ;

\ ctx's env holds the record only until the unit's first call.
: ENV-OPEN ( -- )
   BODY-EMPTY
   L-CTX GET,  OP-I32-LOAD ALIGN-32 WPROF:CTX-ENV MEM,  L-ENV SET,
   1 ENV !
   HERE-OFF BODY-AT ! ;

: REC-STORE ( vreg n -- ) {: s:vreg n:n :}
   L-REC GET,  s GET-REG  OP-I64-STORE ALIGN-64 n CELL-BYTES * MEM, ;

\ The record's address in the lower half, $80000000 in the upper.
: REC-QUOT ( n vreg -- ) {: n:n d:vreg :}
   L-REC GET,  OP-I64-EXTEND-I32-U B,
   $8000000000000000 n CELL-BYTES * + I64,  OP-I64-ADD B,  d SET-REG ;

: ENV-LOAD ( vreg n -- ) {: d:vreg n:n :}
   ENV @ 0= if E-T0-UNIT throw then
   L-ENV GET,  OP-I64-LOAD ALIGN-64 n CELL-BYTES * MEM,  d SET-REG ;

\ ---- loops ---------------------------------------------------------------------
0 constant INDEX-OFF
8 constant LIMIT-OFF

: LTOP ( -- ) LDEPTH-AT WPROF:LOOP-BASE 4 TOP, ;

\ AT becomes the frame k levels out of the innermost.
: FRAME ( n -- ) LTOP  1 + STACK-ABI:LOOP-FRAME-BYTES * AT-, ;

: LIMIT, ( -- ) L-AT GET,  OP-I64-LOAD ALIGN-64 LIMIT-OFF MEM, ;

: LOOP-PUSH ( vreg vreg -- ) {: s:vreg l:vreg :}
   LTOP
   WPROF:LOOP-BASE STACK-ABI:LOOP-BYTES +  STACK-ABI:LOOP-FRAME-BYTES -  BOUND
   s INDEX-OFF AT-STORE  l LIMIT-OFF AT-STORE
   LDEPTH-AT 1 DEPTH+ ;

: LOOP-POP ( -- ) LDEPTH-AT -1 DEPTH+ ;
: LOOP-I ( vreg -- ) 0 FRAME  INDEX-OFF AT-LOAD ;
: LOOP-J ( vreg -- ) 1 FRAME  INDEX-OFF AT-LOAD ;

: HEAD-PUSH ( n -- ) {: at:n :}
   CF-N @ {: k:n :}
   k 1 + CF-ENTRY-RESERVE
   at k CF-ENTRY !
   OP-LOOP OPENER,  CF-DO-HEAD CF-PUSH ;

: LOOP-OPEN ( vreg vreg bool -- ) {: s:vreg l:vreg entry:bool :}
   OP-BLOCK OPENER,  CF-DO CF-PUSH
   entry if
      s GET-REG  l GET-REG  HERE-OFF  OP-I64-GE-S B,  0 BR-IF,
   else -1 then
   HEAD-PUSH ;

: LEAVE, ( -- ) CF-DO DEPTH-OF BR, ;

: LOOP-NEXT ( vreg -- ) {: t:vreg :}
   0 FRAME
   t INDEX-OFF AT-LOAD  t 1 OP-I64-ADD LIT-OP  t INDEX-OFF AT-STORE
   t GET-REG  LIMIT,  OP-I64-LT-S B,  CF-DO-HEAD DEPTH-OF BR-IF, ;

\ `+loop` skips equal bounds only: its `?do` compare becomes i64.eq.
: ENTRY-EQ ( -- )
   CF-DO-HEAD CF-FIND CF-ENTRY @ {: at:n :}
   at 0 < if exit then
   OP-I64-EQ  SINK BUF:SPAN$ drop at + c! ;

\ With t = index - limit before the step, the index crossed the boundary when
\ (t xor t + s) and (t xor s) is negative.
: LOOP-STEP ( vreg vreg -- ) {: s:vreg t:vreg :}
   0 FRAME
   t INDEX-OFF AT-LOAD  t GET-REG  LIMIT,  OP-I64-SUB B,  t SET-REG
   L-AT GET,  L-AT GET,  OP-I64-LOAD ALIGN-64 INDEX-OFF MEM,  s GET-REG  OP-I64-ADD B,
   OP-I64-STORE ALIGN-64 INDEX-OFF MEM,
   t GET-REG  t GET-REG  s GET-REG  OP-I64-ADD B,  OP-I64-XOR B,
   t GET-REG  s GET-REG  OP-I64-XOR B,  OP-I64-AND B,
   0 I64,  OP-I64-GE-S B,  CF-DO-HEAD DEPTH-OF BR-IF,
   ENTRY-EQ ;

: LOOP-CLOSE ( -- ) END,  CF-POP  END,  CF-POP  LOOP-POP  CLOSE-DONE ;

\ ---- the seam ----------------------------------------------------------------
: FILL-SLOT ( n -- )
   {: k:n :}
   NVREG k EMIT-VREGS-N !
   ['] OPEN k EMIT-OPEN-XT !
   ['] QUOT-OPEN k EMIT-QUOT-OPEN-XT !
   ['] UNIT-OPEN k EMIT-UNIT-OPEN-XT !
   ['] ADDR-QUOT k EMIT-ADDR-QUOT-XT !
   ['] CALL-SELF k EMIT-CALL-SELF-XT !
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
   ['] REC-CELLS! k EMIT-REC-CELLS-XT !
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
   ['] ENDCASE, k EMIT-ENDCASE,-XT !
   ['] TOR k EMIT-TOR-XT !
   ['] RFROM k EMIT-RFROM-XT !
   ['] RFETCH k EMIT-RFETCH-XT !
   ['] LOOP-PUSH k EMIT-LOOP-PUSH-XT !
   ['] LOOP-POP k EMIT-LOOP-POP-XT !
   ['] LOOP-I k EMIT-LOOP-I-XT !
   ['] LOOP-J k EMIT-LOOP-J-XT !
   ['] LOOP-OPEN k EMIT-LOOP-OPEN-XT !
   ['] LEAVE, k EMIT-LEAVE,-XT !
   ['] LOOP-NEXT k EMIT-LOOP-NEXT-XT !
   ['] LOOP-STEP k EMIT-LOOP-STEP-XT !
   ['] LOOP-CLOSE k EMIT-LOOP-CLOSE-XT ! ;

CTARGET-ARCH:WASM ' FILL-SLOT EMIT-PROVIDE

;using
;package
;package
