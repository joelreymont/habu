\ dynamic.f - WDYN, the Wasm backend's dynamic calls: the adapter that puts a
\ function of the link in a table slot, and the runtime functions execute and
\ catch that call through one (docs/wasm-backend.md 7.2, 8.1, 9).
\
\ AN XT IS A TABLE SLOT. ADAPTER+ gives a function of the link an adapter of
\ type (i32 ctx) -> (i32 status), puts it in the table's next slot and answers
\ the slot: the descriptor a quotation's or a code literal's address becomes.
\ The adapter takes the function's inputs off the context stack and leaves its
\ outputs there, the aligned frame's convention (src/arch/wasm/select.f THE
\ CALL), so every slot has one Wasm type and the stack carries the Habu row.
\ Wasm's signature check cannot see that row, so the adapter checks the depth
\ first: a stack holding fewer cells than the function's inputs is fatal, as a
\ read below the native stack's base faults, and records STACK-BOUNDS with the
\ address the inputs would start at, then executes unreachable. A function in
\ its lanes is called with its inputs as lanes, the stack top lowered past
\ them as a direct call leaves it; a status 1 returns at once, the stack where
\ the throw left it, and a status 0 stores the outputs where the inputs stood,
\ after the room check every store to the stack makes when they outnumber the
\ inputs. A function in the frame reads and writes the stack itself.
\
\ EXECUTE AND CATCH. The selector calls the engine's execute and catch in the
\ frame with the xt on top (src/arch/wasm/select.f THE DYNAMIC CALL); in a
\ module those calls reach these runtime functions. Each pops the xt and calls
\ its slot through call_indirect, whose type field WLINK writes as the type
\ (i32) -> (i32) (INDIRECT+). An xt is a slot, so one whose upper 32 bits are
\ set traps before the call, and call_indirect traps on slot 0, which holds no
\ function, and on a slot past the table. execute answers the adapter's
\ status. catch answers 0: it restores the depth it began with less the xt,
\ and pushes the code, the full 64-bit ctx.throw-code after a status 1 and zero
\ after a 0. A trap passes through both (section 8.2), and restoring the depth
\ restores no cell a throw overwrote (8.1).
\
\ Each body is Wasm written here through WENC's opcode bytes, as WLINK writes
\ its wrappers: none is selected from HIR, and WENC refuses call_indirect.

require lib/prelude.f
require lib/span.f
require src/core/engine-error.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/select.f
require src/arch/wasm/encode.f
require src/arch/wasm/link.f

package WDYN
private

\ ---- the Wasm bytes beside WENC's opcode table -------------------------------
$04 constant OP-IF
$0B constant OP-END
$1B constant OP-SELECT               \ select, of two i64s here
$20 constant OP-LOCAL-GET
$21 constant OP-LOCAL-SET
$22 constant OP-LOCAL-TEE
$40 constant EMPTY-TYPE              \ an if taking and leaving nothing
$49 constant OP-I32-LT-U
$4B constant OP-I32-GT-U
$7F constant I32
$7E constant I64
0 constant TABLE0                    \ call_indirect's table, the module's one
2 constant ALIGN-32
3 constant ALIGN-64
8 constant CELL-BYTES
4 constant HIGH-HALF                 \ a cell's upper 32 bits, little-endian
10 constant LEB-MOST

\ The locals: ctx, every body's parameter; the stack address a body works at;
\ the status a lane function answered; then that function's outputs.
0 constant L-CTX
1 constant L-AT
2 constant L-STATUS
3 constant L-OUT

\ ---- the body being written ----------------------------------------------------
DYNAMIC-BUFFER BODY u8
variable BODY-U

: B, ( n -- )
   {: v:n :}
   BODY-U @ 1+ BODY-RESERVE
   v BODY-U @ BODY c!
   1 BODY-U +! ;

: ROOM ( -- SPAN:span<u8> )
   BODY-U @ LEB-MOST + BODY-RESERVE
   BODY-U @ BODY LEB-MOST SPAN:MAKE ;

: U32, ( n -- )  ROOM WLEB:U32! BODY-U +! ;
: S32, ( n -- )  ROOM WLEB:S32! BODY-U +! ;

\ A padded zero for WLINK to rewrite; answers where it starts.
: PAD32, ( -- n )
   BODY-U @  0 ROOM WLEB:U32-PAD! BODY-U +! ;

: OP, ( WSTRUCT:opcode -- )  WENC:OPCODE-BYTE B, ;
: GET, ( n -- )  OP-LOCAL-GET B, U32, ;
: SET, ( n -- )  OP-LOCAL-SET B, U32, ;
: TEE, ( n -- )  OP-LOCAL-TEE B, U32, ;
: I32, ( n -- )  WSTRUCT-OPCODE:I32-CONST OP, S32, ;
: IF, ( -- )  OP-IF B, EMPTY-TYPE B, ;

\ A load or a store: the opcode, its alignment's exponent, its offset.
: MEM, ( WSTRUCT:opcode n n -- )
   {: o:WSTRUCT:opcode al:n off:n :}
   o OP,  al U32,  off U32, ;

\ The local declarations: n32 i32s, then n64 i64s.
: LOCALS, ( n n -- )
   {: n32:n n64:n :}
   n64 0 > if 2 else 1 then U32,
   n32 U32,  I32 B,
   n64 0 > if  n64 U32,  I64 B,  then ;

\ ctx's i32 field at off.
: CTX@, ( n -- )
   {: off:n :}
   L-CTX GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 off MEM, ;

\ AT plus n bytes.
: AT+, ( n -- )
   L-AT GET,  I32,  WSTRUCT-OPCODE:I32-ADD OP, ;

\ ctx.stack-top := AT + n bytes.
: TOP!, ( n -- )
   {: n:n :}
   L-CTX GET,  n AT+,
   WSTRUCT-OPCODE:I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM, ;

\ The fault of a stack access outside the region, at AT + n bytes: the exit
\ native's guard page raises, STACK-BOUNDS, and that address; then
\ unreachable.
: FAULT, ( n -- )
   {: n:n :}
   L-CTX GET,  ENGINE-ERROR:STACK-BOUNDS I32,
   WSTRUCT-OPCODE:I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
   L-CTX GET,  n AT+,  WSTRUCT-OPCODE:I64-EXTEND-I32-U OP,
   WSTRUCT-OPCODE:I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM,
   WSTRUCT-OPCODE:UNREACHABLE OP, ;

\ ---- the adapter --------------------------------------------------------------
\ AT := the stack top less in cells, where the inputs start, which must not
\ lie below the stack's base.
: DEPTH, ( n -- )
   {: in:n :}
   WPROF:CTX-STACK-TOP CTX@,  in CELL-BYTES * I32,  WSTRUCT-OPCODE:I32-SUB OP,
   L-AT SET,
   in 0= if exit then
   L-AT GET,  WPROF:CTX-STACK-BASE CTX@,  OP-I32-LT-U B,
   IF,  0 FAULT,  OP-END B, ;

\ The outputs must end inside the stack region.
: ROOM, ( n -- )
   {: out:n :}
   out CELL-BYTES * AT+,  WPROF:DATA-BASE I32,  OP-I32-GT-U B,
   IF,  out CELL-BYTES * FAULT,  OP-END B, ;

\ A call of the function, answering where its padded index starts.
: CALL, ( -- n )
   WSTRUCT-OPCODE:CALL OP,  PAD32, ;

\ A function in the aligned frame takes the stack as it stands.
: FRAMED, ( n -- n )
   {: in:n :}
   1 0 LOCALS,
   in DEPTH,
   L-CTX GET,  CALL,
   OP-END B, ;

\ A function in its lanes: the inputs as lanes, the outputs back on the stack.
: LANES, ( n n -- n )
   {: in:n out:n :}
   2 out LOCALS,
   in DEPTH,
   0 TOP!,
   L-CTX GET,
   in 0 ?do  L-AT GET,  WSTRUCT-OPCODE:I64-LOAD ALIGN-64 i CELL-BYTES * MEM,  loop
   CALL, {: at:n :}
   out 0 ?do  L-OUT out + 1- i - SET,  loop
   L-STATUS TEE,
   IF,  L-STATUS GET,  WSTRUCT-OPCODE:RETURN OP,  OP-END B,
   out in > if  out ROOM,  then
   out 0 ?do
      L-AT GET,  L-OUT i + GET,  WSTRUCT-OPCODE:I64-STORE ALIGN-64 i CELL-BYTES * MEM,
   loop
   out CELL-BYTES * TOP!,
   0 I32,
   OP-END B,
   at ;

\ ---- the runtime functions ----------------------------------------------------
\ Pop the xt, AT its cell, and call its slot; answers where call_indirect's
\ padded type index starts. An xt whose upper half is not zero traps before
\ its low half is read as the slot.
: DISPATCH, ( -- n )
   L-CTX GET,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  WSTRUCT-OPCODE:I32-SUB OP,  L-AT TEE,
   WSTRUCT-OPCODE:I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM,
   L-AT GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 HIGH-HALF MEM,
   IF,  WSTRUCT-OPCODE:UNREACHABLE OP,  OP-END B,
   L-CTX GET,
   L-AT GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 0 MEM,
   WSTRUCT-OPCODE:CALL-INDIRECT OP,  PAD32,  TABLE0 B, ;

: EXECUTE-BODY ( -- n )
   1 0 LOCALS,
   DISPATCH,
   OP-END B, ;

\ The code at AT, ctx.throw-code when the status is nonzero and else zero,
\ and the top just past it.
: CATCH-BODY ( -- n )
   2 0 LOCALS,
   DISPATCH,
   L-STATUS SET,
   L-AT GET,
   L-CTX GET,  WSTRUCT-OPCODE:I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE MEM,
   WSTRUCT-OPCODE:I64-CONST OP,  0 B,          \ zero, its SLEB one byte
   L-STATUS GET,  OP-SELECT B,
   WSTRUCT-OPCODE:I64-STORE ALIGN-64 0 MEM,
   CELL-BYTES TOP!,
   0 I32,
   OP-END B, ;

\ The body the word writes, a kernel function of the link with its indirect
\ site; answers its handle.
: RUNTIME ( [ -- n ] -- n )
   {: body :}
   0 BODY-U !
   body execute {: at:n :}
   0 BODY BODY-U @  0 0 0 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ {: k:n :}
   k at WLINK:INDIRECT+
   k ;

public

\ Function f of the link, taking in cells and leaving out, gets an adapter in
\ the table's next slot; answers the slot, the descriptor of an xt of f.
: ADAPTER+ ( n n n -- n )
   {: f:n in:n out:n :}
   0 BODY-U !
   in out WSEL:FRAMED? if  in FRAMED,  else  in out LANES,  then {: at:n :}
   0 BODY BODY-U @  0 0 0 WLINK-ORIGIN:ADAPTER WLINK:FUNCTION+ {: a:n :}
   a at f WLINK:CALL+
   a WLINK:TABLE+ ;

\ The runtime execute and catch, as kernel functions of the link; each answers
\ its handle.
: EXECUTE+ ( -- n )  [: EXECUTE-BODY ;] RUNTIME ;
: CATCH+ ( -- n )  [: CATCH-BODY ;] RUNTIME ;

;package
