\ dynamic.f - WDYN, the Wasm backend's dynamic calls: ADAPTER+ puts a function
\ of the link in a table slot, and the runtime execute, catch and finally call
\ through one (docs/xcomp.md "Calls", "Execution tokens"). An xt is a table
\ slot. Each body is Wasm written through WENC's opcode bytes, as WLINK writes
\ its wrappers: none is selected from HIR, and WENC refuses call_indirect.

require src/lib/prelude.f
require src/lib/span.f
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
\ the status a call answered; then a lane function's outputs, or finally's
\ cleanup xt and its body's code.
0 constant L-CTX
1 constant L-AT
2 constant L-STATUS
3 constant L-OUT
3 constant L-CLEANUP
4 constant L-CODE

\ ---- the body being written ----------------------------------------------------
DYNAMIC-BUFFER BODY u8
variable BODY-U
DYNAMIC-BUFFER SITES n               \ where each call_indirect's type index starts
variable SITES-N

: B, ( n -- )
   {: v:n :}
   BODY-U @ 1+ BODY-RESERVE
   v BODY-U @ BODY c!
   1 BODY-U +! ;

: SITE+ ( n -- )
   SITES-N @ 1+ SITES-RESERVE
   SITES-N @ SITES !
   1 SITES-N +! ;

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

: CTX@, ( n -- )
   {: off:n :}
   L-CTX GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 off MEM, ;

: AT+, ( n -- )
   L-AT GET,  I32,  WSTRUCT-OPCODE:I32-ADD OP, ;

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
\ An adapter has type (i32 ctx) -> (i32 status) and moves the Habu row on the
\ context stack, the aligned frame's convention (src/arch/wasm/select.f THE
\ CALL), so every slot has one Wasm type. Wasm's signature check cannot see the
\ row, so the adapter checks the depth first: fewer cells than the inputs is
\ fatal, as a read below the native stack's base faults. A lane function gets
\ its inputs as lanes with the stack top lowered past them, as a direct call
\ leaves it; status 1 returns at once with the stack where the throw left it;
\ status 0 stores the outputs at the top as the function left it, after the room
\ check from there, so a row the function grew or shrank below them stays as it
\ left it. A framed function reads and writes the stack itself.
: DEPTH, ( n -- )
   {: in:n :}
   WPROF:CTX-STACK-TOP CTX@,  in CELL-BYTES * I32,  WSTRUCT-OPCODE:I32-SUB OP,
   L-AT SET,
   in 0= if exit then
   L-AT GET,  WPROF:CTX-STACK-BASE CTX@,  OP-I32-LT-U B,
   IF,  0 FAULT,  OP-END B, ;

: ROOM, ( n -- )
   {: out:n :}
   out CELL-BYTES * AT+,  WPROF:STACK-END I32,  OP-I32-GT-U B,
   IF,  out CELL-BYTES * FAULT,  OP-END B, ;

\ Answers where the call's padded index starts.
: CALL, ( -- n )
   WSTRUCT-OPCODE:CALL OP,  PAD32, ;

: FRAMED, ( n -- n )
   {: in:n :}
   1 0 LOCALS,
   in DEPTH,
   L-CTX GET,  CALL,
   OP-END B, ;

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
   out 0 > if
      WPROF:CTX-STACK-TOP CTX@,  L-AT SET,
      out ROOM,
      out 0 ?do
         L-AT GET,  L-OUT i + GET,  WSTRUCT-OPCODE:I64-STORE ALIGN-64 i CELL-BYTES * MEM,
      loop
      out CELL-BYTES * TOP!,
   then
   0 I32,
   OP-END B,
   at ;

\ ---- the runtime functions ----------------------------------------------------
\ The selector calls execute, catch and finally in the frame with the xts on top
\ (src/arch/wasm/select.f THE DYNAMIC CALL). call_indirect's type field WLINK
\ writes as (i32) -> (i32) (INDIRECT+); it traps on slot 0, which holds no
\ function, and on a slot past the table. execute answers the adapter's status.
\ catch answers 0: it restores the depth it began with less the xt and pushes
\ the code, ctx.throw-code (all 64 bits) after a status 1 and zero after a 0.
\ finally ( body cleanup -- ) pops the cleanup xt and runs the body as execute
\ does; after a status 1 it keeps ctx.throw-code and restores the depth finally
\ began with less both xts. It then runs the cleanup, after a room check for
\ its xt. A cleanup's status 1 is answered with the cleanup's code, which
\ supersedes the body's; otherwise a body's status 1 is answered with the body's
\ code stored again, since a catch inside the cleanup may have overwritten it;
\ otherwise 0, the body's results left as it left them. A cleanup xt of 0 traps
\ before the body runs, as native finally refuses it first. A trap passes
\ through all three, and restoring the depth restores no cell a throw
\ overwrote (docs/xcomp.md "Exceptions and fatal failures").
\
\ DISPATCH, records where call_indirect's padded type index starts. An xt whose
\ upper half is not zero traps before its low half is read as the slot.
: DISPATCH, ( -- )
   L-CTX GET,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  WSTRUCT-OPCODE:I32-SUB OP,  L-AT TEE,
   WSTRUCT-OPCODE:I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM,
   L-AT GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 HIGH-HALF MEM,
   IF,  WSTRUCT-OPCODE:UNREACHABLE OP,  OP-END B,
   L-CTX GET,
   L-AT GET,  WSTRUCT-OPCODE:I32-LOAD ALIGN-32 0 MEM,
   WSTRUCT-OPCODE:CALL-INDIRECT OP,  PAD32, SITE+  TABLE0 B, ;

: EXECUTE-BODY ( -- )
   1 0 LOCALS,
   DISPATCH,
   OP-END B, ;

: CATCH-BODY ( -- )
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

: FINALLY-BODY ( -- )
   2 2 LOCALS,
   L-CTX GET,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  WSTRUCT-OPCODE:I32-SUB OP,  L-AT TEE,
   WSTRUCT-OPCODE:I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM,
   L-AT GET,  WSTRUCT-OPCODE:I64-LOAD ALIGN-64 0 MEM,  L-CLEANUP TEE,
   WSTRUCT-OPCODE:I64-EQZ OP,
   IF,  WSTRUCT-OPCODE:UNREACHABLE OP,  OP-END B,
   DISPATCH,  L-STATUS SET,
   L-CTX GET,  WSTRUCT-OPCODE:I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE MEM,  L-CODE SET,
   L-STATUS GET,
   IF,  0 TOP!,  OP-END B,
   WPROF:CTX-STACK-TOP CTX@,  L-AT SET,
   1 ROOM,
   L-AT GET,  L-CLEANUP GET,  WSTRUCT-OPCODE:I64-STORE ALIGN-64 0 MEM,
   CELL-BYTES TOP!,
   DISPATCH,
   IF,  1 I32,  WSTRUCT-OPCODE:RETURN OP,  OP-END B,
   L-STATUS GET,
   IF,
      L-CTX GET,  L-CODE GET,
      WSTRUCT-OPCODE:I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
   OP-END B,
   L-STATUS GET,
   OP-END B, ;

\ The body the word writes, a kernel function of the link with its indirect
\ sites; answers its handle.
: RUNTIME ( [ -- ] -- n )
   {: body :}
   0 BODY-U !  0 SITES-N !
   body execute
   0 BODY BODY-U @  0 0 0 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ {: k:n :}
   SITES-N @ 0 ?do  k i SITES @ WLINK:INDIRECT+  loop
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

\ The runtime execute, catch and finally, as kernel functions of the link; each
\ answers its handle.
: EXECUTE+ ( -- n )  [: EXECUTE-BODY ;] RUNTIME ;
: CATCH+ ( -- n )  [: CATCH-BODY ;] RUNTIME ;
: FINALLY+ ( -- n )  [: FINALLY-BODY ;] RUNTIME ;

;package
