\ link-routines.f - the kernel routines a Wasm module carries, each laid when the
\ program reaches its word and shaped as tier 0's code is (src/habu/t0-wasm.f).
require src/lib/string.f
require src/lib/span.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/lib/error.f

package LINKER
private

package WASM
private

\ The exit status a checked-memory fault records, src/habu/t0-wasm.f CRASH-RC.
134 constant CRASH-RC

$00 constant OP-UNREACHABLE
$04 constant OP-IF
$0B constant OP-END
$20 constant OP-LOCAL-GET
$21 constant OP-LOCAL-SET
$28 constant OP-I32-LOAD
$29 constant OP-I64-LOAD
$36 constant OP-I32-STORE
$37 constant OP-I64-STORE
$3C constant OP-I64-STORE8
$41 constant OP-I32-CONST
$42 constant OP-I64-CONST
$4F constant OP-I32-GE-U
$54 constant OP-I64-LT-U
$56 constant OP-I64-GT-U
$6A constant OP-I32-ADD
$6B constant OP-I32-SUB
$72 constant OP-I32-OR
$A7 constant OP-I32-WRAP-I64
$7F constant I32
$7E constant I64
$40 constant EMPTY-TYPE              \ an if taking and leaving nothing
0 constant ALIGN-8                   \ a load's or store's alignment exponent
2 constant ALIGN-32
3 constant ALIGN-64
8 constant CELL-BYTES
10 constant LEB-MOST

\ The locals: ctx, the parameter; the stack address the routine works at; emit's
\ OUT length or a store's address.
0 constant L-CTX
1 constant L-SP
2 constant L-LEN
2 constant L-ADDR

\ The OUT region ends where the stack starts (test/wasm/harness.f OUT-CAP).
WPROF:STACK-BASE WPROF:OUT-BASE - constant OUT-CAP

\ ---- division, views and atomics
$0F constant OP-RETURN
$50 constant OP-I64-EQZ
$51 constant OP-I64-EQ
$7D constant OP-I64-SUB
$7F constant OP-I64-DIV-S
$81 constant OP-I64-REM-S
\ The i64 locals after a store's address: a division's divisor, atomic-cas's
\ cell as read.
2 constant L-DIVISOR
3 constant L-ACTUAL

\ ---- floats
$1B constant OP-SELECT
$2B constant OP-F64-LOAD
$39 constant OP-F64-STORE
$44 constant OP-F64-CONST
$61 constant OP-F64-EQ
$62 constant OP-F64-NE
$63 constant OP-F64-LT
$64 constant OP-F64-GT
$99 constant OP-F64-ABS
$9A constant OP-F64-NEG
$9F constant OP-F64-SQRT
$A0 constant OP-F64-ADD
$A1 constant OP-F64-SUB
$A2 constant OP-F64-MUL
$A3 constant OP-F64-DIV
$AD constant OP-I64-EXTEND-I32-U
$B9 constant OP-F64-CONVERT-I64-S
$FC constant OP-MISC                 \ the prefix of the saturating truncations
6 constant MISC-I64-TRUNC-SAT-F64-S
$7C constant F64

\ A float body's locals after sp: the doubles a and b and the answer r.
2 constant L-A
3 constant L-B
4 constant L-R

\ The NaN every float operation that makes one answers, src/arch/wasm/select.f
\ NAN-MADE.
$7FF8000000000000 constant NAN-MADE

\ ---- the body being written ----------------------------------------------------
DYNAMIC-BUFFER BUF u8
variable BUF-U

: B, ( n -- )
   {: v:n :}
   BUF-U @ 1+ BUF-RESERVE
   v BUF-U @ BUF c!
   1 BUF-U +! ;

: ROOM ( -- SPAN:span<u8> )
   BUF-U @ LEB-MOST + BUF-RESERVE
   BUF-U @ BUF LEB-MOST SPAN:MAKE ;

: U32, ( n -- )  ROOM WLEB:U32! BUF-U +! ;
: S32, ( n -- )  ROOM WLEB:S32! BUF-U +! ;
: S64, ( n -- )  ROOM WLEB:S64! BUF-U +! ;
: GET, ( n -- )  OP-LOCAL-GET B, U32, ;
: SET, ( n -- )  OP-LOCAL-SET B, U32, ;
: I32, ( n -- )  OP-I32-CONST B, S32, ;
: I64, ( n -- )  OP-I64-CONST B, S64, ;
: IF, ( -- )  OP-IF B, EMPTY-TYPE B, ;

\ A load or a store: the opcode, its alignment's exponent, its memory offset.
: MEM, ( n n n -- )
   {: op:n al:n off:n :}
   op B,  al U32,  off U32, ;

: CTX@, ( n -- )
   {: off:n :}
   L-CTX GET,  OP-I32-LOAD ALIGN-32 off MEM, ;

\ The context's top becomes SP: the caller reloads its stack pointer from it.
: SP!, ( -- )
   L-CTX GET,  L-SP GET,  OP-I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM, ;


\ emit ( c -- ) stores c's low byte at OUT-BASE plus ctx.out-len and counts it;
\ a full OUT traps, as tier 1's (src/arch/wasm/kernel.f EMIT-ROW).
: EMIT-BODY ( -- )
   1 U32,  2 U32,  I32 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  OP-I32-SUB B,  L-SP SET,
   WPROF:CTX-OUT-LEN CTX@,  L-LEN SET,
   L-LEN GET,  OUT-CAP I32,  OP-I32-GE-U B,  IF,  OP-UNREACHABLE B,  OP-END B,
   WPROF:OUT-BASE I32,  L-LEN GET,  OP-I32-ADD B,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,
   OP-I64-STORE8 ALIGN-8 0 MEM,
   L-CTX GET,  L-LEN GET,  1 I32,  OP-I32-ADD B,
   OP-I32-STORE ALIGN-32 WPROF:CTX-OUT-LEN MEM,
   SP!,
   0 I32,  OP-END B, ;


\ The address in L-ADDR is at or past WPROF:CTX-BASE and below 2^32, else the
\ fault records CRASH-RC with it, as src/habu/t0-wasm.f CHECKED.
: ADDRESS-CHECK, ( -- )
   L-ADDR GET,  WPROF:CTX-BASE I64,  OP-I64-LT-U B,
   L-ADDR GET,  $FFFFFFFF I64,  OP-I64-GT-U B,
   OP-I32-OR B,  IF,
      L-CTX GET,  CRASH-RC I32,  OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
      L-CTX GET,  L-ADDR GET,  OP-I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM,
      OP-UNREACHABLE B,
   OP-END B, ;

\ A store ( x a -- ) by opcode op of alignment al, its address checked.
: STORE-BODY ( n n -- )
   {: op:n al:n :}
   2 U32,  1 U32,  I32 B,  1 U32,  I64 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES 2 * I32,  OP-I32-SUB B,  L-SP SET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES MEM,  L-ADDR SET,
   ADDRESS-CHECK,
   L-ADDR GET,  OP-I32-WRAP-I64 B,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,
   op al 0 MEM,
   SP!,
   0 I32,  OP-END B, ;


\ ---- division, views and atomics
\ The cell stored at SP is the routine's one result: the top moves past it.
: ONE-RESULT, ( -- )
   L-SP GET,  CELL-BYTES I32,  OP-I32-ADD B,  L-SP SET,  SP!, ;


\ `/` or `mod` by op, as tier 1's SEL-DIV: a zero divisor throws E-DIV-ZERO, the
\ stack kept. With neg1 a divisor of -1 answers 0 - a: div_s traps on MIN-N / -1.
: DIVIDE-BODY ( n bool -- )
   {: op:n neg1:bool :}
   2 U32,  1 U32,  I32 B,  1 U32,  I64 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES 2 * I32,  OP-I32-SUB B,  L-SP SET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES MEM,  L-DIVISOR SET,
   L-DIVISOR GET,  OP-I64-EQZ B,  IF,
      L-CTX GET,  E-DIV-ZERO I64,  OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
      1 I32,  OP-RETURN B,
   OP-END B,
   neg1 if
      L-DIVISOR GET,  -1 I64,  OP-I64-EQ B,  IF,
         L-SP GET,  0 I64,  L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  OP-I64-SUB B,
         OP-I64-STORE ALIGN-64 0 MEM,
         ONE-RESULT,  0 I32,  OP-RETURN B,
      OP-END B,
   then
   L-SP GET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  L-DIVISOR GET,  op B,
   OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,
   0 I32,  OP-END B, ;

\ `cell-view` and `byte-view` ( a -- a ) leave the address as it is.
: VIEW-BODY ( -- )  0 U32,  0 I32,  OP-END B, ;


\ atomic-cas ( expected new a -- actual ), its address checked as a store's; a
\ module runs one task, so the plain compare and store is atomic.
: CAS-BODY ( -- )
   2 U32,  1 U32,  I32 B,  2 U32,  I64 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES 3 * I32,  OP-I32-SUB B,  L-SP SET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES 2 * MEM,  L-ADDR SET,
   ADDRESS-CHECK,
   L-ADDR GET,  OP-I32-WRAP-I64 B,  OP-I64-LOAD ALIGN-64 0 MEM,  L-ACTUAL SET,
   L-ACTUAL GET,  L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  OP-I64-EQ B,  IF,
      L-ADDR GET,  OP-I32-WRAP-I64 B,
      L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES MEM,
      OP-I64-STORE ALIGN-64 0 MEM,
   OP-END B,
   L-SP GET,  L-ACTUAL GET,  OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,
   0 I32,  OP-END B, ;


\ ---- floats
\ A real is one stack cell holding the double's bits, which f64.load and
\ f64.store move unchanged.
: FLOAT-LOCALS, ( -- )  2 U32,  1 U32,  I32 B,  3 U32,  F64 B, ;
: DONE, ( -- )  0 I32,  OP-END B, ;

: SP-AT, ( n -- )
   {: back:n :}
   WPROF:CTX-STACK-TOP CTX@,  back I32,  OP-I32-SUB B,  L-SP SET, ;

: F@, ( n -- )
   {: off:n :}
   L-SP GET,  OP-F64-LOAD ALIGN-64 off MEM, ;

\ Its eight bytes follow the opcode low first.
: F64, ( n -- )
   {: v:n :}
   OP-F64-CONST B,  8 0 do  v i 8 * rshift $FF and B,  loop ;

\ Over x and y on the Wasm stack: x when the double in local c is a NaN, else y.
: IF-NAN, ( n -- )
   {: c:n :}
   c GET,  c GET,  OP-F64-NE B,  OP-SELECT B, ;

\ select.f THE FLOATS' rule without a branch: r becomes NAN-MADE when it is a
\ NaN, then the operand in local c when that is one.
: CANON, ( -- )  NAN-MADE F64,  L-R GET,  L-R IF-NAN,  L-R SET, ;
: PASS, ( n -- )  {: c:n :}  c GET,  L-R GET,  c IF-NAN,  L-R SET, ;

\ ( a b -- r ) by the f64 opcode op; of two NaN operands a is the answer.
: FARITH-BODY ( n -- )
   {: op:n :}
   FLOAT-LOCALS,  CELL-BYTES 2 * SP-AT,
   0 F@,  L-A SET,  CELL-BYTES F@,  L-B SET,
   L-A GET,  L-B GET,  op B,  L-R SET,
   CANON,  L-B PASS,  L-A PASS,
   L-SP GET,  L-R GET,  OP-F64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,  DONE, ;

: FSQRT-BODY ( -- )
   FLOAT-LOCALS,  CELL-BYTES SP-AT,
   0 F@,  L-A SET,
   L-A GET,  OP-F64-SQRT B,  L-R SET,
   CANON,  L-A PASS,
   L-SP GET,  L-R GET,  OP-F64-STORE ALIGN-64 0 MEM,
   DONE, ;

\ The top cell, loaded by ld, turned by what the caller writes next, stored back
\ by st: the top does not move.
: IN-PLACE, ( n -- )
   {: ld:n :}
   FLOAT-LOCALS,  CELL-BYTES SP-AT,
   L-SP GET,  L-SP GET,  ld ALIGN-64 0 MEM, ;

: ;IN-PLACE, ( n -- )  ALIGN-64 0 MEM,  DONE, ;

: FSIGN-BODY ( n -- )  OP-F64-LOAD IN-PLACE,  B,  OP-F64-STORE ;IN-PLACE, ;
: S>F-BODY ( -- )  OP-I64-LOAD IN-PLACE,  OP-F64-CONVERT-I64-S B,  OP-F64-STORE ;IN-PLACE, ;

\ The truncation saturates and answers 0 for a NaN, as the engine BF>S's FCVTZS.
: F>S-BODY ( -- )
   OP-F64-LOAD IN-PLACE,
   OP-MISC B,  MISC-I64-TRUNC-SAT-F64-S U32,
   OP-I64-STORE ;IN-PLACE, ;

\ The i32 predicate on the Wasm stack, over the i64 0 pushed before it, as a
\ flag: 0 - extend_u(p), all ones for true.
: FLAG, ( -- )  OP-I64-EXTEND-I32-U B,  OP-I64-SUB B, ;

\ ( a b -- flag ) by the f64 compare op.
: FCOMPARE-BODY ( n -- )
   {: op:n :}
   FLOAT-LOCALS,  CELL-BYTES 2 * SP-AT,
   L-SP GET,  0 I64,  0 F@,  CELL-BYTES F@,  op B,  FLAG,
   OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,  DONE, ;

\ ( a -- flag ) by op against +0.0, whose bits are zero: -0.0 equals it and is
\ not below it.
: FCOMPARE0-BODY ( n -- )
   {: op:n :}
   FLOAT-LOCALS,  CELL-BYTES SP-AT,
   L-SP GET,  0 I64,  0 F@,  0 F64,  op B,  FLAG,
   OP-I64-STORE ALIGN-64 0 MEM,
   DONE, ;

: BODY$ ( -- ptr u8 n )  0 BUF BUF-U @ ;

public

\ The named kernel word's routine body, as WLINK:FUNCTION+ takes one and valid
\ until the next call, and true; an empty span and false when it has none.
: ROUTINE-BODY ( ptr u8 n -- ptr u8 n bool )
   {: name:ptr u:n :}
   0 BUF-U !
   name u s" emit" STR=CI if EMIT-BODY BODY$ true exit then
   name u s" !" STR=CI if OP-I64-STORE ALIGN-64 STORE-BODY BODY$ true exit then
   name u s" c!" STR=CI if OP-I64-STORE8 ALIGN-8 STORE-BODY BODY$ true exit then
   \ ---- division, views and atomics
   name u s" /" STR=CI if OP-I64-DIV-S true DIVIDE-BODY BODY$ true exit then
   name u s" mod" STR=CI if OP-I64-REM-S false DIVIDE-BODY BODY$ true exit then
   name u s" cell-view" STR=CI if VIEW-BODY BODY$ true exit then
   name u s" byte-view" STR=CI if VIEW-BODY BODY$ true exit then
   name u s" atomic!" STR=CI if OP-I64-STORE ALIGN-64 STORE-BODY BODY$ true exit then
   name u s" atomic-cas" STR=CI if CAS-BODY BODY$ true exit then
   \ ---- floats
   name u s" f+" STR=CI if OP-F64-ADD FARITH-BODY BODY$ true exit then
   name u s" f-" STR=CI if OP-F64-SUB FARITH-BODY BODY$ true exit then
   name u s" f*" STR=CI if OP-F64-MUL FARITH-BODY BODY$ true exit then
   name u s" f/" STR=CI if OP-F64-DIV FARITH-BODY BODY$ true exit then
   name u s" fsqrt" STR=CI if FSQRT-BODY BODY$ true exit then
   name u s" fnegate" STR=CI if OP-F64-NEG FSIGN-BODY BODY$ true exit then
   name u s" fabs" STR=CI if OP-F64-ABS FSIGN-BODY BODY$ true exit then
   name u s" f<" STR=CI if OP-F64-LT FCOMPARE-BODY BODY$ true exit then
   name u s" f>" STR=CI if OP-F64-GT FCOMPARE-BODY BODY$ true exit then
   name u s" f=" STR=CI if OP-F64-EQ FCOMPARE-BODY BODY$ true exit then
   name u s" f0<" STR=CI if OP-F64-LT FCOMPARE0-BODY BODY$ true exit then
   name u s" f0=" STR=CI if OP-F64-EQ FCOMPARE0-BODY BODY$ true exit then
   name u s" s>f" STR=CI if S>F-BODY BODY$ true exit then
   name u s" f>s" STR=CI if F>S-BODY BODY$ true exit then
   s" " false ;

;package
;package
