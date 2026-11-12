\ link-routines.f - the kernel routines a Wasm module carries, each laid when the
\ program reaches its word and shaped as tier 0's code is (src/habu/t0-wasm.f).
require src/lib/string.f
require src/lib/span.f
require src/core/engine-error.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/arch/wasm/code.f
require src/habu/layout.f
require src/lib/error.f

package LINKER
private

package WASM
private
using WCODE

8 constant CELL-BYTES
3 constant CELL-SHIFT                \ log2 of CELL-BYTES
10 constant LEB-MOST

\ The locals: ctx, the parameter; the stack address the routine works at; emit's
\ OUT length or a store's address.
0 constant L-CTX
1 constant L-SP
2 constant L-LEN
2 constant L-ADDR

\ ---- division, views and atomics
\ Local 2 of a division is its divisor; local 3 of atomic-cas is the cell as read.
2 constant L-DIVISOR
3 constant L-ACTUAL

\ ---- floats
\ A float body's locals after sp: the doubles a and b and the answer r.
2 constant L-A
3 constant L-B
4 constant L-R

\ The NaN every float operation that makes one answers.
$7FF8000000000000 constant NAN-MADE

\ ---- map-anon and munmap
0 constant MEMORY-0

\ log2 of WPROF:PAGE-BYTES.
16 constant PAGE-SHIFT
\ log2 of memory32's 2^32 bytes: a memory offset has no bit at or above it.
32 constant OFFSET-BITS

\ A free run's node is its first page: the next run's address, 0 for none, and
\ its page count, each an i32. ctx's free head names the first, by address.
0 constant NODE-NEXT
8 constant NODE-PAGES

\ The allocator's locals after sp, seven i32 then four i64; the link is the
\ address of the cell naming cur, a free run's node.
2 constant L-PAGES
3 constant L-LINK
4 constant L-CUR
5 constant L-NEXT
6 constant L-TOTAL
7 constant L-REST
8 constant L-REM
9 constant L-BYTES
10 constant L-IOR
11 constant L-PTR
12 constant L-END

\ ---- throw, die and execute
0 constant TABLE0                    \ call_indirect's table, the module's one
4 constant HIGH-HALF                 \ a cell's upper 32 bits, little-endian
2 constant L-CODE                    \ throw's code
3 constant L-U                       \ die's message length

\ ---- catch and finally
2 constant L-STATUS                  \ the first callee's status
3 constant L-RDEPTH                  \ the two depths as the routine began
4 constant L-LDEPTH
5 constant L-CLEANUP                 \ finally's cleanup xt
6 constant L-BODY-CODE               \ finally's body's throw code

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

\ A throw that is not a fault clears the fault kind: the host reports a fault
\ for an uncaught throw only while the kind is nonzero.
: NO-FAULT, ( -- )
   L-CTX GET,  0 I32,  OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM, ;


\ emit ( c -- ) stores c's low byte at OUT-BASE plus ctx.out-len and counts it;
\ a full OUT traps.
: EMIT-BODY ( -- )
   1 U32,  2 U32,  I32 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  OP-I32-SUB B,  L-SP SET,
   WPROF:CTX-OUT-LEN CTX@,  L-LEN SET,
   L-LEN GET,  WPROF:OUT-CAP I32,  OP-I32-GE-U B,  IF,  OP-UNREACHABLE B,  OP-END B,
   WPROF:OUT-BASE I32,  L-LEN GET,  OP-I32-ADD B,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,
   OP-I64-STORE8 ALIGN-8 0 MEM,
   L-CTX GET,  L-LEN GET,  1 I32,  OP-I32-ADD B,
   OP-I32-STORE ALIGN-32 WPROF:CTX-OUT-LEN MEM,
   SP!,
   0 I32,  OP-END B, ;


\ The address in L-ADDR is at or past WPROF:CTX-BASE and below 2^32, else the
\ fault records WPROF:CRASH-RC with it and throws ENGINE-ERROR:MEMORY-FAULT,
\ the context's top as the routine found it.
: ADDRESS-CHECK, ( -- )
   L-ADDR GET,  WPROF:CTX-BASE I64,  OP-I64-LT-U B,
   L-ADDR GET,  $FFFFFFFF I64,  OP-I64-GT-U B,
   OP-I32-OR B,  IF,
      L-CTX GET,  WPROF:CRASH-RC I32,  OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
      L-CTX GET,  L-ADDR GET,  OP-I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM,
      L-CTX GET,  ENGINE-ERROR:MEMORY-FAULT I64,
      OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
      1 I32,  OP-RETURN B,
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
      NO-FAULT,  1 I32,  OP-RETURN B,
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

\ `cell-view`, `byte-view` and `PTR>N` ( a -- a ) leave the address as it is.
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


\ ---- map-anon and munmap
\ Room for n bytes of cells past SP, as src/habu/t0-wasm.f ROOM: an end above
\ WPROF:STACK-END records ENGINE-ERROR:STACK-BOUNDS with that address and traps.
: STACK-ROOM, ( n -- )
   {: n:n :}
   L-SP GET,  n I32,  OP-I32-ADD B,  WPROF:STACK-END I32,  OP-I32-GT-U B,  IF,
      L-CTX GET,  ENGINE-ERROR:STACK-BOUNDS I32,
      OP-I32-STORE ALIGN-32 WPROF:CTX-FAULT-KIND MEM,
      L-CTX GET,  L-SP GET,  n I32,  OP-I32-ADD B,  OP-I64-EXTEND-I32-U B,
      OP-I64-STORE ALIGN-64 WPROF:CTX-FAULT-ADDR MEM,
      OP-UNREACHABLE B,
   OP-END B, ;

: BLOCK, ( -- )  OP-BLOCK B,  EMPTY-TYPE B, ;
: LOOP, ( -- )  OP-LOOP B,  EMPTY-TYPE B, ;
\ A branch to the label n blocks, loops or ifs out from here, 0 the innermost.
: BR, ( n -- )  OP-BR B,  U32, ;
: BR-IF, ( n -- )  OP-BR-IF B,  U32, ;

: ALLOC-LOCALS, ( -- )  2 U32,  8 U32,  I32 B,  4 U32,  I64 B, ;

\ The i32 at offset off past the address in i32 local l.
: LD32, ( n n -- )
   {: l:n off:n :}
   l GET,  OP-I32-LOAD ALIGN-32 off MEM, ;

\ An i32 store at offset off past the address under the value.
: ST32, ( n -- )
   {: off:n :}
   OP-I32-STORE ALIGN-32 off MEM, ;

\ The i64 bytes on the Wasm stack, 1 through 2^32, as the i32 pages they take.
: PAGES-OF, ( -- )
   WPROF:PAGE-BYTES 1- I64,  OP-I64-ADD B,
   PAGE-SHIFT I64,  OP-I64-SHR-U B,  OP-I32-WRAP-I64 B, ;

\ The i32 pages on the Wasm stack as their i64 bytes.
: BYTES-OF, ( -- )  OP-I64-EXTEND-I32-U B,  PAGE-SHIFT I64,  OP-I64-SHL B, ;

\ The i64 address past the free run whose node is in i32 local l.
: RUN-END, ( n -- )
   {: l:n :}
   l GET,  OP-I64-EXTEND-I32-U B,  l NODE-PAGES LD32,  BYTES-OF,  OP-I64-ADD B, ;

\ The link becomes the address of ctx's free head, cur the run it names.
: LIST-HEAD, ( -- )
   L-CTX GET,  WPROF:CTX-FREE-HEAD I32,  OP-I32-ADD B,  L-LINK SET,
   L-LINK NODE-NEXT LD32,  L-CUR SET, ;

\ The link becomes cur's next cell, cur the run it names.
: LIST-STEP, ( -- )
   L-CUR GET,  L-LINK SET,  L-CUR NODE-NEXT LD32,  L-CUR SET, ;

\ map-anon's scan found cur with the pages: its run, but for a tail left free in
\ place, is cleared and answered. The branch leaves the if, loop and two blocks.
: FIT, ( -- )
   L-CUR NODE-PAGES LD32,  L-PAGES GET,  OP-I32-SUB B,  L-REST SET,
   L-REST GET,  OP-I32-EQZ B,  IF,
      L-LINK GET,  L-CUR NODE-NEXT LD32,  NODE-NEXT ST32,
   OP-ELSE B,
      L-CUR GET,  L-PAGES GET,  PAGE-SHIFT I32,  OP-I32-SHL B,  OP-I32-ADD B,  L-REM SET,
      L-REM GET,  L-CUR NODE-NEXT LD32,  NODE-NEXT ST32,
      L-REM GET,  L-REST GET,  NODE-PAGES ST32,
      L-LINK GET,  L-REM GET,  NODE-NEXT ST32,
   OP-END B,
   L-CUR GET,  0 I32,  L-PAGES GET,  PAGE-SHIFT I32,  OP-I32-SHL B,
   OP-MISC B,  MISC-MEMORY-FILL U32,  MEMORY-0 B,
   L-CUR GET,  OP-I64-EXTEND-I32-U B,  L-PTR SET,  0 I64,  L-IOR SET,
   3 BR, ;

\ map-anon ( bytes -- ptr ior ): first fit, else growth; a count outside 1
\ through 2^32 or a refused growth answers 0 -1.
: MAP-BODY ( -- )
   ALLOC-LOCALS,  CELL-BYTES SP-AT,  CELL-BYTES 2 * STACK-ROOM,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  L-BYTES SET,
   -1 I64,  L-IOR SET,
   BLOCK,
      L-BYTES GET,  0 I64,  OP-I64-LE-S B,
      L-BYTES GET,  WPROF:PAGES-MAX WPROF:PAGE-BYTES * I64,  OP-I64-GT-S B,
      OP-I32-OR B,  0 BR-IF,
      L-BYTES GET,  PAGES-OF,  L-PAGES SET,
      LIST-HEAD,
      BLOCK,
         LOOP,
            L-CUR GET,  OP-I32-EQZ B,  1 BR-IF,
            L-CUR NODE-PAGES LD32,  L-PAGES GET,  OP-I32-GE-U B,  IF,  FIT,  OP-END B,
            LIST-STEP,  0 BR,
         OP-END B,
      OP-END B,
      L-PAGES GET,  OP-MEMORY-GROW B,  MEMORY-0 B,  L-CUR SET,
      L-CUR GET,  -1 I32,  OP-I32-EQ B,  0 BR-IF,
      L-CUR GET,  BYTES-OF,  L-PTR SET,  0 I64,  L-IOR SET,
   OP-END B,
   L-SP GET,  L-PTR GET,  OP-I64-STORE ALIGN-64 0 MEM,
   L-SP GET,  L-IOR GET,  OP-I64-STORE ALIGN-64 CELL-BYTES MEM,
   L-SP GET,  CELL-BYTES 2 * I32,  OP-I32-ADD B,  L-SP SET,  SP!,
   DONE, ;

\ Where munmap's scan stopped: a cur starting before the freed run's end overlaps
\ it, as in a double free, and traps before any store; one at that end merges in.
: AFTER-RUN, ( -- )
   L-CUR GET,  L-NEXT SET,  L-PAGES GET,  L-TOTAL SET,
   L-CUR GET,  IF,
      L-CUR GET,  OP-I64-EXTEND-I32-U B,  L-END GET,  OP-I64-LT-S B,  IF,
         OP-UNREACHABLE B,
      OP-END B,
      L-END GET,  L-CUR GET,  OP-I64-EXTEND-I32-U B,  OP-I64-EQ B,  IF,
         L-CUR NODE-NEXT LD32,  L-NEXT SET,
         L-PAGES GET,  L-CUR NODE-PAGES LD32,  OP-I32-ADD B,  L-TOTAL SET,
      OP-END B,
   OP-END B, ;

\ The freed run joins the list at the link; the run before, when the link is its
\ node and it ends at the freed run, takes it in.
: INSERT-RUN, ( -- )
   L-PTR GET,  OP-I32-WRAP-I64 B,  L-NEXT GET,  NODE-NEXT ST32,
   L-PTR GET,  OP-I32-WRAP-I64 B,  L-TOTAL GET,  NODE-PAGES ST32,
   L-LINK GET,  L-PTR GET,  OP-I32-WRAP-I64 B,  NODE-NEXT ST32,
   L-LINK GET,  L-CTX GET,  WPROF:CTX-FREE-HEAD I32,  OP-I32-ADD B,  OP-I32-NE B,  IF,
      L-LINK RUN-END,  L-PTR GET,  OP-I64-EQ B,  IF,
         L-LINK GET,  L-NEXT GET,  NODE-NEXT ST32,
         L-LINK GET,  L-LINK NODE-PAGES LD32,  L-TOTAL GET,  OP-I32-ADD B,  NODE-PAGES ST32,
      OP-END B,
   OP-END B, ;

\ munmap ( addr bytes -- ior ): -1 for bytes outside 1 through 2^32, addr 0, or
\ an addr that is not a page-aligned offset below 2^32.
: UNMAP-BODY ( -- )
   ALLOC-LOCALS,  CELL-BYTES 2 * SP-AT,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  L-PTR SET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES MEM,  L-BYTES SET,
   -1 I64,  L-IOR SET,
   BLOCK,
      L-BYTES GET,  1 I64,  OP-I64-SUB B,  L-PTR GET,  OP-I64-OR B,
      OFFSET-BITS I64,  OP-I64-SHR-U B,
      L-PTR GET,  WPROF:PAGE-BYTES 1- I64,  OP-I64-AND B,  OP-I64-OR B,
      0 I64,  OP-I64-NE B,  L-PTR GET,  OP-I64-EQZ B,  OP-I32-OR B,  0 BR-IF,
      L-BYTES GET,  PAGES-OF,  L-PAGES SET,
      L-PTR GET,  L-PAGES GET,  BYTES-OF,  OP-I64-ADD B,  L-END SET,
      LIST-HEAD,
      BLOCK,
         LOOP,
            L-CUR GET,  OP-I32-EQZ B,  1 BR-IF,
            L-CUR RUN-END,  L-PTR GET,  OP-I64-GT-S B,  1 BR-IF,
            LIST-STEP,  0 BR,
         OP-END B,
      OP-END B,
      AFTER-RUN,  INSERT-RUN,
      0 I64,  L-IOR SET,
   OP-END B,
   L-SP GET,  L-IOR GET,  OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,  DONE, ;


\ ---- throw, die and execute
\ Where each call_indirect's padded type index starts in the body, for
\ WLINK:INDIRECT+ to name (i32) -> (i32) there.
DYNAMIC-BUFFER SITES n
variable SITES-N

: TYPE-PAD, ( -- )
   SITES-N @ 1+ SITES-RESERVE
   BUF-U @ SITES-N @ SITES !
   1 SITES-N +!
   0 ROOM WLEB:U32-PAD! BUF-U +! ;


\ throw ( n -- ) pops its code into ctx.throw-code and answers status 1; a zero
\ code is stored as ENGINE-ERROR:THROW-ZERO, as tier 1's (kernel.f THROW-ROW).
: THROW-BODY ( -- )
   2 U32,  1 U32,  I32 B,  1 U32,  I64 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  OP-I32-SUB B,  L-SP SET,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  L-CODE SET,
   SP!,
   L-CODE GET,  OP-I64-EQZ B,  IF,
      ENGINE-ERROR:THROW-ZERO I64,  L-CODE SET,
   OP-END B,
   L-CTX GET,  L-CODE GET,  OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
   NO-FAULT,  1 I32,  OP-END B, ;


\ die ( a u code -- ) appends its message to OUT, leaves its code in ctx.throw-code
\ and traps, which no catch consumes; a message past OUT's room traps uncopied.
: DIE-BODY ( -- )
   2 U32,  2 U32,  I32 B,  1 U32,  I64 B,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES 3 * I32,  OP-I32-SUB B,  L-SP SET,
   L-CTX GET,  L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES 2 * MEM,
   OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,  NO-FAULT,
   L-SP GET,  OP-I64-LOAD ALIGN-64 CELL-BYTES MEM,  L-U SET,
   WPROF:CTX-OUT-LEN CTX@,  L-LEN SET,
   L-U GET,  WPROF:OUT-CAP I32,  L-LEN GET,  OP-I32-SUB B,  OP-I64-EXTEND-I32-U B,
   OP-I64-GT-U B,  IF,  OP-UNREACHABLE B,  OP-END B,
   WPROF:OUT-BASE I32,  L-LEN GET,  OP-I32-ADD B,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  OP-I32-WRAP-I64 B,
   L-U GET,  OP-I32-WRAP-I64 B,
   OP-MISC B,  MISC-MEMORY-COPY U32,  0 B,  0 B,
   L-CTX GET,  L-LEN GET,  L-U GET,  OP-I32-WRAP-I64 B,  OP-I32-ADD B,
   OP-I32-STORE ALIGN-32 WPROF:CTX-OUT-LEN MEM,
   OP-UNREACHABLE B,  OP-END B, ;


\ Pops a cell and calls a table slot for its status: a zero upper half leaves the slot below
\ it; exactly $80000000 a record's address, stored in ctx.env, slot in cell 0; any other traps.
: DISPATCH, ( -- )
   L-CTX GET,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  OP-I32-SUB B,
   OP-LOCAL-TEE B,  L-SP U32,
   OP-I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM,
   L-CTX GET,
   L-SP GET,  OP-I32-LOAD ALIGN-32 HIGH-HALF MEM,
   OP-IF B,  I32 B,
      L-SP GET,  OP-I32-LOAD ALIGN-32 HIGH-HALF MEM,
      -2147483648 I32,  OP-I32-NE B,          \ $80000000 as i32.const's signed operand
      IF,  OP-UNREACHABLE B,  OP-END B,
      L-CTX GET,  L-SP GET,  OP-I32-LOAD ALIGN-32 0 MEM,
      OP-I32-STORE ALIGN-32 WPROF:CTX-ENV MEM,
      L-SP GET,  OP-I32-LOAD ALIGN-32 0 MEM,  OP-I32-LOAD ALIGN-32 0 MEM,
   OP-ELSE B,
      L-SP GET,  OP-I32-LOAD ALIGN-32 0 MEM,
   OP-END B,
   OP-CALL-INDIRECT B,  TYPE-PAD,  TABLE0 B, ;


\ execute ( xt -- ) answers the callee's status.
: EXECUTE-BODY ( -- )
   1 U32,  1 U32,  I32 B,
   DISPATCH,
   OP-END B, ;


\ ---- catch and finally
\ A throw leaves the return and loop stacks as deep as inside the thrown body;
\ a caught one restores the depths saved before it, as the native handler frame.
: DEPTHS-SAVE, ( -- )
   0 I32,  OP-I64-LOAD ALIGN-64 RDEPTH-AT MEM,  L-RDEPTH SET,
   0 I32,  OP-I64-LOAD ALIGN-64 LDEPTH-AT MEM,  L-LDEPTH SET, ;

: DEPTHS-RESTORE, ( -- )
   0 I32,  L-RDEPTH GET,  OP-I64-STORE ALIGN-64 RDEPTH-AT MEM,
   0 I32,  L-LDEPTH GET,  OP-I64-STORE ALIGN-64 LDEPTH-AT MEM, ;


\ catch ( xt -- code ): after a status 1 the code is ctx.throw-code, stored
\ where the xt lay; after a 0 it is zero, pushed over the callee's results.
: CATCH-BODY ( -- )
   2 U32,  2 U32,  I32 B,  2 U32,  I64 B,
   DEPTHS-SAVE,
   DISPATCH,  L-STATUS SET,
   L-STATUS GET,  IF,
      DEPTHS-RESTORE,
      L-SP GET,  L-CTX GET,  OP-I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE MEM,
      OP-I64-STORE ALIGN-64 0 MEM,
      ONE-RESULT,  0 I32,  OP-RETURN B,
   OP-END B,
   WPROF:CTX-STACK-TOP CTX@,  L-SP SET,
   CELL-BYTES STACK-ROOM,
   L-SP GET,  0 I64,  OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,
   0 I32,  OP-END B, ;


\ finally ( body cleanup -- ): the body's throw code is stored again whether or
\ not the cleanup throws; the cleanup's code stands only when the body returned.
: FINALLY-BODY ( -- )
   2 U32,  2 U32,  I32 B,  4 U32,  I64 B,
   L-CTX GET,
   WPROF:CTX-STACK-TOP CTX@,  CELL-BYTES I32,  OP-I32-SUB B,
   OP-LOCAL-TEE B,  L-SP U32,
   OP-I32-STORE ALIGN-32 WPROF:CTX-STACK-TOP MEM,
   L-SP GET,  OP-I64-LOAD ALIGN-64 0 MEM,  OP-LOCAL-TEE B,  L-CLEANUP U32,
   OP-I64-EQZ B,  IF,  OP-UNREACHABLE B,  OP-END B,
   DEPTHS-SAVE,
   DISPATCH,  L-STATUS SET,
   L-CTX GET,  OP-I64-LOAD ALIGN-64 WPROF:CTX-THROW-CODE MEM,  L-BODY-CODE SET,
   L-STATUS GET,  IF,  SP!,  DEPTHS-RESTORE,  OP-END B,
   WPROF:CTX-STACK-TOP CTX@,  L-SP SET,
   CELL-BYTES STACK-ROOM,
   L-SP GET,  L-CLEANUP GET,  OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,
   DISPATCH,
   L-STATUS GET,  IF,
      L-CTX GET,  L-BODY-CODE GET,  OP-I64-STORE ALIGN-64 WPROF:CTX-THROW-CODE MEM,
   OP-END B,
   L-STATUS GET,  OP-I32-OR B,
   OP-END B, ;


\ ---- depth
\ depth ( -- n ) pushes the count of cells from ctx's stack base to its top.
: DEPTH-BODY ( -- )
   1 U32,  1 U32,  I32 B,
   WPROF:CTX-STACK-TOP CTX@,  L-SP SET,
   CELL-BYTES STACK-ROOM,
   L-SP GET,
   L-SP GET,  WPROF:CTX-STACK-BASE CTX@,  OP-I32-SUB B,  OP-I64-EXTEND-I32-U B,
   CELL-SHIFT I64,  OP-I64-SHR-U B,
   OP-I64-STORE ALIGN-64 0 MEM,
   ONE-RESULT,  DONE, ;

\ The count of call_indirect sites in the last body ROUTINE-BODY answered, and
\ site k's offset in it.
: ROUTINE-SITES ( -- n )  SITES-N @ ;
: ROUTINE-SITE ( n -- n )  SITES @ ;

: BODY$ ( -- ptr u8 n )  0 BUF BUF-U @ ;

public

\ The named kernel word's routine body, as WLINK:FUNCTION+ takes one and valid
\ until the next call, and true; an empty span and false when it has none.
: ROUTINE-BODY ( ptr u8 n -- ptr u8 n bool )
   {: name:ptr u:n :}
   0 BUF-U !  0 SITES-N !
   name u s" emit" STR=CI if EMIT-BODY BODY$ true exit then
   name u s" !" STR=CI if OP-I64-STORE ALIGN-64 STORE-BODY BODY$ true exit then
   name u s" c!" STR=CI if OP-I64-STORE8 ALIGN-8 STORE-BODY BODY$ true exit then
   \ ---- division, views and atomics
   name u s" /" STR=CI if OP-I64-DIV-S true DIVIDE-BODY BODY$ true exit then
   name u s" mod" STR=CI if OP-I64-REM-S false DIVIDE-BODY BODY$ true exit then
   name u s" cell-view" STR=CI if VIEW-BODY BODY$ true exit then
   name u s" byte-view" STR=CI if VIEW-BODY BODY$ true exit then
   name u s" PTR>N" STR=CI if VIEW-BODY BODY$ true exit then
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
   \ ---- map-anon and munmap
   name u s" map-anon" STR=CI if MAP-BODY BODY$ true exit then
   name u s" munmap" STR=CI if UNMAP-BODY BODY$ true exit then
   \ ---- throw, die and execute
   name u s" throw" STR=CI if THROW-BODY BODY$ true exit then
   name u s" die" STR=CI if DIE-BODY BODY$ true exit then
   name u s" execute" STR=CI if EXECUTE-BODY BODY$ true exit then
   \ ---- catch and finally
   name u s" catch" STR=CI if CATCH-BODY BODY$ true exit then
   name u s" finally" STR=CI if FINALLY-BODY BODY$ true exit then
   \ ---- depth
   name u s" depth" STR=CI if DEPTH-BODY BODY$ true exit then
   s" " false ;

;using
;package
;package
