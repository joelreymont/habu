\ device.f - the wasm device check: WLINK's modules validated by wasm-tools and
\ run by bun through test/wasm/harness.f. It needs both tools on PATH, so it is
\ no row of the ordinary gate; run it from the tree's root as
\ `bin/hb --load test/wasm/device.f` (docs/bootstrap.md).
\
\ A one-function module writes OUT, stores a throw code that a double would
\ round and answers status 1: all three are read back. A body that traps exits
\ 2; one answering an i64 where its type says i32 is invalid and exits 3.
\ W03's module (test/wasm/w03.f), 140 functions chained by calls, validates
\ and runs to status 0 with no output. WKERNEL's hand-built rows
\ (src/arch/wasm/kernel.f) run under callers that store cells to the context
\ stack as compiled code does: emit writes OUT and traps one byte past it,
\ depth counts the cells, and throw takes its code, zero included.
\ test/wasm/dynamic.f runs first, installs the backend and reports on its own:
\ it links a captured window with WASMLINK and runs execute and catch through
\ table slots and .s through kernel-words.f's `.`. The modules stay in the
\ printed directories.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/arch/wasm/link.f
require src/arch/wasm/encode.f
require src/arch/wasm/kernel.f
require lib/le.f
require test/wasm/harness.f
require test/wasm/w03.f

require test/wasm/dynamic.f                    \ installs the backend WKERNEL builds under

package WASM-DEVICE
private
using WASM-W03

FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: PATH
variable PATH-U

: SETUP ( -- )
   s" wasm-device" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm device: modules in " type  DIR u type cr ;

\ The module run calling function e, written as name in DIR; answers its path.
: MODULE ( n ptr u8 n -- ptr u8 n )
   {: e:n name:ptr nu:n :}
   DIR DIR-U @ name nu PATH JOIN-PATH PATH-U !
   PATH PATH-U @  e WLINK:LINK  WRITE-ALL
   PATH PATH-U @ ;

\ ---- bodies, in WASM-W03's buffer, test/wasm/w03.f --------------------------------
: S32, ( n -- )   ROOM WLEB:S32! BODY-U +! ;
: S64, ( n -- )   ROOM WLEB:S64! BODY-U +! ;

\ The body as kernel function 0 of a new link, of no lanes.
: KERNEL0 ( -- )
   WLINK:RESET
   BODY$ 0 0 0 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ drop ;

\ ---- one function ------------------------------------------------------------------
$8000000000000001 constant CODE        \ MIN-N + 1, which a double rounds to MIN-N
: TEXT$ ( -- ptr u8 n )  S\" wasm ok\n" ;

\ i32.const ctx's base, the address of a store into one of its fields.
: CTX, ( -- )  $41 B, WPROF:CTX-BASE S32, ;

\ TEXT$ into OUT a byte at a time and its length into out-len, CODE into the
\ throw code, then status 1.
: THROWER ( -- )
   0 BODY-U !
   0 B,
   TEXT$ {: t:ptr tu:n :}
   tu 0 do
      $41 B, WPROF:OUT-BASE i + S32,  $41 B, t i + c@ S32,  $3A B, 0 B, 0 B,
   loop
   CTX,  $41 B, tu S32,  $36 B, 2 B, WPROF:CTX-OUT-LEN B,
   CTX,  $42 B, CODE S64,  $37 B, 3 B, WPROF:CTX-THROW-CODE B,
   $41 B, 1 S32,
   $0B B, ;

: ONE-ROW ( -- )
   THROWER KERNEL0
   0 s" one.wasm" MODULE {: a:ptr u:n :}
   s" a one-function module validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" it exits 1, its entry's status" T-LABEL
   a u WASM-HARNESS:RUN 1 T=
   s" the bytes it wrote to OUT are read back" T-LABEL
   WASM-HARNESS:OUT$ TEXT$ T$=
   s" its throw code is read back whole" T-LABEL
   WASM-HARNESS:THROW-CODE CODE T= ;

: TRAP-ROW ( -- )
   0 BODY-U !  0 B,  $00 B,  $0B B,            \ unreachable
   KERNEL0
   0 s" trap.wasm" MODULE {: a:ptr u:n :}
   s" a body that traps validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" it exits 2" T-LABEL
   a u WASM-HARNESS:RUN 2 T= ;

: INVALID-ROW ( -- )
   0 BODY-U !  0 B,  $42 B, 0 S64,  $0B B,     \ an i64 for its i32 status
   KERNEL0
   0 s" invalid.wasm" MODULE {: a:ptr u:n :}
   s" a body answering the wrong type is invalid" T-LABEL
   a u WASM-HARNESS:VALID? TFALSE
   s" it exits 3" T-LABEL
   a u WASM-HARNESS:RUN 3 T= ;

\ ---- W03 -------------------------------------------------------------------------
: W03-ROW ( -- )
   BUILD
   CHAIN s" w03.wasm" MODULE {: a:ptr u:n :}
   s" W03's module validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" W03's module runs to status 0" T-LABEL
   a u WASM-HARNESS:RUN 0 T=
   s" W03's module writes no output" T-LABEL
   WASM-HARNESS:OUT$ nip 0 T= ;

\ ---- WKERNEL's rows -------------------------------------------------------------
\ Each module links the four rows as kernel functions 0..3, in the order WENC
\ emits them, and an entry of no lanes that calls each row by the function the
\ map answers for its name. No entry here calls .s, whose call to `.` stays
\ unlinked.
WPROF:STACK-BASE WPROF:OUT-BASE - constant OUT-CAP
: EMITTED ( -- ptr u8 n )  S\" emit row\n" ;

16 constant SITES-CAP
SITES-CAP TYPED-BUFFER SITE-AT n       \ each call field of the body being built
SITES-CAP TYPED-BUFFER SITE-TO n       \ the function it calls
variable SITES

: ROW ( ptr u8 n -- n )
   WKERNEL:PROVIDER MATCH provider
      row OF ENDOF
      word OF 2drop -1 ENDOF
   ;MATCH ;

\ A call of function f, its field recorded for SITES+.
: CALL, ( n -- )
   {: f:n :}
   $10 B,
   BODY-U @ SITES @ SITE-AT !  f SITES @ SITE-TO !  1 SITES +!
   0 PAD5, ;

: SITES+ ( n -- )
   {: f:n :}
   SITES @ 0 ?do  f  i SITE-AT @  i SITE-TO @  WLINK:CALL+  loop ;

\ Row k's field at byte off of its entry in WENC's header.
: HEADER ( n n -- ptr u8 )
   {: k:n off:n :}
   WENC:BYTES 8 + k 12 * + off + ;

: ROWS+ ( -- )
   WKERNEL:ENCODE
   WENC:FUNS 0 ?do
      WENC:BYTES i WENC:FUNCTION-OFFSET@ +  i 4 HEADER LE:U32@
      i 8 HEADER c@  i 9 HEADER c@  i 10 HEADER c@
      WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ drop
   loop ;

\ A new link of the rows.
: KERNEL+ ( -- )
   WLINK:RESET
   ROWS+ ;

\ An entry body: ctx is local 0, an i32 local 1 and an i64 local 2.
: MAIN-OPEN ( -- )
   0 SITES !  0 BODY-U !
   2 B,  1 B, $7F B,  1 B, $7E B, ;

\ The entry closed on the status it leaves, added and linked as name.
: MAIN-SHUT ( ptr u8 n -- ptr u8 n )
   {: name:ptr nu:n :}
   $0B B,
   BODY$ 0 0 0 WLINK-ORIGIN:CAPTURED WLINK:FUNCTION+ {: m:n :}
   m SITES+
   m name nu MODULE ;

\ ctx's stack top, the address a cell is stored at.
: TOP, ( -- )  CTX, $28 B, 2 B, WPROF:CTX-STACK-TOP B, ;

\ The i64 left after TOP, stored there and the top moved past it.
: PUSHED, ( -- )
   $37 B, 3 B, 0 B,
   CTX, TOP, $41 B, 8 S32, $6A B, $36 B, 2 B, WPROF:CTX-STACK-TOP B, ;

: PUSH, ( n -- )  TOP, $42 B, S64, PUSHED, ;

\ A call of the row named, which passes no lane.
: ROW, ( ptr u8 n -- )  CTX, ROW CALL, ;

\ depth, its count kept in local 2 and pushed.
: DEPTH, ( -- )
   s" depth" ROW,  $21 B, 2 B,  $1A B,
   TOP, $20 B, 2 B, PUSHED, ;

: EMIT-ROW ( -- )
   KERNEL+
   MAIN-OPEN
   EMITTED {: t:ptr tu:n :}
   tu 0 do
      CTX, $42 B, t i + c@ S64, s" emit" ROW CALL,
      i 0<> if $72 B, then                       \ i32.or of the statuses
   loop
   s" emit.wasm" MAIN-SHUT {: a:ptr u:n :}
   s" a module calling the emit row validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" each emit answers status 0" T-LABEL
   a u WASM-HARNESS:RUN 0 T=
   s" the bytes emitted are OUT's" T-LABEL
   WASM-HARNESS:OUT$ EMITTED T$= ;

\ n emits in a loop, local 1 counting them.
: FILL ( n ptr u8 n -- ptr u8 n )
   {: n:n name:ptr nu:n :}
   KERNEL+
   MAIN-OPEN
   $02 B, $40 B,  $03 B, $40 B,
      $20 B, 1 B,  $41 B, n S32,  $46 B,  $0D B, 1 B,
      CTX, $42 B, 120 S64, s" emit" ROW CALL, $1A B,
      $20 B, 1 B,  $41 B, 1 S32,  $6A B,  $21 B, 1 B,
      $0C B, 0 B,
   $0B B, $0B B,
   $41 B, 0 S32,
   name nu MAIN-SHUT ;

: FULL-ROW ( -- )
   OUT-CAP s" full.wasm" FILL {: a:ptr u:n :}
   s" emit fills OUT to its last byte" T-LABEL
   a u WASM-HARNESS:RUN 0 T=
   WASM-HARNESS:OUT$ nip OUT-CAP T=
   OUT-CAP 1+ s" over.wasm" FILL {: b:ptr v:n :}
   s" an emit past OUT traps" T-LABEL
   b v WASM-HARNESS:RUN 2 T= ;

\ 1 2 3 9 throw takes the 9, so the depth thrown after it is 3.
: DEPTH-ROW ( -- )
   KERNEL+
   MAIN-OPEN
   1 PUSH, 2 PUSH, 3 PUSH, 9 PUSH,
   s" throw" ROW, $1A B,
   DEPTH,
   s" throw" ROW,
   s" depth.wasm" MAIN-SHUT {: a:ptr u:n :}
   s" a module calling the depth and throw rows validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" throw answers status 1" T-LABEL
   a u WASM-HARNESS:RUN 1 T=
   s" depth counts the cells the throw left" T-LABEL
   WASM-HARNESS:THROW-CODE 3 T= ;

: THROWN ( n ptr u8 n -- )
   {: c:n name:ptr nu:n :}
   KERNEL+
   MAIN-OPEN
   c PUSH,  s" throw" ROW,
   name nu MAIN-SHUT {: a:ptr u:n :}
   a u WASM-HARNESS:RUN 1 T=
   WASM-HARNESS:THROW-CODE c T= ;

: THROW-ROW ( -- )
   s" a throw's code is stored whole" T-LABEL
   CODE s" throw.wasm" THROWN
   s" a zero throw throws, as the engine's does" T-LABEL
   0 s" throw0.wasm" THROWN ;

public

: RUN ( -- )
   T-RESET
   SETUP
   ONE-ROW
   TRAP-ROW
   INVALID-ROW
   W03-ROW
   EMIT-ROW
   FULL-ROW
   DEPTH-ROW
   THROW-ROW
   T-REPORT ;

;package

WASM-DEVICE:RUN
