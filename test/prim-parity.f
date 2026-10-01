\ prim-parity.f — the engine primitive parity gate.
\
\ src/habu/prims.f is the machine-independent specification of the engine's
\ primitives. This file is the behaviour half of that contract: case sets
\ written once in target-free checked Habu, run against the backend's
\ own body AND — where the row carries a `REF` clause — against the reference
\ implementation in src/habu/prim-ref.f, asserting the two agree. The same file
\ runs unchanged on the next backend: same cases, same expectations, so the
\ x86_64 and Cortex-M bodies answer the arm64 numbers or the gate is red.
\
\ A CASE IS DATA. `<overload> CASES <name>` opens a case set for one row and
\ every line inside it is inputs and expected outputs:
\
\     0 CASES +
\             3       4          7  NN-N
\            -7       4         -3  NN-N
\     ;CASES
\
\ The integer sets are the data file test/prim-cases.f and the float sets
\ test/prim-float-cases.f, which this file includes and another runner can
\ include to hold a backend to the same cases.
\
\ HOW A CASE REACHES A PRIMITIVE. The sealed product engine has no way for
\ checked code to execute a name it computed: no xt table, no `evaluate`, no
\ dynamic `execute` (docs/forth.md — execution vectors are typed `defer` words,
\ and a quotation cannot be pushed at top level). The one route is a compiled
\ body that spells the primitive, so each case word calls a name-keyed
\ dispatcher whose arms are exactly that, one line per primitive:
\
\     na nu s" +"    STR= IF a b +             EXIT THEN
\     na nu s" swap" STR= IF 1 2 3 4 swap ENC4 EXIT THEN
\
\ That arm is the only place in the gate where a primitive is named. A subject
\ with no arm dies naming itself, and so does a row that declares a `REF` the
\ mirror dispatcher does not answer, so the table and the gate cannot drift
\ apart in silence. Every `REF` spelling in the table is also asserted to
\ resolve to a real word.
\
\ SHUFFLERS ARE ONE NUMBER. A stack primitive is handed the sentinels 1 2 3 4
\ and the case is the digit spelling of the whole window it leaves, the cells it
\ must not touch included: `swap` is 1243, `2over` is 123412, `drop` is 123.
\
\ FLOATS REUSE THE INTEGER SHAPES. Source has no float literals, so a float case
\ is integers and the arm converts with `s>f`/`f>s`. That pins the primitive's
\ presence and its integral behaviour; fractional semantics belong to
\ lib/float-test.f and to f64 text, not here. The two bit shapes, RR-R and R-R,
\ cast a double's bits instead, which is how a case reaches an infinity and the
\ NaN an operation makes.
\
\ COVERAGE IS BY ROW. The zero-based overload ordinal selects one row of that
\ name in table order. Closing a nonempty case set marks that row; a
\ numeric case never covers a pointer or boolean sibling. REF also belongs to
\ that exact row. Uncovered rows print their absolute table index with the name;
\ the gate reports that gap rather than failing on it. The memory store/fetch
\ pairs exercise both primitives in one round trip and mark both rows.
\
\ A REFUSAL IS A CASE TOO. A primitive that rejects its inputs is pinned by the
\ code it throws, not by a value: `NN-THROWS` runs the arm under `catch` and the
\ case column is the expected throw code, so the two dividing refusals below read
\ like any other row and the reference is held to the same code.
\
\     0 CASES /
\             7       0   E-DIV-ZERO  NN-THROWS
\
\ WHAT THIS GATE CANNOT HOLD.
\ - I/O, syscalls, process, code publication, profiler, engine-state and FFI
\   rows. They have no (inputs -> outputs) case and no honest reference; several
\   are TRUSTED-only or owner-private and a checked gate cannot even name them.
\   They are listed as uncovered.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/errors.f                    \ E-DIV-ZERO, the dividing rows' refusal
require src/habu/prim-ref.f
require lib/ieee754.f                  \ the bit shapes' casts

package PARITY
private

1 constant PARITY-RC                    \ nonzero exit is the suite's red
$200 constant COVER-CAP                 \ >= the table's row capacity
$40 constant SUBJ-CAP
$80 constant LBL-CAP
8 constant PER-LINE                     \ uncovered names printed per line
3 constant BUMP                         \ the addend the `+!` scenario applies

$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N

create FIX-CELLS 4 cells allot
$20 BUFFER: FIX-BYTES                   \ declared: `ptr-field` refuses a raw base
create COVERED COVER-CAP allot
create SUBJ-BUF SUBJ-CAP allot
create LBL-BUF LBL-CAP allot

variable SUBJ-U
variable LBL-U
variable SUBJ-N                         \ cases run under the current subject
variable SUBJ-ROW
variable CI
variable SI
variable NPRINT
variable NCOVER
variable NREF
variable NPRIM-ROWS

\ A refusing case's two operands. They are cells and not locals because the arm
\ runs inside the quotation `catch` takes, and a quotation captures nothing.
variable DV-A
variable DV-B

$3A constant COLON-B

: NO ( -- bool )
   0 0= 0= ;

: B>N ( bool -- n )                     \ a case column is 0/1; T= reports numbers
   IF 1 EXIT THEN
   0 ;

\ ---- the stack-window encoding ----------------------------------------------
: ENC2 ( n n -- n ) {: a:n b:n :}
   a 10 * b + ;

: ENC3 ( n n n -- n ) {: a:n b:n c:n :}
   a b ENC2 c ENC2 ;

: ENC4 ( n n n n -- n ) {: a:n b:n c:n d:n :}
   a b ENC2 c ENC2 d ENC2 ;

: ENC5 ( n n n n n -- n ) {: a:n b:n c:n d:n e:n :}
   a b ENC2 c ENC2 d ENC2 e ENC2 ;

: ENC6 ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n :}
   a b ENC2 c ENC2 d ENC2 e ENC2 f ENC2 ;

\ ---- the subject under test --------------------------------------------------
: SUBJ$ ( -- ptr u8 n )
   SUBJ-BUF SUBJ-U @ ;

: SUBJ! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SUBJ-CAP > IF s" prim-parity: primitive name too long" PARITY-RC die THEN
   a SUBJ-BUF u BYTE-COPY
   u SUBJ-U ! ;

: FIND-ROW ( ptr u8 n n -- n ) {: a:ptr u:n overload:n :}
   0
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:NAME$ a u STR= if
         dup overload = if drop i unloop exit then
         1+
      then
   loop drop -1 ;

: NO-ARM ( ptr u8 n ptr u8 n -- ) {: ka:ptr ku:n na:ptr nu:n :}
   s" prim-parity: no " type ka ku type s"  arm for " type na nu type cr
   s" prim-parity: unwired primitive" PARITY-RC die ;

: LBL ( ptr u8 n -- ) {: ta:ptr tu:n :}   \ "<subject> <tail>" as the failure label
   SUBJ-U @ tu + 1+ LBL-CAP > IF s" prim-parity: label too long" PARITY-RC die THEN
   SUBJ-BUF LBL-BUF SUBJ-U @ BYTE-COPY
   $20 LBL-BUF SUBJ-U @ + c!
   ta LBL-BUF SUBJ-U @ + 1+ tu BYTE-COPY
   SUBJ-U @ tu + 1+ LBL-U !
   LBL-BUF LBL-U @ T-LABEL ;

: LBL-PRIM ( -- )
   s" primitive" LBL ;

: LBL-REF ( -- )
   s" reference" LBL ;

: REF? ( -- bool )
   SUBJ-ROW @ PRIM-SPEC:REF$ nip 0= 0= ;

: CASE+ ( -- )
   SUBJ-N @ 1+ SUBJ-N ! ;

\ ---- stack shufflers ---------------------------------------------------------
\ Sentinels in, the whole remaining window encoded out. The cells the primitive
\ must leave alone are inside the encoding, so a shuffler that reaches too deep
\ fails here too.
: SHUF-PRIM ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu s" dup"   STR= IF 1 2 3 4 dup   ENC5 EXIT THEN
   na nu s" drop"  STR= IF 1 2 3 4 drop  ENC3 EXIT THEN
   na nu s" swap"  STR= IF 1 2 3 4 swap  ENC4 EXIT THEN
   na nu s" over"  STR= IF 1 2 3 4 over  ENC5 EXIT THEN
   na nu s" nip"   STR= IF 1 2 3 4 nip   ENC3 EXIT THEN
   na nu s" tuck"  STR= IF 1 2 3 4 tuck  ENC5 EXIT THEN
   na nu s" rot"   STR= IF 1 2 3 4 rot   ENC4 EXIT THEN
   na nu s" -rot"  STR= IF 1 2 3 4 -rot  ENC4 EXIT THEN
   na nu s" 2dup"  STR= IF 1 2 3 4 2dup  ENC6 EXIT THEN
   na nu s" 2drop" STR= IF 1 2 3 4 2drop ENC2 EXIT THEN
   na nu s" 2swap" STR= IF 1 2 3 4 2swap ENC4 EXIT THEN
   na nu s" 2over" STR= IF 1 2 3 4 2over ENC6 EXIT THEN
   s" shuffler" na nu NO-ARM ;

: SHUF-REF ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu s" dup"   STR= IF 1 2 3 4 PRIM-REF:S-DUP   ENC5 EXIT THEN
   na nu s" drop"  STR= IF 1 2 3 4 PRIM-REF:S-DROP  ENC3 EXIT THEN
   na nu s" swap"  STR= IF 1 2 3 4 PRIM-REF:S-SWAP  ENC4 EXIT THEN
   na nu s" over"  STR= IF 1 2 3 4 PRIM-REF:S-OVER  ENC5 EXIT THEN
   na nu s" nip"   STR= IF 1 2 3 4 PRIM-REF:S-NIP   ENC3 EXIT THEN
   na nu s" tuck"  STR= IF 1 2 3 4 PRIM-REF:S-TUCK  ENC5 EXIT THEN
   na nu s" rot"   STR= IF 1 2 3 4 PRIM-REF:S-ROT   ENC4 EXIT THEN
   na nu s" -rot"  STR= IF 1 2 3 4 PRIM-REF:S-UNROT ENC4 EXIT THEN
   na nu s" 2dup"  STR= IF 1 2 3 4 PRIM-REF:S-2DUP  ENC6 EXIT THEN
   na nu s" 2drop" STR= IF 1 2 3 4 PRIM-REF:S-2DROP ENC2 EXIT THEN
   na nu s" 2swap" STR= IF 1 2 3 4 PRIM-REF:S-2SWAP ENC4 EXIT THEN
   na nu s" 2over" STR= IF 1 2 3 4 PRIM-REF:S-2OVER ENC6 EXIT THEN
   s" shuffler reference" na nu NO-ARM ;

\ ---- two numbers in, one out -------------------------------------------------
: NN-N-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" +"      STR= IF a b +      EXIT THEN
   na nu s" -"      STR= IF a b -      EXIT THEN
   na nu s" *"      STR= IF a b *      EXIT THEN
   na nu s" /"      STR= IF a b /      EXIT THEN
   na nu s" mod"    STR= IF a b mod    EXIT THEN
   na nu s" and"    STR= IF a b and    EXIT THEN
   na nu s" or"     STR= IF a b or     EXIT THEN
   na nu s" xor"    STR= IF a b xor    EXIT THEN
   na nu s" min"    STR= IF a b min    EXIT THEN
   na nu s" max"    STR= IF a b max    EXIT THEN
   na nu s" lshift" STR= IF a b lshift EXIT THEN
   na nu s" rshift" STR= IF a b rshift EXIT THEN
   na nu s" f+"     STR= IF a s>f b s>f f+ f>s EXIT THEN
   na nu s" f-"     STR= IF a s>f b s>f f- f>s EXIT THEN
   na nu s" f*"     STR= IF a s>f b s>f f* f>s EXIT THEN
   na nu s" f/"     STR= IF a s>f b s>f f/ f>s EXIT THEN
   s" binary" na nu NO-ARM ;

: NN-N-REF ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" +"      STR= IF a b PRIM-REF:ADD     EXIT THEN
   na nu s" -"      STR= IF a b PRIM-REF:SUB     EXIT THEN
   na nu s" *"      STR= IF a b PRIM-REF:MUL     EXIT THEN
   na nu s" /"      STR= IF a b PRIM-REF:DIV     EXIT THEN
   na nu s" mod"    STR= IF a b PRIM-REF:REM     EXIT THEN
   na nu s" and"    STR= IF a b PRIM-REF:BAND    EXIT THEN
   na nu s" or"     STR= IF a b PRIM-REF:BOR     EXIT THEN
   na nu s" xor"    STR= IF a b PRIM-REF:BXOR    EXIT THEN
   na nu s" min"    STR= IF a b PRIM-REF:MINIMUM EXIT THEN
   na nu s" max"    STR= IF a b PRIM-REF:MAXIMUM EXIT THEN
   na nu s" lshift" STR= IF a b PRIM-REF:SHL     EXIT THEN
   na nu s" rshift" STR= IF a b PRIM-REF:SHR     EXIT THEN
   s" binary reference" na nu NO-ARM ;

\ ---- one number in, one out --------------------------------------------------
: N-N-PRIM ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" 1+"      STR= IF a 1+     EXIT THEN
   na nu s" 1-"      STR= IF a 1-     EXIT THEN
   na nu s" negate"  STR= IF a negate EXIT THEN
   na nu s" invert"  STR= IF a invert EXIT THEN
   na nu s" abs"     STR= IF a abs    EXIT THEN
   na nu s" cells"   STR= IF a cells  EXIT THEN
   na nu s" chars"   STR= IF a chars  EXIT THEN
   na nu s" cell+"   STR= IF a cell+  EXIT THEN
   na nu s" char+"   STR= IF a char+  EXIT THEN
   na nu s" fnegate" STR= IF a s>f fnegate f>s EXIT THEN
   na nu s" fabs"    STR= IF a s>f fabs    f>s EXIT THEN
   na nu s" fsqrt"   STR= IF a s>f fsqrt   f>s EXIT THEN
   na nu s" s>f"     STR= IF a s>f f>s EXIT THEN
   na nu s" f>s"     STR= IF a s>f f>s EXIT THEN
   s" unary" na nu NO-ARM ;

: N-N-REF ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" 1+"     STR= IF a PRIM-REF:INC        EXIT THEN
   na nu s" 1-"     STR= IF a PRIM-REF:DEC        EXIT THEN
   na nu s" negate" STR= IF a PRIM-REF:NEG        EXIT THEN
   na nu s" invert" STR= IF a PRIM-REF:BNOT       EXIT THEN
   na nu s" abs"    STR= IF a PRIM-REF:ABSOLUTE   EXIT THEN
   na nu s" cells"  STR= IF a PRIM-REF:CELL-BYTES EXIT THEN
   na nu s" chars"  STR= IF a PRIM-REF:CHAR-BYTES EXIT THEN
   na nu s" cell+"  STR= IF a PRIM-REF:CELL-STEP  EXIT THEN
   na nu s" char+"  STR= IF a PRIM-REF:CHAR-STEP  EXIT THEN
   s" unary reference" na nu NO-ARM ;

\ ---- two numbers in, one flag out --------------------------------------------
: NN-F-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" ="   STR= IF a b =  B>N EXIT THEN
   na nu s" <>"  STR= IF a b <> B>N EXIT THEN
   na nu s" <"   STR= IF a b <  B>N EXIT THEN
   na nu s" >"   STR= IF a b >  B>N EXIT THEN
   na nu s" <="  STR= IF a b <= B>N EXIT THEN
   na nu s" >="  STR= IF a b >= B>N EXIT THEN
   na nu s" f<"  STR= IF a s>f b s>f f< B>N EXIT THEN
   na nu s" f>"  STR= IF a s>f b s>f f> B>N EXIT THEN
   na nu s" f="  STR= IF a s>f b s>f f= B>N EXIT THEN
   s" order" na nu NO-ARM ;

: NN-F-REF ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" ="  STR= IF a b PRIM-REF:EQ  B>N EXIT THEN
   na nu s" <>" STR= IF a b PRIM-REF:NEQ B>N EXIT THEN
   na nu s" <"  STR= IF a b PRIM-REF:LT  B>N EXIT THEN
   na nu s" >"  STR= IF a b PRIM-REF:GT  B>N EXIT THEN
   na nu s" <=" STR= IF a b PRIM-REF:LE  B>N EXIT THEN
   na nu s" >=" STR= IF a b PRIM-REF:GE  B>N EXIT THEN
   s" order reference" na nu NO-ARM ;

\ ---- one number in, one flag out ---------------------------------------------
: N-F-PRIM ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" 0="  STR= IF a 0= B>N EXIT THEN
   na nu s" 0<"  STR= IF a 0< B>N EXIT THEN
   na nu s" f0=" STR= IF a s>f f0= B>N EXIT THEN
   na nu s" f0<" STR= IF a s>f f0< B>N EXIT THEN
   s" predicate" na nu NO-ARM ;

: N-F-REF ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" 0<" STR= IF a PRIM-REF:ZERO-NEG? B>N EXIT THEN
   s" predicate reference" na nu NO-ARM ;

\ ---- two flags in, one flag out ----------------------------------------------
\ The boolean rows of `and`, `or` and `xor`, which are separate rows from their
\ numeric ones and get their own arms.
: FF-F-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" and" STR= IF a 0= 0= b 0= 0= and B>N EXIT THEN
   na nu s" or"  STR= IF a 0= 0= b 0= 0= or  B>N EXIT THEN
   na nu s" xor" STR= IF a 0= 0= b 0= 0= xor B>N EXIT THEN
   s" flag" na nu NO-ARM ;

: FF-F-REF ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" and" STR= IF a 0= 0= b 0= 0= PRIM-REF:BAND-F B>N EXIT THEN
   na nu s" or"  STR= IF a 0= 0= b 0= 0= PRIM-REF:BOR-F  B>N EXIT THEN
   na nu s" xor" STR= IF a 0= 0= b 0= 0= PRIM-REF:BXOR-F B>N EXIT THEN
   s" flag reference" na nu NO-ARM ;

\ ---- two numbers in, two out -------------------------------------------------
: NN-NN-PRIM ( n n ptr u8 n -- n n ) {: a:n b:n na:ptr nu:n :}
   na nu s" /mod" STR= IF a b /mod EXIT THEN
   s" divmod" na nu NO-ARM ;

: NN-NN-REF ( n n ptr u8 n -- n n ) {: a:n b:n na:ptr nu:n :}
   na nu s" /mod" STR= IF a b PRIM-REF:DIVREM EXIT THEN
   s" divmod reference" na nu NO-ARM ;

\ ---- a double's bits in and out ----------------------------------------------
\ No row of these has a reference, so the shapes run the primitive alone.
: RR-R-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   a IEEE754:BITS>F64 b IEEE754:BITS>F64 {: x:r y:r :}
   na nu s" f+" STR= IF x y f+ IEEE754:F64>BITS EXIT THEN
   na nu s" f-" STR= IF x y f- IEEE754:F64>BITS EXIT THEN
   na nu s" f*" STR= IF x y f* IEEE754:F64>BITS EXIT THEN
   na nu s" f/" STR= IF x y f/ IEEE754:F64>BITS EXIT THEN
   s" binary bits" na nu NO-ARM ;

: R-R-PRIM ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" fsqrt" STR= IF a IEEE754:BITS>F64 fsqrt IEEE754:F64>BITS EXIT THEN
   s" unary bits" na nu NO-ARM ;

\ ---- two numbers in, a refusal out -------------------------------------------
\ The arm answers the code the primitive threw. Each drops whatever the body
\ left, so a primitive that wrongly ANSWERS is caught by the code comparison
\ (`catch` gives 0) rather than by an unbalanced stack further down the gate.
: NN-THROW-PRIM ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu s" /"    STR= IF [: DV-A @ DV-B @ /    drop  ;] catch EXIT THEN
   na nu s" mod"  STR= IF [: DV-A @ DV-B @ mod  drop  ;] catch EXIT THEN
   na nu s" /mod" STR= IF [: DV-A @ DV-B @ /mod 2drop ;] catch EXIT THEN
   s" refusing binary" na nu NO-ARM ;

: NN-THROW-REF ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu s" /"    STR= IF [: DV-A @ DV-B @ PRIM-REF:DIV    drop  ;] catch EXIT THEN
   na nu s" mod"  STR= IF [: DV-A @ DV-B @ PRIM-REF:REM    drop  ;] catch EXIT THEN
   na nu s" /mod" STR= IF [: DV-A @ DV-B @ PRIM-REF:DIVREM 2drop ;] catch EXIT THEN
   s" refusing binary reference" na nu NO-ARM ;

\ ---- memory ------------------------------------------------------------------
\ A round trip over the file's own fixtures passes the case value through a
\ store primitive and reads it back through its paired fetch primitive. Each
\ store/fetch pair runs once and covers both rows.
: MEM-PRIM ( n ptr u8 n -- n ) {: v:n na:ptr nu:n :}
   na nu s" !" STR= IF
      v FIX-CELLS ! FIX-CELLS @ EXIT THEN
   na nu s" c!" STR= IF
      v FIX-BYTES c! FIX-BYTES c@ EXIT THEN
   na nu s" +!"        STR= IF v FIX-CELLS ! BUMP FIX-CELLS +! FIX-CELLS @ EXIT THEN
   na nu s" count"     STR= IF v FIX-BYTES c! FIX-BYTES count nip EXIT THEN
   na nu s" byte-view" STR= IF v FIX-CELLS ! FIX-CELLS byte-view c@ EXIT THEN
   na nu s" cell-view" STR= IF v FIX-BYTES cell-view ! FIX-BYTES cell-view @ EXIT THEN
   s" memory" na nu NO-ARM ;

: MEM-REF ( n ptr u8 n -- n ) {: v:n na:ptr nu:n :}
   na nu s" +!"    STR= IF v FIX-CELLS ! BUMP FIX-CELLS PRIM-REF:ADD-TO FIX-CELLS @ EXIT THEN
   na nu s" count" STR= IF v FIX-BYTES c! FIX-BYTES PRIM-REF:COUNTED$ nip EXIT THEN
   s" memory reference" na nu NO-ARM ;

\ Pointer columns are offsets into FIX-BYTES; results are offsets or distances.
\ All accesses stay within that fixture. Each arm has the row's checked types.
: PN-P-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" +" STR= IF FIX-BYTES a + b + FIX-BYTES - EXIT THEN
   na nu s" -" STR= IF FIX-BYTES a + b - FIX-BYTES - EXIT THEN
   na nu s" ptr-field" STR= IF FIX-BYTES a + b ptr-field byte-view FIX-BYTES - EXIT THEN
   s" pointer step" na nu NO-ARM ;

: P-P-PRIM ( n ptr u8 n -- n ) {: a:n na:ptr nu:n :}
   na nu s" 1+"    STR= IF FIX-BYTES a + 1+    FIX-BYTES - EXIT THEN
   na nu s" 1-"    STR= IF FIX-BYTES a + 1-    FIX-BYTES - EXIT THEN
   na nu s" cell+" STR= IF FIX-BYTES a + cell+ FIX-BYTES - EXIT THEN
   na nu s" char+" STR= IF FIX-BYTES a + char+ FIX-BYTES - EXIT THEN
   s" pointer unary" na nu NO-ARM ;

: PP-F-PRIM ( n n ptr u8 n -- n ) {: a:n b:n na:ptr nu:n :}
   na nu s" ="  STR= IF FIX-BYTES a + FIX-BYTES b + =  B>N EXIT THEN
   na nu s" <>" STR= IF FIX-BYTES a + FIX-BYTES b + <> B>N EXIT THEN
   na nu s" <"  STR= IF FIX-BYTES a + FIX-BYTES b + <  B>N EXIT THEN
   na nu s" >"  STR= IF FIX-BYTES a + FIX-BYTES b + >  B>N EXIT THEN
   na nu s" <=" STR= IF FIX-BYTES a + FIX-BYTES b + <= B>N EXIT THEN
   na nu s" >=" STR= IF FIX-BYTES a + FIX-BYTES b + >= B>N EXIT THEN
   s" pointer order" na nu NO-ARM ;

: PTR-CASE+ ( -- )
   CASE+
   REF? IF s" pointer reference" SUBJ$ NO-ARM THEN ;

: BITS-CASE+ ( -- )
   CASE+
   REF? IF s" bits reference" SUBJ$ NO-ARM THEN ;

\ ---- coverage ----------------------------------------------------------------
: PRIM-ROW? ( n -- bool ) {: row:n :}
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PRIM =
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = or ;

: MARK ( n -- )
   COVERED + 1 swap c! ;

: COVERED? ( n -- bool )
   COVERED swap + c@ 0= 0= ;

: MARK-PAIRED ( ptr u8 n -- )
   0 FIND-ROW dup 0 < IF
      s" prim-parity: missing paired primitive" PARITY-RC die
   THEN
   MARK ;

public

: CASES ( n -- ) {: overload:n :}       \ <overload> CASES <name> ... ;CASES
   parse-name {: a:ptr u:n :}
   u 0= IF s" prim-parity: CASES needs a primitive name" PARITY-RC die THEN
   a u SUBJ!
   a u overload FIND-ROW SUBJ-ROW !
   SUBJ-ROW @ 0 < IF
      s" prim-parity: no table row for " type SUBJ$ type cr
      s" prim-parity: unknown primitive overload" PARITY-RC die
   THEN
   SUBJ-ROW @ PRIM-ROW? 0= IF
      s" prim-parity: not an effect row: " type SUBJ$ type cr
      s" prim-parity: wrong row kind" PARITY-RC die
   THEN
   0 SUBJ-N ! ;

: ;CASES ( -- )
   SUBJ-N @ 0= IF
      s" prim-parity: empty case set for " type SUBJ$ type cr
      s" prim-parity: empty case set" PARITY-RC die
   THEN
   SUBJ-ROW @ MARK
   0 SUBJ-U ! ;

: SHUF ( n -- ) {: want:n :}
   CASE+
   LBL-PRIM SUBJ$ SHUF-PRIM want T=
   REF? IF LBL-REF SUBJ$ SHUF-REF want T= THEN ;

: NN-N ( n n n -- ) {: a:n b:n want:n :}
   CASE+
   LBL-PRIM a b SUBJ$ NN-N-PRIM want T=
   REF? IF LBL-REF a b SUBJ$ NN-N-REF want T= THEN ;

: N-N ( n n -- ) {: a:n want:n :}
   CASE+
   LBL-PRIM a SUBJ$ N-N-PRIM want T=
   REF? IF LBL-REF a SUBJ$ N-N-REF want T= THEN ;

: NN-F ( n n n -- ) {: a:n b:n want:n :}
   CASE+
   LBL-PRIM a b SUBJ$ NN-F-PRIM want T=
   REF? IF LBL-REF a b SUBJ$ NN-F-REF want T= THEN ;

: N-F ( n n -- ) {: a:n want:n :}
   CASE+
   LBL-PRIM a SUBJ$ N-F-PRIM want T=
   REF? IF LBL-REF a SUBJ$ N-F-REF want T= THEN ;

: RR-R ( n n n -- ) {: a:n b:n want:n :}
   BITS-CASE+
   LBL-PRIM a b SUBJ$ RR-R-PRIM want T= ;

: R-R ( n n -- ) {: a:n want:n :}
   BITS-CASE+
   LBL-PRIM a SUBJ$ R-R-PRIM want T= ;

: FF-F ( n n n -- ) {: a:n b:n want:n :}
   CASE+
   LBL-PRIM a b SUBJ$ FF-F-PRIM want T=
   REF? IF LBL-REF a b SUBJ$ FF-F-REF want T= THEN ;

: NN-NN ( n n n n -- ) {: a:n b:n w1:n w2:n :}
   CASE+
   LBL-PRIM a b SUBJ$ NN-NN-PRIM {: g1:n g2:n :}
   g2 w2 T=
   LBL-PRIM g1 w1 T=
   REF? IF
      LBL-REF a b SUBJ$ NN-NN-REF {: r1:n r2:n :}
      r2 w2 T=
      LBL-REF r1 w1 T=
   THEN ;

: NN-THROWS ( n n n -- ) {: a:n b:n want:n :}
   CASE+
   a DV-A !  b DV-B !
   LBL-PRIM SUBJ$ NN-THROW-PRIM want T=
   REF? IF LBL-REF SUBJ$ NN-THROW-REF want T= THEN ;

: MEM ( n n -- ) {: v:n want:n :}
   CASE+
   LBL-PRIM v SUBJ$ MEM-PRIM want T=
   REF? IF LBL-REF v SUBJ$ MEM-REF want T= THEN ;

: PN-P ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   LBL-PRIM a b SUBJ$ PN-P-PRIM want T= ;

: NP-P ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   SUBJ$ s" +" STR= 0= IF s" number plus pointer" SUBJ$ NO-ARM THEN
   LBL-PRIM a FIX-BYTES b + + FIX-BYTES - want T= ;

: PP-N ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   SUBJ$ s" -" STR= 0= IF s" pointer distance" SUBJ$ NO-ARM THEN
   LBL-PRIM FIX-BYTES a + FIX-BYTES b + - want T= ;

: P-P ( n n -- ) {: a:n want:n :}
   PTR-CASE+
   LBL-PRIM a SUBJ$ P-P-PRIM want T= ;

: PP-F ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   LBL-PRIM a b SUBJ$ PP-F-PRIM want T= ;

private

\ ---- the report --------------------------------------------------------------
: COLON-AT ( ptr u8 n -- n ) {: a:ptr u:n :}   \ first colon, or -1
   0 SI !
   BEGIN SI @ u < WHILE
      a SI @ + c@ COLON-B = IF SI @ EXIT THEN
      SI @ 1+ SI !
   REPEAT
   -1 ;

\ A REF clause is a qualified name: the qualifier must be this gate's reference
\ package and the tail must be a word in it.
: REF-RESOLVED? ( n -- bool ) {: row:n :}
   row PRIM-SPEC:REF$ {: ra:ptr ru:n :}
   ra ru COLON-AT {: k:n :}
   k 0 < IF NO EXIT THEN
   ra k s" PRIM-REF" STR= 0= IF NO EXIT THEN
   ra k + 1+  ru k - 1-  PRIM-REF:WID search-wl 0= 0= ;

: REF-RESOLVES ( -- )                   \ every REF spelling in the table is a real word
   0 CI !
   BEGIN CI @ PRIM-SPEC:COUNT < WHILE
      CI @ PRIM-SPEC:REF$ nip 0= 0= IF
         CI @ PRIM-SPEC:NAME$ SUBJ!
         s" REF names a word in PRIM-REF" LBL
         CI @ REF-RESOLVED? TTRUE
         s" REF is exercised by a case set" LBL
         CI @ COVERED? TTRUE          \ a reference nothing runs is not a reference
      THEN
      CI @ 1+ CI !
   REPEAT ;

: TALLY ( -- )
   0 NCOVER !  0 NREF !  0 NPRIM-ROWS !
   0 CI !
   BEGIN CI @ PRIM-SPEC:COUNT < WHILE
      CI @ PRIM-ROW? IF
         NPRIM-ROWS @ 1+ NPRIM-ROWS !
         CI @ COVERED? IF NCOVER @ 1+ NCOVER ! THEN
         CI @ PRIM-SPEC:REF$ nip 0= 0= IF NREF @ 1+ NREF ! THEN
      THEN
      CI @ 1+ CI !
   REPEAT ;

: UNCOVERED. ( -- )
   s" prim-parity: rows with no case set:" type cr
   0 NPRINT !
   0 CI !
   BEGIN CI @ PRIM-SPEC:COUNT < WHILE
      CI @ PRIM-ROW? CI @ COVERED? 0= and IF
         s"   " type CI @ FMT:.U s" :" type CI @ PRIM-SPEC:NAME$ type
         NPRINT @ 1+ NPRINT !
         NPRINT @ PER-LINE mod 0= IF cr THEN
      THEN
      CI @ 1+ CI !
   REPEAT
   NPRINT @ PER-LINE mod 0= 0= IF cr THEN ;

: REPORT ( -- )
   TALLY
   s" prim-parity: rows " type PRIM-SPEC:COUNT .
   s" prim-parity: effect rows " type NPRIM-ROWS @ .
   s" prim-parity: rows with cases " type NCOVER @ .
   s" prim-parity: rows with a reference " type NREF @ .
   s" prim-parity: rows with neither " type NPRIM-ROWS @ NCOVER @ - .
   s" prim-parity: assertions " type T-CASES .
   UNCOVERED. ;

: MAP-FITS ( -- )
   PRIM-SPEC:COUNT COVER-CAP > IF
      s" prim-parity: table outgrew the coverage map" PARITY-RC die
   THEN ;

MAP-FITS
T-RESET

\ ---- the coverage map marks exactly the row a set names ----------------------
\ It runs before the shared sets, which also cover `+`'s pointer rows.
0 CASES +
           3           4                   7  NN-N
;CASES

s" numeric + covers only its own row" T-LABEL
s" +" 0 FIND-ROW COVERED? TTRUE
s" +" 1 FIND-ROW COVERED? TFALSE
s" +" 2 FIND-ROW COVERED? TFALSE
s" unknown overload is absent" T-LABEL
s" +" 3 FIND-ROW -1 T=
s" +" -1 FIND-ROW -1 T=

\ =============================================================================
\ The cases, in table order: the integer sets are data in test/prim-cases.f,
\ the float sets in test/prim-float-cases.f.
\ =============================================================================

s" test/prim-cases.f" included
s" @" MARK-PAIRED                       \ fetched by the `!` round trip
s" c@" MARK-PAIRED                      \ fetched by the `c!` round trip

s" test/prim-float-cases.f" included

REF-RESOLVES
REPORT
T-REPORT

;package
