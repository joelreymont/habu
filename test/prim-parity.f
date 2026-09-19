\ prim-parity.f — the engine primitive parity gate.
\
\ src/habu/prims.f is the machine-independent specification of the engine's
\ primitives. This file is the behaviour half of that contract: one case set per
\ primitive, written once in target-free checked Habu, run against the backend's
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
\ presence and its integral behaviour; fractional and NaN semantics belong to
\ lib/float-test.f and to f64 text, not here.
\
\ COVERAGE IS BY ROW. The zero-based overload ordinal selects one row of that
\ name in table order. Closing a nonempty case set marks only that row; a
\ numeric case never covers a pointer or boolean sibling. REF also belongs to
\ that exact row. Uncovered rows print their absolute table index with the name;
\ the gate reports that gap rather than failing on it.
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
create FIX-BYTES $20 allot
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
\ One scenario per row over the file's own fixtures: the case value goes in
\ through the primitive under test and comes back out, so a store row is read
\ back and a fetch row is written first.
: MEM-PRIM ( n ptr u8 n -- n ) {: v:n na:ptr nu:n :}
   na nu s" !"         STR= IF v FIX-CELLS ! FIX-CELLS @ EXIT THEN
   na nu s" @"         STR= IF v FIX-CELLS ! FIX-CELLS @ EXIT THEN
   na nu s" c!"        STR= IF v FIX-BYTES c! FIX-BYTES c@ EXIT THEN
   na nu s" c@"        STR= IF v FIX-BYTES c! FIX-BYTES c@ EXIT THEN
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

\ ---- coverage ----------------------------------------------------------------
: PRIM-ROW? ( n -- bool ) {: row:n :}
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PRIM =
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = or ;

: MARK ( n -- )
   COVERED + 1 swap c! ;

: COVERED? ( n -- bool )
   COVERED swap + c@ 0= 0= ;

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

\ =============================================================================
\ The cases. Table order.
\ =============================================================================

\ ---- stack shufflers: 1 2 3 4 in, the whole window out ----------------------
0 CASES dup     12344 SHUF ;CASES
0 CASES drop      123 SHUF ;CASES
0 CASES swap     1243 SHUF ;CASES
0 CASES over    12343 SHUF ;CASES
0 CASES nip       124 SHUF ;CASES
0 CASES tuck    12434 SHUF ;CASES
0 CASES rot      1342 SHUF ;CASES
0 CASES -rot     1423 SHUF ;CASES
0 CASES 2dup   123434 SHUF ;CASES
0 CASES 2drop      12 SHUF ;CASES
0 CASES 2swap    3412 SHUF ;CASES
0 CASES 2over  123412 SHUF ;CASES

\ ---- arithmetic --------------------------------------------------------------
0 CASES +
           3           4                   7  NN-N
          -7           4                  -3  NN-N
           0           0                   0  NN-N
       MAX-N           1               MIN-N  NN-N     \ wraps
       MIN-N       MIN-N                   0  NN-N
;CASES

s" numeric + covers only its own row" T-LABEL
s" +" 0 FIND-ROW COVERED? TTRUE
s" +" 1 FIND-ROW COVERED? TFALSE
s" +" 2 FIND-ROW COVERED? TFALSE
s" unknown overload is absent" T-LABEL
s" +" 3 FIND-ROW -1 T=
s" +" -1 FIND-ROW -1 T=

1 CASES +                              \ (ptr a n -- ptr a)
           4           3                   7  PN-P
           8          -3                   5  PN-P
           0           0                   0  PN-P
;CASES

2 CASES +                              \ (n ptr a -- ptr a)
           3           4                   7  NP-P
          -3           8                   5  NP-P
           0           0                   0  NP-P
;CASES

0 CASES -
           7           4                   3  NN-N
           4           7                  -3  NN-N
       MIN-N           1               MAX-N  NN-N     \ wraps
          -1          -1                   0  NN-N
;CASES

1 CASES -                              \ (ptr a n -- ptr a)
           8           3                   5  PN-P
           4          -3                   7  PN-P
           0           0                   0  PN-P
;CASES

2 CASES -                              \ (ptr a ptr a -- n)
           8           3                   5  PP-N
           3           8                  -5  PP-N
           4           4                   0  PP-N
;CASES

0 CASES *
           6           7                  42  NN-N
          -6           7                 -42  NN-N
          -6          -7                  42  NN-N
           0       MAX-N                   0  NN-N
 $100000000 $100000000                   0  NN-N     \ wraps
;CASES

\ Truncated toward zero, so the remainder carries the dividend's sign. A zero
\ divisor is refused by name and MIN-N -1 wraps: both are contracts every backend
\ answers, stated in docs/forth.md.
0 CASES /
           7           2                   3  NN-N
          -7           2                  -3  NN-N
           7          -2                  -3  NN-N
          -7          -2                   3  NN-N
           6           3                   2  NN-N
           0           5                   0  NN-N
       MAX-N           2   $3FFFFFFFFFFFFFFF  NN-N
       MIN-N          -1               MIN-N  NN-N     \ wraps
           7           0          E-DIV-ZERO  NN-THROWS
       MIN-N           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES mod
           7           2                   1  NN-N
          -7           2                  -1  NN-N
           7          -2                   1  NN-N
          -7          -2                  -1  NN-N
           6           3                   0  NN-N
       MIN-N          -1                   0  NN-N     \ the wrapped quotient's remainder
           7           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES /mod
           7           2               1     3  NN-NN
          -7           2              -1    -3  NN-NN
           7          -2               1    -3  NN-NN
          -7          -2              -1     3  NN-NN
       MIN-N          -1               0 MIN-N  NN-NN   \ wraps
           7           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES and
         $F0         $3C                 $30  NN-N
          -1         $FF                 $FF  NN-N
           0          -1                   0  NN-N
;CASES
1 CASES and
           1           1                   1  FF-F
           1           0                   0  FF-F
           0           0                   0  FF-F
;CASES

0 CASES or
         $F0         $0C                 $FC  NN-N
           0           0                   0  NN-N
          -1           0                  -1  NN-N
;CASES
1 CASES or
           1           0                   1  FF-F
           0           0                   0  FF-F
           1           1                   1  FF-F
;CASES

0 CASES xor
         $F0         $3C                 $CC  NN-N
          -1          -1                   0  NN-N
         $FF           0                 $FF  NN-N
;CASES
1 CASES xor
           1           1                   0  FF-F
           1           0                   1  FF-F
           0           0                   0  FF-F
;CASES

0 CASES 1+
           3                               4  N-N
          -1                               0  N-N
       MAX-N                           MIN-N  N-N     \ wraps
;CASES

1 CASES 1+
           0                               1  P-P
           7                               8  P-P
;CASES

0 CASES 1-
           3                               2  N-N
           0                              -1  N-N
       MIN-N                           MAX-N  N-N     \ wraps
;CASES

1 CASES 1-
           1                               0  P-P
           8                               7  P-P
;CASES

0 CASES negate
           5                              -5  N-N
          -5                               5  N-N
           0                               0  N-N
       MIN-N                           MIN-N  N-N     \ the one fixed point
;CASES

0 CASES invert
           0                              -1  N-N
          -1                               0  N-N
         $F0            $FFFFFFFFFFFFFF0F  N-N
;CASES

0 CASES 0=
           0                               1  N-F
           1                               0  N-F
          -1                               0  N-F
;CASES

0 CASES 0<
          -1                               1  N-F
           0                               0  N-F
           1                               0  N-F
       MIN-N                               1  N-F
       MAX-N                               0  N-F
;CASES

0 CASES =
           3           3                   1  NN-F
           3           4                   0  NN-F
          -1          -1                   1  NN-F
;CASES

1 CASES =
           3           3                   1  PP-F
           3           4                   0  PP-F
;CASES

0 CASES <
           3           4                   1  NN-F
           4           3                   0  NN-F
           3           3                   0  NN-F
          -1           1                   1  NN-F
       MIN-N       MAX-N                   1  NN-F
;CASES

1 CASES <
           3           4                   1  PP-F
           4           3                   0  PP-F
           3           3                   0  PP-F
;CASES

0 CASES >
           4           3                   1  NN-F
           3           4                   0  NN-F
       MAX-N       MIN-N                   1  NN-F
;CASES

1 CASES >
           4           3                   1  PP-F
           3           4                   0  PP-F
           3           3                   0  PP-F
;CASES

0 CASES <>
           3           3                   0  NN-F
           3           4                   1  NN-F
;CASES

1 CASES <>
           3           3                   0  PP-F
           3           4                   1  PP-F
;CASES

0 CASES <=
           3           3                   1  NN-F
           3           4                   1  NN-F
           4           3                   0  NN-F
       MIN-N       MAX-N                   1  NN-F
;CASES

1 CASES <=
           3           3                   1  PP-F
           3           4                   1  PP-F
           4           3                   0  PP-F
;CASES

0 CASES >=
           3           3                   1  NN-F
           4           3                   1  NN-F
           3           4                   0  NN-F
;CASES

1 CASES >=
           3           3                   1  PP-F
           4           3                   1  PP-F
           3           4                   0  PP-F
;CASES

0 CASES abs
          -5                               5  N-N
           5                               5  N-N
           0                               0  N-N
       MIN-N                           MIN-N  N-N     \ negate's fixed point again
;CASES

0 CASES min
           3           4                   3  NN-N
          -3           4                  -3  NN-N
           5           5                   5  NN-N
       MIN-N       MAX-N               MIN-N  NN-N
;CASES

0 CASES max
           3           4                   4  NN-N
          -3          -4                  -3  NN-N
       MIN-N       MAX-N               MAX-N  NN-N
;CASES

\ The shift count is taken modulo the cell width: 64 shifts by none. Measured on
\ arm64 and pinned here so a backend that answers zero instead is red.
0 CASES lshift
           1           4                 $10  NN-N
           1          63               MIN-N  NN-N
           1          64                   1  NN-N
           1          65                   2  NN-N
          -1           1                  -2  NN-N
;CASES

0 CASES rshift
         $10           4                   1  NN-N
          -1           1               MAX-N  NN-N     \ logical, not arithmetic
          -1          63                   1  NN-N
           1          64                   1  NN-N
;CASES

0 CASES cells
           1                               8  N-N
           3                              24  N-N
           0                               0  N-N
;CASES

1 CASES cell+
           0                               8  N-N
           8                              16  N-N
;CASES

0 CASES cell+
           0                               8  P-P
           8                              16  P-P
;CASES

0 CASES chars
           1                               1  N-N
           7                               7  N-N
;CASES

1 CASES char+
           0                               1  N-N
           7                               8  N-N
;CASES

0 CASES char+
           0                               1  P-P
           7                               8  P-P
;CASES

\ ---- memory ------------------------------------------------------------------
0 CASES !
           0                               0  MEM
       $1234                           $1234  MEM
          -1                              -1  MEM
;CASES

0 CASES @
           0                               0  MEM
       $1234                           $1234  MEM
          -1                              -1  MEM
;CASES

0 CASES c!
           0                               0  MEM
         $7F                             $7F  MEM
         $FF                             $FF  MEM
;CASES

0 CASES c@
           0                               0  MEM
         $7F                             $7F  MEM
         $FF                             $FF  MEM
;CASES

0 CASES +!
           0                               3  MEM
          10                              13  MEM
          -3                               0  MEM
;CASES

0 CASES count
           0                               0  MEM
           5                               5  MEM
         $FF                             $FF  MEM
;CASES

0 CASES byte-view
       $1234                             $34  MEM     \ little-endian, as both live targets are
          -1                             $FF  MEM
           0                               0  MEM
;CASES

0 CASES cell-view
       $1234                           $1234  MEM
          -1                              -1  MEM
;CASES

\ ---- floats ------------------------------------------------------------------
0 CASES f+
           3           4                   7  NN-N
          -3           4                   1  NN-N
           0           0                   0  NN-N
;CASES

0 CASES f-
           7           4                   3  NN-N
           4           7                  -3  NN-N
;CASES

0 CASES f*
           6           7                  42  NN-N
          -6           7                 -42  NN-N
;CASES

0 CASES f/
          12           3                   4  NN-N
         -12           3                  -4  NN-N
;CASES

0 CASES fnegate
           5                              -5  N-N
          -5                               5  N-N
;CASES

0 CASES fabs
          -5                               5  N-N
           5                               5  N-N
;CASES

0 CASES fsqrt
          16                               4  N-N
           0                               0  N-N
;CASES

0 CASES f<
           3           4                   1  NN-F
           4           3                   0  NN-F
;CASES

0 CASES f>
           4           3                   1  NN-F
           3           4                   0  NN-F
;CASES

0 CASES f=
           3           3                   1  NN-F
           3           4                   0  NN-F
;CASES

0 CASES f0<
          -1                               1  N-F
           1                               0  N-F
           0                               0  N-F
;CASES

0 CASES f0=
           0                               1  N-F
           1                               0  N-F
;CASES

0 CASES s>f
           7                               7  N-N
          -7                              -7  N-N
;CASES

0 CASES f>s
           7                               7  N-N
          -7                              -7  N-N
;CASES

REF-RESOLVES
REPORT
T-REPORT

;package
