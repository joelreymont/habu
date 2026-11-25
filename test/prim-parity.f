\ prim-parity.f - the engine primitive parity gate: the case sets of
\ test/prim-cases.f and test/prim-float-cases.f, in target-free checked Habu,
\ run against the backend's own body and, where a src/habu/prims.f row carries
\ `REF`, against src/habu/prim-ref.f; every backend answers the arm64 numbers. A
\ set is data: `<overload> CASES <name>` ... `;CASES`, each line inputs, outputs
\ and a shape (`3 4 7 NN-N`); the overload ordinal picks the row.

require src/core/exit-status.f
require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/error.f                    \ E-DIV-ZERO, in the cases switched off
require src/habu/prim-ref.f
require src/lib/ieee754.f                  \ the bit shapes' casts

package PARITY
private

$40 constant SUBJ-CAP
$80 constant LBL-CAP
8 constant PER-LINE
3 constant BUMP                         \ the addend the `+!` scenario applies

$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N

create FIX-CELLS 4 cells allot
$20 BUFFER: FIX-BYTES                   \ byte fixture for the offset rows
4 PTR-U8-TABLE FIX-PTRS                 \ pointer table for the `ptr-field` row
create COVERED PRIM-SPEC:COUNT allot
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

: SUBJ$ ( -- str )
   SUBJ-BUF SUBJ-U @ >str ;

: SUBJ! ( str -- ) {: s:str :}
   s STR:LEN SUBJ-CAP > IF s" prim-parity: primitive name too long" EXIT-STATUS:FAILURE die THEN
   s STR:DATA SUBJ-BUF s STR:LEN BYTE-COPY
   s STR:LEN SUBJ-U ! ;

: FIND-ROW ( str n -- n ) {: s:str overload:n :}
   0
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:NAME$ s STR:= if
         dup overload = if drop i unloop exit then
         1+
      then
   loop drop -1 ;

: NO-ARM ( str str -- ) {: ks:str ns:str :}
   s" prim-parity: no " type ks type s"  arm for " type ns type cr
   s" prim-parity: unwired primitive" EXIT-STATUS:FAILURE die ;

: LBL ( str -- ) {: ts:str :}   \ "<subject> <tail>" as the failure label
   SUBJ-U @ ts STR:LEN + 1+ LBL-CAP > IF s" prim-parity: label too long" EXIT-STATUS:FAILURE die THEN
   SUBJ-BUF LBL-BUF SUBJ-U @ BYTE-COPY
   $20 LBL-BUF SUBJ-U @ + c!
   ts STR:DATA LBL-BUF SUBJ-U @ + 1+ ts STR:LEN BYTE-COPY
   SUBJ-U @ ts STR:LEN + 1+ LBL-U !
   LBL-BUF LBL-U @ >str T-LABEL ;

: LBL-PRIM ( -- )
   s" primitive" LBL ;

: LBL-REF ( -- )
   s" reference" LBL ;

: REF? ( -- bool )
   SUBJ-ROW @ PRIM-SPEC:REF$ STR:LEN 0= 0= ;

: CASE+ ( -- )
   SUBJ-N @ 1+ SUBJ-N ! ;

\ The sealed engine cannot execute a computed name (no xt table, `evaluate` or
\ dynamic `execute`), so each shape's dispatcher spells every primitive in one
\ arm; a subject with no arm, or a `REF` the mirror dispatcher does not answer,
\ dies naming itself.
\ Sentinels 1 2 3 4 in; the case is the digit spelling of the whole window left,
\ untouched cells included (`swap` 1243, `2over` 123412, `drop` 123), so a
\ shuffler that reaches too deep fails.
: SHUF-PRIM ( str -- n ) {: ns:str :}
   ns s" dup"   STR:= IF 1 2 3 4 dup   ENC5 EXIT THEN
   ns s" drop"  STR:= IF 1 2 3 4 drop  ENC3 EXIT THEN
   ns s" swap"  STR:= IF 1 2 3 4 swap  ENC4 EXIT THEN
   ns s" over"  STR:= IF 1 2 3 4 over  ENC5 EXIT THEN
   ns s" nip"   STR:= IF 1 2 3 4 nip   ENC3 EXIT THEN
   ns s" tuck"  STR:= IF 1 2 3 4 tuck  ENC5 EXIT THEN
   ns s" rot"   STR:= IF 1 2 3 4 rot   ENC4 EXIT THEN
   ns s" -rot"  STR:= IF 1 2 3 4 -rot  ENC4 EXIT THEN
   ns s" 2dup"  STR:= IF 1 2 3 4 2dup  ENC6 EXIT THEN
   ns s" 2drop" STR:= IF 1 2 3 4 2drop ENC2 EXIT THEN
   ns s" 2swap" STR:= IF 1 2 3 4 2swap ENC4 EXIT THEN
   ns s" 2over" STR:= IF 1 2 3 4 2over ENC6 EXIT THEN
   s" shuffler" ns NO-ARM ;

: SHUF-REF ( str -- n ) {: ns:str :}
   ns s" dup"   STR:= IF 1 2 3 4 PRIM-REF:S-DUP   ENC5 EXIT THEN
   ns s" drop"  STR:= IF 1 2 3 4 PRIM-REF:S-DROP  ENC3 EXIT THEN
   ns s" swap"  STR:= IF 1 2 3 4 PRIM-REF:S-SWAP  ENC4 EXIT THEN
   ns s" over"  STR:= IF 1 2 3 4 PRIM-REF:S-OVER  ENC5 EXIT THEN
   ns s" nip"   STR:= IF 1 2 3 4 PRIM-REF:S-NIP   ENC3 EXIT THEN
   ns s" tuck"  STR:= IF 1 2 3 4 PRIM-REF:S-TUCK  ENC5 EXIT THEN
   ns s" rot"   STR:= IF 1 2 3 4 PRIM-REF:S-ROT   ENC4 EXIT THEN
   ns s" -rot"  STR:= IF 1 2 3 4 PRIM-REF:S-UNROT ENC4 EXIT THEN
   ns s" 2dup"  STR:= IF 1 2 3 4 PRIM-REF:S-2DUP  ENC6 EXIT THEN
   ns s" 2drop" STR:= IF 1 2 3 4 PRIM-REF:S-2DROP ENC2 EXIT THEN
   ns s" 2swap" STR:= IF 1 2 3 4 PRIM-REF:S-2SWAP ENC4 EXIT THEN
   ns s" 2over" STR:= IF 1 2 3 4 PRIM-REF:S-2OVER ENC6 EXIT THEN
   s" shuffler reference" ns NO-ARM ;

: NN-N-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" +"      STR:= IF a b +      EXIT THEN
   ns s" -"      STR:= IF a b -      EXIT THEN
   ns s" *"      STR:= IF a b *      EXIT THEN
   ns s" /"      STR:= IF a b /      EXIT THEN
   ns s" mod"    STR:= IF a b mod    EXIT THEN
   ns s" and"    STR:= IF a b and    EXIT THEN
   ns s" or"     STR:= IF a b or     EXIT THEN
   ns s" xor"    STR:= IF a b xor    EXIT THEN
   ns s" min"    STR:= IF a b min    EXIT THEN
   ns s" max"    STR:= IF a b max    EXIT THEN
   ns s" lshift" STR:= IF a b lshift EXIT THEN
   ns s" rshift" STR:= IF a b rshift EXIT THEN
   ns s" f+"     STR:= IF a s>f b s>f f+ f>s EXIT THEN
   ns s" f-"     STR:= IF a s>f b s>f f- f>s EXIT THEN
   ns s" f*"     STR:= IF a s>f b s>f f* f>s EXIT THEN
   ns s" f/"     STR:= IF a s>f b s>f f/ f>s EXIT THEN
   s" binary" ns NO-ARM ;

: NN-N-REF ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" +"      STR:= IF a b PRIM-REF:ADD     EXIT THEN
   ns s" -"      STR:= IF a b PRIM-REF:SUB     EXIT THEN
   ns s" *"      STR:= IF a b PRIM-REF:MUL     EXIT THEN
   ns s" /"      STR:= IF a b PRIM-REF:DIV     EXIT THEN
   ns s" mod"    STR:= IF a b PRIM-REF:REM     EXIT THEN
   ns s" and"    STR:= IF a b PRIM-REF:BAND    EXIT THEN
   ns s" or"     STR:= IF a b PRIM-REF:BOR     EXIT THEN
   ns s" xor"    STR:= IF a b PRIM-REF:BXOR    EXIT THEN
   ns s" min"    STR:= IF a b PRIM-REF:MINIMUM EXIT THEN
   ns s" max"    STR:= IF a b PRIM-REF:MAXIMUM EXIT THEN
   ns s" lshift" STR:= IF a b PRIM-REF:SHL     EXIT THEN
   ns s" rshift" STR:= IF a b PRIM-REF:SHR     EXIT THEN
   s" binary reference" ns NO-ARM ;

: N-N-PRIM ( n str -- n ) {: a:n ns:str :}
   ns s" 1+"      STR:= IF a 1+     EXIT THEN
   ns s" 1-"      STR:= IF a 1-     EXIT THEN
   ns s" negate"  STR:= IF a negate EXIT THEN
   ns s" invert"  STR:= IF a invert EXIT THEN
   ns s" abs"     STR:= IF a abs    EXIT THEN
   ns s" cells"   STR:= IF a cells  EXIT THEN
   ns s" chars"   STR:= IF a chars  EXIT THEN
   ns s" cell+"   STR:= IF a cell+  EXIT THEN
   ns s" char+"   STR:= IF a char+  EXIT THEN
   ns s" fnegate" STR:= IF a s>f fnegate f>s EXIT THEN
   ns s" fabs"    STR:= IF a s>f fabs    f>s EXIT THEN
   ns s" fsqrt"   STR:= IF a s>f fsqrt   f>s EXIT THEN
   ns s" s>f"     STR:= IF a s>f f>s EXIT THEN
   ns s" f>s"     STR:= IF a s>f f>s EXIT THEN
   s" unary" ns NO-ARM ;

: N-N-REF ( n str -- n ) {: a:n ns:str :}
   ns s" 1+"     STR:= IF a PRIM-REF:INC        EXIT THEN
   ns s" 1-"     STR:= IF a PRIM-REF:DEC        EXIT THEN
   ns s" negate" STR:= IF a PRIM-REF:NEG        EXIT THEN
   ns s" invert" STR:= IF a PRIM-REF:BNOT       EXIT THEN
   ns s" abs"    STR:= IF a PRIM-REF:ABSOLUTE   EXIT THEN
   ns s" cells"  STR:= IF a PRIM-REF:CELL-BYTES EXIT THEN
   ns s" chars"  STR:= IF a PRIM-REF:CHAR-BYTES EXIT THEN
   ns s" cell+"  STR:= IF a PRIM-REF:CELL-STEP  EXIT THEN
   ns s" char+"  STR:= IF a PRIM-REF:CHAR-STEP  EXIT THEN
   s" unary reference" ns NO-ARM ;

: NN-F-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" ="   STR:= IF a b =  B>N EXIT THEN
   ns s" <>"  STR:= IF a b <> B>N EXIT THEN
   ns s" <"   STR:= IF a b <  B>N EXIT THEN
   ns s" >"   STR:= IF a b >  B>N EXIT THEN
   ns s" <="  STR:= IF a b <= B>N EXIT THEN
   ns s" >="  STR:= IF a b >= B>N EXIT THEN
   ns s" f<"  STR:= IF a s>f b s>f f< B>N EXIT THEN
   ns s" f>"  STR:= IF a s>f b s>f f> B>N EXIT THEN
   ns s" f="  STR:= IF a s>f b s>f f= B>N EXIT THEN
   ns s" f<=" STR:= IF a s>f b s>f f<= B>N EXIT THEN
   ns s" f>=" STR:= IF a s>f b s>f f>= B>N EXIT THEN
   s" order" ns NO-ARM ;

: NN-F-REF ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" ="  STR:= IF a b PRIM-REF:EQ  B>N EXIT THEN
   ns s" <>" STR:= IF a b PRIM-REF:NEQ B>N EXIT THEN
   ns s" <"  STR:= IF a b PRIM-REF:LT  B>N EXIT THEN
   ns s" >"  STR:= IF a b PRIM-REF:GT  B>N EXIT THEN
   ns s" <=" STR:= IF a b PRIM-REF:LE  B>N EXIT THEN
   ns s" >=" STR:= IF a b PRIM-REF:GE  B>N EXIT THEN
   s" order reference" ns NO-ARM ;

: N-F-PRIM ( n str -- n ) {: a:n ns:str :}
   ns s" 0="  STR:= IF a 0= B>N EXIT THEN
   ns s" 0<"  STR:= IF a 0< B>N EXIT THEN
   ns s" f0=" STR:= IF a s>f f0= B>N EXIT THEN
   ns s" f0<" STR:= IF a s>f f0< B>N EXIT THEN
   s" predicate" ns NO-ARM ;

: N-F-REF ( n str -- n ) {: a:n ns:str :}
   ns s" 0<" STR:= IF a PRIM-REF:ZERO-NEG? B>N EXIT THEN
   s" predicate reference" ns NO-ARM ;

\ The boolean rows of `and`, `or` and `xor` are separate rows from their numeric
\ ones.
: FF-F-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" and" STR:= IF a 0= 0= b 0= 0= and B>N EXIT THEN
   ns s" or"  STR:= IF a 0= 0= b 0= 0= or  B>N EXIT THEN
   ns s" xor" STR:= IF a 0= 0= b 0= 0= xor B>N EXIT THEN
   s" flag" ns NO-ARM ;

: FF-F-REF ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" and" STR:= IF a 0= 0= b 0= 0= PRIM-REF:BAND-F B>N EXIT THEN
   ns s" or"  STR:= IF a 0= 0= b 0= 0= PRIM-REF:BOR-F  B>N EXIT THEN
   ns s" xor" STR:= IF a 0= 0= b 0= 0= PRIM-REF:BXOR-F B>N EXIT THEN
   s" flag reference" ns NO-ARM ;

: NN-NN-PRIM ( n n str -- n n ) {: a:n b:n ns:str :}
   ns s" /mod" STR:= IF a b /mod EXIT THEN
   s" divmod" ns NO-ARM ;

: NN-NN-REF ( n n str -- n n ) {: a:n b:n ns:str :}
   ns s" /mod" STR:= IF a b PRIM-REF:DIVREM EXIT THEN
   s" divmod reference" ns NO-ARM ;

\ Source has no float literals: a float case is integers the arm converts with
\ `s>f`/`f>s`, which pins presence and integral behaviour (fractional semantics:
\ src/lib/float-test.f). RR-R, RR-F and R-R cast a double's bits instead,
\ reaching infinities, NaN and signed zeros.
\ No row of these has a reference, so the shapes run the primitive alone.
: RR-R-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   a IEEE754:BITS>F64 b IEEE754:BITS>F64 {: x:r y:r :}
   ns s" f+" STR:= IF x y f+ IEEE754:F64>BITS EXIT THEN
   ns s" f-" STR:= IF x y f- IEEE754:F64>BITS EXIT THEN
   ns s" f*" STR:= IF x y f* IEEE754:F64>BITS EXIT THEN
   ns s" f/" STR:= IF x y f/ IEEE754:F64>BITS EXIT THEN
   s" binary bits" ns NO-ARM ;

: RR-F-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   a IEEE754:BITS>F64 b IEEE754:BITS>F64 {: x:r y:r :}
   ns s" f<"  STR:= IF x y f<  B>N EXIT THEN
   ns s" f>"  STR:= IF x y f>  B>N EXIT THEN
   ns s" f="  STR:= IF x y f=  B>N EXIT THEN
   ns s" f<=" STR:= IF x y f<= B>N EXIT THEN
   ns s" f>=" STR:= IF x y f>= B>N EXIT THEN
   s" order bits" ns NO-ARM ;

: R-R-PRIM ( n str -- n ) {: a:n ns:str :}
   ns s" fsqrt" STR:= IF a IEEE754:BITS>F64 fsqrt IEEE754:F64>BITS EXIT THEN
   s" unary bits" ns NO-ARM ;

\ A refusal is pinned by its code, the reference's held to the same; each arm
\ drops what the body left, so a wrong answer fails the code comparison.
: NN-THROW-PRIM ( str -- n ) {: ns:str :}
   ns s" /"    STR:= IF [: DV-A @ DV-B @ /    drop  ;] catch EXIT THEN
   ns s" mod"  STR:= IF [: DV-A @ DV-B @ mod  drop  ;] catch EXIT THEN
   ns s" /mod" STR:= IF [: DV-A @ DV-B @ /mod 2drop ;] catch EXIT THEN
   s" refusing binary" ns NO-ARM ;

: NN-THROW-REF ( str -- n ) {: ns:str :}
   ns s" /"    STR:= IF [: DV-A @ DV-B @ PRIM-REF:DIV    drop  ;] catch EXIT THEN
   ns s" mod"  STR:= IF [: DV-A @ DV-B @ PRIM-REF:REM    drop  ;] catch EXIT THEN
   ns s" /mod" STR:= IF [: DV-A @ DV-B @ PRIM-REF:DIVREM 2drop ;] catch EXIT THEN
   s" refusing binary reference" ns NO-ARM ;

\ A store/fetch pair round-trips the case value over the file's own fixtures and
\ covers both rows. `PTR>N` reads no memory: the value is a byte offset into
\ FIX-CELLS, answered back as the number less FIX-CELLS's own distance from
\ NULL-PTR.
: MEM-PRIM ( n str -- n ) {: v:n ns:str :}
   ns s" !" STR:= IF
      v FIX-CELLS ! FIX-CELLS @ EXIT THEN
   ns s" c!" STR:= IF
      v FIX-BYTES c! FIX-BYTES c@ EXIT THEN
   ns s" +!"        STR:= IF v FIX-CELLS ! BUMP FIX-CELLS +! FIX-CELLS @ EXIT THEN
   ns s" count"     STR:= IF v FIX-BYTES c! FIX-BYTES count STR:LEN EXIT THEN
   ns s" byte-view" STR:= IF v FIX-CELLS ! FIX-CELLS byte-view c@ EXIT THEN
   ns s" cell-view" STR:= IF v FIX-BYTES cell-view ! FIX-BYTES cell-view @ EXIT THEN
   ns s" PTR>N"     STR:= IF FIX-CELLS v + PTR>N  FIX-CELLS NULL-PTR - - EXIT THEN
   s" memory" ns NO-ARM ;

: MEM-REF ( n str -- n ) {: v:n ns:str :}
   ns s" +!"    STR:= IF v FIX-CELLS ! BUMP FIX-CELLS PRIM-REF:ADD-TO FIX-CELLS @ EXIT THEN
   ns s" count" STR:= IF v FIX-BYTES c! FIX-BYTES PRIM-REF:COUNTED$ STR:LEN EXIT THEN
   s" memory reference" ns NO-ARM ;

\ Pointer columns are offsets into FIX-BYTES, or into FIX-PTRS for `ptr-field`;
\ results are offsets or distances. All accesses stay within those fixtures. Each arm has the row's checked types.
: PN-P-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" +" STR:= IF FIX-BYTES a + b + FIX-BYTES - EXIT THEN
   ns s" -" STR:= IF FIX-BYTES a + b - FIX-BYTES - EXIT THEN
   ns s" ptr-field" STR:= IF FIX-PTRS a + b ptr-field NULL-PTR - FIX-PTRS NULL-PTR - - EXIT THEN
   s" pointer step" ns NO-ARM ;

: P-P-PRIM ( n str -- n ) {: a:n ns:str :}
   ns s" 1+"    STR:= IF FIX-BYTES a + 1+    FIX-BYTES - EXIT THEN
   ns s" 1-"    STR:= IF FIX-BYTES a + 1-    FIX-BYTES - EXIT THEN
   ns s" cell+" STR:= IF FIX-BYTES a + cell+ FIX-BYTES - EXIT THEN
   ns s" char+" STR:= IF FIX-BYTES a + char+ FIX-BYTES - EXIT THEN
   s" pointer unary" ns NO-ARM ;

: PP-F-PRIM ( n n str -- n ) {: a:n b:n ns:str :}
   ns s" ="  STR:= IF FIX-BYTES a + FIX-BYTES b + =  B>N EXIT THEN
   ns s" <>" STR:= IF FIX-BYTES a + FIX-BYTES b + <> B>N EXIT THEN
   ns s" <"  STR:= IF FIX-BYTES a + FIX-BYTES b + <  B>N EXIT THEN
   ns s" >"  STR:= IF FIX-BYTES a + FIX-BYTES b + >  B>N EXIT THEN
   ns s" <=" STR:= IF FIX-BYTES a + FIX-BYTES b + <= B>N EXIT THEN
   ns s" >=" STR:= IF FIX-BYTES a + FIX-BYTES b + >= B>N EXIT THEN
   s" pointer order" ns NO-ARM ;

: PTR-CASE+ ( -- )
   CASE+
   REF? IF s" pointer reference" SUBJ$ NO-ARM THEN ;

: BITS-CASE+ ( -- )
   CASE+
   REF? IF s" bits reference" SUBJ$ NO-ARM THEN ;

\ Not held here: I/O, syscall, process, code publication, profiler, engine-state
\ and FFI rows (no inputs-to-outputs case, no honest reference; several are
\ TRUSTED-only or owner-private); they are listed as uncovered.
\ Coverage is by row: closing a nonempty set marks its row; a numeric case never
\ covers a pointer or boolean sibling; REF belongs to that exact row. Uncovered
\ rows print their absolute table index and are reported, not failed.
: PRIM-ROW? ( n -- bool ) {: row:n :}
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PRIM =
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-PKG-PRIVATE = or ;

: MARK ( n -- )
   COVERED + 1 swap c! ;

: COVERED? ( n -- bool )
   COVERED swap + c@ 0= 0= ;

: MARK-PAIRED ( str -- )
   0 FIND-ROW dup 0 < IF
      s" prim-parity: missing paired primitive" EXIT-STATUS:FAILURE die
   THEN
   MARK ;

public

: CASES ( n -- ) {: overload:n :}       \ <overload> CASES <name> ... ;CASES
   parse-name {: s:str :}
   s STR:LEN 0= IF s" prim-parity: CASES needs a primitive name" EXIT-STATUS:FAILURE die THEN
   s SUBJ!
   s overload FIND-ROW SUBJ-ROW !
   SUBJ-ROW @ 0 < IF
      s" prim-parity: no table row for " type SUBJ$ type cr
      s" prim-parity: unknown primitive overload" EXIT-STATUS:FAILURE die
   THEN
   SUBJ-ROW @ PRIM-ROW? 0= IF
      s" prim-parity: not an effect row: " type SUBJ$ type cr
      s" prim-parity: wrong row kind" EXIT-STATUS:FAILURE die
   THEN
   0 SUBJ-N ! ;

: ;CASES ( -- )
   SUBJ-N @ 0= IF
      s" prim-parity: empty case set for " type SUBJ$ type cr
      s" prim-parity: empty case set" EXIT-STATUS:FAILURE die
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

: RR-F ( n n n -- ) {: a:n b:n want:n :}
   BITS-CASE+
   LBL-PRIM a b SUBJ$ RR-F-PRIM want T= ;

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
   SUBJ$ s" +" STR:= 0= IF s" number plus pointer" SUBJ$ NO-ARM THEN
   LBL-PRIM a FIX-BYTES b + + FIX-BYTES - want T= ;

: PP-N ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   SUBJ$ s" -" STR:= 0= IF s" pointer distance" SUBJ$ NO-ARM THEN
   LBL-PRIM FIX-BYTES a + FIX-BYTES b + - want T= ;

: P-P ( n n -- ) {: a:n want:n :}
   PTR-CASE+
   LBL-PRIM a SUBJ$ P-P-PRIM want T= ;

: PP-F ( n n n -- ) {: a:n b:n want:n :}
   PTR-CASE+
   LBL-PRIM a b SUBJ$ PP-F-PRIM want T= ;

private

: COLON-AT ( str -- n ) {: s:str :}   \ first colon, or -1
   0 SI !
   BEGIN SI @ s STR:LEN < WHILE
      s STR:DATA SI @ + c@ COLON-B = IF SI @ EXIT THEN
      SI @ 1+ SI !
   REPEAT
   -1 ;

\ A REF clause is a qualified name: the qualifier must be this gate's reference
\ package and the tail must be a word in it.
: REF-RESOLVED? ( n -- bool ) {: row:n :}
   row PRIM-SPEC:REF$ {: r:str :}
   r COLON-AT {: k:n :}
   k 0 < IF NO EXIT THEN
   r STR:DATA k >str s" PRIM-REF" STR:= 0= IF NO EXIT THEN
   r STR:DATA k + 1+  r STR:LEN k - 1- >str  PRIM-REF:WID search-wl 0= 0= ;

: REF-RESOLVES ( -- )
   0 CI !
   BEGIN CI @ PRIM-SPEC:COUNT < WHILE
      CI @ PRIM-SPEC:REF$ STR:LEN 0= 0= IF
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
         CI @ PRIM-SPEC:REF$ STR:LEN 0= 0= IF NREF @ 1+ NREF ! THEN
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

T-RESET

\ The coverage map marks exactly the row a set names. This runs before the
\ shared sets, which also cover `+`'s pointer rows.
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

s" test/prim-cases.f" included
s" @" MARK-PAIRED                       \ fetched by the `!` round trip
s" c@" MARK-PAIRED                      \ fetched by the `c!` round trip

s" test/prim-float-cases.f" included

REF-RESOLVES
REPORT
T-REPORT

;package
