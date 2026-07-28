\ ir-id-proof.f - the compiler identity parity gate.
\
\ It reopens `package COMPILER-ID-PROOF`, the package that already owns the
\ frozen identity schema, and binds that schema to the machine-checked model in
\ `formal/Common/`. Until this gate exists the Rocq files prove things about a
\ model and the Habu source is a separate description of the same design, and
\ nothing stops the two from drifting.
\
\ One artifact, two readers. `test/compiler/ir-id-schema.f` holds the canonical
\ bytes, their committed SHA-256 digest, and the ordered valid and hostile
\ numeric vectors. This gate runs every one of those rows through the public
\ `IR-ID` words, and hands the same rows to `test/compiler/ir-id-obligations.f`,
\ which turns them into Rocq obligations about `Habu.Common.Ids`. Neither side
\ carries a copy: change a row and both sides are asked the new question, delete
\ a row and both sides stop being asked, and change the production source and
\ Rocq is asked to prove the new constant.
\
\ What the gate refuses:
\
\   - a schema byte, through the committed digest;
\   - a vector decision, through the Habu run and the generated Rocq file, which
\     also names the exact predicate a rejected row must fail on, so the Habu
\     error code and the Rocq conjunct stay bound row by row;
\   - a Habu constant, or the exhaustion guard, or the compare-and-swap operands
\     and success test, or the retry result, through the frozen bodies read
\     structurally out of `src/compiler/ir/id.f` - the three module-serial
\     guards cannot be driven by any number from outside `IR-ID`, so this is how
\     they are bound;
\   - a scalar packing, through the round-trip rows and their Rocq twins;
\   - a checker rejection, through the wrong-family candidates and the matching
\     Rocq `Fail` commands;
\   - a replay reset, through a real child load of the replay fixture;
\   - an unexpected or missing assumption, an unbound theorem, or an `Admitted`,
\     through the committed manifest compared as a whole and the structural
\     inventory of what the four proof files actually declare;
\   - a rewritten theorem statement, through the statement each manifest row
\     pins. The row's type is ascribed to a generated definition whose body is
\     the theorem, so Rocq itself has to accept the proved statement as the one
\     the committed manifest wrote down. Nothing reads a statement out of a
\     proof file, so editing a proof file cannot move its own target. A
\     manifest that names a theorem and leaves its statement unwritten is
\     refused when the manifest is read, before anything is asked of Rocq.
\
\ Focused command: compile the four files under `formal/Common`, then
\ `bin/hb --load test/compiler/ir-id-proof.f`. The gate compiles them itself, so
\ the second half alone is enough.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/outcome.f
require lib/process-command.f
require test/checker-assert.f
require test/compiler/ir-id-obligations.f

package COMPILER-ID-PROOF
private

$8000 constant AX-CAP
$4000 constant TY-CAP
$1000 constant REP-CAP
$100 constant PATH-CAP
128 constant TH-MAX
8 constant AXIOM-MAX
4 constant PROOF-FILES
300000 constant ROCQ-MS
60000 constant HB-MS
$0A constant LF
$23 constant HASH

create AX-RAW AX-CAP allot
create AX-WANT AX-CAP allot
create AX-GOT AX-CAP allot
create TY-RAW TY-CAP allot
create REP-WANT REP-CAP allot
create REP-GOT REP-CAP allot
create TH-OFF TH-MAX cells allot
create TH-LEN TH-MAX cells allot
create TYPE-OFF TH-MAX cells allot
create TYPE-LEN TH-MAX cells allot
create AXIOM-OFF AXIOM-MAX cells allot
create AXIOM-LEN AXIOM-MAX cells allot
create VPATH PATH-CAP allot

variable AX-RAW-U
variable AX-WANT-U
variable AX-GOT-U
variable TY-U
variable REP-WANT-U
variable REP-GOT-U
variable VPATH-U
variable TH-N
variable TYPE-N
variable PEND
variable AXIOM-N
variable CLOSED-N
variable BEARING-N
variable ADMITTED-N
variable DECL-N
variable CUR
variable SRC-SERIAL-MAX
variable SRC-LOCAL-MAX
variable SRC-LOCAL-BITS
variable SRC-INIT

\ ---- byte buffers ------------------------------------------------------------

: SINK+ ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr srcu:n dst:ptr cap:n lenv:ptr :}
   lenv @ srcu + cap > if E-STR-CAPACITY throw then
   src dst lenv @ + srcu BYTE-COPY
   lenv @ srcu + lenv ! ;

: SINK-C+ ( n ptr u8 n ptr a -- ) {: c:n dst:ptr cap:n lenv:ptr :}
   lenv @ 1+ cap > if E-STR-CAPACITY throw then
   c dst lenv @ + c!
   lenv @ 1+ lenv ! ;

: WANT+ ( ptr u8 n -- )
   AX-WANT AX-CAP AX-WANT-U SINK+ ;

: WANT-NL ( -- )
   LF AX-WANT AX-CAP AX-WANT-U SINK-C+ ;

: GOT+ ( ptr u8 n -- )
   AX-GOT AX-CAP AX-GOT-U SINK+ ;

: TY+ ( ptr u8 n -- )
   TY-RAW TY-CAP TY-U SINK+ ;

: GOT-NL ( -- )
   LF AX-GOT AX-CAP AX-GOT-U SINK-C+ ;

: REP+ ( ptr u8 n -- )
   REP-GOT REP-CAP REP-GOT-U SINK+ ;

: REP-NL ( -- )
   LF REP-GOT REP-CAP REP-GOT-U SINK-C+ ;

: REP-N+ ( n -- )
   SB-RESET FMT:SB-INT SB$ REP+ ;

\ ---- lines -------------------------------------------------------------------

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n off:n :}
   off
   begin dup u < while
      dup a + c@ LF = if exit then
      1+
   repeat ;

: TRIM-LEAD ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   begin dup u < while
      dup a + c@ $20 <> if exit then
      1+
   repeat ;

: TRIM-TAIL ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   u
   begin dup start > while
      dup 1- a + c@ $20 <> if exit then
      1-
   repeat ;

: TRIMMED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u TRIM-LEAD {: start:n :}
   a start +
   a u start TRIM-TAIL start - ;

\ ---- the committed manifest: statements and assumptions ----------------------

: MANIFEST-PATH$ ( -- ptr u8 n )
   s" test/compiler/ir-id-axioms.txt" ;

: COMMENT-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if true exit then
   a c@ HASH = ;

\ The two row tags, and the widths that skip them, from one text each, so a row
\ cannot be recognised by one spelling and stripped by another.
: THEOREM-TAG$ ( -- ptr u8 n )
   s" theorem " ;

: TYPE-TAG$ ( -- ptr u8 n )
   s" type " ;

: THEOREM-TAG ( -- n )
   THEOREM-TAG$ nip ;

: TYPE-TAG ( -- n )
   TYPE-TAG$ nip ;

: THEOREM-LINE? ( ptr u8 n -- bool )
   THEOREM-TAG$ STARTS-WITH? ;

: TYPE-LINE? ( ptr u8 n -- bool )
   TYPE-TAG$ STARTS-WITH? ;

: THEOREM+ ( n n -- ) {: off:n u:n :}
   TH-N @ TH-MAX >= if E-CID-AXIOM throw then
   off TH-N @ cells TH-OFF + !
   u TH-N @ cells TH-LEN + !
   TH-N @ 1+ TH-N ! ;

\ The statement the manifest says the pending theorem makes. `PEND` carries the
\ index of the theorem still waiting for its statement, and clearing it is what
\ makes a statement belong to the theorem row directly above it: a `type` row
\ with no theorem waiting, which covers both an orphan row and a second row for
\ one theorem, is refused here.
: TYPE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 < if E-CID-AXIOM throw then
   TY-U @ PEND @ cells TYPE-OFF + !
   u PEND @ cells TYPE-LEN + !
   a u TY+
   TYPE-N @ 1+ TYPE-N !
   -1 PEND ! ;

\ A manifest row that names a theorem also records where its name sits in the
\ stripped body, so the generated Rocq file asks for exactly these names in
\ exactly this order, and opens the requirement that the next row pin what that
\ theorem states.
: MANIFEST-THEOREM ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 >= if E-CID-AXIOM throw then
   a u WANT+ WANT-NL
   AX-WANT-U @ u 1+ - THEOREM-TAG + u THEOREM-TAG - THEOREM+
   TH-N @ 1- PEND ! ;

\ Every other row is an assumption row, compared byte for byte against what Rocq
\ reported. One may not stand between a theorem row and its type row, so a
\ theorem whose statement is left unpinned fails the read rather than the
\ comparison.
: MANIFEST-ASSUMPTION ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND @ 0 >= if E-CID-AXIOM throw then
   a u WANT+ WANT-NL ;

: MANIFEST-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u COMMENT-LINE? if exit then
   a u TYPE-LINE? if a TYPE-TAG + u TYPE-TAG - TYPE+ exit then
   a u THEOREM-LINE? if a u MANIFEST-THEOREM exit then
   a u MANIFEST-ASSUMPTION ;

: MANIFEST-RESET ( -- )
   0 AX-WANT-U !
   0 TY-U !
   0 TH-N !
   0 TYPE-N !
   -1 PEND ! ;

\ The manifest grammar, over bytes rather than over the committed file, so the
\ shape rules above can be shown to refuse a malformed manifest.
: MANIFEST-TEXT ( ptr u8 n -- ) {: a:ptr u:n :}
   MANIFEST-RESET
   0 CUR !
   begin CUR @ u < while
      a u CUR @ LINE-END {: stop:n :}
      a CUR @ + stop CUR @ - MANIFEST-LINE
      stop 1+ CUR !
   repeat
   PEND @ 0 >= if E-CID-AXIOM throw then ;

: READ-MANIFEST ( -- )
   MANIFEST-PATH$ 2dup FILE-SIZE {: a:ptr u:n size:n :}
   size AX-CAP > if E-STR-CAPACITY throw then
   a u AX-RAW AX-CAP READ-ALL AX-RAW-U !
   AX-RAW AX-RAW-U @ MANIFEST-TEXT ;

: THEOREM$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k TH-N @ >= or if E-CID-AXIOM throw then
   AX-WANT k cells TH-OFF + @ + k cells TH-LEN + @ ;

: TYPE$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k TYPE-N @ >= or if E-CID-AXIOM throw then
   TY-RAW k cells TYPE-OFF + @ + k cells TYPE-LEN + @ ;

\ ---- rendering what Rocq answered --------------------------------------------

: AXIOM-DISTINCT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   AXIOM-N @ 0 ?do
      AX-GOT i cells AXIOM-OFF + @ + i cells AXIOM-LEN + @ a u STR= if
         false unloop exit
      then
   loop
   true ;

: AXIOM-KEEP ( n n -- ) {: off:n u:n :}
   AX-GOT off + u AXIOM-DISTINCT? 0= if exit then
   AXIOM-N @ AXIOM-MAX >= if E-CID-AXIOM throw then
   off AXIOM-N @ cells AXIOM-OFF + !
   u AXIOM-N @ cells AXIOM-LEN + !
   AXIOM-N @ 1+ AXIOM-N ! ;

: RENDER-MARKER ( ptr u8 n -- ) {: a:ptr u:n :}
   s" theorem " GOT+
   a 11 + u 11 - GOT+
   GOT-NL ;

: RENDER-AXIOM ( ptr u8 n -- ) {: a:ptr u:n :}
   s" axiom " GOT+
   AX-GOT AX-GOT-U @ + {: at:ptr :}
   a u GOT+
   GOT-NL
   at AX-GOT - u AXIOM-KEEP ;

: RENDER-LINE ( ptr u8 n -- ) {: raw:ptr rawu:n :}
   raw rawu TRIMMED$ {: a:ptr u:n :}
   u 0= if exit then
   a u s" == theorem " STARTS-WITH? if a u RENDER-MARKER exit then
   a u s" Closed under the global context" STR= if
      s" closed" GOT+ GOT-NL
      CLOSED-N @ 1+ CLOSED-N !
      exit
   then
   a u s" Axioms:" STR= if
      BEARING-N @ 1+ BEARING-N !
      exit
   then
   a u RENDER-AXIOM ;

: RENDER ( ptr u8 n -- ) {: a:ptr u:n :}
   0 AX-GOT-U !
   0 AXIOM-N !
   0 CLOSED-N !
   0 BEARING-N !
   0 CUR !
   begin CUR @ u < while
      a u CUR @ LINE-END {: stop:n :}
      a CUR @ + stop CUR @ - RENDER-LINE
      stop 1+ CUR !
   repeat ;

\ ---- running a command -------------------------------------------------------

: ROCQ-ARGS ( -- )
   PROC-CMD:RESET
   s" rocq" >LEN PROC-CMD:ARG+
   s" compile" >LEN PROC-CMD:ARG+
   s" -q" >LEN PROC-CMD:ARG+
   s" -Q" >LEN PROC-CMD:ARG+
   s" formal" >LEN PROC-CMD:ARG+
   s" Habu" >LEN PROC-CMD:ARG+ ;

\ `/usr/bin/env` is the path lookup: the proof assistant is a toolchain
\ dependency on PATH, not a file at a fixed place in the tree.
: ROCQ-RUN ( ptr u8 n -- ) {: a:ptr u:n :}
   ROCQ-ARGS
   a u >LEN PROC-CMD:ARG+
   s" /usr/bin/env" >LEN ROCQ-MS >MS PROC-CMD:RUN-OUTCOME 0 T-OUTCOME-EXITED= ;

: PROOF-STEM$ ( n -- ptr u8 n )
   case
      0 of s" Ids" endof
      1 of s" IdAllocator" endof
      2 of s" IdLaws" endof
      3 of s" IdAllocatorLaws" endof
      E-CID-ROCQ throw
   endcase ;

: PROOF-PATH$ ( n -- ptr u8 n )
   case
      0 of s" formal/Common/Ids.v" endof
      1 of s" formal/Common/IdAllocator.v" endof
      2 of s" formal/Common/IdLaws.v" endof
      3 of s" formal/Common/IdAllocatorLaws.v" endof
      E-CID-ROCQ throw
   endcase ;

: HB-LOAD-OUTCOME ( ptr u8 n -- outcome ) {: a:ptr u:n :}
   PROC-CMD:RESET
   s" --load" >LEN PROC-CMD:ARG+
   a u >LEN PROC-CMD:ARG+
   s" bin/hb" >LEN HB-MS >MS PROC-CMD:RUN-OUTCOME ;

\ The same load path with the fixture's forced arm switched on.
: HB-FORCED-OUTCOME ( ptr u8 n -- outcome ) {: a:ptr u:n :}
   PROC-CMD:RESET
   s" HABU_IR_ID_REPLAY_FORCE" >LEN s" 1" >LEN PROC-CMD:ENV+
   s" --load" >LEN PROC-CMD:ARG+
   a u >LEN PROC-CMD:ARG+
   s" bin/hb" >LEN HB-MS >MS PROC-CMD:RUN-OUTCOME ;

;package

package COMPILER-ID-PROOF
private

\ ---- phase 1: the frozen artifact --------------------------------------------

: PHASE-DIGEST ( -- )
   BUILD
   s" the identity schema digest equals its committed freeze" T-LABEL
   DIGEST-HEX$ EXPECTED-DIGEST$ T$= ;

\ ---- phase 2: the structural reader, under hostile sources -------------------

: GUARD-BODY$ ( -- ptr u8 n )
   s" dup 0 < over MODULE-MAX >= or if E-IR-MODULE-EXHAUSTED throw then 1+" ;

: HOSTILE-COMMENTED ( -- )
   S\" \\ : SERIAL-NEXT dup 0 < over MODULE-MAX >= or ;\n: KEEP ( -- ) ;"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a line comment does not declare the guard" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:DEFS 0 T= ;

: HOSTILE-PARENS ( -- )
   S\" ( : SERIAL-NEXT dup 0 < )\n: KEEP ( -- ) ;"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a parenthesised comment does not declare the guard" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:DEFS 0 T= ;

: HOSTILE-STRING ( -- )
   S\" : DECOY ( -- )\n   s\" : SERIAL-NEXT dup 0 < over MODULE-MAX >= or if E-IR-MODULE-EXHAUSTED throw then 1+ ;\" 2drop ;"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a quoted literal does not declare the guard" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:DEFS 0 T=
   s" a quoted literal is not a token run" T-LABEL
   GUARD-BODY$ COMPILER-ID-SRC:RUNS 0 T= ;

: HOSTILE-DUPLICATE ( -- )
   S\" : SERIAL-NEXT ( n -- n ) 1+ ;\n: SERIAL-NEXT ( n -- n ) 1+ ;"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a duplicated definition is counted, not resolved" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:DEFS 2 T=
   s" a duplicated definition has no single body" T-LABEL
   [: s" SERIAL-NEXT" COMPILER-ID-SRC:BODY$ 2drop ;] E-CID-DEF TTHROWSQ ;

: HOSTILE-REORDER ( -- )
   S\" : SERIAL-NEXT ( n -- n )\n   1+ dup 0 < over MODULE-MAX >= or if E-IR-MODULE-EXHAUSTED throw then ;"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a reordered guard is not the frozen body" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:BODY$ GUARD-BODY$ T$<> ;

: HOSTILE-ROLE ( -- )
   S\" : NOT-SERIAL-NEXT ( n -- n )\n   dup 0 < over MODULE-MAX >= or if E-IR-MODULE-EXHAUSTED throw then 1+ ;\nMODULE-MAX constant OTHER-NAME"
   COMPILER-ID-SRC:SCAN-TEXT
   s" the guard under another name is not the guard" T-LABEL
   s" SERIAL-NEXT" COMPILER-ID-SRC:DEFS 0 T=
   s" a name used as a literal is not a constant" T-LABEL
   s" MODULE-MAX" COMPILER-ID-SRC:CONSTS 0 T=
   s" a missing definition has no body" T-LABEL
   [: s" SERIAL-NEXT" COMPILER-ID-SRC:BODY$ 2drop ;] E-CID-DEF TTHROWSQ ;

: HOSTILE-CONST-DUPLICATE ( -- )
   S\" $7FFFFFFF constant MODULE-MAX\n$7FFFFFFE constant MODULE-MAX"
   COMPILER-ID-SRC:SCAN-TEXT
   s" a duplicated constant has no single literal" T-LABEL
   [: s" MODULE-MAX" COMPILER-ID-SRC:CONST@ drop ;] E-CID-CONST TTHROWSQ ;

: HOSTILE-UNTERMINATED ( -- )
   s" a source with an unterminated literal is refused" T-LABEL
   [: S\" : DECOY ( -- ) s\" open ;" COMPILER-ID-SRC:SCAN-TEXT ;]
      E-CID-LEX TTHROWSQ ;

: PHASE-HOSTILE ( -- )
   HOSTILE-COMMENTED
   HOSTILE-PARENS
   HOSTILE-STRING
   HOSTILE-DUPLICATE
   HOSTILE-REORDER
   HOSTILE-ROLE
   HOSTILE-CONST-DUPLICATE
   HOSTILE-UNTERMINATED ;

\ ---- phase 3: the production source ------------------------------------------

: BODY-IS ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n want:ptr wantu:n :}
   s" a frozen identity word still has its frozen body" T-LABEL
   name nameu COMPILER-ID-SRC:BODY$ want wantu T$= ;

: FROZEN-ALLOCATOR ( -- )
   s" SERIAL-NEXT" GUARD-BODY$ BODY-IS
   s" TRY-SERIAL"
      s" NEXT-SERIAL atomic@ {: current:n :} current SERIAL-NEXT {: next:n :} current next NEXT-SERIAL atomic-cas current = if next 0 0= else 0 0 0 <> then"
      BODY-IS
   s" TAKE-SERIAL"
      s" begin TRY-SERIAL dup 0= while 2drop repeat drop"
      BODY-IS ;

: FROZEN-PACKING ( -- )
   s" KEY-SERIAL"
      s" KEY>N dup 0= if E-IR-MODULE-ZERO throw then dup 0 < over MODULE-MAX > or if E-IR-MODULE-RANGE throw then"
      BODY-IS
   s" LOCAL-CHECK"
      s" dup 0 < over LOCAL-MAX > or if E-IR-INDEX-RANGE throw then"
      BODY-IS
   s" PACK-N"
      s" {: key:IR-ID:ir-module-key local:n :} key KEY-SERIAL LOCAL-BITS lshift local LOCAL-CHECK or"
      BODY-IS
   s" OWNER-N"
      s" LOCAL-BITS rshift MINT-MODULE"
      BODY-IS
   s" LOCAL-N"
      s" LOCAL-MAX and"
      BODY-IS
   s" CHECK-N"
      s" {: key:IR-ID:ir-module-key bound:IR-ID:ir-count raw:n :} raw OWNER-N MODULE>N key KEY-SERIAL <> if E-IR-OWNER throw then raw LOCAL-N bound COUNT>N >= if E-IR-INDEX-BOUND throw then raw"
      BODY-IS ;

: FROZEN-SCALARS ( -- )
   s" MINT-COUNT"
      s" dup 0 < if E-IR-SCALAR-RANGE throw then"
      BODY-IS
   s" MINT-POOL-OFF"
      s" dup 0 < if E-IR-SCALAR-RANGE throw then"
      BODY-IS ;

\ The allocator cell is declared once, cell-aligned, and starts at a value that
\ is a legal state and never a legal serial. That whole run is frozen together,
\ so dropping the alignment prologue or moving the declaration fails here.
: FROZEN-STATE ( -- )
   s" the allocator cell is declared aligned and zeroed exactly once" T-LABEL
   s" here CELL 1- and CELL swap - CELL 1- and allot variable NEXT-SERIAL 0 NEXT-SERIAL !"
      COMPILER-ID-SRC:RUNS 1 T= ;

: SOURCE-CONSTANTS ( -- )
   s" MODULE-MAX" COMPILER-ID-SRC:CONST@ SRC-SERIAL-MAX !
   s" LOCAL-MAX" COMPILER-ID-SRC:CONST@ SRC-LOCAL-MAX !
   s" LOCAL-BITS" COMPILER-ID-SRC:CONST@ SRC-LOCAL-BITS !
   s" NEXT-SERIAL" COMPILER-ID-SRC:STORE@ SRC-INIT !
   s" the production module ceiling is the frozen serial ceiling" T-LABEL
   SRC-SERIAL-MAX @ SERIAL-MAX T=
   s" the production local ceiling is the frozen local ceiling" T-LABEL
   SRC-LOCAL-MAX @ LOCAL-MAX T=
   s" the production shift is the frozen local shift" T-LABEL
   SRC-LOCAL-BITS @ LOCAL-SHIFT T=
   s" the allocator starts below the first issuable serial" T-LABEL
   SRC-INIT @ SERIAL-MIN 1- T= ;

: PHASE-SOURCE ( -- )
   s" src/compiler/ir/id.f" COMPILER-ID-SRC:SCAN-FILE
   SOURCE-CONSTANTS
   FROZEN-ALLOCATOR
   FROZEN-PACKING
   FROZEN-SCALARS
   FROZEN-STATE ;

;package

package COMPILER-ID-PROOF
private

\ ---- phase 4: every vector row through the production words ------------------

: PACK-ROW-RUN ( n -- ) {: idx:n :}
   idx PACK-LOCAL {: local:n :}
   idx PACK-CLASS {: class:n :}
   s" pack vector row reaches its recorded decision" T-LABEL
   local PACK-OUTCOME class T=
   class 0 <> if exit then
   s" an accepted local index comes back out of the packed identity" T-LABEL
   local PACK-LOCAL-BACK local T=
   s" an accepted packed identity keeps its minting module" T-LABEL
   local PACK-OWNER-KEPT? TTRUE ;

: CHECK-ROW-RUN ( n -- ) {: idx:n :}
   s" check vector row reaches its recorded decision" T-LABEL
   idx CHECK-LOCAL idx CHECK-BOUND idx CHECK-OWNER CHECK-OUTCOME
      idx CHECK-CLASS T= ;

: SCALAR-PROJECTION ( n n -- ) {: value:n back:n :}
   s" a scalar is not its own local projection" T-LABEL
   back value LOCAL-MASK and T<>
   s" a scalar is not its own owner projection" T-LABEL
   back value LOCAL-SHIFT rshift T<> ;

: SCALAR-IDENTITY ( n -- ) {: value:n :}
   s" a count keeps the whole value" T-LABEL
   value COUNT-BACK value T=
   s" a pool offset keeps the whole value" T-LABEL
   value POOL-BACK value T=
   value LOCAL-SHIFT rshift 0= if exit then
   value value COUNT-BACK SCALAR-PROJECTION
   value value POOL-BACK SCALAR-PROJECTION ;

: SCALAR-ROW-RUN ( n -- ) {: idx:n :}
   idx SCALAR-VALUE {: value:n :}
   idx SCALAR-CLASS {: class:n :}
   s" count vector row reaches its recorded decision" T-LABEL
   value COUNT-OUTCOME class T=
   s" pool-offset vector row reaches its recorded decision" T-LABEL
   value POOL-OUTCOME class T=
   class 0 <> if exit then
   value SCALAR-IDENTITY ;

\ The frozen discriminator is the one cell where the model's arithmetic shift
\ and the engine's logical shift read different owners. It is in all three
\ tables, and all three reject it before any projection runs, so the reading
\ that differs is never taken on the production path. The generated Rocq file
\ records both readings and confines the disagreement to cells the packer
\ cannot produce.
: DISCRIMINATOR-PRESENT ( -- )
   LOCAL-MASK LOCAL-SHIFT lshift 5 or {: cell:n :}
   s" the frozen discriminator is offered to the packer and refused" T-LABEL
   cell PACK-OUTCOME E-IR-INDEX-RANGE T=
   s" the frozen discriminator is offered as a bound and refused" T-LABEL
   42 cell OWNER-SAME CHECK-OUTCOME E-IR-SCALAR-RANGE T=
   s" the frozen discriminator is offered as a scalar and refused" T-LABEL
   cell COUNT-OUTCOME E-IR-SCALAR-RANGE T=
   cell POOL-OUTCOME E-IR-SCALAR-RANGE T=
   s" the logical reading of that cell is above the serial ceiling" T-LABEL
   cell LOCAL-SHIFT rshift SERIAL-MAX > TTRUE ;

: PHASE-VECTORS ( -- )
   PACK-ROWS 0 ?do i PACK-ROW-RUN loop
   CHECK-ROWS 0 ?do i CHECK-ROW-RUN loop
   SCALAR-ROWS 0 ?do i SCALAR-ROW-RUN loop
   DISCRIMINATOR-PRESENT ;

\ ---- phase 5: the checker's own family distinction ---------------------------

\ Each candidate below has a `Fail Definition` twin in the generated Rocq file.
\ The two module-key candidates have no Rocq twin, because the model exposes its
\ constructors and only Habu seals them; those stay owned by
\ `test/compiler/ir-id.f`.
: REFUSED ( ptr u8 n -- )
   s" the checker refuses a wrong-family identity use" T-LABEL
   CHECK-QUIET-CANDIDATE! 0 T= ;

: PHASE-WRONG-FAMILY ( -- )
   s" IR-PARITY-BAD-SRC ( IR-ID:ir-module-key n -- IR-ID:ir-fun-id ) IR-ID:PACK-SOURCE"
      REFUSED
   s" IR-PARITY-BAD-FN ( IR-ID:ir-module-key n -- IR-ID:ir-source-id ) IR-ID:PACK-FUN"
      REFUSED
   s" IR-PARITY-BAD-OWNER ( IR-ID:ir-source-id -- IR-ID:ir-count ) IR-ID:SOURCE-OWNER"
      REFUSED
   s" IR-PARITY-BAD-LOCAL ( IR-ID:ir-fun-id -- n ) IR-ID:SOURCE-LOCAL"
      REFUSED
   s" IR-PARITY-BAD-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-fun-id -- IR-ID:ir-fun-id ) IR-ID:SOURCE-CHECK"
      REFUSED ;

;package

package COMPILER-ID-PROOF
private

\ ---- phase 6: the manifest's own shape, under hostile manifests --------------

\ A theorem row and its type row are the pin. If a manifest could name a theorem
\ and leave its statement unwritten, or write a statement no theorem row claims,
\ the pin would be optional and a row could quietly lose it. These fixtures are
\ how that is refused rather than assumed.

: HOSTILE-TYPE-ORPHAN ( -- )
   s" a statement with no theorem row above it is refused" T-LABEL
   [: S\" type cell_min = -9223372036854775808\nclosed" MANIFEST-TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-TYPE-TWICE ( -- )
   s" a second statement for one theorem is refused" T-LABEL
   [: S\" theorem Habu.Common.Ids.cell_min_exact\ntype cell_min = 1\ntype cell_min = 2\nclosed"
      MANIFEST-TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

\ The statement belongs between the theorem row and its assumption rows. A
\ manifest that writes it after them still pins every theorem exactly once, so
\ only the row order refuses it, and this is the case that says the order is a
\ rule rather than a habit.
: HOSTILE-TYPE-LATE ( -- )
   s" a statement written after its assumption rows is refused" T-LABEL
   [: S\" theorem Habu.Common.Ids.cell_min_exact\nclosed\ntype cell_min = -9223372036854775808"
      MANIFEST-TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-TYPE-TRAILING ( -- )
   s" a theorem left unpinned at the end of the file is refused" T-LABEL
   [: S\" theorem Habu.Common.Ids.cell_min_exact" MANIFEST-TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-THEOREM-UNPINNED ( -- )
   s" a second theorem row before the first is pinned is refused" T-LABEL
   [: S\" theorem Habu.Common.Ids.cell_min_exact\ntheorem Habu.Common.Ids.cell_max_exact\ntype cell_max = 1\nclosed"
      MANIFEST-TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

\ Commentary may sit between a theorem row and its type row; it is commentary
\ everywhere else in the file, and this says so on purpose rather than by
\ accident.
: MANIFEST-PAIR ( -- )
   S\" theorem Habu.Common.Ids.cell_min_exact\n# a note\ntype cell_min = -9223372036854775808\nclosed"
   MANIFEST-TEXT
   s" a well-formed row names one theorem" T-LABEL
   TH-N @ 1 T=
   s" a well-formed row pins one statement" T-LABEL
   TYPE-N @ 1 T=
   s" the theorem row still answers with its qualified name" T-LABEL
   0 THEOREM$ s" Habu.Common.Ids.cell_min_exact" T$=
   s" the pinned statement is exactly the type row's text" T-LABEL
   0 TYPE$ s" cell_min = -9223372036854775808" T$= ;

: PHASE-MANIFEST ( -- )
   HOSTILE-TYPE-ORPHAN
   HOSTILE-TYPE-TWICE
   HOSTILE-TYPE-LATE
   HOSTILE-TYPE-TRAILING
   HOSTILE-THEOREM-UNPINNED
   MANIFEST-PAIR ;

\ ---- phase 7: what the proof files actually declare --------------------------

: DECL-HEAD? ( n -- bool ) {: k:n :}
   k COMPILER-ID-SRC:TOKEN$ s" Theorem" STR= if true exit then
   k COMPILER-ID-SRC:TOKEN$ s" Lemma" STR= if true exit then
   k COMPILER-ID-SRC:TOKEN$ s" Example" STR= ;

: ADMITTED-TOKEN? ( n -- bool ) {: k:n :}
   k COMPILER-ID-SRC:TOKEN$ s" Admitted" STR= if true exit then
   k COMPILER-ID-SRC:TOKEN$ s" Admitted." STR= if true exit then
   k COMPILER-ID-SRC:TOKEN$ s" admit" STR= ;

: DECL-QUALIFIED$ ( n n -- ptr u8 n ) {: file:n k:n :}
   SB-RESET
   s" Habu.Common." SB-APPEND
   file PROOF-STEM$ SB-APPEND
   s" ." SB-APPEND
   k 1+ COMPILER-ID-SRC:TOKEN$ SB-APPEND
   SB$ ;

: DECL-ROW ( n n -- ) {: file:n k:n :}
   s" every proved statement is bound by the committed manifest" T-LABEL
   DECL-N @ TH-N @ < TTRUE
   DECL-N @ TH-N @ < if
      s" the manifest binds that statement, at that position" T-LABEL
      file k DECL-QUALIFIED$ DECL-N @ THEOREM$ T$=
   then
   DECL-N @ 1+ DECL-N ! ;

: DECL-FILE ( n -- ) {: file:n :}
   file PROOF-PATH$ COMPILER-ID-SRC:SCAN-FILE
   COMPILER-ID-SRC:TOKENS 0 ?do
      i ADMITTED-TOKEN? if ADMITTED-N @ 1+ ADMITTED-N ! then
      i DECL-HEAD? if file i DECL-ROW then
   loop ;

: PHASE-DECLARATIONS ( -- )
   READ-MANIFEST
   0 DECL-N !
   0 ADMITTED-N !
   PROOF-FILES 0 ?do i DECL-FILE loop
   s" the manifest names every proved statement and no others" T-LABEL
   DECL-N @ TH-N @ T=
   s" every named statement also has its type pinned" T-LABEL
   TYPE-N @ TH-N @ T=
   s" no statement in the identity proofs is admitted" T-LABEL
   ADMITTED-N @ 0 T= ;

;package

package COMPILER-ID-PROOF
private

\ ---- phase 8: making Rocq answer ---------------------------------------------

: SCRATCH-DIR ( -- ptr u8 n )
   s" habu-ir-id-parity" TMPDIR-MKDIR ;

\ What one manifest row asks Rocq: prove that this name still states this, and
\ then report what that proof rests on.
: OBLIGATION-ROW ( n -- ) {: k:n :}
   k THEOREM$ k TYPE$ COMPILER-ID-ROCQ:STATEMENT+
   k THEOREM$ COMPILER-ID-ROCQ:ASSUMPTION+ ;

: EMIT-OBLIGATIONS ( -- )
   SRC-SERIAL-MAX @ SRC-LOCAL-MAX @ SRC-LOCAL-BITS @ SRC-INIT @
      COMPILER-ID-ROCQ:SOURCE-FACTS!
   COMPILER-ID-ROCQ:START
   TH-N @ 0 ?do i OBLIGATION-ROW loop ;

: WRITE-OBLIGATIONS ( ptr u8 n -- ) {: dir:ptr diru:n :}
   dir diru s" IdParity.v" VPATH JOIN-PATH VPATH-U !
   VPATH VPATH-U @ COMPILER-ID-ROCQ:TEXT$ ATOMIC-WRITE-FILE ;

: PHASE-ROCQ ( -- )
   s" every identity proof file compiles" T-LABEL
   PROOF-FILES 0 ?do i PROOF-PATH$ ROCQ-RUN loop
   EMIT-OBLIGATIONS
   CLEANUP-RESET
   SCRATCH-DIR 2dup CLEANUP-TREE+ WRITE-OBLIGATIONS
   s" every generated parity obligation is proved" T-LABEL
   VPATH VPATH-U @ ROCQ-RUN
   PROC-CMD:OUT$ RENDER
   CLEANUP-RUN ;

\ ---- phase 9: the assumption set ---------------------------------------------

: PHASE-ASSUMPTIONS ( -- )
   s" the reported assumption set is the committed manifest, entire" T-LABEL
   AX-GOT AX-GOT-U @ AX-WANT AX-WANT-U @ T$= ;

;package

package COMPILER-ID-PROOF
private

\ ---- phase 10: the committed report ------------------------------------------

: REPORT-PATH$ ( -- ptr u8 n )
   s" docs/compiler-id-assumptions.md" ;

: REPORT-AXIOM-ROW ( n -- ) {: k:n :}
   s" - " REP+
   AX-GOT k cells AXIOM-OFF + @ + k cells AXIOM-LEN + @ REP+
   REP-NL ;

: REPORT-COUNT ( ptr u8 n n -- ) {: label:ptr labelu:n value:n :}
   label labelu REP+ value REP-N+ REP-NL ;

: BUILD-REPORT ( -- )
   0 REP-GOT-U !
   s" # Compiler identity: what the proofs rest on" REP+ REP-NL
   REP-NL
   s" Regenerated by `bin/hb --load test/compiler/ir-id-proof.f` and committed," REP+ REP-NL
   s" so a change in what the machine-checked identity proofs assume shows up as" REP+ REP-NL
   s" a diff to this file. The gate rebuilds it and compares it byte for byte; a" REP+ REP-NL
   s" run that disagrees with this file fails." REP+ REP-NL
   REP-NL
   s" Frozen schema digest: " REP+ DIGEST-HEX$ REP+ REP-NL
   REP-NL
   s" Statements bound by name and pinned type: " TH-N @ REPORT-COUNT
   s" Closed under the global context: " CLOSED-N @ REPORT-COUNT
   s" Resting on an external assumption: " BEARING-N @ REPORT-COUNT
   s" Admitted: " ADMITTED-N @ REPORT-COUNT
   REP-NL
   s" The whole external assumption set, and nothing else:" REP+ REP-NL
   REP-NL
   AXIOM-N @ 0 ?do i REPORT-AXIOM-ROW loop
   REP-NL
   s" `host_atomic_cas` is the host compare-and-swap the module allocator calls," REP+ REP-NL
   s" left as a parameter rather than modelled. `atomic_cas_linearizable` is its" REP+ REP-NL
   s" one law: a call behaves as the atomic transition at its linearization" REP+ REP-NL
   s" point. Every `host_*` refinement theorem is proved from that law alone, so" REP+ REP-NL
   s" nothing else about the host operation is assumed anywhere. The exact" REP+ REP-NL
   s" per-statement expectation lives in `test/compiler/ir-id-axioms.txt`; this" REP+ REP-NL
   s" file is the reviewed summary of it." REP+ REP-NL
   REP-NL
   s" That file also carries a written copy of what each bound statement says," REP+ REP-NL
   s" and the gate makes Rocq accept every proved statement at exactly that" REP+ REP-NL
   s" type, so a theorem cannot be rewritten to say less while keeping its name" REP+ REP-NL
   s" and its assumption set." REP+ REP-NL ;

: READ-REPORT ( -- )
   REPORT-PATH$ 2dup FILE-SIZE {: a:ptr u:n size:n :}
   size REP-CAP > if E-STR-CAPACITY throw then
   a u REP-WANT REP-CAP READ-ALL REP-WANT-U ! ;

: PHASE-REPORT ( -- )
   BUILD-REPORT
   READ-REPORT
   s" the committed assumptions report is what this run produces" T-LABEL
   REP-GOT REP-GOT-U @ REP-WANT REP-WANT-U @ T$= ;

\ ---- phase 11: the replay fixture --------------------------------------------

: PHASE-REPLAY ( -- )
   s" the module allocator survives a require replay in a child load" T-LABEL
   s" test/compiler/ir-id-replay.f" HB-LOAD-OUTCOME 0 T-OUTCOME-EXITED=
   s" a forced replay of the identity source is refused by the seal" T-LABEL
   s" test/compiler/ir-id-replay.f" HB-FORCED-OUTCOME
      ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED= ;

public

: RUN ( -- )
   T-RESET
   PHASE-DIGEST
   PHASE-HOSTILE
   PHASE-SOURCE
   PHASE-VECTORS
   PHASE-WRONG-FAMILY
   PHASE-MANIFEST
   PHASE-DECLARATIONS
   PHASE-ROCQ
   PHASE-ASSUMPTIONS
   PHASE-REPORT
   PHASE-REPLAY
   T-REPORT ;

;package

COMPILER-ID-PROOF:RUN
