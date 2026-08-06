\ ir-intern-cases.f - the Habu half of the interning parity binding.
\
\ The module lives in `package COMPILER-INTERN-CASES`. It takes the frozen rows
\ in `package COMPILER-INTERN-PROOF` and asks the shipped compiler about them,
\ in four groups:
\
\   - the compared-field list of every scan predicate, read structurally out of
\     the production source rather than matched as text. A predicate reads a
\     stored cell as `<offset> RC@`, so the reader collects the token before
\     every `RC@` with the operator three tokens on. Add, drop, reorder, or stop
\     comparing a field and the extracted list stops equalling the frozen one,
\     which is the only place the meaning of "the same key" is written down.
\
\   - the check-before-write ordering of the four intern paths. The reader first
\     classifies every definition in the owning file: a definition WRITES if it
\     carries the arena push token or calls a writer, and it CHECKS CAPACITY if
\     it carries the committed-capacity token or calls a capacity check. Both
\     relations are closed under calls, because `IR-SYM:INTERN` pushes two calls
\     deep and would otherwise contain no push token at all and pass vacuously.
\     Each intern body must then contain at least one writer, at least one
\     capacity check, and no capacity check after its first writer.
\
\   - the reference guards, frozen whole. They are what makes the model's
\     `refs_ok` hypothesis true of the shipped code: a pointer's pointee and
\     every staged function element must already be a constructed row of this
\     same table, so a reference always points strictly backwards.
\
\   - every intern sequence, run through the real `IR-SYM`, `IR-TYPE` and
\     `IR-ATTR` words. The same rows become Rocq obligations in
\     `test/compiler/ir-intern-obligations.f`; this file never restates them.
\
\ Consumers: `test/compiler/ir-intern-manifest.f` (these four groups alone) and
\ `test/compiler/ir-intern-proof.f` (these four plus the Rocq half).

require lib/test.f
require lib/string.f
require test/compiler/ir-id-source.f
require test/compiler/ir-intern-schema.f

package COMPILER-INTERN-CASES
using COMPILER-INTERN-PROOF
private

$200 constant RUN-CAP
512 constant DEF-MAX
8 constant TY-LIST-CELLS
8 constant AT-POOL-CELLS
256 constant SYM-POOL-BYTES

create RUN-BUF RUN-CAP allot
create DEF-HEAD DEF-MAX cells allot
create DEF-LO DEF-MAX cells allot
create DEF-HI DEF-MAX cells allot
create DEF-WRITES DEF-MAX cells allot
create DEF-CHECKS DEF-MAX cells allot

variable RUN-U
variable DEF-N
variable LAST-WORD
variable HITS
variable FOUND
variable NEWLY
variable CUR

\ ---- a normalized token run --------------------------------------------------

: RUN-RESET ( -- )
   0 RUN-U ! ;

: RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   RUN-U @ u + RUN-CAP > if E-CIN-STRUCT throw then
   a RUN-BUF RUN-U @ + u BYTE-COPY
   RUN-U @ u + RUN-U ! ;

: RUN-WORD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   RUN-U @ 0 > if s"  " RUN+ then
   a u RUN+ ;

: RUN$ ( -- ptr u8 n )
   RUN-BUF RUN-U @ ;

\ ---- reading one definition body ---------------------------------------------

: TOK$ ( n -- ptr u8 n )
   COMPILER-ID-SRC:TOKEN$ ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k TOK$ a u STR= ;

\ How many times the exact token appears in the half-open span.
: SPAN-COUNT ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   0 HITS !
   e b ?do
      i a u TOK-IS? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

\ Where it first appears, or the span end when it never does.
: SPAN-FIRST ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   e FOUND !
   e b ?do
      i a u TOK-IS? if
         FOUND @ e = if i FOUND ! then
      then
   loop
   FOUND @ ;

: LAST-WORD$ ( n n -- ptr u8 n ) {: b:n e:n :}
   -1 LAST-WORD !
   e b ?do
      i COMPILER-ID-SRC:WORD-TOKEN? if i LAST-WORD ! then
   loop
   LAST-WORD @ 0 < if E-CIN-STRUCT throw then
   LAST-WORD @ TOK$ ;

\ ---- the compared-field list -------------------------------------------------

: COMPARE-OP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" <>" STR= if true exit then
   a u s" =" STR= ;

\ A stored cell is read as `<offset> RC@` and compared two tokens later, so the
\ compared list is the token before each `RC@` paired with that comparison. A
\ field that is read but no longer compared drops out of the list.
: FIELD-RUN$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   RUN-RESET
   a u COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   e b ?do
      i 1+ s" RC@" TOK-IS? if
         i 3 + TOK$ COMPARE-OP? if
            i TOK$ RUN-WORD+
            i 3 + TOK$ RUN-WORD+
         then
      then
   loop
   RUN$ ;

: PRED-ROW ( n -- ) {: row:n :}
   row PRED-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" the scan predicate compares the frozen fields, in the frozen order" T-LABEL
   row PRED-WORD$ FIELD-RUN$ row PRED-FIELDS$ T$=
   s" the scan predicate still ends on the step that decides identity" T-LABEL
   row PRED-WORD$ COMPILER-ID-SRC:BODY-SPAN LAST-WORD$ row PRED-VERIFY$ T$= ;

public

: FIELDS ( -- )
   PRED-COUNT 0 ?do i PRED-ROW loop ;

private

\ ---- who writes, and who checks capacity -------------------------------------

: DEF-NAME$ ( n -- ptr u8 n ) {: d:n :}
   d cells DEF-HEAD + @ COMPILER-ID-SRC:DEF-NAME-AT$ ;

: COLLECT-DEFS ( -- )
   0 DEF-N !
   COMPILER-ID-SRC:TOKENS 0 ?do
      i COMPILER-ID-SRC:DEF-HEAD? if
         DEF-N @ DEF-MAX >= if E-CIN-STRUCT throw then
         i DEF-HEAD DEF-N @ cells + !
         i COMPILER-ID-SRC:DEF-SPAN-AT {: b:n e:n :}
         b DEF-LO DEF-N @ cells + !
         e DEF-HI DEF-N @ cells + !
         0 DEF-WRITES DEF-N @ cells + !
         0 DEF-CHECKS DEF-N @ cells + !
         DEF-N @ 1+ DEF-N !
      then
   loop ;

: FLAG@ ( n ptr n -- n ) {: d:n base:ptr :}
   base d cells + @ ;

: FLAG! ( n ptr n -- ) {: d:n base:ptr :}
   1 base d cells + ! ;

: BODY-HAS? ( n ptr u8 n -- bool ) {: d:n a:ptr u:n :}
   d cells DEF-LO + @ d cells DEF-HI + @ a u SPAN-COUNT 0 > ;

: SEED-FLAGS ( ptr u8 n ptr n -- ) {: a:ptr u:n base:ptr :}
   DEF-N @ 0 ?do
      i a u BODY-HAS? if i base FLAG! then
   loop ;

\ Does this token name a definition that already carries the flag?
: FLAGGED-NAME? ( ptr u8 n ptr n -- bool ) {: a:ptr u:n base:ptr :}
   DEF-N @ 0 ?do
      i base FLAG@ 0 <> if
         i DEF-NAME$ a u STR= if true unloop exit then
      then
   loop
   false ;

: CALLS-FLAGGED? ( n ptr n -- bool ) {: d:n base:ptr :}
   d cells DEF-LO + @ d cells DEF-HI + @ {: b:n e:n :}
   e b ?do
      i TOK$ base FLAGGED-NAME? if true unloop exit then
   loop
   false ;

: FLAG-PASS ( ptr n -- n ) {: base:ptr :}
   0 NEWLY !
   DEF-N @ 0 ?do
      i base FLAG@ 0= if
         i base CALLS-FLAGGED? if
            i base FLAG!
            NEWLY @ 1+ NEWLY !
         then
      then
   loop
   NEWLY @ ;

\ Close the flag under calls. Each pass flags at least one more definition or
\ none at all, so the definition count bounds the number of passes.
: FLAG-CLOSE ( ptr n -- ) {: base:ptr :}
   DEF-N @ 1+ 0 ?do
      base FLAG-PASS 0= if unloop exit then
   loop ;

: CLASSIFY ( -- )
   COLLECT-DEFS
   PUSH-TOKEN$ DEF-WRITES SEED-FLAGS
   CAP-TOKEN$ DEF-CHECKS SEED-FLAGS
   DEF-WRITES FLAG-CLOSE
   DEF-CHECKS FLAG-CLOSE ;

: WRITER-TOKEN? ( n -- bool ) {: k:n :}
   k PUSH-TOKEN$ TOK-IS? if true exit then
   k TOK$ DEF-WRITES FLAGGED-NAME? ;

: CHECKER-TOKEN? ( n -- bool ) {: k:n :}
   k CAP-TOKEN$ TOK-IS? if true exit then
   k TOK$ DEF-CHECKS FLAGGED-NAME? ;

: FIRST-WRITER ( n n -- n ) {: b:n e:n :}
   e FOUND !
   e b ?do
      i WRITER-TOKEN? if
         FOUND @ e = if i FOUND ! then
      then
   loop
   FOUND @ ;

: WRITER-COUNT ( n n -- n ) {: b:n e:n :}
   0 HITS !
   e b ?do
      i WRITER-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: CHECKER-COUNT ( n n -- n ) {: b:n e:n :}
   0 HITS !
   e b ?do
      i CHECKER-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: LATE-CHECKS ( n n n -- n ) {: b:n e:n first:n :}
   0 HITS !
   e first ?do
      i CHECKER-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: ORDER-ROW ( n -- ) {: row:n :}
   row ORDER-FILE$ COMPILER-ID-SRC:SCAN-FILE
   CLASSIFY
   row ORDER-WORD$ COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   s" the intern path still writes rows through the arena" T-LABEL
   b e WRITER-COUNT 0 > TTRUE
   s" the intern path still reads its table's committed capacity" T-LABEL
   b e CHECKER-COUNT 0 > TTRUE
   s" every capacity check runs before the first arena push" T-LABEL
   b e  b e FIRST-WRITER  LATE-CHECKS 0 T= ;

public

: WRITE-ORDER ( -- )
   ORDER-COUNT 0 ?do i ORDER-ROW loop ;

private

\ ---- the reference guards ----------------------------------------------------

: GUARD-ROW ( n -- ) {: row:n :}
   s" a reference guard still has its frozen body" T-LABEL
   row GUARD-WORD$ COMPILER-ID-SRC:BODY$ row GUARD-BODY$ T$= ;

: POINTER-ORDER ( -- )
   s" POINTER" COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   s" the pointer constructor checks an identity exactly once" T-LABEL
   b e s" ID-CK" SPAN-COUNT 1 T=
   s" the pointer constructor interns exactly once" T-LABEL
   b e s" INTERN4" SPAN-COUNT 1 T=
   s" the pointee check runs before the intern" T-LABEL
   b e s" ID-CK" SPAN-FIRST
   b e s" INTERN4" SPAN-FIRST < TTRUE ;

public

: REF-GUARDS ( -- )
   TYPE-FILE$ COMPILER-ID-SRC:SCAN-FILE
   GUARD-COUNT 0 ?do i GUARD-ROW loop
   s" the checked identity is the pointee the caller handed in" T-LABEL
   PTEE-CHECK$ COMPILER-ID-SRC:RUNS 1 T=
   s" the staged elements are bounded by the table's live row count" T-LABEL
   STAGE-CHECK$ COMPILER-ID-SRC:RUNS 1 T=
   POINTER-ORDER ;

private

\ ---- driving the sequences through the real interners ------------------------

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A caught quotation cannot read the enclosing word's locals, so each step
\ carries everything it needs on the data stack and hands its answer back in the
\ slot it was given. The slot keeps the -1 it went in with when the step throws.

: SYM-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p:ptr u:n slot:n :}
   c a r key p u IR-SYM:INTERN IR-ID:SYMBOL-LOCAL {: got:n :}
   c a r key p u got ;

: SYM-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- n n )
   -1 [: SYM-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p:ptr u:n got:n :}
   rc got ;

: TY-MAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n :}
   ki TY-CTOR CTOR-INT = if
      c a r key ki TY-ARG-A WIDTH-AT ki TY-ARG-B SIGN-AT IR-TYPE:INT exit
   then
   c a r key ki TY-ARG-A SPACE-AT
      key ki TY-ARG-B IR-ID:PACK-TYPE IR-TYPE:POINTER ;

: TY-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n slot:n :}
   c a r key ki TY-MAKE IR-ID:TYPE-LOCAL {: got:n :}
   c a r key ki got ;

: TY-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- n n )
   -1 [: TY-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n got:n :}
   rc got ;

: AT-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n slot:n :}
   c a r key ki AT-KEY IR-ATTR:INT IR-ID:ATTR-LOCAL {: got:n :}
   c a r key ki got ;

: AT-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- n n )
   -1 [: AT-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n got:n :}
   rc got ;

\ ---- one step's two assertions -----------------------------------------------

: STEP-ANSWER ( n n n -- ) {: st:n rc:n got:n :}
   s" the interner reaches the answer the shared vector row records" T-LABEL
   rc st STEP-CLASS@ T=
   st STEP-CLASS@ 0 <> if exit then
   s" an accepted key answers the ordinal the shared vector row records" T-LABEL
   got st STEP-ORD@ T= ;

: SYM-ROW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   c a r key st STEP-KEY@ SYM-KEY$ SYM-STEP {: rc:n got:n :}
   st rc got STEP-ANSWER ;

: TY-ROW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   c a r key st STEP-KEY@ TY-STEP {: rc:n got:n :}
   st rc got STEP-ANSWER ;

: AT-ROW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   c a r key st STEP-KEY@ AT-STEP {: rc:n got:n :}
   st rc got STEP-ANSWER ;

\ ---- the forced filter collision ---------------------------------------------
\ The collision sequence is only worth anything if the two byte strings really
\ do share a content filter, so the sequence asks the production filter rather
\ than assuming it.

: COLLISION-CHECK ( n -- ) {: s:n :}
   s SCN-ROLE@ ROLE-COLLISION <> if exit then
   s SCN-BASE@ {: b:n :}
   s" the two collision keys really share one content filter" T-LABEL
   b STEP-KEY@ SYM-KEY$ IR-SYM:FILTER
   b 1+ STEP-KEY@ SYM-KEY$ IR-SYM:FILTER T=
   s" the two collision keys are different byte strings" T-LABEL
   b STEP-KEY@ SYM-KEY$ b 1+ STEP-KEY@ SYM-KEY$ T-STR= TFALSE ;

\ ---- one sequence per table --------------------------------------------------

: SYM-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key s SCN-CEIL@ SYM-POOL-BYTES IR-SYM:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      c a r key s SCN-BASE@ i + SYM-ROW
   loop
   s COLLISION-CHECK
   s" the sequence ends holding the row count the shared row records" T-LABEL
   r IR-SYM:SYMBOLS s SCN-FINAL@ T= ;

: TY-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key s SCN-CEIL@ TY-LIST-CELLS IR-TYPE:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      c a r key s SCN-BASE@ i + TY-ROW
   loop
   s" the sequence ends holding the row count the shared row records" T-LABEL
   r IR-TYPE:TYPES s SCN-FINAL@ T= ;

: AT-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key s SCN-CEIL@ AT-POOL-CELLS IR-ATTR:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      c a r key s SCN-BASE@ i + AT-ROW
   loop
   s" the sequence ends holding the row count the shared row records" T-LABEL
   r IR-ATTR:ATTRS s SCN-FINAL@ T= ;

: SEQ-BODY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ SCN-KIND@ {: k:n :}
   k KIND-SYM = if c SYM-SEQ exit then
   k KIND-TYPE = if c TY-SEQ exit then
   k KIND-ATTR <> if E-CIN-ROW throw then
   c AT-SEQ ;

\ ---- the table cannot quietly lose a case ------------------------------------

: ROLE-COVERED? ( n -- bool ) {: role:n :}
   SCENARIOS 0 ?do
      i SCN-ROLE@ role = if true unloop exit then
   loop
   false ;

: KIND-COVERED? ( n -- bool ) {: kind:n :}
   SCENARIOS 0 ?do
      i SCN-KIND@ kind = if true unloop exit then
   loop
   false ;

: SEQ-USES-KEY? ( n n -- bool ) {: s:n keyi:n :}
   s SCN-LEN@ 0 ?do
      s SCN-BASE@ i + STEP-KEY@ keyi = if true unloop exit then
   loop
   false ;

: KEY-USED? ( n n -- bool ) {: kind:n keyi:n :}
   SCENARIOS 0 ?do
      i SCN-KIND@ kind = if
         i keyi SEQ-USES-KEY? if true unloop exit then
      then
   loop
   false ;

: KEYS-COVERED ( n n -- ) {: kind:n keys:n :}
   keys 0 ?do
      s" every declared key is presented by a vector row" T-LABEL
      kind i KEY-USED? TTRUE
   loop ;

public

\ The sequence table carries no digest, so this is its freeze. A role that stops
\ being covered, an interner that stops being driven, or a key left behind by a
\ deleted sequence each fail here rather than quietly shrinking what the gate
\ asks.
: COVERAGE ( -- )
   ROLE-COUNT 0 ?do
      s" every frozen sequence role is covered by a vector row" T-LABEL
      i ROLE-COVERED? TTRUE
   loop
   KIND-COUNT 0 ?do
      s" every interner the model covers is driven by a vector row" T-LABEL
      i KIND-COVERED? TTRUE
   loop
   KIND-SYM SYM-KEYS KEYS-COVERED
   KIND-TYPE TY-KEYS KEYS-COVERED
   KIND-ATTR AT-KEYS KEYS-COVERED ;

: VECTORS ( -- )
   SCENARIOS 0 ?do
      i CUR !
      BND [: SEQ-BODY ;] IR-CTX:WITH-CONTEXT
   loop ;

: HABU-SIDE ( -- )
   FIELDS
   WRITE-ORDER
   REF-GUARDS
   COVERAGE
   VECTORS ;

;using
;package
