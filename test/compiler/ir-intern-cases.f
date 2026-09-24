\ ir-intern-cases.f - Exercise the shipped interners against the shared interning schema.

require lib/test.f
require lib/string.f
require test/compiler/ir-intern-schema.f

package COMPILER-INTERN-CASES
using COMPILER-INTERN-PROOF
private

variable CUR

8 constant TY-LIST-CELLS
8 constant AT-POOL-CELLS
256 constant SYM-POOL-BYTES

\ ---- driving the sequences through the real interners ------------------------

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A caught quotation cannot read the enclosing word's locals, so each step
\ carries everything it needs on the data stack and writes its answer to ANS,
\ which the runner preset: `catch` restores the DEPTH of both stacks and never
\ their contents, so no stack slot can carry a value out of a body that threw.
\ The cells the step was handed come back stale from the catch and are dropped.
variable ANS

: SYM-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key p:ptr u:n :}
   c a r key p u IR-SYM:INTERN IR-ID:SYMBOL-LOCAL ANS !
   c a r key p u ;

: SYM-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- n n )
   -1 ANS !
   [: SYM-TRY ;] catch {: rc:n :}
   2drop 2drop 2drop
   rc ANS @ ;

: TY-MAKE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n :}
   ki TY-CTOR CTOR-INT = if
      c a r key ki TY-ARG-A WIDTH-AT ki TY-ARG-B SIGN-AT IR-TYPE:INT exit
   then
   c a r key ki TY-ARG-A SPACE-AT
      key ki TY-ARG-B IR-ID:PACK-TYPE IR-TYPE:POINTER ;

: TY-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n :}
   c a r key ki TY-MAKE IR-ID:TYPE-LOCAL ANS !
   c a r key ki ;

: TY-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- n n )
   -1 ANS !
   [: TY-TRY ;] catch {: rc:n :}
   2drop 2drop drop
   rc ANS @ ;

: AT-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key ki:n :}
   c a r key ki AT-KEY IR-ATTR:INT IR-ID:ATTR-LOCAL ANS !
   c a r key ki ;

: AT-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- n n )
   -1 ANS !
   [: AT-TRY ;] catch {: rc:n :}
   2drop 2drop drop
   rc ANS @ ;

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

public

: VECTORS ( -- )
   SCENARIOS 0 ?do
      i CUR !
      BND [: SEQ-BODY ;] IR-CTX:WITH-CONTEXT
   loop ;

: HABU-SIDE ( -- )
   VECTORS ;

;using
;package
