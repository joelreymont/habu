\ Execute shared storage examples through IR-CTX and IR-ARENA. These check
\ answers and named refusals, including reads/counts after rejected operations.
\ ir-context.f and ir-arena.f cover dynamic growth, owner/stale checks and normal
\ and exceptional cleanup through the real runtime. No source scanner is used.

require lib/test.f
require lib/string.f
require test/compiler/ir-storage-schema.f

package COMPILER-STORE-CASES
using COMPILER-STORE-PROOF
private

1 constant SCRATCH-CEIL
variable CUR
variable CCUR
TYPED-VARIABLE IX IR-ARENA:cell-id
TYPED-VARIABLE VW IR-ARENA:view

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Every handle slot starts holding a real handle of a third arena that no row
\ addresses, so a row that used a slot before setting it would be refused by the
\ production owner check rather than reading something plausible.
: PLACEHOLDERS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c SCRATCH-CEIL IR-ARENA:NEW {: s:IR-ARENA:arena :}
   c s 0 IR-ARENA:PUSH IX !
   s IR-ARENA:FREEZE VW ! ;

: SEL ( IR-ARENA:arena IR-ARENA:arena n -- IR-ARENA:arena )
   {: a:IR-ARENA:arena b:IR-ARENA:arena w:n :}
   w 0 = if a exit then
   b ;

: FREEZE-DO ( IR-ARENA:arena -- n )
   IR-ARENA:FREEZE {: v:IR-ARENA:view :}
   v VW !
   v IR-ARENA:SIZE ;

: AT-DO ( n -- n ) {: st:n :}
   VW @ dup st STEP-ARG@ IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

: KEEP-DO ( IR-ARENA:arena n -- n ) {: t:IR-ARENA:arena st:n :}
   t st STEP-ARG@ IR-ARENA:NTH {: x:IR-ARENA:cell-id :}
   x IX !
   x IR-ARENA:ORD ;

: PEEK-DO ( IR-ARENA:arena n -- n ) {: t:IR-ARENA:arena st:n :}
   t dup st STEP-ARG@ IR-ARENA:NTH IR-ARENA:PEEK ;

: PUSH-DO ( IR-CTX:ctx IR-ARENA:arena n -- n )
   {: c:IR-CTX:ctx t:IR-ARENA:arena st:n :}
   c t st STEP-ARG@ IR-ARENA:PUSH IR-ARENA:ORD ;

: DO-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena b:IR-ARENA:arena st:n :}
   st STEP-OP@ {: op:n :}
   a b st STEP-WHICH@ SEL {: t:IR-ARENA:arena :}
   op OP-PUSH = if c t st PUSH-DO exit then
   op OP-PEEK = if t st PEEK-DO exit then
   op OP-USED = if t IR-ARENA:USED exit then
   op OP-FREEZE = if t FREEZE-DO exit then
   op OP-AT = if st AT-DO exit then
   op OP-KEEP = if t st KEEP-DO exit then
   op OP-READ = if t IX @ IR-ARENA:PEEK exit then
   op OP-ABORT <> if E-CST-ROW throw then
   t IR-ARENA:ABORT 0 ;

\ A caught quotation cannot read the enclosing word's locals, so the step carries
\ everything it needs on the data stack and hands its answer back in the slot it
\ was given. The slot keeps the -1 it went in with when the step throws.
: STEP-TRY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n n -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena b:IR-ARENA:arena st:n slot:n :}
   c a b st  c a b st DO-OP ;

: STEP-RUN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- n n )
   -1 [: STEP-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx a:IR-ARENA:arena b:IR-ARENA:arena st:n got:n :}
   rc got ;

: STEP-CHECK ( n n n -- ) {: st:n rc:n got:n :}
   s" the storage word reaches the answer the shared vector row records" T-LABEL
   rc st STEP-CLASS@ T=
   st STEP-CLASS@ 0 <> if exit then
   s" an accepted step answers the number the shared vector row records" T-LABEL
   got st STEP-ANS@ T= ;

: ROW-STEP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena b:IR-ARENA:arena st:n :}
   c a b st STEP-RUN {: rc:n got:n :}
   st rc got STEP-CHECK ;

: ROW-BODY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c PLACEHOLDERS
   c s SCN-CEIL@ IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c s SCN-CEIL@ IR-ARENA:NEW {: b:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      c a b s SCN-BASE@ i + ROW-STEP
   loop ;

\ ---- the context rows --------------------------------------------------------

: SCRATCH-DO ( IR-CTX:ctx n -- n ) {: c:IR-CTX:ctx st:n :}
   c st CSTEP-ARG@ IR-CTX:SCRATCH-TAKE drop drop
   c IR-CTX:SCRATCH-USED ;

: MINT-DO ( IR-CTX:ctx -- n ) {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop drop
   c IR-CTX:MINTED ;

: CDO-OP ( IR-CTX:ctx n -- n ) {: c:IR-CTX:ctx st:n :}
   st CSTEP-OP@ {: op:n :}
   op COP-SCRATCH = if c st SCRATCH-DO exit then
   op COP-MINT = if c MINT-DO exit then
   op COP-USED = if c IR-CTX:SCRATCH-USED exit then
   op COP-MINTED <> if E-CST-ROW throw then
   c IR-CTX:MINTED ;

: CSTEP-TRY ( IR-CTX:ctx n n -- IR-CTX:ctx n n )
   {: c:IR-CTX:ctx st:n slot:n :}
   c st  c st CDO-OP ;

: CSTEP-RUN ( IR-CTX:ctx n -- n n )
   -1 [: CSTEP-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx st:n got:n :}
   rc got ;

: CSTEP-CHECK ( n n n -- ) {: st:n rc:n got:n :}
   s" the context word reaches the answer the shared vector row records" T-LABEL
   rc st CSTEP-CLASS@ T=
   st CSTEP-CLASS@ 0 <> if exit then
   s" an accepted context step answers the number the row records" T-LABEL
   got st CSTEP-ANS@ T= ;

: CROW-STEP ( IR-CTX:ctx n -- ) {: c:IR-CTX:ctx st:n :}
   c st CSTEP-RUN {: rc:n got:n :}
   st rc got CSTEP-CHECK ;

: CROW-BODY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CCUR @ {: s:n :}
   s CSCN-LEN@ 0 ?do
      c s CSCN-BASE@ i + CROW-STEP
   loop ;

\ ---- the nesting depth rows --------------------------------------------------
\ Entering a context is a scoped combinator, so reaching a given nesting depth
\ means nesting that many times rather than looping. The nester reaches itself
\ through a forward reference and hands the innermost frame to the probe. Only
\ the probe catches, so an entry refused at a shallower depth escapes the whole
\ nest and is reported as such instead of being read as the probe's answer, and
\ the probe records that it ran at all, so a row cannot pass by never arriving.

variable NEST-LEFT
variable PROBE-RC
variable PROBE-HIT

: IDLE ( IR-CTX:ctx -- )
   drop ;

: ENTER-ONE ( -- )
   BND [: IDLE ;] IR-CTX:WITH-CONTEXT ;

: PROBE ( -- )
   [: ENTER-ONE ;] catch PROBE-RC !
   1 PROBE-HIT ! ;

defer NEST-XT ( -- )

: DEEPER ( IR-CTX:ctx -- )
   drop NEST-XT ;

: NEST-ONE ( -- )
   NEST-LEFT @ 0= if PROBE exit then
   NEST-LEFT @ 1- NEST-LEFT !
   BND [: DEEPER ;] IR-CTX:WITH-CONTEXT ;

: NEST-INSTALL ( -- )
   [: NEST-ONE ;] is NEST-XT ;

NEST-INSTALL

: DEPTH-ROW ( n -- ) {: row:n :}
   row DSCN-DEPTH@ NEST-LEFT !
   0 PROBE-HIT !
   -1 PROBE-RC !
   [: NEST-ONE ;] catch {: rc:n :}
   s" opening the frozen number of nested contexts throws nothing itself" T-LABEL
   rc 0 T=
   s" the innermost entry attempt actually ran" T-LABEL
   PROBE-HIT @ 1 T=
   s" one more context entry answers what the shared depth row records" T-LABEL
   PROBE-RC @ row DSCN-CLASS@ T= ;

public

: VECTORS ( -- )
   SCENARIOS 0 ?do
      i CUR !
      BND [: ROW-BODY ;] IR-CTX:WITH-CONTEXT
   loop
   CSCENARIOS 0 ?do
      i CCUR !
      BND i CSCN-CEIL@ [: CROW-BODY ;] IR-CTX:WITH-CONTEXT-BOUND
   loop
   DSCENARIOS 0 ?do i DEPTH-ROW loop ;

: HABU-SIDE ( -- )
   VECTORS ;

;using
;package
