\ ir-storage-cases.f - the Habu half of the storage parity binding.
\
\ The module lives in `package COMPILER-STORE-CASES`. It takes the frozen rows in
\ `package COMPILER-STORE-PROOF` and asks the shipped compiler about them, in
\ five groups:
\
\   - the pinned capacity constants, read as literals out of the production
\     source. The model in `formal/Common/Storage.v` states the same numbers, so
\     renumbering the seed span, the mapping, the registry depth or the slot
\     width on either side makes the two sides disagree.
\
\   - the check-before-write ordering of the nine words that guard storage. The
\     reader first classifies every definition in the owning file: a definition
\     WRITES if it carries one of the frozen write tokens or calls a writer, and
\     it GUARDS this row if it names the row's throw code or calls a guard. Both
\     relations are closed under calls. That closure is not decoration:
\     `IR-ARENA:PUSH` carries no `E-IR-ARENA-FULL` token of its own and
\     `IR-CTX:WITH-CONTEXT-BOUND` carries neither a write token nor its ceiling
\     code, so a one-level token scan would find nothing in either body and pass
\     without asking anything. Each body must then contain at least one writer,
\     at least one guard, and no guard-only token at or after its first
\     write-only token.
\
\   - the frozen guard bodies. These are the words that make the model's
\     hypotheses true of the shipped code, plus the two bodies the model records
\     findings against: `IR-ARENA:ROLLBACK`, which truncates the cursor without
\     bumping the generation, and `IR-CTX:CTX-ENTER`, whose registry truncation
\     runs after the caller's quotation and is therefore skipped by a throw. Both
\     are frozen whole and the ordering inside `CTX-ENTER` is read structurally.
\
\   - the shape of the vector table itself: a frozen operation must address a
\     real arena, a read through the frozen view must follow a freeze, a read of
\     a kept index must follow the keep, and a rollback must follow the mark it
\     names. Without those a row could quietly ask nothing.
\
\   - every vector row, driven through the real `IR-CTX` and `IR-ARENA` words:
\     the arena and context step rows, and the nesting depth rows, which open
\     real contexts one inside another until the registry's limit is in sight.
\     The same rows become Rocq obligations in
\     `test/compiler/ir-storage-obligations.f`; this file never restates them.
\
\ Consumers: `test/compiler/ir-storage-manifest.f` (these five groups alone) and
\ `test/compiler/ir-storage-proof.f` (these five plus the Rocq half).

require lib/test.f
require lib/string.f
require test/compiler/ir-id-source.f
require test/compiler/ir-storage-schema.f

package COMPILER-STORE-CASES
using COMPILER-STORE-PROOF
private

512 constant DEF-MAX
1 constant SCRATCH-CEIL              \ the placeholder arena holds one cell

create DEF-HEAD DEF-MAX cells allot
create DEF-LO DEF-MAX cells allot
create DEF-HI DEF-MAX cells allot
create DEF-WRITES DEF-MAX cells allot
create DEF-GUARDS DEF-MAX cells allot

variable DEF-N
variable HITS
variable FOUND
variable LAST
variable NEWLY
variable CUR
variable CCUR

\ The four handles a vector row carries between steps. They are sealed nominals,
\ so they cannot live in a raw cell; a typed variable is the sanctioned storage
\ and it keeps the step dispatcher's stack shallow.
TYPED-VARIABLE MK0 IR-ARENA:mark
TYPED-VARIABLE MK1 IR-ARENA:mark
TYPED-VARIABLE IX IR-ARENA:cell-id
TYPED-VARIABLE VW IR-ARENA:view

: TOK$ ( n -- ptr u8 n )
   COMPILER-ID-SRC:TOKEN$ ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k TOK$ a u STR= ;

\ ---- 1. the pinned capacity constants ----------------------------------------

: PIN-ROW ( n -- ) {: row:n :}
   row PIN-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" the production source declares the pinned capacity exactly once" T-LABEL
   row PIN-NAME$ COMPILER-ID-SRC:CONSTS 1 T=
   s" the pinned capacity still carries the frozen literal" T-LABEL
   row PIN-NAME$ COMPILER-ID-SRC:CONST@ row PIN-VALUE T= ;

public

: CONSTANTS ( -- )
   PIN-COUNT 0 ?do i PIN-ROW loop ;

private

\ ---- 2. who writes, and who guards -------------------------------------------

: DEF-NAME$ ( n -- ptr u8 n ) {: d:n :}
   d cells DEF-HEAD + @ COMPILER-ID-SRC:DEF-NAME-AT$ ;

: COLLECT-DEFS ( -- )
   0 DEF-N !
   COMPILER-ID-SRC:TOKENS 0 ?do
      i COMPILER-ID-SRC:DEF-HEAD? if
         DEF-N @ DEF-MAX >= if E-CST-STRUCT throw then
         i DEF-HEAD DEF-N @ cells + !
         i COMPILER-ID-SRC:DEF-SPAN-AT {: b:n e:n :}
         b DEF-LO DEF-N @ cells + !
         e DEF-HI DEF-N @ cells + !
         0 DEF-WRITES DEF-N @ cells + !
         0 DEF-GUARDS DEF-N @ cells + !
         DEF-N @ 1+ DEF-N !
      then
   loop ;

: SPAN-COUNT ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   0 HITS !
   e b ?do
      i a u TOK-IS? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: SPAN-LAST ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   -1 LAST !
   e b ?do
      i a u TOK-IS? if i LAST ! then
   loop
   LAST @ ;

: FLAG@ ( n ptr n -- n ) {: d:n base:ptr :}
   base d cells + @ ;

: FLAG! ( n ptr n -- ) {: d:n base:ptr :}
   1 base d cells + ! ;

: BODY-HAS? ( n ptr u8 n -- bool ) {: d:n a:ptr u:n :}
   d cells DEF-LO + @ d cells DEF-HI + @ a u SPAN-COUNT 0 > ;

: SEED-ONE ( ptr u8 n ptr n -- ) {: a:ptr u:n base:ptr :}
   DEF-N @ 0 ?do
      i a u BODY-HAS? if i base FLAG! then
   loop ;

: SEED-WRITES ( -- )
   WRITE-TOKENS 0 ?do
      i WRITE-TOKEN$ DEF-WRITES SEED-ONE
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

: CLASSIFY ( ptr u8 n -- ) {: g:ptr gu:n :}
   COLLECT-DEFS
   SEED-WRITES
   g gu DEF-GUARDS SEED-ONE
   DEF-WRITES FLAG-CLOSE
   DEF-GUARDS FLAG-CLOSE ;

: WRITE-SEED-TOKEN? ( n -- bool ) {: k:n :}
   WRITE-TOKENS 0 ?do
      k i WRITE-TOKEN$ TOK-IS? if true unloop exit then
   loop
   false ;

: WRITER-TOKEN? ( n -- bool ) {: k:n :}
   k WRITE-SEED-TOKEN? if true exit then
   k TOK$ DEF-WRITES FLAGGED-NAME? ;

: GUARD-TOKEN? ( n ptr u8 n -- bool ) {: k:n g:ptr gu:n :}
   k g gu TOK-IS? if true exit then
   k TOK$ DEF-GUARDS FLAGGED-NAME? ;

\ A token that only writes, and a token that only guards. A word that does both -
\ `IR-ARENA:GROW` checks its ceiling and then publishes a span - is neither, and
\ has its own row proving it checks before it writes.
: WRITE-ONLY? ( n ptr u8 n -- bool ) {: k:n g:ptr gu:n :}
   k WRITER-TOKEN? 0= if false exit then
   k g gu GUARD-TOKEN? 0= ;

: GUARD-ONLY? ( n ptr u8 n -- bool ) {: k:n g:ptr gu:n :}
   k g gu GUARD-TOKEN? 0= if false exit then
   k WRITER-TOKEN? 0= ;

: WRITER-COUNT ( n n -- n ) {: b:n e:n :}
   0 HITS !
   e b ?do
      i WRITER-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: GUARD-COUNT-IN ( n n ptr u8 n -- n ) {: b:n e:n g:ptr gu:n :}
   0 HITS !
   e b ?do
      i g gu GUARD-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: FIRST-WRITE-ONLY ( n n ptr u8 n -- n ) {: b:n e:n g:ptr gu:n :}
   e FOUND !
   e b ?do
      i g gu WRITE-ONLY? if
         FOUND @ e = if i FOUND ! then
      then
   loop
   FOUND @ ;

: LATE-GUARDS ( n n ptr u8 n n -- n ) {: b:n e:n g:ptr gu:n first:n :}
   0 HITS !
   e first ?do
      i g gu GUARD-ONLY? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: ORDER-ROW ( n -- ) {: row:n :}
   row ORDER-FILE$ COMPILER-ID-SRC:SCAN-FILE
   row ORDER-GUARD$ CLASSIFY
   row ORDER-WORD$ COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   s" the storage path still publishes through a write of its own" T-LABEL
   b e WRITER-COUNT 0 > TTRUE
   s" the storage path still reaches its named guard" T-LABEL
   b e row ORDER-GUARD$ GUARD-COUNT-IN 0 > TTRUE
   s" every guard runs before the first write it protects" T-LABEL
   b e row ORDER-GUARD$
      b e row ORDER-GUARD$ FIRST-WRITE-ONLY
      LATE-GUARDS 0 T= ;

public

: WRITE-ORDER ( -- )
   ORDER-COUNT 0 ?do i ORDER-ROW loop ;

private

\ ---- 3. the frozen guard bodies ----------------------------------------------

: GUARD-ROW ( n -- ) {: row:n :}
   row GUARD-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" a storage guard still has its frozen body" T-LABEL
   row GUARD-WORD$ COMPILER-ID-SRC:BODY$ row GUARD-BODY$ T$= ;

\ The registry truncation that releases a context and every child inside it runs
\ AFTER the caller's quotation, which is why a throw never reaches it. That
\ ordering is the model's first finding, so it is read here rather than trusted.
: TEARDOWN-ORDER ( -- )
   CTX-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" the context body is invoked exactly once" T-LABEL
   BODY-CALL$ COMPILER-ID-SRC:RUNS 1 T=
   s" the registry truncation appears exactly once" T-LABEL
   TEARDOWN-RUN$ COMPILER-ID-SRC:RUNS 1 T=
   s" CTX-ENTER" COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   s" the context body is invoked exactly once inside CTX-ENTER" T-LABEL
   b e s" execute" SPAN-COUNT 1 T=
   s" the registry truncation runs after the context body, not before" T-LABEL
   b e s" DEPTH" SPAN-LAST
   b e s" execute" SPAN-LAST > TTRUE ;

public

: GUARDS ( -- )
   GUARD-COUNT 0 ?do i GUARD-ROW loop
   TEARDOWN-ORDER ;

private

\ ---- 4. the vector table cannot quietly ask nothing --------------------------

: ROLE-COVERED? ( n -- bool ) {: role:n :}
   SCENARIOS 0 ?do
      i SCN-ROLE@ role = if true unloop exit then
   loop
   CSCENARIOS 0 ?do
      i CSCN-ROLE@ role = if true unloop exit then
   loop
   role ROLE-DEPTH = DSCENARIOS 0 > and ;

: OP-USED-BY-ROW? ( n -- bool ) {: op:n :}
   STEPS 0 ?do
      i STEP-OP@ op = if true unloop exit then
   loop
   false ;

: COP-USED-BY-ROW? ( n -- bool ) {: op:n :}
   CSTEPS 0 ?do
      i CSTEP-OP@ op = if true unloop exit then
   loop
   false ;

\ Does an earlier step of this row carry that operation, addressed to the same
\ arena, with the same argument when the argument names a slot?
: EARLIER? ( n n n n -- bool ) {: base:n at:n op:n arg:n :}
   at base ?do
      i STEP-OP@ op = if
         arg 0 < if true unloop exit then
         i STEP-ARG@ arg = if true unloop exit then
      then
   loop
   false ;

: STEP-SHAPE ( n n -- ) {: base:n at:n :}
   s" every frozen step names a real operation on a real arena" T-LABEL
   at STEP-OP@ 0 >= at STEP-OP@ OP-COUNT < and
   at STEP-WHICH@ 0 >= and at STEP-WHICH@ 2 < and TTRUE
   at STEP-OP@ OP-AT = if
      s" a read through the frozen view follows the freeze that made it" T-LABEL
      base at OP-FREEZE -1 EARLIER? TTRUE
   then
   at STEP-OP@ OP-READ = if
      s" a read of the kept index follows the step that kept it" T-LABEL
      base at OP-KEEP -1 EARLIER? TTRUE
   then
   at STEP-OP@ OP-ROLL = if
      s" a rollback follows the mark whose slot it names" T-LABEL
      base at OP-MARK at STEP-ARG@ EARLIER? TTRUE
   then ;

: ROW-SHAPE ( n -- ) {: s:n :}
   s SCN-LEN@ 0 ?do
      s SCN-BASE@ s SCN-BASE@ i + STEP-SHAPE
   loop ;

public

\ The vector tables carry no digest, so this is their freeze. A role that stops
\ being covered, an operation no row drives any more, or a row that reads a
\ handle it never took each fail here rather than quietly shrinking the gate.
: COVERAGE ( -- )
   ROLE-COUNT 0 ?do
      s" every frozen vector role is covered by a row" T-LABEL
      i ROLE-COVERED? TTRUE
   loop
   OP-COUNT 0 ?do
      s" every arena operation the model covers is driven by a row" T-LABEL
      i OP-USED-BY-ROW? TTRUE
   loop
   COP-COUNT 0 ?do
      s" every context operation the model covers is driven by a row" T-LABEL
      i COP-USED-BY-ROW? TTRUE
   loop
   SCENARIOS 0 ?do i ROW-SHAPE loop ;

private

\ ---- 5. driving the rows through the real storage words ----------------------

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Every handle slot starts holding a real handle of a third arena that no row
\ addresses, so a row that used a slot before setting it would be refused by the
\ production owner check rather than reading something plausible. The table shape
\ check above makes that unreachable; this makes it fail closed anyway.
: PLACEHOLDERS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c SCRATCH-CEIL IR-ARENA:NEW {: s:IR-ARENA:arena :}
   c s 0 IR-ARENA:PUSH IX !
   s IR-ARENA:MARK MK0 !
   s IR-ARENA:MARK MK1 !
   s IR-ARENA:FREEZE VW ! ;

: SEL ( IR-ARENA:arena IR-ARENA:arena n -- IR-ARENA:arena )
   {: a:IR-ARENA:arena b:IR-ARENA:arena w:n :}
   w 0 = if a exit then
   b ;

: MARK-DO ( IR-ARENA:arena n -- n ) {: t:IR-ARENA:arena st:n :}
   t IR-ARENA:MARK {: m:IR-ARENA:mark :}
   st STEP-ARG@ 0 = if m MK0 ! else m MK1 ! then
   t IR-ARENA:USED ;

: SLOT-MARK ( n -- IR-ARENA:mark ) {: st:n :}
   st STEP-ARG@ 0 = if MK0 @ exit then
   MK1 @ ;

: ROLL-DO ( IR-ARENA:arena n -- n ) {: t:IR-ARENA:arena st:n :}
   t st SLOT-MARK IR-ARENA:ROLLBACK
   t IR-ARENA:USED ;

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
   op OP-MARK = if t st MARK-DO exit then
   op OP-ROLL = if t st ROLL-DO exit then
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
   CONSTANTS
   WRITE-ORDER
   GUARDS
   COVERAGE
   VECTORS ;

;using
;package
