\ native-locals-scope.f - a locals group that opens and closes INSIDE a control
\ structure, run against the engine's own compilation of the same source.
\ One concern: which names a row can see once groups nest inside structures, and
\ what each seam of a structure therefore carries.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. A locals group
\ compiles to nothing at all: `:}` moves value ids from the compile-time vector
\ into named slots and emits no instruction. Nothing about the resulting code says
\ which name a slot held, so a suite that counted blocks or operations would pass
\ against an elaborator that had bound a group's values to the wrong slots, kept a
\ name in scope past its structure's closer, or carried a dead name across an
\ edge. What every one of those mistakes changes is a NUMBER the routine answers,
\ so every case here is DIFFERENTIAL: the same source text compiled twice, once by
\ the engine's own emitter and once by the native chain, run against each other on
\ pinned inputs.
\
\ EVERY VALUE IS WEIGHTED WITH A DISTINCT ODD FACTOR, and that is not decoration.
\ Two names combined by addition answer the same number whichever slot each of
\ them came back in, so a body that exchanged two locals would agree with the
\ engine and the row would prove only that the right NUMBER of cells came back.
\ Distinct odd multipliers make the ANSWER say which name was read where.
\
\ THE SEAMS ARE WHAT THE CASES ARE CHOSEN FOR, and each is a different mistake.
\ A group in one arm of an `if` must be gone at the `then`, so the join carries
\ the names the `if` opened with; a group in a loop body must be gone at the
\ `loop`, so the back edge and the header agree and the block after the loop never
\ hears of it; a `while` keeps its frame open, so a name bound before it is still
\ readable in the body and gone after the `repeat`; an arm of a `case` gives its
\ names back at its own `endof` and not at the `endcase`; `leave` branches out of
\ a loop whose body is still holding names, so it has to carry the LOOP's list and
\ not the walk's; and a call carries the walk's, which is the one place the two
\ differ on purpose.
\
\ AND THE SLOT IS GIVEN BACK, WHICH IS THE OTHER HALF. Two structures that do not
\ contain each other may bind the same name, and the second one takes the slot the
\ first gave back. NLS-REUSE is that row and it is weighted, so a second loop
\ reading the first loop's value answers the first loop's number.
\
\ THE TWO RE-RESOLUTION ROWS ARE THE ONES A REFUSAL COULD NOT REPLACE. Out of
\ scope is a DIFFERENT MEANING, not an error: after its group's structure closes,
\ the same spelling is whatever else the body means by it. NLS-SHADOW reads a
\ CONSTANT of that name after the `then` and NLS-IDX reads the enclosing loop's
\ INDEX after the inner `loop`, and both answer through the chain what the engine
\ answers. An elaborator that kept binding them as the local would compile a body
\ that runs and answers something else.
\
\ Run: bin/hb --load test/compiler/native-locals-scope.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f

\ ---- the engine's compilation: the reference ---------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, whose locals frame really carves cells and really gives them back.
package NLS-FIXTURE

public

\ The name NLS-SHADOW reads after its scope has closed. It is a constant rather
\ than a colon word so the row needs no callee staging: what it proves is which
\ MEANING the second mention takes, and a constant answers that without dragging
\ a call seam into the case.
99 constant NLS-W

\ ---- a group inside one arm of an if -----------------------------------------
\ lib/ptx/kernel-abi.f FIELD-OFF! reduced to its shape: the outer group at the top
\ of the body, a second group inside the arm, and the outer name read after the
\ `then`.
: NLS-ARM ( n n -- n ) {: cls:n src:n :}
   src 0 > if
      cls 7 * {: off:n :}
      off cls 3 * +
   else
      cls 5 *
   then
   cls + ;

\ Nested groups inside one arm. Both die at the same `then` and both are read
\ before it, so the inner one's slot sits directly above the outer one's.
: NLS-NEST ( n -- n )
   dup 0 > if
      {: x:n :}
      x 3 *  {: y:n :}
      y 5 *  x 7 *  +
   else
      drop 11
   then ;

\ ---- a group inside a loop body ----------------------------------------------
\ lib/test/suite.f ITEM-ARGS-FEED reduced: a second group inside the loop body,
\ with the outer name read inside it and again after the loop.
: NLS-FEED ( n n -- n ) {: base:n lim:n :}
   0 lim 0 ?do
      base i +  {: u:n :}
      u 3 *  i 5 *  +  +
   loop
   base 7 * + ;

\ TWO groups in one loop body, which is the shape most of the blocked definitions
\ have. Each is weighted, so a bind that took the other's slot changes the answer.
: NLS-TWO ( n n -- n ) {: base:n lim:n :}
   0 lim 0 ?do
      base i +  {: u:n :}
      u 3 *     {: v:n :}
      v 5 *  u 7 *  +  +
   loop
   base + ;

\ An enclosing group crossing the loop edge while an inner one closes at the
\ loop's own closer: `k` is live at both ends of every edge of this loop and `a`
\ at none of them.
: NLS-CROSS ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do
      k i +  {: a:n :}
      a 3 *  +
   loop
   k 5 * + ;

\ THE SAME NAME IN TWO STRUCTURES THAT DO NOT CONTAIN EACH OTHER, which is the
\ slot given back and taken again. The weights differ, so a second loop reading
\ the first loop's slot answers the first loop's number.
: NLS-REUSE ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do  k i +  {: a:n :}  a 3 *  +  loop
   lim 0 ?do    k i -  {: a:n :}  a 5 *  +  loop
   k 11 * + ;

\ ---- a group under the other closers -----------------------------------------
\ A `while` keeps its frame open, so `c` is bound before the test and still
\ readable in the body after it - and gone after the `repeat`. The carried value
\ counts UP, so the loop terminates at every input rather than at the ones a
\ reader happened to check.
: NLS-WHILE ( n n -- n ) {: k:n lim:n :}
   0 0 begin
      {: c:n :}
      c 1 +
      dup lim <
   while
      swap  c k + 3 *  +  swap
   repeat
   drop  k 5 * + ;

\ A group in one `case` arm and the same name in the next arm's own scope: each
\ arm gives its names back at its own `endof`, so the second arm's `a` takes the
\ slot the first arm's gave up.
: NLS-CASE ( n n -- n ) {: k:n sel:n :}
   sel case
      1 of  k 3 *  {: a:n :}  a 5 *   endof
      2 of  k 7 *  {: a:n :}  a 11 *  endof
      k 13 *  swap
   endcase ;

\ `leave` branches out of a loop whose body has already bound a name, so the edge
\ carries the LOOP's list and not the walk's; the body goes on after the `then`
\ still holding that name.
: NLS-LEAVE ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do
      k i +  {: a:n :}
      a 3 > if leave then
      a 5 *  +
   loop
   k 7 * + ;

\ `exit` leaves the word from inside an arm that has bound a name, and the return
\ block takes no locals at all.
: NLS-EXIT ( n -- n )
   dup 0 > if
      {: x:n :}
      x 3 *  exit
   then
   drop 11 ;

\ ---- what the name means once the scope has closed ---------------------------
\ The mention after the `then` is the CONSTANT above, not the local. An
\ elaborator that kept the name bound would compile a body that runs and answers
\ 10 where this one answers 109.
: NLS-SHADOW ( n -- n )
   dup 0 > if  {: nls-w:n :}  nls-w 2 *  else  drop 7  then
   nls-w + ;

\ And the mention after the inner `loop` is the ENCLOSING loop's index. Inside the
\ inner body the same spelling is the local, which is what docs/forth.md means by
\ local-first; after the inner loop closes it is `i` again.
: NLS-IDX ( n -- n ) {: k:n :}
   0 3 0 ?do
      2 0 ?do
         k i +  {: i:n :}
         i 3 *  +
      loop
      i 5 * +
   loop ;

\ ---- a call under a scoped name ----------------------------------------------
\ Long enough that neither generator copies it, so what crosses is really a call.
: NLS-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ The one seam where the frame's list and the walk's differ on purpose: every edge
\ of the loop carries `k` alone, and the call carries `k` and `a` both.
: NLS-CALL ( n n n -- n ) {: k:n s:n lim:n :}
   0 lim 0 ?do
      s i +  {: a:n :}
      a NLS-CALLEE  k +  a 3 *  +  +
   loop
   k 5 * + ;

\ ONE SLOT, TWO NAMES, AND ONLY THE SECOND OF THEM TRAVELS. The first loop stands
\ before this body's only call, so nothing a call can reach reads `a` and `a`
\ stays where it is computed; the second loop stands after it and reads `b` on
\ both sides of the call, so `b` has to travel through a data-stack slot. The two
\ take the SAME slot, because the first gave it back at its own `loop` - so a
\ reader that asked which locals travel by SLOT rather than by NAME reads `a`'s
\ answer for `b`, leaves `b` in a register the callee destroys, and this row
\ answers something else. It is the one shape in this file where the two index
\ spaces of src/compiler/native/elaborate.f differ AND the difference is
\ observable, which is why LSX exists at all.
: NLS-SLOT ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do  {: a:n :}  a 3 *  loop
   lim 0 ?do
      {: b:n :}
      b NLS-CALLEE  k +  b 5 *  +
   loop
   k 7 * + ;

\ ---- a catch under a scoped name ---------------------------------------------
\ THE COMBINED SHAPE THE TWO LANDINGS MAKE, and neither suite alone reaches it.
\ `catch` stages ONE call to the engine's own routine (elaborate.f DO-CATCH,
\ through STAGE-WCALL), so every live local of the site is an operand of it and a
\ result of it - through the carrier this file's other rows measure. A name bound
\ inside a loop body or an arm therefore has to survive a `catch` in that same
\ body, and be gone at the closer all the same.
\
\ THE CAUGHT BODY RETURNS, AND THAT IS THE ONLY COMBINED SHAPE THERE IS. Three
\ ceilings were measured on this tree before these two rows were written, and
\ none of them is this landing's:
\
\   `[: dup 3 > if 9 throw then 3 * ;] catch`   -> E-IR-VERIFY-SUCCARG (-8088)
\   `[: NAMED-WORD ;] catch` with any live local -> E-IR-VERIFY-SCOPE (-8092)
\   `[: drop 9 throw ;] catch`                   -> E-NELAB-QUOT (-8651)
\
\ The first two reproduce on base bc72170f with no group inside a structure at
\ all - `{: k:n lim:n :} lim [: T ;] catch drop k 3 * +` answers -8092 there - so
\ they are the catch lane's and not this one's. The third is DO-CATCH's own
\ documented refusal of a body that never returns, dot
\ habu-compile-a-quotation-7efa798e. What is left, and what these two measure, is
\ the intersection this landing really does own: a name bound INSIDE a structure
\ is live across the call `catch` stages, travels as its operand and comes back as
\ its result, and is gone at the structure's closer all the same.
\
\ THE CODE IS READ AND THE VALUE SLOT IS NOT. The engine restores the stack's
\ DEPTH on a throw and never its CONTENTS, so `nip` keeps the throw code - which
\ both compilations owe each other - and drops the cell, which they do not.
: NLS-CATCH ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do
      k i +  {: a:n :}
      a [: 3 * ;] catch nip
      a 3 *  +  +
   loop
   k 5 * + ;

: NLS-ARMCATCH ( n n -- n ) {: k:n sel:n :}
   sel 0 > if
      k 7 *  {: a:n :}
      a [: 3 * ;] catch nip
      a 3 *  +
   else
      k 5 *
   then
   k + ;

\ ---- a scoped name spelled like a control word -------------------------------
\ THE SCAN THAT FINDS THE SCOPES READS CONTROL WORDS, SO IT HAS TO ASK ABOUT
\ NAMES FIRST, and these two are that question inside a structure.
\ test/compiler/native-again.f already runs `{: again:n :} again again +` at the
\ top of a body; what it cannot reach is the same spelling bound INSIDE one,
\ where a scan that read the mention as the closer would give back a mark no
\ structure ever took and put the name out of scope before the walk reads it.
\ Neither name closes the structure it sits in - `again` is not what ends a
\ counted loop and `endof` is not what ends an arm of an `if` - so the body still
\ says what it means with the local shadowing the keyword, which is what
\ docs/forth.md calls local-first.
: NLS-AGAINLOC ( n n -- n ) {: k:n lim:n :}
   0 lim 0 ?do
      k i +  {: again:n :}
      again 3 *  +
   loop
   k 5 * + ;

: NLS-ENDOFLOC ( n n -- n ) {: k:n sel:n :}
   sel 0 > if
      k 7 *  {: endof:n :}
      endof 3 *
   else
      k 5 *
   then
   k + ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry. The refusals are caught
\ into cells here and asserted below, because a migration runs while the fixture
\ package is open and an assertion reads better beside the others.
package NLS-MIGRATED

private

18 constant REGS

variable RC-SHADOW                   \ what a group shadowing a live name answered
variable RC-DISJOINT                 \ and one reusing a name whose scope had closed
variable RC-TWICE                    \ and one declaring the same name twice at once

: ARM ( -- )
   s" : NLS-ARM-N ( n n -- n ) {: cls:n src:n :} src 0 > if cls 7 * {: off:n :} off cls 3 * + else cls 5 * then cls + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: NEST ( -- )
   s" : NLS-NEST-N ( n -- n ) dup 0 > if {: x:n :} x 3 * {: y:n :} y 5 * x 7 * + else drop 11 then ;"
   1 1 REGS NMIGRATE:DEFINE ;

: FEED ( -- )
   s" : NLS-FEED-N ( n n -- n ) {: base:n lim:n :} 0 lim 0 ?do base i + {: u:n :} u 3 * i 5 * + + loop base 7 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: TWO ( -- )
   s" : NLS-TWO-N ( n n -- n ) {: base:n lim:n :} 0 lim 0 ?do base i + {: u:n :} u 3 * {: v:n :} v 5 * u 7 * + + loop base + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: CROSS ( -- )
   s" : NLS-CROSS-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a 3 * + loop k 5 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: REUSE ( -- )
   s" : NLS-REUSE-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a 3 * + loop lim 0 ?do k i - {: a:n :} a 5 * + loop k 11 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: WLOOP ( -- )
   s" : NLS-WHILE-N ( n n -- n ) {: k:n lim:n :} 0 0 begin {: c:n :} c 1 + dup lim < while swap c k + 3 * + swap repeat drop k 5 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: DISPATCH ( -- )
   s" : NLS-CASE-N ( n n -- n ) {: k:n sel:n :} sel case 1 of k 3 * {: a:n :} a 5 * endof 2 of k 7 * {: a:n :} a 11 * endof k 13 * swap endcase ;"
   2 1 REGS NMIGRATE:DEFINE ;

: LEAVELOOP ( -- )
   s" : NLS-LEAVE-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a 3 > if leave then a 5 * + loop k 7 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: EARLY-EXIT ( -- )
   s" : NLS-EXIT-N ( n -- n ) dup 0 > if {: x:n :} x 3 * exit then drop 11 ;"
   1 1 REGS NMIGRATE:DEFINE ;

: SHADOW ( -- )
   s" : NLS-SHADOW-N ( n -- n ) dup 0 > if {: nls-w:n :} nls-w 2 * else drop 7 then nls-w + ;"
   1 1 REGS NMIGRATE:DEFINE ;

: IDX ( -- )
   s" : NLS-IDX-N ( n -- n ) {: k:n :} 0 3 0 ?do 2 0 ?do k i + {: i:n :} i 3 * + loop i 5 * + loop ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NLS-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALLEE-STAGE ( -- )
   s" NLS-CALLEE-N" s" NLS-FIXTURE:NLS-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE ;

: CALL ( -- )
   CALLEE-STAGE
   s" : NLS-CALL-N ( n n n -- n ) {: k:n s:n lim:n :} 0 lim 0 ?do s i + {: a:n :} a NLS-CALLEE-N k + a 3 * + + loop k 5 * + ;"
   3 1 REGS NMIGRATE:DEFINE-CALLING ;

\ THE CALLEE THIS ONE STAGES IS THE ENGINE'S OWN ROUTINE, and that is the whole
\ difference. A callee the chain compiled has a clobber row, so
\ src/compiler/native/elaborate.f CALL-KEEPS? answers that it keeps registers for
\ its caller and no local of the caller has to travel at all. A routine the chain
\ never compiled has no such row, is taken to destroy the whole pool, and every
\ local a call can reach then travels through a data-stack slot - which is the
\ mixed state a partly-migrated tree really is in, and the only state in which
\ CROSS-L is not zero.
: CALLEE-ENGINE ( -- )
   s" NLS-CALLEE" s" NLS-FIXTURE:NLS-CALLEE" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE ;

: SLOT ( -- )
   CALLEE-ENGINE
   s" : NLS-SLOT-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do {: a:n :} a 3 * loop lim 0 ?do {: b:n :} b NLS-CALLEE k + b 5 * + loop k 7 * + ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: CATCHLOOP ( -- )
   s" : NLS-CATCH-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a [: 3 * ;] catch nip a 3 * + + loop k 5 * + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: CATCHARM ( -- )
   s" : NLS-ARMCATCH-N ( n n -- n ) {: k:n sel:n :} sel 0 > if k 7 * {: a:n :} a [: 3 * ;] catch nip a 3 * + else k 5 * then k + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: KEYWORDLOC ( -- )
   s" : NLS-AGAINLOC-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: again:n :} again 3 * + loop k 5 * + ;"
   2 1 REGS NMIGRATE:DEFINE
   s" : NLS-ENDOFLOC-N ( n n -- n ) {: k:n sel:n :} sel 0 > if k 7 * {: endof:n :} endof 3 * else k 5 * then k + ;"
   2 1 REGS NMIGRATE:DEFINE ;

\ ---- the three duplicate questions, measured and recorded ---------------------
\ MEASURE-HELD runs every stage a publication runs and keeps none of it, so a
\ refusal is the throw it answers with and a compilable body is a zero. The three
\ bodies differ ONLY in whether the two declarations of one name are in scope at
\ the same time.
\
\ A READER KEYED ON "THE NAME APPEARS TWICE" REDS THE FIRST OF THEM, because two
\ loops that each bind `a` are an ordinary body the checker certifies and the tree
\ writes. A reader that asked nothing at all reds the other two. Only a reader of
\ what is in SCOPE where the declaration stands answers all three.
: TRY-DISJOINT ( -- )
   s" : NLS-Z1-N ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a 3 * + loop lim 0 ?do k i - {: a:n :} a 5 * + loop k 11 * + ;"
   2 1 REGS NMIGRATE:MEASURE-HELD ;

: TRY-SHADOW ( -- )
   s" : NLS-Z2-N ( n -- n ) {: v:n :} v 0 > if 1 0= {: v:bool :} v if 1 else 2 then else 0 then ;"
   1 1 REGS NMIGRATE:MEASURE-HELD ;

: TRY-TWICE ( -- )
   s" : NLS-Z3-N ( n n -- n ) {: a:n a:n :} a ;"
   2 1 REGS NMIGRATE:MEASURE-HELD ;

public

: RC-SHADOW@ ( -- n ) RC-SHADOW @ ;
: RC-DISJOINT@ ( -- n ) RC-DISJOINT @ ;
: RC-TWICE@ ( -- n ) RC-TWICE @ ;

: RUN ( -- )
   ARM NEST
   FEED TWO CROSS REUSE
   WLOOP DISPATCH LEAVELOOP EARLY-EXIT
   SHADOW IDX
   CALLEE CALL SLOT
   CATCHLOOP CATCHARM KEYWORDLOC
   [: TRY-DISJOINT ;] catch RC-DISJOINT !
   [: TRY-SHADOW ;] catch RC-SHADOW !
   [: TRY-TWICE ;] catch RC-TWICE ! ;

;package

package NLS-FIXTURE
public

NLS-MIGRATED:RUN

;package

package NLS-TEST

private

\ The ends of the signed range, where arithmetic that is right for small numbers
\ is most likely to disagree.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ ---- the differentials -------------------------------------------------------
: ARM= ( n n -- ) {: a:n b:n :}
   a b NLS-FIXTURE:NLS-ARM  a b NLS-FIXTURE:NLS-ARM-N  T= ;

: NEST= ( n -- ) {: k:n :}
   k NLS-FIXTURE:NLS-NEST  k NLS-FIXTURE:NLS-NEST-N  T= ;

: FEED= ( n n -- ) {: a:n lim:n :}
   a lim NLS-FIXTURE:NLS-FEED   a lim NLS-FIXTURE:NLS-FEED-N   T=
   a lim NLS-FIXTURE:NLS-TWO    a lim NLS-FIXTURE:NLS-TWO-N    T=
   a lim NLS-FIXTURE:NLS-CROSS  a lim NLS-FIXTURE:NLS-CROSS-N  T=
   a lim NLS-FIXTURE:NLS-REUSE  a lim NLS-FIXTURE:NLS-REUSE-N  T= ;

: WHILE= ( n n -- ) {: k:n lim:n :}
   k lim NLS-FIXTURE:NLS-WHILE  k lim NLS-FIXTURE:NLS-WHILE-N  T= ;

: CASE= ( n n -- ) {: k:n sel:n :}
   k sel NLS-FIXTURE:NLS-CASE  k sel NLS-FIXTURE:NLS-CASE-N  T= ;

: LEAVE= ( n n -- ) {: k:n lim:n :}
   k lim NLS-FIXTURE:NLS-LEAVE  k lim NLS-FIXTURE:NLS-LEAVE-N  T= ;

: EXIT= ( n -- ) {: k:n :}
   k NLS-FIXTURE:NLS-EXIT  k NLS-FIXTURE:NLS-EXIT-N  T= ;

: SHADOW= ( n -- ) {: k:n :}
   k NLS-FIXTURE:NLS-SHADOW  k NLS-FIXTURE:NLS-SHADOW-N  T=
   k NLS-FIXTURE:NLS-IDX     k NLS-FIXTURE:NLS-IDX-N     T= ;

: CALL= ( n n n -- ) {: k:n s:n lim:n :}
   k s lim NLS-FIXTURE:NLS-CALL  k s lim NLS-FIXTURE:NLS-CALL-N  T= ;

: SLOT= ( n n -- ) {: k:n lim:n :}
   k lim NLS-FIXTURE:NLS-SLOT  k lim NLS-FIXTURE:NLS-SLOT-N  T= ;

: CATCH= ( n n -- ) {: k:n lim:n :}
   k lim NLS-FIXTURE:NLS-CATCH     k lim NLS-FIXTURE:NLS-CATCH-N     T=
   k lim NLS-FIXTURE:NLS-ARMCATCH  k lim NLS-FIXTURE:NLS-ARMCATCH-N  T= ;

: KEYWORD= ( n n -- ) {: k:n lim:n :}
   k lim NLS-FIXTURE:NLS-AGAINLOC  k lim NLS-FIXTURE:NLS-AGAINLOC-N  T=
   k lim NLS-FIXTURE:NLS-ENDOFLOC  k lim NLS-FIXTURE:NLS-ENDOFLOC-N  T= ;

\ ---- the cases ---------------------------------------------------------------
\ EVERY INPUT BELOW IS ON ONE SIDE OF A TEST THE BODY MAKES. NLS-ARM branches on
\ `src > 0`, so both arms run; without a negative row an arm that was never taken
\ could not tell the two compilations apart.
: ARM-CASE ( -- )
   s" a group in one arm of an if is gone at the then" T-LABEL
   0 0 ARM=  1 1 ARM=  1 -1 ARM=  3 0 ARM=  -3 1 ARM=  -3 -1 ARM=
   MAX-INT 1 ARM=  MIN-INT 1 ARM=  MAX-INT -1 ARM=
   0 NEST=  1 NEST=  -1 NEST=  7 NEST=  MIN-INT NEST=  MAX-INT NEST= ;

\ ZERO TURNS, ONE TURN AND SEVERAL, which is what tells a body that lost a name on
\ the way INTO the loop from one that lost it on the way OUT: at zero turns the
\ header runs once and the body never does, so the group never binds at all.
: LOOP-CASE ( -- )
   s" a group in a loop body is gone at the loop" T-LABEL
   0 0 FEED=  0 1 FEED=  0 5 FEED=
   7 0 FEED=  7 1 FEED=  7 4 FEED=  -3 4 FEED=
   MAX-INT 3 FEED=  MIN-INT 3 FEED= ;

\ THE `while` ROWS STRADDLE ITS OWN CUT. The carried value starts at one and
\ counts up, so a limit at, below and above that runs the body never, once and
\ several times.
: WHILE-CASE ( -- )
   s" a group before a while is readable in the body and gone after the repeat" T-LABEL
   0 0 WHILE=  0 1 WHILE=  0 2 WHILE=  0 5 WHILE=
   7 0 WHILE=  7 1 WHILE=  7 5 WHILE=  -3 5 WHILE=
   MAX-INT 3 WHILE=  MIN-INT 3 WHILE= ;

: CASE-CASE ( -- )
   s" an arm's group is gone at its own endof" T-LABEL
   0 0 CASE=  0 1 CASE=  0 2 CASE=  0 3 CASE=
   7 1 CASE=  7 2 CASE=  7 9 CASE=  -3 1 CASE=  -3 2 CASE= ;

\ `leave` CUTS AT `a > 3` AND `a` IS `k + i`, so limits and offsets on both sides
\ of that cut run the loop to its end, exactly to the cut, and past it.
: EARLY-CASE ( -- )
   s" leave carries the loop's names and exit carries none" T-LABEL
   0 0 LEAVE=  0 1 LEAVE=  0 5 LEAVE=  3 5 LEAVE=  4 5 LEAVE=
   7 5 LEAVE=  -3 5 LEAVE=  MAX-INT 3 LEAVE=
   0 EXIT=  1 EXIT=  -1 EXIT=  7 EXIT=  MIN-INT EXIT=  MAX-INT EXIT= ;

\ THE TWO RE-RESOLUTION ROWS. NLS-SHADOW answers 109 and 106 through both
\ compilations because the mention after the `then` is the constant; a chain that
\ kept the name bound answers 10 and 7 and reds here.
: MEANING-CASE ( -- )
   s" a name out of scope is what the body means by it, not the local" T-LABEL
   5 NLS-FIXTURE:NLS-SHADOW-N 109 T=
   -1 NLS-FIXTURE:NLS-SHADOW-N 106 T=
   0 SHADOW=  1 SHADOW=  -1 SHADOW=  5 SHADOW=  7 SHADOW=
   MIN-INT SHADOW=  MAX-INT SHADOW= ;

: CALL-CASE ( -- )
   s" a call carries the walk's names and the loop's edges carry the frame's" T-LABEL
   0 0 0 CALL=  1 2 0 CALL=  1 2 1 CALL=  3 5 4 CALL=
   0 0 SLOT=  1 0 SLOT=  1 1 SLOT=  3 4 SLOT=  -3 4 SLOT=
   7 2 SLOT=  MAX-INT 3 SLOT=  MIN-INT 3 SLOT= ;

\ THE ARM ROWS STRADDLE `sel > 0` AND THE LOOP ROWS RUN NO TURNS, ONE AND
\ SEVERAL, so both arms of the `if` and all three trip counts reach the catch -
\ and the negative-limit rows prove the group never binds at all on a loop that
\ does not run, which is where a chain that carried the name anyway would differ.
: CATCH-CASE ( -- )
   s" a scoped name survives a catch in the same body" T-LABEL
   0 0 CATCH=  0 1 CATCH=  0 5 CATCH=  1 5 CATCH=  3 5 CATCH=
   4 1 CATCH=  7 4 CATCH=  -3 5 CATCH=
   0 -1 CATCH=  1 -1 CATCH=  7 -1 CATCH= ;

\ A name spelled like a control word, bound inside a structure. The scan that
\ finds the scopes reads control words, so a mention it did not ask about first
\ would close a structure the body never opened - which is the one regression
\ this landing shipped and the gate caught, on the top-level shape
\ test/compiler/native-again.f already owns.
: KEYWORD-CASE ( -- )
   s" a scoped name spelled like a control word is still the name" T-LABEL
   0 0 KEYWORD=  0 1 KEYWORD=  3 4 KEYWORD=  7 2 KEYWORD=
   -3 4 KEYWORD=  2 -1 KEYWORD=  MAX-INT 3 KEYWORD=  MIN-INT 3 KEYWORD= ;

\ ---- the duplicate question --------------------------------------------------
\ THE THREE ANSWERS ARE THE FIXTURE, and no two of them may move together. The
\ first is an ordinary body: two loops whose scopes never overlap may both bind
\ `a`, and a reader keyed on the spelling appearing twice reds it. The other two
\ declare a name that is already in scope, and the chain refuses them because its
\ two authorities disagree about what a mention of that name MEANS - the checker
\ resolves it to the innermost binding (src/core/checker.f LOC-REF? counts down
\ from #LOC), the engine to the outermost (src/habu/habu2.f EMIT-LOC-FIND counts
\ up from zero). Dot habu-reconcile-the-locals-ca3fdb26 carries the
\ reconciliation; when it lands this row's reason changes or the refusal goes.
: DUPLICATE-CASE ( -- )
   s" the duplicate the chain refuses is a LIVE one, and only a live one" T-LABEL
   NLS-MIGRATED:RC-DISJOINT@ 0 T=
   NLS-MIGRATED:RC-SHADOW@ E-NELAB-LOCAL T=
   NLS-MIGRATED:RC-TWICE@ E-NELAB-LOCAL T= ;

public

: RUN ( -- )
   T-RESET
   ARM-CASE
   LOOP-CASE
   WHILE-CASE
   CASE-CASE
   EARLY-CASE
   MEANING-CASE
   CALL-CASE
   CATCH-CASE
   KEYWORD-CASE
   DUPLICATE-CASE
   T-REPORT ;

;package

NLS-TEST:RUN
