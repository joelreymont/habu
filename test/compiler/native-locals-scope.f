\ native-locals-scope.f - locals that open and close inside control structures.

require test/compiler/native-eval-fixture.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/prelude.f
require src/compiler/native/compiler.f

\ ---- the production programs under test --------------------------------------
package NLS-FIXTURE

public

\ The name NLS-SHADOW reads after its scope has closed. It is a constant rather
\ than a colon word so the row has no call in it at all: what it proves is which
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
\ `catch` stages ONE call to the runtime primitive (elaborate.f DO-CATCH,
\ through STAGE-WCALL), so every live local of the site is an operand of it and a
\ result of it - through the carrier this file's other rows measure. A name bound
\ inside a loop body or an arm therefore has to survive a `catch` in that same
\ body, and be gone at the closer all the same.
\
\ THE CAUGHT BODY RETURNS, AND THAT IS THE ONLY COMBINED SHAPE THERE IS. Three
\ ceilings were measured on this tree before these two rows were written, and
\ none of them is this landing's:
\
\   `[: dup 3 > if 9 throw then 3 * ;] catch`   -> a body holding control flow
\   `[: NAMED-WORD ;] catch` with any live local -> a body that CALLS under a group
\   `[: drop 9 throw ;] catch`                   -> E-NELAB-QUOT (-8651)
\
\ The first two reproduce on base bc72170f with no group inside a structure at
\ all - `{: k:n lim:n :} lim [: T ;] catch drop k 3 * +` answers -8092 there - so
\ they are the quotation path's and not this one's. The SECOND has since been
\ lifted: a body is now built with no local scope of its own to inherit
\ (src/compiler/native/elaborate.f FUN-STATE!, measured in
\ test/compiler/native-quot-scope.f), so a calling body under a group compiles.
\ The FIRST has been lifted too: a body's successors are named in the module's
\ own block table and every machine pass now subtracts the base its function's
\ blocks start at, so a body holding control flow compiles and answers what the
\ production code answers (measured in the same file). The third is
\ DO-CATCH's own documented refusal of a body that never returns, dot
\ habu-compile-a-quotation-7efa798e. What is left, and what these two measure, is
\ the intersection this landing really does own: a name bound INSIDE a structure
\ is live across the call `catch` stages, travels as its operand and comes back as
\ its result, and is gone at the structure's closer all the same.
\
\ THE CODE IS READ AND THE VALUE SLOT IS NOT. `catch` restores the stack's
\ DEPTH on a throw and never its CONTENTS, so `nip` keeps the throw code - which
\ the site must preserve - and drops the cell, which it does not promise.
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

package NLS-TEST

private

: DEF-RC ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

\ The ends of the signed range, where arithmetic that is right for small numbers
\ is most likely to disagree.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ ---- the cases ---------------------------------------------------------------
\ EVERY INPUT BELOW IS ON ONE SIDE OF A TEST THE BODY MAKES. NLS-ARM branches on
\ `src > 0`, so both arms run; without a negative row an arm that was never taken
\ could not tell whether the arm was compiled correctly.
: ARM-CASE ( -- )
   s" a group in one arm of an if is gone at the then" T-LABEL
   3 1 NLS-FIXTURE:NLS-ARM 33 T=
   3 -1 NLS-FIXTURE:NLS-ARM 18 T=
   2 NLS-FIXTURE:NLS-NEST 44 T=
   -1 NLS-FIXTURE:NLS-NEST 11 T= ;

\ ZERO TURNS, ONE TURN AND SEVERAL, which is what tells a body that lost a name on
\ the way INTO the loop from one that lost it on the way OUT: at zero turns the
\ header runs once and the body never does, so the group never binds at all.
: LOOP-CASE ( -- )
   s" a group in a loop body is gone at the loop" T-LABEL
   2 0 NLS-FIXTURE:NLS-FEED 14 T=
   2 1 NLS-FIXTURE:NLS-FEED 20 T=
   2 2 NLS-FIXTURE:NLS-FEED 34 T=
   1 0 NLS-FIXTURE:NLS-TWO 1 T=
   1 1 NLS-FIXTURE:NLS-TWO 23 T=
   1 3 NLS-FIXTURE:NLS-TWO 133 T=
   2 0 NLS-FIXTURE:NLS-CROSS 10 T=
   2 1 NLS-FIXTURE:NLS-CROSS 16 T=
   2 2 NLS-FIXTURE:NLS-CROSS 25 T=
   2 0 NLS-FIXTURE:NLS-REUSE 22 T=
   2 1 NLS-FIXTURE:NLS-REUSE 38 T=
   2 2 NLS-FIXTURE:NLS-REUSE 52 T= ;

\ THE `while` ROWS STRADDLE ITS OWN CUT. The carried value starts at one and
\ counts up, so a limit at, below and above that runs the body never, once and
\ several times.
: WHILE-CASE ( -- )
   s" a group before a while is readable in the body and gone after the repeat" T-LABEL
   2 1 NLS-FIXTURE:NLS-WHILE 10 T=
   2 2 NLS-FIXTURE:NLS-WHILE 16 T=
   2 4 NLS-FIXTURE:NLS-WHILE 37 T= ;

: CASE-CASE ( -- )
   s" an arm's group is gone at its own endof" T-LABEL
   2 1 NLS-FIXTURE:NLS-CASE 30 T=
   2 2 NLS-FIXTURE:NLS-CASE 154 T=
   2 9 NLS-FIXTURE:NLS-CASE 26 T= ;

\ `leave` CUTS AT `a > 3` AND `a` IS `k + i`, so limits and offsets on both sides
\ of that cut run the loop to its end, exactly to the cut, and past it.
: EARLY-CASE ( -- )
   s" leave carries the loop's names and exit carries none" T-LABEL
   2 5 NLS-FIXTURE:NLS-LEAVE 39 T=
   2 NLS-FIXTURE:NLS-EXIT 6 T=
   -1 NLS-FIXTURE:NLS-EXIT 11 T= ;

\ THE TWO RE-RESOLUTION ROWS. NLS-SHADOW answers 109 and 106 through both
\ paths because the mention after the `then` is the constant; code that
\ kept the name bound answers 10 and 7 and reds here.
: MEANING-CASE ( -- )
   s" a name out of scope is what the body means by it, not the local" T-LABEL
   5 NLS-FIXTURE:NLS-SHADOW 109 T=
   -1 NLS-FIXTURE:NLS-SHADOW 106 T= ;

: CALL-CASE ( -- )
   s" a call carries the walk's names and the loop's edges carry the frame's" T-LABEL
   2 0 0 NLS-FIXTURE:NLS-CALL 10 T=
   2 0 1 NLS-FIXTURE:NLS-CALL 61 T=
   2 0 3 NLS-FIXTURE:NLS-CALL 368 T=
   2 0 NLS-FIXTURE:NLS-SLOT 14 T=
   2 1 NLS-FIXTURE:NLS-SLOT 65 T=
   2 3 NLS-FIXTURE:NLS-SLOT 147079 T= ;

\ THE ARM ROWS STRADDLE `sel > 0` AND THE LOOP ROWS RUN NO TURNS, ONE AND
\ SEVERAL, so both arms of the `if` and all three trip counts reach the catch -
\ and the negative-limit rows prove the group never binds at all on a loop that
\ does not run, which is where code that carried the name anyway would differ.
: CATCH-CASE ( -- )
   s" a scoped name survives a catch in the same body" T-LABEL
   2 0 NLS-FIXTURE:NLS-CATCH 10 T=
   2 1 NLS-FIXTURE:NLS-CATCH 16 T=
   2 3 NLS-FIXTURE:NLS-CATCH 37 T=
   2 -1 NLS-FIXTURE:NLS-ARMCATCH 12 T=
   2 1 NLS-FIXTURE:NLS-ARMCATCH 44 T= ;

: DUPLICATE-CASE ( -- )
   s" disjoint scopes may reuse a local name" T-LABEL
   s" : NLS-Z1 ( n n -- n ) {: k:n lim:n :} 0 lim 0 ?do k i + {: a:n :} a 3 * + loop lim 0 ?do k i - {: a:n :} a 5 * + loop k 11 * + ;"
   DEF-RC 0 T=

   s" a live shadow and a duplicate in one group remain named refusals" T-LABEL
   s" : NLS-Z2 ( n -- n ) {: v:n :} v 0 > if 1 0= {: v:bool :} v if 1 else 2 then else 0 then ;"
   DEF-RC E-NELAB-LOCAL T=
   s" : NLS-Z3 ( n n -- n ) {: a:n a:n :} a ;"
   DEF-RC E-NELAB-LOCAL T= ;

\ A name spelled like a control word, bound inside a structure. The scan that
\ finds the scopes reads control words, so a mention it did not ask about first
\ would close a structure the body never opened - which is the one regression
\ this landing shipped and the gate caught, on the top-level shape
\ test/compiler/native-again.f already owns.
: KEYWORD-CASE ( -- )
   s" a scoped name spelled like a control word is still the name" T-LABEL
   2 1 NLS-FIXTURE:NLS-AGAINLOC 16 T=
   2 1 NLS-FIXTURE:NLS-ENDOFLOC 44 T= ;

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
