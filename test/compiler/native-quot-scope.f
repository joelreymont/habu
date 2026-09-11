\ native-quot-scope.f - quotation bodies, locals, and control-flow scope.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/compiler.f

\ ---- the programs under test --------------------------------------------------
package QSC-FIXTURE

public

\ A callee reached through the conservative calling convention. Every live name
\ mentioned after the call must therefore travel across it.
: QSC-OK1 ( n -- n )
   1+ ;

\ The same for a callee that throws for a big enough input, so ONE text measures
\ both paths of the same catch: what the caller gets back and what the name holds
\ afterwards are different questions on the two paths.
: QSC-BAD ( n -- n )
   dup 20 > if 9 throw then 2 * ;

\ The route a quotation reached a body by before `catch` existed: an argument a
\ callee declares. It is here because the ceiling this file is about is not
\ `catch`'s - it is the body's - and this is the second consumer that proves it.
: QSC-APPLY ( [ n -- n ] n -- n )
   swap execute ;

\ THE PRODUCTION SHAPE, END TO END. The code into a name, a decision on it, and
\ the throw passed on unchanged - which is how the tree writes a catch: twenty of
\ the forty catch sites in src and lib are written this way, and until this lane
\ the caught body could not CALL anything.
: QSC-P ( n -- n )
   [: QSC-BAD ;] catch {: rc:n :}
   rc 0 <> if rc throw then ;

\ The dot's own reproducer, weighted. `lim` is spent before the call and `k`
\ after it, so `k` is the one that has to survive the call and `lim` is the one
\ it must not be confused with.
: QSC-A ( n n -- n )
   {: k:n lim:n :}
   lim 5 * [: QSC-OK1 ;] catch drop  k 3 * + ;

\ The same shape with the code kept as data, so a caller can measure the
\ throwing path as well: the value the window holds afterwards is the runtime's
\ answer about a caught throw, and the name added to it is this file's subject.
: QSC-T ( n n -- n n )
   {: k:n lim:n :}
   lim 5 * [: QSC-BAD ;] catch {: rc:n :}
   k 3 * +  rc ;

\ TWO names live across the same call, both read after it, each weighted
\ differently - so a carrier that handed them over in the other order answers a
\ different number rather than the same sum.
: QSC-2 ( n n -- n )
   {: a:n b:n :}
   a 3 * [: QSC-OK1 ;] catch drop  a 5 * +  b 7 * + ;

\ The same body through the pre-catch route, under a group: the quotation is an
\ argument the callee declared and the body is entered by `execute`.
: QSC-E ( n n -- n )
   {: k:n lim:n :}
   [: QSC-OK1 ;] lim 5 * QSC-APPLY  k 3 * + ;

\ EVERYTHING AT ONCE: a counted loop, a group opened INSIDE its body, a call in
\ the quotation, and a name from each scope read after that call. The loop's
\ counters, the outer name and the arm's name all cross the same call site.
: QSC-L ( n -- n )
   {: v:n :}
   0 3 0 ?do
      i {: t:n :}
      v [: QSC-OK1 ;] catch drop  t 7 * +  +
   loop ;

\ ---- bodies that hold a control structure of their own ------------------------
\ A BODY HOLDING AN `if`, BOTH ARMS WEIGHTED. This is the smallest text that can
\ name a successor at all, and the smallest that could name the wrong one.
: QSC-IF ( n -- n n )
   [: dup 3 > if 3 * else 5 * then ;] catch ;

\ A BODY HOLDING A COUNTED LOOP. The trip count is a constant and the value under
\ test is what the turns work on, because a count driven by that value hangs the
\ suite at some inputs.
: QSC-DO ( n -- n n )
   [: 4 0 ?do 2 * 1+ loop ;] catch ;

\ Both at once: a decision inside a loop body, which is the widest block window a
\ body here has - a loop head, two arms, their join, and the exit.
: QSC-IFDO ( n -- n n )
   [: 4 0 ?do dup 9 > if 3 * else 5 + then loop ;] catch ;

\ THE PRODUCTION SHAPE WITH THE DECISION INSIDE THE BODY, which is the shape this
\ ceiling really bounded: the code into a name, a decision on it, the throw passed
\ on unchanged - and the body itself choosing whether to throw.
: QSC-PB ( n -- n )
   [: dup 3 > if 9 throw then 7 * ;] catch {: rc:n :}
   rc 0 <> if rc throw then ;

\ TWO BRANCHING BODIES IN ONE DEFINITION, and it is the case a single body cannot
\ make: the second body's blocks start where the first body's ended, so a pass
\ reading a successor without the base names a block of the FIRST BODY for the
\ second one, where the first body named a block of the definition.
: QSC-2B ( n -- n )
   [: dup 3 > if 3 * else 5 * then ;] catch drop
   [: dup 20 > if 7 * else 11 * then ;] catch drop ;

\ The same with the enclosing routine branching too, so no two of the three
\ functions in the module start at the same ordinal.
: QSC-3B ( n -- n )
   dup 3 > if 1+ then
   [: dup 3 > if 3 * else 5 * then ;] catch drop
   [: dup 9 > if 1+ else 2 + then ;] catch drop ;

\ A branching body under a definition with a locals group: the two ceilings this
\ file is about, met in one text. `lim` is spent on the body and `k` is read
\ after it, so `k` is the one that has to survive.
: QSC-BG ( n n -- n )
   {: k:n lim:n :}
   lim [: dup 3 > if 3 * else 5 * then ;] catch drop  k 7 * + ;

\ The pre-catch route with a branching body - a quotation an argument declares,
\ entered by `execute` - because the ceiling was the BODY's and never `catch`'s.
: QSC-EB ( n n -- n )
   {: k:n lim:n :}
   [: dup 3 > if 3 * else 5 * then ;] lim QSC-APPLY  k 7 * + ;

: QSC-THROW ( n -- n n )
   [: dup 3 > if 9 throw else 5 throw then ;] catch ;


;package

package QSC-TEST

private

TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

public

\ THE CEILING ITSELF, MEASURED AS AN ACCEPTANCE. Every case below runs code the
\ compiler produced, so a refusal would take the whole file down at load with the
\ compilation's own throw; this one asks the question where a reader can see the
\ answer, and its twin without the group is what says the group is what the
\ question is about.
 : PRODUCTION-CASE ( -- )
   s" the production catch shape, end to end, on both paths" T-LABEL
   7 QSC-FIXTURE:QSC-P 14 T=
   [: 30 QSC-FIXTURE:QSC-P drop ;] 9 TTHROWSQ ;

: CALL-LOCALS-CASE ( -- )
   s" a name spent before the call and a name read after it" T-LABEL
   3 4 QSC-FIXTURE:QSC-A 30 T= ;

: THROW-LOCALS-CASE ( -- )
   s" the same shape with the code kept, on both paths" T-LABEL
   3 4 QSC-FIXTURE:QSC-T 0 T= 49 T=
   3 100 QSC-FIXTURE:QSC-T 9 T= 509 T= ;

: TWO-LOCALS-CASE ( -- )
   s" two names live across one call, each weighted" T-LABEL
   3 5 QSC-FIXTURE:QSC-2 60 T= ;

: EXEC-CASE ( -- )
   s" the same body under a group through the pre-catch route" T-LABEL
   3 4 QSC-FIXTURE:QSC-E 30 T= ;

: LOOP-GROUP-CASE ( -- )
   s" a calling body in a loop whose own body holds a group" T-LABEL
   5 QSC-FIXTURE:QSC-L 39 T= ;

: ENCLOSING-SHAPE-CASE ( -- )
   s" a branching body compiles under distinct enclosing block shapes" T-LABEL
   [: s" : QSC-B1 ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;" EV ;] 0 TTHROWSQ
   [: s" : QSC-B2 ( n -- n n ) dup 3 > if 1+ then [: dup 3 > if 1+ then ;] catch ;" EV ;]
   0 TTHROWSQ
   [: s" : QSC-B3 ( n -- n n ) dup 3 > if 1+ else 2 + then [: dup 3 > if 1+ then ;] catch ;" EV ;]
   0 TTHROWSQ
   [: s" : QSC-B4 ( n -- n n ) dup 3 > if 1+ then [: 1+ ;] catch ;" EV ;]
   0 TTHROWSQ

   s" and differing enclosing/body join widths compile too" T-LABEL
   [: s" : QSC-B5 ( n -- n n ) dup 3 > if 1+ then [: dup dup 3 > if 1+ else 2 + then + 1- ;] catch ;" EV ;]
   0 TTHROWSQ
   [: s" : QSC-B6 ( n n -- n n n ) 2dup > if 1+ then [: dup 3 > if 1+ then ;] catch ;" EV ;]
   0 TTHROWSQ ;

: STILL-REFUSED-CASE ( -- )
   s" catch runs a quotation whose every path throws" T-LABEL
   7 QSC-FIXTURE:QSC-THROW 9 T= 7 T=
   2 QSC-FIXTURE:QSC-THROW 5 T= 2 T=
   [: s" : QSC-R2 ( n -- n n ) [: dup 3 > if 9 throw else 1+ then ;] catch ;" EV ;]
   0 TTHROWSQ

   s" exit inside a quotation arm remains an arity refusal" T-LABEL
   [: s" : QSC-R3 ( n -- n n ) [: dup 3 > if 1+ exit then 2 + ;] catch ;" EV ;]
   E-NELAB-ARITY TTHROWSQ ;

\ THE CEILING THAT WAS HERE, MEASURED FROM THE OTHER SIDE. A body holding a
\ control structure used to be refused, and before the elaborator's half of the
\ fix WHICH refusal it got depended on the routine around it: the body's
\ successors were named by ordinals in the ENCLOSING function's block window, so
\ one text was refused as a successor-argument mismatch under a straight-line
\ definition (E-IR-VERIFY-SUCCARG), as a dominance failure under one holding an
\ `if` (E-IR-VERIFY-DOM), and as a join disagreement whenever the two joins
\ carried different numbers of values (E-NELAB-JOIN, which the two rows about
\ differing join widths are shaped to produce). The elaborator then made it ONE
\ refusal - the register allocator's - and this lane's subtraction made it none.
\
\ A BODY'S COMPILATION CANNOT DEPEND ON THE SHAPE OF THE ROUTINE AROUND IT, and
\ the six texts that proved it as one refusal prove it here as one acceptance.
\ They are still MEASURED and not published: what they are for is the compile,
\ and their answers are the production cases' business.
\ ---- what the branching bodies answer ----------------------------------------
\ The acceptances above say the module reached the emitter; these pin the code's
\ results over inputs that take each arm.
: CONTROL-BODY-CASE ( -- )
   s" a body holding an if, both arms" T-LABEL
   7 QSC-FIXTURE:QSC-IF 0 T= 21 T=
   3 QSC-FIXTURE:QSC-IF 0 T= 15 T=
   s" a body holding a counted loop" T-LABEL
   1 QSC-FIXTURE:QSC-DO 0 T= 31 T=
   s" and a decision inside that loop takes both paths" T-LABEL
   1 QSC-FIXTURE:QSC-IFDO 0 T= 99 T=
   10 QSC-FIXTURE:QSC-IFDO 0 T= 810 T= ;

: BRANCHING-PRODUCTION-CASE ( -- )
   s" the production shape with the decision inside the body, both paths" T-LABEL
   3 QSC-FIXTURE:QSC-PB 21 T=
   [: 30 QSC-FIXTURE:QSC-PB drop ;] 9 TTHROWSQ ;

: TWO-BODY-CASE ( -- )
   s" two branching bodies in one definition" T-LABEL
   3 QSC-FIXTURE:QSC-2B 165 T=
   7 QSC-FIXTURE:QSC-2B 147 T=
   s" and the enclosing routine may branch before both bodies" T-LABEL
   3 QSC-FIXTURE:QSC-3B 16 T=
   9 QSC-FIXTURE:QSC-3B 31 T=
   -5 QSC-FIXTURE:QSC-3B -23 T= ;

: BRANCHING-GROUP-CASE ( -- )
   s" a branching body under a definition with a locals group" T-LABEL
   3 4 QSC-FIXTURE:QSC-BG 33 T=
   3 3 QSC-FIXTURE:QSC-BG 36 T=
   s" and the same body through the pre-catch route" T-LABEL
   3 4 QSC-FIXTURE:QSC-EB 33 T=
   3 3 QSC-FIXTURE:QSC-EB 36 T= ;

: RUN ( -- )
   ENCLOSING-SHAPE-CASE
   STILL-REFUSED-CASE
   CONTROL-BODY-CASE
   BRANCHING-PRODUCTION-CASE
   TWO-BODY-CASE
   BRANCHING-GROUP-CASE
   PRODUCTION-CASE
   CALL-LOCALS-CASE
   THROW-LOCALS-CASE
   TWO-LOCALS-CASE
   EXEC-CASE
   LOOP-GROUP-CASE ;

;package

T-RESET
QSC-TEST:RUN
T-REPORT
