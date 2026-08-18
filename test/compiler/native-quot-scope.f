\ native-quot-scope.f - a quotation body is a function of its own, and the scope
\ it is built in is its own too. One concern: what a body may carry across a call
\ when the definition around it holds locals.
\
\ WHAT WAS WRONG AND WHY IT ONLY SHOWED HERE. A quotation body is built as
\ another function of the same module, by a second walk over a range of the same
\ tape, after the enclosing function is closed (src/compiler/native/elaborate.f
\ QBUILD). That walk used to inherit the enclosing function's local scope: LBN
\ still counted the names a `{: … :}` group had bound, so CROSS-L counted them as
\ crossing, so a CALL inside the body handed the enclosing function's local
\ values over as operands and took them back as results. The freeze verifier
\ names exactly that - an operand naming a value defined in another function,
\ E-IR-VERIFY-SCOPE - and it is why the production catch shape could not call:
\ `[: WORD ;] catch {: rc :} rc 0<> if rc throw then` is a definition with a
\ group around a body that calls (dot habu-let-a-calling-7578eaaa).
\
\ WHY AN EMPTY SCOPE IS THE TRUTH AND NOT A CONVENIENCE. A quotation is an xt and
\ not a closure: no name of the enclosing routine reaches into the body, and the
\ engine refuses a body that mentions one - `{: k:n :} [: k 1+ ;]` reports `k`
\ undefined and exits 75. So there is nothing for a body's call to carry, and
\ every carrier that would have carried it reads one number, CROSS-L, which
\ counts the LIVE slots.
\
\ WHAT EVERY CASE HERE MEASURES. The chain's compilation of a text against the
\ ENGINE's compilation of the same text, because what has to be proved is an
\ ANSWER: a body that came back having overwritten a local, or a local that came
\ back as the other local, is a wrong NUMBER and no shape assertion sees it.
\ Every local is weighted with a distinct odd factor for that reason - two names
\ added plainly into one answer are exchangeable, and the weights are what make
\ the exchange visible.
\
\ THE SECOND CEILING THIS FILE ONCE PINNED IS ALSO GONE, and its cases are now
\ differentials too. A quotation body holding a control structure of its own was
\ refused, and it was a DIFFERENT missing per-function fact: a block names itself
\ by an ordinal in the MODULE's block table, which is what a successor carries,
\ and the machine passes read one back as an ordinal in the FUNCTION they were
\ lowering. The elaborator learnt the difference first (elaborate.f BBASE) and
\ the machine passes second - the register allocator, its verifier and the
\ emitter each file the base their function's blocks start at and subtract it off
\ every successor they read, which is what src/compiler/native/select.f had done
\ alone (R-BASE, SUCC-IDX). What that missing subtraction really was is worth
\ stating once: with one function in a module every base is zero and the two
\ ordinals agree, so a body - the second function - was the first thing in the
\ tree that could tell them apart, and it named blocks of the routine around it.
\
\ WHY THE WEIGHTS MATTER TWICE AS MUCH HERE. Both arms of every branching body
\ below multiply by a different odd factor, so a compilation that took the other
\ arm, or that reached the enclosing routine's block of the same ordinal, answers
\ a different NUMBER. An `if` whose arms both added would have hidden exactly the
\ bug this landing fixed.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f

\ ---- the engine's compilation: the reference ---------------------------------
package QSC-FIXTURE

public

\ A callee that keeps nothing for its caller, which is the half of "does this
\ local travel" that is about the CALLEE: a local a call can reach travels only
\ when one of the body's calls keeps no register, and a staged callee with no
\ clobber record is that call. So every name mentioned after one of these really
\ is carried, and a fixture whose callee kept registers would measure nothing.
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
\ throwing path as well: the value the window holds afterwards is the engine's
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

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry. They run with the
\ fixture package open, so a bare tail means the fixture's word and the
\ definitions they publish land beside the ones the engine compiled.
package QSC-MIGRATED

private

: DEF-P ( -- )
   s" : QSC-P-N ( n -- n ) [: QSC-BAD ;] catch {: rc:n :} rc 0 <> if rc throw then ;"
   1 1 NMIGRATE:DEFINE ;

: DEF-A ( -- )
   s" : QSC-A-N ( n n -- n ) {: k:n lim:n :} lim 5 * [: QSC-OK1 ;] catch drop k 3 * + ;"
   2 1 NMIGRATE:DEFINE ;

: DEF-T ( -- )
   s" : QSC-T-N ( n n -- n n ) {: k:n lim:n :} lim 5 * [: QSC-BAD ;] catch {: rc:n :} k 3 * + rc ;"
   2 2 NMIGRATE:DEFINE ;

: DEF-TWO ( -- )
   s" : QSC-2-N ( n n -- n ) {: a:n b:n :} a 3 * [: QSC-OK1 ;] catch drop a 5 * + b 7 * + ;"
   2 1 NMIGRATE:DEFINE ;

: DEF-E ( -- )
   s" : QSC-E-N ( n n -- n ) {: k:n lim:n :} [: QSC-OK1 ;] lim 5 * QSC-APPLY k 3 * + ;"
   2 1 NMIGRATE:DEFINE ;

: DEF-L ( -- )
   s" : QSC-L-N ( n -- n ) {: v:n :} 0 3 0 ?do i {: t:n :} v [: QSC-OK1 ;] catch drop t 7 * + + loop ;"
   1 1 NMIGRATE:DEFINE ;

\ The bodies that call and the bodies that do not go through one entry: the
\ chain resolves whatever name a body writes off the dictionary.
: DEF ( ptr u8 n n n -- )
   NMIGRATE:DEFINE ;

: DEF-IF ( -- )
   s" : QSC-IF-N ( n -- n n ) [: dup 3 > if 3 * else 5 * then ;] catch ;" 1 2 DEF ;

: DEF-DO ( -- )
   s" : QSC-DO-N ( n -- n n ) [: 4 0 ?do 2 * 1+ loop ;] catch ;" 1 2 DEF ;

: DEF-IFDO ( -- )
   s" : QSC-IFDO-N ( n -- n n ) [: 4 0 ?do dup 9 > if 3 * else 5 + then loop ;] catch ;" 1 2 DEF ;

: DEF-PB ( -- )
   s" : QSC-PB-N ( n -- n ) [: dup 3 > if 9 throw then 7 * ;] catch {: rc:n :} rc 0 <> if rc throw then ;"
   1 1 DEF ;

: DEF-2B ( -- )
   s" : QSC-2B-N ( n -- n ) [: dup 3 > if 3 * else 5 * then ;] catch drop [: dup 20 > if 7 * else 11 * then ;] catch drop ;"
   1 1 DEF ;

: DEF-3B ( -- )
   s" : QSC-3B-N ( n -- n ) dup 3 > if 1+ then [: dup 3 > if 3 * else 5 * then ;] catch drop [: dup 9 > if 1+ else 2 + then ;] catch drop ;"
   1 1 DEF ;

: DEF-BG ( -- )
   s" : QSC-BG-N ( n n -- n ) {: k:n lim:n :} lim [: dup 3 > if 3 * else 5 * then ;] catch drop k 7 * + ;"
   2 1 DEF ;

: DEF-EB ( -- )
   s" : QSC-EB-N ( n n -- n ) {: k:n lim:n :} [: dup 3 > if 3 * else 5 * then ;] lim QSC-APPLY k 7 * + ;"
   2 1 NMIGRATE:DEFINE ;

public

: RUN ( -- )
   DEF-P DEF-A DEF-T DEF-TWO DEF-E DEF-L
   DEF-IF DEF-DO DEF-IFDO DEF-PB DEF-2B DEF-3B DEF-BG DEF-EB ;

;package

package QSC-FIXTURE
public

QSC-MIGRATED:RUN

;package

\ ---- the differentials -------------------------------------------------------
package QSC-DIFF

private

\ Compiling a body without publishing anything, so a refusal can be measured with
\ nothing left behind on the way out. It takes no staged callee, which is why the
\ refusal cases below are written with bodies that call nothing.
: MEASURE-AT ( ptr u8 n n n -- )
   NMIGRATE:MEASURE-HELD ;

\ BOTH ANSWERS ARE BOUND BEFORE EITHER IS COMPARED. A pair of comparators over
\ four stack cells holds each answer against ITSELF - the top two are the second
\ call's - so naming them is what makes the assertion a comparison between the
\ engine's answer and the chain's. The single-value comparators below need no
\ names: one answer each, and the two cells `T=` reads are one from each.
\
\ ON THE PATH WHERE THE DEFINITION RETHROWS, ONLY THE CODE IS COMPARED, and that
\ is a promise rather than a weakening: what a caught throw restores is the
\ DEPTH, so the cell left under the code is whatever the routine that threw
\ happened to leave in that slot - and on this path the routine that threw IS the
\ subject, compiled two different ways. The code is the whole of what this shape
\ undertakes to pass on. The value is compared wherever the shape does promise it
\ - every non-throwing input here, and both paths of QSC-T below, whose throw
\ comes from one engine-compiled callee in both compilations.
: P= ( n -- ) {: v:n :}
   v [: QSC-FIXTURE:QSC-P ;] catch   {: ev:n er:n :}
   v [: QSC-FIXTURE:QSC-P-N ;] catch {: cv:n cr:n :}
   er cr T=
   er 0= if ev cv T= then ;

: A= ( n n -- ) {: k:n lim:n :}
   k lim QSC-FIXTURE:QSC-A   k lim QSC-FIXTURE:QSC-A-N   T= ;

: TH= ( n n -- ) {: k:n lim:n :}
   k lim QSC-FIXTURE:QSC-T   k lim QSC-FIXTURE:QSC-T-N
   {: ev:n er:n cv:n cr:n :}
   er cr T=  ev cv T= ;

: TWO= ( n n -- ) {: a:n b:n :}
   a b QSC-FIXTURE:QSC-2   a b QSC-FIXTURE:QSC-2-N   T= ;

: E= ( n n -- ) {: k:n lim:n :}
   k lim QSC-FIXTURE:QSC-E   k lim QSC-FIXTURE:QSC-E-N   T= ;

: L= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-L   v QSC-FIXTURE:QSC-L-N   T= ;

\ ---- the branching bodies ----------------------------------------------------
\ Each of these leaves two cells, so both answers are bound before either is
\ compared, for the reason stated above P=.
: IF= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-IF   v QSC-FIXTURE:QSC-IF-N
   {: ev:n er:n cv:n cx:n :}
   er cx T=  ev cv T= ;

: DO= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-DO   v QSC-FIXTURE:QSC-DO-N
   {: ev:n er:n cv:n cx:n :}
   er cx T=  ev cv T= ;

: IFDO= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-IFDO   v QSC-FIXTURE:QSC-IFDO-N
   {: ev:n er:n cv:n cx:n :}
   er cx T=  ev cv T= ;

\ The rethrowing shape, held to the same promise the straight-line one is held to
\ above P=: the code on every path, and the value wherever the shape promises one.
: PB= ( n -- ) {: v:n :}
   v [: QSC-FIXTURE:QSC-PB ;] catch   {: ev:n er:n :}
   v [: QSC-FIXTURE:QSC-PB-N ;] catch {: cv:n cx:n :}
   er cx T=
   er 0= if ev cv T= then ;

: TWOB= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-2B   v QSC-FIXTURE:QSC-2B-N   T= ;

: THREEB= ( n -- ) {: v:n :}
   v QSC-FIXTURE:QSC-3B   v QSC-FIXTURE:QSC-3B-N   T= ;

: BG= ( n n -- ) {: k:n lim:n :}
   k lim QSC-FIXTURE:QSC-BG   k lim QSC-FIXTURE:QSC-BG-N   T= ;

: EB= ( n n -- ) {: k:n lim:n :}
   k lim QSC-FIXTURE:QSC-EB   k lim QSC-FIXTURE:QSC-EB-N   T= ;

public

\ THE CEILING ITSELF, MEASURED AS AN ACCEPTANCE. Every case below runs code the
\ chain compiled, so a refusal would take the whole file down at load with the
\ migration's own throw; this one asks the question where a reader can see the
\ answer, and its twin without the group is what says the group is what the
\ question is about.
: ACCEPT-CASE ( -- )
   s" a calling quotation compiles under a definition with a group" T-LABEL
   [: s" : QSC-ACC1 ( n n -- n ) {: k:n lim:n :} lim [: QSC-FIXTURE:QSC-OK1 ;] catch drop k 3 * + ;"
      2 1 MEASURE-AT ;]
   0 TTHROWSQ
   s" and so does the same body with no group around it" T-LABEL
   [: s" : QSC-ACC2 ( n -- n n ) [: QSC-FIXTURE:QSC-OK1 ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ ;

: PRODUCTION-CASE ( -- )
   s" the production catch shape, end to end, on both paths" T-LABEL
   7 QSC-FIXTURE:QSC-P 14 T=
   [: 30 QSC-FIXTURE:QSC-P-N drop ;] 9 TTHROWSQ
   7 P=  0 P=  20 P=  21 P=  100 P= ;

: CALL-LOCALS-CASE ( -- )
   s" a name spent before the call and a name read after it" T-LABEL
   3 4 QSC-FIXTURE:QSC-A 30 T=
   3 4 A=  4 3 A=  0 0 A=  -2 5 A=  7 7 A= ;

: THROW-LOCALS-CASE ( -- )
   s" the same shape with the code kept, on both paths" T-LABEL
   3 4 QSC-FIXTURE:QSC-T {: ev:n er:n :}
   er 0 T=  ev 49 T=
   3 4 TH=  4 3 TH=  0 0 TH=  -2 5 TH=  3 100 TH= ;

: TWO-LOCALS-CASE ( -- )
   s" two names live across one call, each weighted" T-LABEL
   3 5 QSC-FIXTURE:QSC-2 60 T=
   3 5 TWO=  5 3 TWO=  0 0 TWO=  -4 6 TWO= ;

: EXEC-CASE ( -- )
   s" the same body under a group through the pre-catch route" T-LABEL
   3 4 QSC-FIXTURE:QSC-E 30 T=
   3 4 E=  4 3 E=  0 0 E=  -2 5 E= ;

: LOOP-GROUP-CASE ( -- )
   s" a calling body in a loop whose own body holds a group" T-LABEL
   5 QSC-FIXTURE:QSC-L 39 T=
   5 L=  0 L=  -3 L=  11 L= ;

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
\ and their answers are the differentials' business.
: ENCLOSING-SHAPE-CASE ( -- )
   s" a branching body compiles the same way whatever encloses it" T-LABEL
   [: s" : QSC-B1 ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ
   [: s" : QSC-B2 ( n -- n n ) dup 3 > if 1+ then [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ
   [: s" : QSC-B3 ( n -- n n ) dup 3 > if 1+ else 2 + then [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ
   s" including where the two joins carry different numbers of values" T-LABEL
   [: s" : QSC-B5 ( n -- n n ) dup 3 > if 1+ then [: dup dup 3 > if 1+ else 2 + then + 1- ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ
   [: s" : QSC-B6 ( n n -- n n n ) 2dup > if 1+ then [: dup 3 > if 1+ then ;] catch ;"
      2 3 MEASURE-AT ;]
   0 TTHROWSQ
   s" and so does its straight-line twin, as it always did" T-LABEL
   [: s" : QSC-B4 ( n -- n n ) dup 3 > if 1+ then [: 1+ ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ ;

\ WHAT IS STILL REFUSED, AND EACH BY ITS OWN OWNER. None of these three is this
\ lane's, and every one of them was measured on the tree before the subtraction
\ landed and answered the same code there - so they say what a body may not hold
\ rather than what the allocator could not lower.
\
\ A BODY WHOSE EVERY PATH THROWS is the adversarial one: now that a branching
\ body compiles, a body that branches into two throws is the shape a careless
\ landing would ACCEPT, and it must still be refused for the reason it always
\ was - there is no return to stage (E-NELAB-QUOT, dot
\ habu-compile-a-quotation-7efa798e). Its twin, one arm throwing and the other
\ returning, is a body that does come back and compiles: the two rows together
\ say the refusal is about the RETURN and not about the branch.
\
\ AN `exit` INSIDE A BODY'S `if` is refused as an arity disagreement
\ (E-NELAB-ARITY), which is the elaborator counting a body's outputs without a
\ rule for the early leave rather than a judgement about the body: it is recorded
\ here as measured, and dot habu-let-exit-leave-7e013b93 carries the general case.
\
\ A LOCALS GROUP INSIDE A BODY never reaches the chain at all - the ENGINE
\ refuses to compile it - so it is not written as a chain refusal here;
\ test/compiler/native-elaborate.f owns the chain's own quotation refusals.
: STILL-REFUSED-CASE ( -- )
   s" a body whose every path throws is still refused by name" T-LABEL
   [: s" : QSC-R1 ( n -- n n ) [: dup 3 > if 9 throw else 5 throw then ;] catch ;"
      1 2 MEASURE-AT ;]
   E-NELAB-QUOT TTHROWSQ
   s" while one that throws on one path and returns on the other compiles" T-LABEL
   [: s" : QSC-R2 ( n -- n n ) [: dup 3 > if 9 throw else 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ
   s" and an exit inside a body's arm is still an arity refusal" T-LABEL
   [: s" : QSC-R3 ( n -- n n ) [: dup 3 > if 1+ exit then 2 + ;] catch ;"
      1 2 MEASURE-AT ;]
   E-NELAB-ARITY TTHROWSQ ;

\ ---- what the branching bodies ANSWER ----------------------------------------
\ The acceptances above say the module reached the emitter; these say the code it
\ emitted computes what the engine's does. Every row runs both compilations of
\ one text over inputs that take each arm.
: CONTROL-BODY-CASE ( -- )
   s" a body holding an if, both arms, against the engine" T-LABEL
   7 QSC-FIXTURE:QSC-IF {: ev:n er:n :}
   er 0 T=  ev 21 T=
   7 IF=  3 IF=  4 IF=  0 IF=  -5 IF=  100 IF=
   s" a body holding a counted loop" T-LABEL
   1 QSC-FIXTURE:QSC-DO {: lv:n lr:n :}
   lr 0 T=  lv 31 T=
   1 DO=  0 DO=  7 DO=  -3 DO=
   s" and a decision inside that loop's body" T-LABEL
   1 IFDO=  0 IFDO=  7 IFDO=  9 IFDO=  10 IFDO=  -5 IFDO= ;

: BRANCHING-PRODUCTION-CASE ( -- )
   s" the production shape with the decision inside the body, both paths" T-LABEL
   3 QSC-FIXTURE:QSC-PB 21 T=
   [: 30 QSC-FIXTURE:QSC-PB-N drop ;] 9 TTHROWSQ
   3 PB=  0 PB=  4 PB=  -5 PB=  100 PB= ;

: TWO-BODY-CASE ( -- )
   s" two branching bodies in one definition" T-LABEL
   3 QSC-FIXTURE:QSC-2B 165 T=
   3 TWOB=  0 TWOB=  4 TWOB=  7 TWOB=  -5 TWOB=
   s" and three functions in one module, the enclosing one branching too" T-LABEL
   3 THREEB=  0 THREEB=  4 THREEB=  9 THREEB=  -5 THREEB= ;

: BRANCHING-GROUP-CASE ( -- )
   s" a branching body under a definition with a locals group" T-LABEL
   3 4 QSC-FIXTURE:QSC-BG 33 T=
   3 4 BG=  4 3 BG=  0 0 BG=  -2 5 BG=  7 7 BG=
   s" and the same body through the pre-catch route" T-LABEL
   3 4 QSC-FIXTURE:QSC-EB 33 T=
   3 4 EB=  4 3 EB=  0 0 EB=  -2 5 EB=  7 7 EB= ;

: RUN ( -- )
   ACCEPT-CASE
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
QSC-DIFF:RUN
T-REPORT
