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
\ WHAT IS NOT HERE. A quotation body holding a control structure of its own is
\ still refused, and it is a DIFFERENT missing per-function fact: a successor is
\ named by an ordinal in the MODULE's block table, and the machine-side passes
\ read one back as an ordinal in the FUNCTION they are lowering. The elaborator
\ now names them correctly (elaborate.f BBASE); the remaining refusal is
\ src/compiler/native/regalloc.f SUCC-ORD and its siblings, and it stays pinned
\ where it stands in test/compiler/native-catch.f (dot
\ habu-let-a-quotation-fc37262a).

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f

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

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry. They run with the
\ fixture package open, so a bare tail means the fixture's word and the
\ definitions they publish land beside the ones the engine compiled.
package QSC-MIGRATED

private

18 constant REGS

: CALLEE1 ( ptr u8 n ptr u8 n -- )   \ the spelling the source writes, and the word it denotes
   CODEGEN-COMPARE:CODE-ENTRY 1 1 NMIGRATE:CALLEE ;

: CALLEE2 ( ptr u8 n ptr u8 n -- )   \ the same for a callee that takes two cells
   CODEGEN-COMPARE:CODE-ENTRY 2 1 NMIGRATE:CALLEE ;

: STAGE-OK1 ( -- )
   s" QSC-OK1" s" QSC-FIXTURE:QSC-OK1" CALLEE1 ;

: STAGE-BAD ( -- )
   s" QSC-BAD" s" QSC-FIXTURE:QSC-BAD" CALLEE1 ;

: STAGE-APPLY ( -- )
   s" QSC-APPLY" s" QSC-FIXTURE:QSC-APPLY" CALLEE2 ;

: DEF-P ( -- )
   STAGE-BAD
   s" : QSC-P-N ( n -- n ) [: QSC-BAD ;] catch {: rc:n :} rc 0 <> if rc throw then ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEF-A ( -- )
   STAGE-OK1
   s" : QSC-A-N ( n n -- n ) {: k:n lim:n :} lim 5 * [: QSC-OK1 ;] catch drop k 3 * + ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEF-T ( -- )
   STAGE-BAD
   s" : QSC-T-N ( n n -- n n ) {: k:n lim:n :} lim 5 * [: QSC-BAD ;] catch {: rc:n :} k 3 * + rc ;"
   2 2 REGS NMIGRATE:DEFINE-CALLING ;

: DEF-TWO ( -- )
   STAGE-OK1
   s" : QSC-2-N ( n n -- n ) {: a:n b:n :} a 3 * [: QSC-OK1 ;] catch drop a 5 * + b 7 * + ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEF-E ( -- )
   STAGE-OK1 STAGE-APPLY
   s" : QSC-E-N ( n n -- n ) {: k:n lim:n :} [: QSC-OK1 ;] lim 5 * QSC-APPLY k 3 * + ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEF-L ( -- )
   STAGE-OK1
   s" : QSC-L-N ( n -- n ) {: v:n :} 0 3 0 ?do i {: t:n :} v [: QSC-OK1 ;] catch drop t 7 * + + loop ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

public

: RUN ( -- )
   DEF-P DEF-A DEF-T DEF-TWO DEF-E DEF-L ;

;package

package QSC-FIXTURE
public

QSC-MIGRATED:RUN

;package

\ ---- the differentials -------------------------------------------------------
package QSC-DIFF

private

18 constant REGS

\ A migration that stages a callee cannot be measured without publishing - there
\ is no held entry that takes a staged list - so the case that measures the
\ acceptance itself goes through the publishing entry under a name of its own.
: DEFINE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:DEFINE-CALLING ;

\ Compiling a body without publishing anything, so a refusal can be measured with
\ nothing left behind on the way out. It takes no staged callee, which is why the
\ refusal cases below are written with bodies that call nothing.
: MEASURE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:MEASURE-HELD ;

\ The spelling is the QUALIFIED one here, because these migrations run with this
\ package open rather than the fixture's: the source they compile is evaluated in
\ the scope this file is in, so a bare tail would resolve to nothing.
: STAGE-OK1 ( -- )
   s" QSC-FIXTURE:QSC-OK1" s" QSC-FIXTURE:QSC-OK1"
   CODEGEN-COMPARE:CODE-ENTRY 1 1 NMIGRATE:CALLEE ;

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

public

\ THE CEILING ITSELF, MEASURED AS AN ACCEPTANCE. Every case below runs code the
\ chain compiled, so a refusal would take the whole file down at load with the
\ migration's own throw; this one asks the question where a reader can see the
\ answer, and its twin without the group is what says the group is what the
\ question is about.
: ACCEPT-CASE ( -- )
   s" a calling quotation compiles under a definition with a group" T-LABEL
   [: STAGE-OK1
      s" : QSC-ACC1 ( n n -- n ) {: k:n lim:n :} lim [: QSC-FIXTURE:QSC-OK1 ;] catch drop k 3 * + ;"
      2 1 DEFINE-AT ;]
   0 TTHROWSQ
   s" and so does the same body with no group around it" T-LABEL
   [: STAGE-OK1
      s" : QSC-ACC2 ( n -- n n ) [: QSC-FIXTURE:QSC-OK1 ;] catch ;"
      1 2 DEFINE-AT ;]
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

\ THE REFUSAL THAT IS LEFT, AND THE PROPERTY THIS LANE GAVE IT. A body holding a
\ control structure is still refused, and before this lane WHICH refusal it got
\ depended on the routine around it. Two facts about a function were read at
\ whatever the walk before them left: the body's successors were named by
\ ordinals in the ENCLOSING function's block window, and the body's joins were
\ held to the width and types the enclosing function had stated for the block of
\ the same ordinal. So one body text was refused as a successor-argument
\ mismatch under a straight-line definition (E-IR-VERIFY-SUCCARG), as a
\ dominance failure under one holding an `if` (E-IR-VERIFY-DOM), and as a join
\ disagreement whenever the two joins carried different numbers of values
\ (E-NELAB-JOIN, which the last two rows below are shaped to produce).
\
\ A BODY'S COMPILATION CANNOT DEPEND ON THE SHAPE OF THE ROUTINE AROUND IT, and
\ now it does not: one refusal for all five, from the one pass that still reads a
\ successor as an ordinal in the function it is lowering.
\
\ THIS CASE COMES DUE WHEN THAT PASS IS FIXED (dot
\ habu-let-a-quotation-fc37262a): the refusals become acceptances and the twin
\ below stops being a twin. Its successor is a differential like the ones above.
: REFUSAL-SHAPE-CASE ( -- )
   s" a branching body is refused the same way whatever encloses it" T-LABEL
   [: s" : QSC-B1 ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   E-A64RA-SHAPE TTHROWSQ
   [: s" : QSC-B2 ( n -- n n ) dup 3 > if 1+ then [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   E-A64RA-SHAPE TTHROWSQ
   [: s" : QSC-B3 ( n -- n n ) dup 3 > if 1+ else 2 + then [: dup 3 > if 1+ then ;] catch ;"
      1 2 MEASURE-AT ;]
   E-A64RA-SHAPE TTHROWSQ
   s" including where the two joins carry different numbers of values" T-LABEL
   [: s" : QSC-B5 ( n -- n n ) dup 3 > if 1+ then [: dup dup 3 > if 1+ else 2 + then + 1- ;] catch ;"
      1 2 MEASURE-AT ;]
   E-A64RA-SHAPE TTHROWSQ
   [: s" : QSC-B6 ( n n -- n n n ) 2dup > if 1+ then [: dup 3 > if 1+ then ;] catch ;"
      2 3 MEASURE-AT ;]
   E-A64RA-SHAPE TTHROWSQ
   s" while its straight-line twin compiles under the same definition" T-LABEL
   [: s" : QSC-B4 ( n -- n n ) dup 3 > if 1+ then [: 1+ ;] catch ;"
      1 2 MEASURE-AT ;]
   0 TTHROWSQ ;

: RUN ( -- )
   ACCEPT-CASE
   REFUSAL-SHAPE-CASE
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
