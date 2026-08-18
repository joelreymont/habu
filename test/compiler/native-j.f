\ native-j.f - `j`, the index of the counted loop one frame further out, run
\ against the engine's own `j`. One concern: which frame the reader answers with.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. `j` stages no
\ operation at all - it puts a value that already exists back on the compile-time
\ vector - so a chain that answered with the INNER loop's index, with the
\ OUTERMOST loop's index, or with the enclosing `if`'s stale frame emits the same
\ number of blocks and the same number of instructions as one that answers
\ correctly. What tells those apart is the ANSWER, so every case here is
\ differential: the same source text compiled twice, once by the engine's own
\ emitter and once by the native chain, run against each other on pinned inputs.
\
\ THE WEIGHTS ARE DISTINCT AND ODD, WHICH IS WHAT MAKES THE PAIR FALSIFIABLE. A
\ body that adds `i` and `j` cannot tell them apart - addition does not care
\ which came first - so every fixture below combines them as `i 3 * j 5 * +`.
\ Under an exchange of the two the answer moves at every pair of bounds where the
\ two loops differ, which is why no case is run with its two bounds equal only.
\
\ THE THREE-DEEP CASE IS WHAT SEPARATES "ONE FRAME OUT" FROM "THE OUTERMOST".
\ With two loops open those two readings agree; with three open they do not, and
\ the middle loop is the one `j` names. src/habu/habu2.f J-J reads frame
\ LOOPSP-2, which is the second frame from the TOP of the engine's runtime loop
\ stack, and src/core/checker.f CF-J requires two counted frames to be open -
\ neither of them says anything about the outermost.
\
\ AND THE `if` CASE IS WHAT SEPARATES "THE SECOND COUNTED FRAME" FROM "THE SECOND
\ FRAME". The elaborator's control stack carries a frame for every open
\ structure, not only for counted loops, so a reader that counted frames rather
\ than counted LOOPS would answer an `if` inside the inner loop with the inner
\ loop's own index - and, one structure further out, with a frame whose index
\ cell was never written at all.
\
\ ONE REFUSAL IS THE CHAIN'S ALONE AND IS PINNED WITH ENGINE-ACCEPTED TWINS.
\ src/core/checker.f CF-I and CF-J count the counted frames of the WHOLE control
\ stack and do not stop at a quotation boundary the way CF-LEAVE's CF-FINDDO
\ does, so the checker accepts a `j` written inside `[: … ;]` whose two loops are
\ not both in the quotation's own body - and the engine runs it, because its loop
\ frames are a RUNTIME stack and the quotation is entered while both are open.
\ The chain walks a quotation's body as its own function with its own control
\ stack, exactly as it does for `leave`, so there `j` finds fewer than two
\ counted loops and is refused by name rather than answered with some other
\ loop's counter.
\
\ AND THERE IS NO THIRD INDEX TO MODEL: `k` is not a word of this Forth at all -
\ neither src/habu/habu2.f nor src/core/checker.f CF-TOK? knows the spelling -
\ so a body that writes one is refused as an undefined word before any of this is
\ reached. The case below measures that rather than assuming it.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f
require tools/codegen-loop-inventory.f

\ ---- the engine's compilation: the reference ---------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, which really runs every turn of every one of them.
package NJ-FIXTURE

public

\ Two counted loops and both indices, under each pair of openers the source
\ language spells. The frame both openers push is the same one, so what these
\ four rows say is that the reader finds it whichever word opened it.
: NJ-IJ ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do i 3 * j 5 * + + loop loop ;

: NJ-IJ-DO ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do i 3 * j 5 * + + loop loop ;

: NJ-IJ-DOQ ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 ?do i 3 * j 5 * + + loop loop ;

: NJ-IJ-QDO ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 do i 3 * j 5 * + + loop loop ;

\ `j` under an `if` inside the inner loop, read on BOTH arms so no path is left
\ without it. The frame between the reader and the loops is what this row is
\ about: it is not a counted one and must not be counted.
: NJ-IF ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do i 1 and 0= if j 3 * + else j 5 * + then loop loop ;

\ Three counted loops. `j` is the MIDDLE one's index - one frame out from the
\ innermost - and not the outermost's, which is what the third loop is here to
\ tell apart.
: NJ-TRIPLE ( n n n -- n ) {: a:n b:n c:n :}
   0 a 0 ?do b 0 ?do c 0 ?do i 3 * j 5 * + + loop loop loop ;

\ The callee is long enough that neither generator copies it, so what crosses
\ this loop's body really is a call - and a call is the one thing that renames
\ every open loop's counters, the outer loop's included.
: NJ-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NJ-CALL ( n n n -- n ) {: seed:n a:n b:n :}
   seed a 0 ?do b 0 ?do NJ-CALLEE j + loop loop ;

\ The inner loop is left from the middle of its body while the outer one goes on
\ turning, so the outer index is read on a path that reaches the `leave` and on
\ one that does not.
: NJ-LEAVE ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do j 3 * + i 2 > if leave then loop loop ;

\ A local named `j` is the local, inside two counted loops as anywhere else. Both
\ authorities say so - docs/forth.md § Naming, measured on this engine - and the
\ two readings meet here: the engine answers the local and so must the chain.
: NJ-JLOCAL ( n n n -- n ) {: j:n a:n b:n :}
   0 a 0 ?do b 0 ?do j 3 * i 5 * + + loop loop ;

\ lib/ptx/cg-matmul-emit.f MM-KSTEP-FMA reduced to its indices: two plain `do`
\ loops whose body computes a register number out of both of them. That body is
\ the definition this leaf was opened by - it moved to E-HIR-UNMODELED naming `j`
\ when the plain `do` landed - and this row is its arithmetic with the string
\ building taken out.
: NJ-FMA ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do 10 j 4 * + i + + loop loop ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry.
package NJ-MIGRATED

private

: IJ ( -- )
   s" : NJ-IJ-N ( n n -- n ) {: a:n b:n :} 0 a 0 ?do b 0 ?do i 3 * j 5 * + + loop loop ;"
   NMIGRATE:DEFINE ;

: IJ-DO ( -- )
   s" : NJ-IJ-DO-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 do i 3 * j 5 * + + loop loop ;"
   NMIGRATE:DEFINE ;

: IJ-DOQ ( -- )
   s" : NJ-IJ-DOQ-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 ?do i 3 * j 5 * + + loop loop ;"
   NMIGRATE:DEFINE ;

: IJ-QDO ( -- )
   s" : NJ-IJ-QDO-N ( n n -- n ) {: a:n b:n :} 0 a 0 ?do b 0 do i 3 * j 5 * + + loop loop ;"
   NMIGRATE:DEFINE ;

: GUARD ( -- )
   s" : NJ-IF-N ( n n -- n ) {: a:n b:n :} 0 a 0 ?do b 0 ?do i 1 and 0= if j 3 * + else j 5 * + then loop loop ;"
   NMIGRATE:DEFINE ;

: TRIPLE ( -- )
   s" : NJ-TRIPLE-N ( n n n -- n ) {: a:n b:n c:n :} 0 a 0 ?do b 0 ?do c 0 ?do i 3 * j 5 * + + loop loop loop ;"
   NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NJ-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   NMIGRATE:DEFINE ;

: CALL ( -- )
   s" : NJ-CALL-N ( n n n -- n ) {: seed:n a:n b:n :} seed a 0 ?do b 0 ?do NJ-CALLEE-N j + loop loop ;"
   NMIGRATE:DEFINE ;

: EARLY ( -- )
   s" : NJ-LEAVE-N ( n n -- n ) {: a:n b:n :} 0 a 0 ?do b 0 ?do j 3 * + i 2 > if leave then loop loop ;"
   NMIGRATE:DEFINE ;

: JLOCAL ( -- )
   s" : NJ-JLOCAL-N ( n n n -- n ) {: j:n a:n b:n :} 0 a 0 ?do b 0 ?do j 3 * i 5 * + + loop loop ;"
   NMIGRATE:DEFINE ;

: FMA ( -- )
   s" : NJ-FMA-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 do 10 j 4 * + i + + loop loop ;"
   NMIGRATE:DEFINE ;

public

: RUN ( -- )
   IJ IJ-DO IJ-DOQ IJ-QDO
   GUARD TRIPLE
   CALLEE CALL
   EARLY JLOCAL FMA ;

;package

package NJ-FIXTURE
public

NJ-MIGRATED:RUN

;package

package NJ-TEST

private

\ How many loops a published routine's emitted code still holds, read off the
\ emitted code by tools/codegen-loop-inventory.f rather than assumed.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

: KEPT3 ( ptr u8 n -- )
   LOOPS-IN 3 T= ;

\ Compiling a body without publishing anything, so a refusal can be measured with
\ nothing left behind on the way out.
: MEASURE-AT ( ptr u8 n -- )
   NMIGRATE:MEASURE-HELD ;

\ One source line through the engine's own compiler, caught: whether the ENGINE
\ and the CHECKER accept the text at all, which is a different question from
\ whether the chain can compile it.
TRUSTED: EV-DEF ( ptr u8 n -- n )
   [: evaluate ;] catch ;

\ ---- the differentials -------------------------------------------------------
: IJ= ( n n -- ) {: a:n b:n :}
   a b NJ-FIXTURE:NJ-IJ      a b NJ-FIXTURE:NJ-IJ-N      T=
   a b NJ-FIXTURE:NJ-IJ-DO   a b NJ-FIXTURE:NJ-IJ-DO-N   T=
   a b NJ-FIXTURE:NJ-IJ-DOQ  a b NJ-FIXTURE:NJ-IJ-DOQ-N  T=
   a b NJ-FIXTURE:NJ-IJ-QDO  a b NJ-FIXTURE:NJ-IJ-QDO-N  T= ;

: IF= ( n n -- ) {: a:n b:n :}
   a b NJ-FIXTURE:NJ-IF  a b NJ-FIXTURE:NJ-IF-N  T= ;

: TRIPLE= ( n n n -- ) {: a:n b:n c:n :}
   a b c NJ-FIXTURE:NJ-TRIPLE  a b c NJ-FIXTURE:NJ-TRIPLE-N  T= ;

: CALL= ( n n n -- ) {: seed:n a:n b:n :}
   seed a b NJ-FIXTURE:NJ-CALL  seed a b NJ-FIXTURE:NJ-CALL-N  T= ;

: LEAVE= ( n n -- ) {: a:n b:n :}
   a b NJ-FIXTURE:NJ-LEAVE  a b NJ-FIXTURE:NJ-LEAVE-N  T= ;

: JLOCAL= ( n n n -- ) {: k:n a:n b:n :}
   k a b NJ-FIXTURE:NJ-JLOCAL  k a b NJ-FIXTURE:NJ-JLOCAL-N  T= ;

: FMA= ( n n -- ) {: a:n b:n :}
   a b NJ-FIXTURE:NJ-FMA  a b NJ-FIXTURE:NJ-FMA-N  T= ;

\ ---- the cases ---------------------------------------------------------------
\ THE BOUNDS ARE UNEQUAL WHEREVER THEY CAN BE, because a rectangle whose sides
\ are equal answers the same under an exchange of the two indices. The equal and
\ zero pairs are still run - they are where the two openers differ - but they are
\ never the only ones.
: IJ-CASE ( -- )
   s" both indices of two counted loops, under all four pairs of openers" T-LABEL
   s" NJ-FIXTURE:NJ-IJ-N" KEPT2
   1 3 IJ=  3 1 IJ=  2 5 IJ=  5 2 IJ=  4 4 IJ=
   0 3 IJ=  3 0 IJ=  0 0 IJ=  -2 3 IJ=  3 -2 IJ= ;

: IF-CASE ( -- )
   s" the frame between the reader and its loops is not a counted one" T-LABEL
   1 3 IF=  3 1 IF=  2 5 IF=  5 2 IF=  4 4 IF=  0 3 IF=  3 0 IF= ;

: TRIPLE-CASE ( -- )
   s" with three loops open j is the middle one, not the outermost" T-LABEL
   s" NJ-FIXTURE:NJ-TRIPLE-N" KEPT3
   1 2 3 TRIPLE=  3 2 1 TRIPLE=  2 3 4 TRIPLE=  4 3 2 TRIPLE=
   1 1 5 TRIPLE=  5 1 1 TRIPLE=  0 2 3 TRIPLE=  2 0 3 TRIPLE=  2 3 0 TRIPLE= ;

: CALL-CASE ( -- )
   s" a call in the inner body carries the outer loop's counter too" T-LABEL
   0 1 3 CALL=  7 3 1 CALL=  -5 2 5 CALL=  9 5 2 CALL=  3 0 4 CALL= ;

: LEAVE-CASE ( -- )
   s" the inner loop is left while the outer one goes on turning" T-LABEL
   1 3 LEAVE=  3 1 LEAVE=  2 5 LEAVE=  5 2 LEAVE=  4 4 LEAVE=  0 5 LEAVE= ;

: JLOCAL-CASE ( -- )
   s" a local named j is the local, in the chain as in the engine" T-LABEL
   7 1 3 JLOCAL=  7 3 1 JLOCAL=  -4 2 5 JLOCAL=  11 5 2 JLOCAL= ;

: FMA-CASE ( -- )
   s" the matmul micro-tile's own index arithmetic" T-LABEL
   s" NJ-FIXTURE:NJ-FMA-N" KEPT2
   1 3 FMA=  3 1 FMA=  4 4 FMA=  2 5 FMA=  5 2 FMA= ;

\ THE REFUSALS THAT ARE NOT THE CHAIN'S, measured where a program meets them. One
\ counted loop is not enough for `j` and the checker says so; two counted loops
\ that are not both open at the reader are not enough either; and `k` is not a
\ word at all. Each is written beside the same text WITHOUT the offending
\ placement, which compiles - so what each refusal is about is the placement and
\ not something else in the line.
: OUTSIDE-CASE ( -- )
   s" j under one counted loop is refused where it is written" T-LABEL
   s" : NJ-OK1 ( -- n ) 0 3 0 ?do 2 0 ?do j + loop loop ;" EV-DEF 0 T=
   s" : NJ-BAD1 ( -- n ) 0 3 0 ?do j + loop ;" EV-DEF 0 T<>

   s" and so is one read after the inner loop has closed" T-LABEL
   s" : NJ-OK2 ( -- n ) 0 3 0 ?do 2 0 ?do j + loop i + loop ;" EV-DEF 0 T=
   s" : NJ-BAD2 ( -- n ) 0 3 0 ?do 2 0 ?do i + loop j + loop ;" EV-DEF 0 T<>

   s" and a third index has no spelling at all" T-LABEL
   s" : NJ-OK3 ( -- n ) 0 2 0 ?do 2 0 ?do 2 0 ?do i j + + loop loop loop ;" EV-DEF 0 T=
   s" : NJ-BAD3 ( -- n ) 0 2 0 ?do 2 0 ?do 2 0 ?do k + loop loop loop ;" EV-DEF 0 T<> ;

\ THE SHAPE THE CHAIN REFUSES ON ITS OWN, and each pair is what makes the refusal
\ a fact about the PLACEMENT rather than about the shape it is written in. Both
\ bodies below have two counted loops as the CHECKER counts them, and in neither
\ are both of them in the function that writes the `j`: a quotation's body is
\ another function, walked here with its own control stack.
\
\ THE FIRST PAIR IS THE WHOLE STATEMENT. One quotation writes `j` and the other
\ writes a literal; everything else about the two bodies is the same text. The
\ first is refused by name and the second compiles, so what the refusal is about
\ is the reader and not the quotation, the `catch`, or the loops around it.
\
\ THE SECOND PAIR SAYS THE SAME THING WHERE THE QUOTATION HAS A LOOP OF ITS OWN -
\ one, which is one short. Its twin writes `i`, which that one loop satisfies, and
\ the twin is held only against NOT being this refusal: a counted loop inside a
\ quotation does not compile today for a reason of its own (measured: the module
\ verifier's E-IR-VERIFY-DOM, -8091, for the `i` body and E-IR-VERIFY-SUCCARG,
\ -8088, for a two-loop one), and this case has no business pinning that code. If
\ a later lane makes those bodies compile, the twin answers zero and this line
\ still holds.
: QUOT-CASE ( -- )
   s" the engine accepts a j inside a quotation whose loops are outside it" T-LABEL
   s" : NJ-OK4 ( -- n ) 0 3 0 ?do 2 0 ?do [: j drop ;] catch + loop loop ;" EV-DEF 0 T=
   s" : NJ-OK5 ( -- n ) 0 3 0 ?do [: 0 2 0 ?do j + loop drop ;] catch + loop ;" EV-DEF 0 T=

   s" and the chain refuses it, while the same quotation with a literal compiles" T-LABEL
   [: s" : NJ-QBAD ( -- n ) 0 3 0 ?do 2 0 ?do [: j drop ;] catch + loop loop ;" MEASURE-AT ;]
   E-NELAB-CTRL TTHROWSQ
   [: s" : NJ-QOK ( -- n ) 0 3 0 ?do 2 0 ?do [: 7 drop ;] catch + loop loop ;" MEASURE-AT ;]
   0 TTHROWSQ

   s" and one counted loop inside the quotation is still one short" T-LABEL
   [: s" : NJ-QBAD2 ( -- n ) 0 3 0 ?do [: 0 2 0 ?do j + loop drop ;] catch + loop ;" MEASURE-AT ;]
   E-NELAB-CTRL TTHROWSQ
   [: s" : NJ-QI ( -- n ) 0 3 0 ?do [: 0 2 0 ?do i + loop drop ;] catch + loop ;" MEASURE-AT ;]
   catch E-NELAB-CTRL T<> ;

public

: RUN ( -- )
   IJ-CASE
   IF-CASE
   TRIPLE-CASE
   CALL-CASE
   LEAVE-CASE
   JLOCAL-CASE
   FMA-CASE
   OUTSIDE-CASE
   QUOT-CASE ;

;package

T-RESET
NJ-TEST:RUN
T-REPORT
