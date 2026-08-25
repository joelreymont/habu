\ native-j.f - `j`, the index of the counted loop one frame further out, run

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

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

\ One dynamically defined source line, caught so refusal can be asserted.
TRUSTED: EV-DEF ( ptr u8 n -- n )
   [: evaluate ;] catch ;

\ ---- the cases ---------------------------------------------------------------
\ THE BOUNDS ARE UNEQUAL WHEREVER THEY CAN BE, because a rectangle whose sides
\ are equal answers the same under an exchange of the two indices. The equal and
\ zero pairs are still run - they are where the two openers differ - but they are
\ never the only ones.
: IJ-CASE ( -- )
   s" both indices of two counted loops, under all four pairs of openers" T-LABEL
   s" NJ-FIXTURE:NJ-IJ" KEPT2
   2 3 NJ-FIXTURE:NJ-IJ 33 T=
   2 3 NJ-FIXTURE:NJ-IJ-DO 33 T=
   2 3 NJ-FIXTURE:NJ-IJ-DOQ 33 T=
   2 3 NJ-FIXTURE:NJ-IJ-QDO 33 T= ;

: IF-CASE ( -- )
   s" the frame between the reader and its loops is not a counted one" T-LABEL
   2 2 NJ-FIXTURE:NJ-IF 8 T= ;

: TRIPLE-CASE ( -- )
   s" with three loops open j is the middle one, not the outermost" T-LABEL
   s" NJ-FIXTURE:NJ-TRIPLE" KEPT3
   2 3 1 NJ-FIXTURE:NJ-TRIPLE 30 T= ;

: CALL-CASE ( -- )
   s" a call in the inner body carries the outer loop's counter too" T-LABEL
   7 0 3 NJ-FIXTURE:NJ-CALL 7 T= ;

: LEAVE-CASE ( -- )
   s" the inner loop is left while the outer one goes on turning" T-LABEL
   2 5 NJ-FIXTURE:NJ-LEAVE 12 T= ;

: JLOCAL-CASE ( -- )
   s" a local named j resolves as the local" T-LABEL
   7 2 3 NJ-FIXTURE:NJ-JLOCAL 156 T= ;

: FMA-CASE ( -- )
   s" the matmul micro-tile's own index arithmetic" T-LABEL
   s" NJ-FIXTURE:NJ-FMA" KEPT2
   2 3 NJ-FIXTURE:NJ-FMA 78 T= ;

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

: QUOT-CASE ( -- )
   s" j inside a quotation cannot borrow the caller's loop frames" T-LABEL
   s" : NJ-QBAD ( -- n ) 0 3 0 ?do 2 0 ?do [: j drop ;] catch + loop loop ;"
   EV-DEF E-NELAB-CTRL T=
   s" : NJ-QOK ( -- n ) 0 3 0 ?do 2 0 ?do [: 7 drop ;] catch + loop loop ;"
   EV-DEF 0 T=

   s" one loop inside the quotation is still one short" T-LABEL
   s" : NJ-QBAD2 ( -- n ) 0 3 0 ?do [: 0 2 0 ?do j + loop drop ;] catch + loop ;"
   EV-DEF E-NELAB-CTRL T=
   s" : NJ-QI ( -- n ) 0 3 0 ?do [: 0 2 0 ?do i + loop drop ;] catch + loop ;"
   EV-DEF E-NELAB-CTRL T<> ;

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
