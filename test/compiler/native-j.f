\ native-j.f - production compilation of the outer counted-loop index `j`.
require test/compiler/native-eval-fixture.f
require src/lib/test.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/errors.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

\ Tier 1: the back edges each case counts are the optimizing compiler's code
\ (tools/codegen-loop-inventory.f).
1 set-tier

package NJ-FIXTURE

public

\ Both openers push the same frame, so the four rows say the reader finds it
\ whichever word opened it.
: NJ-IJ ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do i 3 * j 5 * + + loop loop ;
tier@ constant NJ-IJ-TIER

: NJ-IJ-DO ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do i 3 * j 5 * + + loop loop ;

: NJ-IJ-DOQ ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 ?do i 3 * j 5 * + + loop loop ;

: NJ-IJ-QDO ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 do i 3 * j 5 * + + loop loop ;

\ The `if` frame between the reader and the loops is not counted; `j` is read on
\ both arms.
: NJ-IF ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do i 1 and 0= if j 3 * + else j 5 * + then loop loop ;

\ `j` is the middle loop's index, one frame out, not the outermost's.
: NJ-TRIPLE ( n n n -- n ) {: a:n b:n c:n :}
   0 a 0 ?do b 0 ?do c 0 ?do i 3 * j 5 * + + loop loop loop ;

\ The call renames every open loop's counters, the outer loop's included.
: NJ-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NJ-CALL ( n n n -- n ) {: seed:n a:n b:n :}
   seed a 0 ?do b 0 ?do NJ-CALLEE j + loop loop ;

\ The outer index is read on a path that reaches the inner `leave` and on one
\ that does not.
: NJ-LEAVE ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 ?do j 3 * + i 2 > if leave then loop loop ;

\ A local named `j` is the local (docs/forth.md local-first rule).
: NJ-JLOCAL ( n n n -- n ) {: j:n a:n b:n :}
   0 a 0 ?do b 0 ?do j 3 * i 5 * + + loop loop ;

\ A ptx matmul emitter's index arithmetic without the string building: two plain
\ `do` loops computing a register number from both indices.
: NJ-FMA ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do 10 j 4 * + i + + loop loop ;

;package

package NJ-TEST

private

: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

: KEPT3 ( ptr u8 n -- )
   LOOPS-IN 3 T= ;

: EV-DEF ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

\ ---- the cases ---------------------------------------------------------------
\ Bounds are unequal wherever they can be: a square answers the same with the
\ indices exchanged. Equal and zero pairs still run, where the openers differ.
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
   7 0 3 NJ-FIXTURE:NJ-CALL 7 T=
   7 2 3 NJ-FIXTURE:NJ-CALL 91035498042 T= ;

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

\ Refusals that are the checker's, not the chain's: `j` under one counted loop,
\ under two not both open at the reader, and `k`, which is no word. Each sits
\ beside the same text without the offending placement, which compiles.
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

\ The checker counts only active frames in the current quotation or definition.
\ Literal and `i` twins keep the surrounding quotation/catch shape identical.
: QUOT-CASE ( -- )
   s" j inside a quotation cannot borrow the caller's loop frames" T-LABEL
   s" : NJ-QBAD ( -- n ) 0 3 0 ?do 2 0 ?do [: j drop ;] catch + loop loop ;"
   EV-DEF 70 T=
   s" : NJ-QOK ( -- n ) 0 3 0 ?do 2 0 ?do [: 7 drop ;] catch + loop loop ;"
   EV-DEF 0 T=

   s" one loop inside the quotation is still one short" T-LABEL
   s" : NJ-QBAD2 ( -- n ) 0 3 0 ?do [: 0 2 0 ?do j + loop drop ;] catch + loop ;"
   EV-DEF 70 T=
   s" : NJ-QI ( -- n ) 0 3 0 ?do [: 0 2 0 ?do i + loop drop ;] catch + loop ;"
   EV-DEF 0 T= ;

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
s" the nested loop subject was compiled at tier 1" T-LABEL
NJ-FIXTURE:NJ-IJ-TIER 1 T=
NJ-TEST:RUN
T-REPORT
