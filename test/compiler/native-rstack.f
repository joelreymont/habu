\ native-rstack.f - `>r`, `r>`, `r@` and their pair forms, run against the
\ engine's own compilation of the same source.
\ One concern: values parked on the return stack, and what happens to them at a
\ control-flow join, a loop edge and a call.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. This chain does
\ not compile `>r` to anything: it moves a value id from the compile-time DATA
\ vector to a compile-time RETURN vector, emits no instruction, and never touches
\ the engine's return-stack region. Nothing about the resulting code says which
\ vector a value came from, so a suite that counted blocks or operations would
\ pass against an elaborator that had lost a parked value at a join, handed one to
\ the wrong block argument, or given two arms different ones. What the mistake
\ changes is a NUMBER the routine answers, so every case here is DIFFERENTIAL:
\ the same source text compiled twice, once by the engine's own emitter and once
\ by the native chain, run against each other on pinned inputs.
\
\ THE SEAMS ARE WHAT THE CASES ARE CHOSEN FOR, and each is a different mistake.
\ A value parked ACROSS an `if` rides the false stub, so the join's width is the
\ `if`'s own; a value popped INSIDE both arms leaves the join with fewer, so the
\ width is the first arm's; a counted loop's header takes the parked values back
\ as block arguments every turn, and a body that pops and re-parks one per turn
\ makes those arguments genuinely different values each time; and a CALL hands
\ every parked value over as an operand and takes it back as a result, in the
\ group in front of the arguments - because the arguments are the last operands
\ and a live value after them is read by the callee as an argument.
\
\ THE INPUTS ARE CHOSEN, NOT SAMPLED. Every branching case is run at inputs on
\ both sides of its own test and at the boundary, so an arm that was never taken
\ cannot hide; the loop cases are run at zero, one and several turns, so a body
\ that lost the parked value on the way in and a body that lost it on the way out
\ answer differently; and the ends of the signed range are where arithmetic that
\ is right for small numbers is most likely to disagree.
\
\ AND THE REFUSALS ARE HALF THE SUITE, because the interesting ones are about
\ what a CALLEE does rather than what this body does. A word whose declared effect
\ moves its caller's return stack cannot be called from compiled code here - the
\ caller's return stack is compile-time bookkeeping and there is nowhere to put
\ the motion - so src/compiler/native/dict.f asks the checker and refuses. The
\ three cases below are an adversarial trio for that question: one word MENTIONS
\ `>r` and is neutral, one WRITES a `| R -- | R` clause and is neutral, and one
\ writes a clause that really moves a cell. A reader keyed on the body's text
\ reds the first, a reader keyed on the clause's presence reds the second, and
\ only a reader of what the ROWS SAY answers all three.
\
\ AND TWO MORE REFUSALS ARE ABOUT THIS BODY, each measured beside a twin that
\ COMPILES so the row says where the cut falls rather than that deep or awkward
\ bodies are refused in general. The first is the elaborator's own ceiling: the
\ compile-time return vector holds sixteen cells, so a body parking seventeen is
\ refused and one parking sixteen is not. The checker has no return-depth ceiling
\ of its own - the seventeen-deep body certifies and RUNS under `bin/hb` - so the
\ elaborator's wall is the first one such a body meets and an ordinary program
\ can provoke it. The second is the parked QUOTATION, which is the shape
\ src/core/combinators.f BI and TRI arrive here in: a parked cell may only ever
\ be a plain one, and its twin parks the plain cell out of the same body and
\ compiles.
\
\ Run: bin/hb --load test/compiler/native-rstack.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/prelude.f
require src/compiler/native/migrate.f

\ ---- the engine's compilation: the reference ---------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, whose `>r` really writes a cell into the return-stack region.
package NRS-FIXTURE

public

\ ---- straight line -----------------------------------------------------------
: NRS-TOR ( n -- n ) >r 5 r> + ;

\ A peek is not a pop: both reads answer the same cell and the body still owes
\ exactly one `r>`.
: NRS-FET ( n -- n ) >r r@ r@ + r> + ;

\ The pair forms, whose whole content is that the lower cell stays lower - so
\ `2r>` puts the two back the way `2>r` took them. Subtracting rather than adding
\ is what makes the order visible in the answer.
: NRS-PAIR ( n n -- n ) 2>r 2r> - ;

: NRS-2FET ( n n -- n ) 2>r 2r@ - 2r> - + ;

\ EACH PARKED VALUE IS WEIGHTED DIFFERENTLY, and that is not decoration. Three
\ values combined by addition answer the same number in any order, so a chain that
\ handed them back exchanged would agree with the engine and this row would prove
\ only that three cells came back. Distinct odd multipliers make the ANSWER say
\ which cell came back where. The same reason gives NRS-2CALL below its weights,
\ and the pair forms get it for free from subtracting rather than adding.
: NRS-DEEP ( n n n -- n ) >r >r >r 1 r> 3 * + r> 5 * + r> 7 * + ;

\ ---- across a join -----------------------------------------------------------
\ Parked before the `if` and popped in EACH arm, so the join carries one fewer
\ parked value than the `if` opened with and the width is the first arm's.
: NRS-BRANCH ( n -- n ) >r 5 r@ 3 > if r> + else r> - then ;

: NRS-SPLIT ( n -- n ) >r r@ 4 > if r> 2 * else r> 3 * then ;

\ Parked ACROSS the whole `if`, and with no `else`: the join is also reached by
\ the `if`'s own false stub, which carries what the `if` was holding.
: NRS-HELD ( n -- n ) >r 0 r@ 3 > if 1 + then r> + ;

\ Three joins deep, with the parked value read at every level.
: NRS-NEST3 ( n -- n )
   >r 0 r@ 1 > if r@ 2 > if 1 + else 2 + then else 3 + then r> + ;

\ TWO PARKED VALUES ACROSS ONE JOIN, WEIGHTED, and that is the row that says the
\ seam keeps their ORDER. With one parked value a seam that spilled and filled
\ from the wrong end is invisible; with two it exchanges them, and only weights
\ make the answer notice.
: NRS-2HELD ( n n -- n ) {: a:n b:n :}
   a >r b >r 0 r@ 3 > if 1 + then r> 3 * + r> 5 * + ;

\ ---- across a loop edge ------------------------------------------------------
\ The header takes the parked value back as a block argument on every turn.
: NRS-QLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r@ i + + loop r> + ;

: NRS-DLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 do r@ i + + loop r> + ;

\ POPPED AND RE-PARKED ONCE PER TURN, which is the case that makes the header's
\ parked argument a genuinely different value each time round rather than one the
\ latch hands back unchanged.
: NRS-CARRY ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r> dup >r + i + loop r> + ;

\ THE TRIP COUNT READS THE PARKED VALUE AND STILL TERMINATES, and both halves of
\ that are deliberate. A body whose accumulator grows by the parked value itself
\ runs forever at a negative one - the ENGINE's own compilation of it does, so it
\ would be a suite that hangs rather than one that fails - while a body that never
\ read the parked value would pass against a loop that lost it. Adding its low bit
\ makes each turn advance the accumulator by one or two, so the loop always ends,
\ and makes the number of turns depend on the value that has to survive the edge.
: NRS-WHILE ( n n -- n ) {: k:n lim:n :}
   k >r 0 begin dup lim < while r@ 1 and + 1 + repeat r> + ;

: NRS-NESTLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do 3 0 ?do r@ i + + loop loop r> + ;

\ And two of them across a loop edge, for NRS-2HELD's reason: the header takes
\ both back as block arguments every turn, and only weights say which is which.
: NRS-2LOOP ( n n n -- n ) {: a:n b:n lim:n :}
   a >r b >r 0 lim 0 ?do r@ i + + loop r> 3 * + r> 5 * + ;

\ THE TWO LOOP WORDS THAT LEAVE FROM THE MIDDLE, which are the newest seams the
\ parked values cross. `again` closes a `begin` with a back edge and the body
\ leaves through an `exit`; `leave` branches out of the innermost counted loop to
\ the block its `loop` also branches to, so that block is reached by two edges
\ that must agree about how many parked values they carry as well as how many
\ data values.
\
\ THE TRIP COUNT IS BOUNDED BY THE ACCUMULATOR AND NOT BY THE PARKED VALUE, for
\ the reason NRS-WHILE gives: a `begin`/`again` whose exit test read the parked
\ value would not come back at some inputs, and a suite that hangs is worse than
\ one that fails.
: NRS-AGAIN ( n n -- n ) {: k:n lim:n :}
   k >r 0 begin 1 + dup lim > if r> + exit then again ;

: NRS-LEAVE ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do i 2 > if leave then r@ i + + loop r> + ;

\ A tag-dispatch form, whose arms are reached through one another's mismatch
\ stubs rather than through a join.
: NRS-CASE ( n n -- n ) {: k:n sel:n :}
   k >r sel case 1 of r@ 10 * endof 2 of r@ 20 * endof 30 swap endcase r> + ;

\ ---- across a call -----------------------------------------------------------
\ Long enough that neither generator copies it, so what crosses is really a call.
: NRS-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NRS-CALL ( n n -- n ) {: k:n s:n :}
   k >r s NRS-CALLEE r> + ;

: NRS-2CALL ( n n n -- n ) {: a:n b:n s:n :}
   a >r b >r s NRS-CALLEE r> 3 * + r> 5 * + ;

\ A call inside a counted loop with a bound local read after it, so the loop's
\ counters, the crossing local AND the parked value all travel as operands of
\ every edge and of the call - which is the one place their order matters.
: NRS-CALLLOOP ( n n n -- n ) {: k:n s:n lim:n :}
   k >r s lim 0 ?do NRS-CALLEE k + loop r> + ;

\ ---- the three words the callee refusal is asked about -----------------------
\ Neutral because its ROWS move nothing, though its body mentions `>r`: a reader
\ that answered from the body's text refuses every internal use in the tree.
: NRS-BAL ( n -- n ) >r r> ;

\ Neutral because its rows are one row variable on both sides, though it WRITES a
\ `| rin -- rout` clause: a reader that answered from the clause's presence
\ refuses a word for how it is spelled.
: NRS-RVAR ( n | R -- n | R ) ;

\ And the one that really moves a cell, with its mirror.
: NRS-PUSH ( n | -- | n ) >r ;
: NRS-POP ( | n -- n | ) r> ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry. The refusals are caught
\ into cells here and asserted below, because a migration runs while the fixture
\ package is open and an assertion reads better beside the others.
package NRS-MIGRATED

private

variable RC-BAL                      \ what a call to a body-mentions-`>r` word answered
variable RC-RVAR                     \ and to one whose clause moves nothing
variable RC-PUSH                     \ and to one whose clause really moves a cell
variable RC-D17                      \ what a body parking seventeen cells answered
variable RC-D16                      \ and one parking sixteen, which is the ceiling
variable RC-QUOT                     \ what a body parking a QUOTATION answered
variable RC-CELL                     \ and the same body parking the plain cell

: TOR ( -- )
   s" : NRS-TOR-N ( n -- n ) >r 5 r> + ;" NMIGRATE:DEFINE ;

: FET ( -- )
   s" : NRS-FET-N ( n -- n ) >r r@ r@ + r> + ;" NMIGRATE:DEFINE ;

: PAIR ( -- )
   s" : NRS-PAIR-N ( n n -- n ) 2>r 2r> - ;" NMIGRATE:DEFINE ;

: TWOFET ( -- )
   s" : NRS-2FET-N ( n n -- n ) 2>r 2r@ - 2r> - + ;" NMIGRATE:DEFINE ;

: DEEP ( -- )
   s" : NRS-DEEP-N ( n n n -- n ) >r >r >r 1 r> 3 * + r> 5 * + r> 7 * + ;"
   NMIGRATE:DEFINE ;

: BRANCH ( -- )
   s" : NRS-BRANCH-N ( n -- n ) >r 5 r@ 3 > if r> + else r> - then ;"
   NMIGRATE:DEFINE ;

: SPLIT ( -- )
   s" : NRS-SPLIT-N ( n -- n ) >r r@ 4 > if r> 2 * else r> 3 * then ;"
   NMIGRATE:DEFINE ;

: HELD ( -- )
   s" : NRS-HELD-N ( n -- n ) >r 0 r@ 3 > if 1 + then r> + ;"
   NMIGRATE:DEFINE ;

: NEST3 ( -- )
   s" : NRS-NEST3-N ( n -- n ) >r 0 r@ 1 > if r@ 2 > if 1 + else 2 + then else 3 + then r> + ;"
   NMIGRATE:DEFINE ;

: TWOHELD ( -- )
   s" : NRS-2HELD-N ( n n -- n ) {: a:n b:n :} a >r b >r 0 r@ 3 > if 1 + then r> 3 * + r> 5 * + ;"
   NMIGRATE:DEFINE ;

: TWOLOOP ( -- )
   s" : NRS-2LOOP-N ( n n n -- n ) {: a:n b:n lim:n :} a >r b >r 0 lim 0 ?do r@ i + + loop r> 3 * + r> 5 * + ;"
   NMIGRATE:DEFINE ;

: QLOOP ( -- )
   s" : NRS-QLOOP-N ( n n -- n ) {: k:n lim:n :} k >r 0 lim 0 ?do r@ i + + loop r> + ;"
   NMIGRATE:DEFINE ;

: DLOOP ( -- )
   s" : NRS-DLOOP-N ( n n -- n ) {: k:n lim:n :} k >r 0 lim 0 do r@ i + + loop r> + ;"
   NMIGRATE:DEFINE ;

: CARRY ( -- )
   s" : NRS-CARRY-N ( n n -- n ) {: k:n lim:n :} k >r 0 lim 0 ?do r> dup >r + i + loop r> + ;"
   NMIGRATE:DEFINE ;

: WLOOP ( -- )
   s" : NRS-WHILE-N ( n n -- n ) {: k:n lim:n :} k >r 0 begin dup lim < while r@ 1 and + 1 + repeat r> + ;"
   NMIGRATE:DEFINE ;

: NESTLOOP ( -- )
   s" : NRS-NESTLOOP-N ( n n -- n ) {: k:n lim:n :} k >r 0 lim 0 ?do 3 0 ?do r@ i + + loop loop r> + ;"
   NMIGRATE:DEFINE ;

: AGAINLOOP ( -- )
   s" : NRS-AGAIN-N ( n n -- n ) {: k:n lim:n :} k >r 0 begin 1 + dup lim > if r> + exit then again ;"
   NMIGRATE:DEFINE ;

: LEAVELOOP ( -- )
   s" : NRS-LEAVE-N ( n n -- n ) {: k:n lim:n :} k >r 0 lim 0 ?do i 2 > if leave then r@ i + + loop r> + ;"
   NMIGRATE:DEFINE ;

: DISPATCH ( -- )
   s" : NRS-CASE-N ( n n -- n ) {: k:n sel:n :} k >r sel case 1 of r@ 10 * endof 2 of r@ 20 * endof 30 swap endcase r> + ;"
   NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NRS-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   NMIGRATE:DEFINE ;


: CALL ( -- )
   s" : NRS-CALL-N ( n n -- n ) {: k:n s:n :} k >r s NRS-CALLEE-N r> + ;"
   NMIGRATE:DEFINE ;

: TWOCALL ( -- )
   s" : NRS-2CALL-N ( n n n -- n ) {: a:n b:n s:n :} a >r b >r s NRS-CALLEE-N r> 3 * + r> 5 * + ;"
   NMIGRATE:DEFINE ;

: CALLLOOP ( -- )
   s" : NRS-CALLLOOP-N ( n n n -- n ) {: k:n s:n lim:n :} k >r s lim 0 ?do NRS-CALLEE-N k + loop r> + ;"
   NMIGRATE:DEFINE ;

\ ---- the three callee questions, measured and recorded ------------------------
\ MEASURE-HELD runs every stage a publication runs and keeps none of it, so a
\ refusal is the throw it answers with and a compilable body is a zero. The three
\ bodies differ ONLY in which word they call.
: TRY-BAL ( -- )
   s" : NRS-Z1-N ( n -- n ) NRS-BAL 1 + ;" NMIGRATE:MEASURE-HELD ;

: TRY-RVAR ( -- )
   s" : NRS-Z2-N ( n -- n ) NRS-RVAR 1 + ;" NMIGRATE:MEASURE-HELD ;

: TRY-PUSH ( -- )
   s" : NRS-Z3-N ( n -- n ) NRS-PUSH NRS-POP 1 + ;" NMIGRATE:MEASURE-HELD ;

\ ---- the ceiling, and the depth just below it --------------------------------
\ SEVENTEEN PARKED CELLS IS ONE PAST WHAT THE COMPILE-TIME RETURN VECTOR HOLDS
\ (src/compiler/native/elaborate.f RMAX, sixteen), and this is the pair that says
\ so. The CHECKER has no return-depth ceiling: this exact body certifies and runs
\ under `bin/hb`, answering 153. So the wall is the elaborator's, an ordinary
\ program reaches it, and the sixteen-deep twin below compiles - without it the
\ row would read as "a deep body is refused" and would still pass against a
\ chain that refused every depth over four.
\
\ THE LITERALS ARE PUSHED BEFORE THE PARKING so the definition declares no inputs
\ at all: what is measured is the vector's own ceiling, and nothing here depends
\ on how many cells a routine may be entered with.
: TRY-D17 ( -- )
   s" : NRS-Z4-N ( -- n ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> + + + + + + + + + + + + + + + + ;"
   NMIGRATE:MEASURE-HELD ;

: TRY-D16 ( -- )
   s" : NRS-Z5-N ( -- n ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> + + + + + + + + + + + + + + + ;"
   NMIGRATE:MEASURE-HELD ;

\ ---- a parked quotation, and the same body parking a plain cell --------------
\ THIS IS THE ARRIVAL SHAPE THE REFUSAL WAS WRITTEN FOR, reduced to one line.
\ src/core/combinators.f BI and TRI take a quotation as a parameter and park it
\ with `>r`; here the quotation arrives the same way, `>r` parks it and `r>`
\ takes it back for `execute`. The elaborator refuses the PARK, because a
\ quotation mark says "this cell is body k of this emission" and the return
\ vector carries no marks - so parking one would hand its consumer a cell nobody
\ can name a body for (elaborate.f RSTACK-CK).
\
\ THE TWIN PARKS THE PLAIN CELL OUT OF THE SAME BODY, which is what makes the row
\ above about the MARK and not about the neighbourhood. Both texts declare a
\ quotation parameter, both park with `>r`, both take it back with `r>` and both
\ end in `execute`; only WHICH of the two cells is parked differs. A reader keyed
\ on "this body holds a quotation and parks something" refuses both and reds the
\ twin.
\
\ THE CODE IS SHARED TODAY AND THIS ASSERTION IS THE ONE THAT MOVES. E-NELAB-BUNDLE
\ carries both the quotation-mark clause and the real multi-cell-value clause; dot
\ habu-give-the-quotation-df06937c mints the quotation mark a code of its own, and
\ when it lands the expected code here changes with it.
: TRY-QUOT ( -- )
   s" : NRS-Z6-N ( n [ n -- n ] -- n ) >r r> execute ;" NMIGRATE:MEASURE-HELD ;

: TRY-CELL ( -- )
   s" : NRS-Z7-N ( n [ n -- n ] -- n ) swap >r r> swap execute ;" NMIGRATE:MEASURE-HELD ;

public

: RC-BAL@ ( -- n ) RC-BAL @ ;
: RC-RVAR@ ( -- n ) RC-RVAR @ ;
: RC-PUSH@ ( -- n ) RC-PUSH @ ;
: RC-D17@ ( -- n ) RC-D17 @ ;
: RC-D16@ ( -- n ) RC-D16 @ ;
: RC-QUOT@ ( -- n ) RC-QUOT @ ;
: RC-CELL@ ( -- n ) RC-CELL @ ;

: RUN ( -- )
   TOR FET PAIR TWOFET DEEP
   BRANCH SPLIT HELD NEST3 TWOHELD
   QLOOP DLOOP CARRY WLOOP NESTLOOP TWOLOOP AGAINLOOP LEAVELOOP DISPATCH
   CALLEE CALL TWOCALL CALLLOOP
   [: TRY-BAL ;] catch RC-BAL !
   [: TRY-RVAR ;] catch RC-RVAR !
   [: TRY-PUSH ;] catch RC-PUSH !
   [: TRY-D17 ;] catch RC-D17 !
   [: TRY-D16 ;] catch RC-D16 !
   [: TRY-QUOT ;] catch RC-QUOT !
   [: TRY-CELL ;] catch RC-CELL ! ;

;package

package NRS-FIXTURE
public

NRS-MIGRATED:RUN

;package

package NRS-TEST

private

\ The ends of the signed range, where arithmetic that is right for small numbers
\ is most likely to disagree.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ ---- the differentials -------------------------------------------------------
: TOR= ( n -- ) {: k:n :}
   k NRS-FIXTURE:NRS-TOR  k NRS-FIXTURE:NRS-TOR-N  T=
   k NRS-FIXTURE:NRS-FET  k NRS-FIXTURE:NRS-FET-N  T= ;

: PAIR= ( n n -- ) {: a:n b:n :}
   a b NRS-FIXTURE:NRS-PAIR  a b NRS-FIXTURE:NRS-PAIR-N  T=
   a b NRS-FIXTURE:NRS-2FET  a b NRS-FIXTURE:NRS-2FET-N  T= ;

: DEEP= ( n n n -- ) {: a:n b:n c:n :}
   a b c NRS-FIXTURE:NRS-DEEP  a b c NRS-FIXTURE:NRS-DEEP-N  T= ;

: BRANCH= ( n -- ) {: k:n :}
   k NRS-FIXTURE:NRS-BRANCH  k NRS-FIXTURE:NRS-BRANCH-N  T=
   k NRS-FIXTURE:NRS-SPLIT   k NRS-FIXTURE:NRS-SPLIT-N   T=
   k NRS-FIXTURE:NRS-HELD    k NRS-FIXTURE:NRS-HELD-N    T=
   k NRS-FIXTURE:NRS-NEST3   k NRS-FIXTURE:NRS-NEST3-N   T= ;

: TWOHELD= ( n n -- ) {: a:n b:n :}
   a b NRS-FIXTURE:NRS-2HELD  a b NRS-FIXTURE:NRS-2HELD-N  T= ;

: TWOLOOP= ( n n n -- ) {: a:n b:n lim:n :}
   a b lim NRS-FIXTURE:NRS-2LOOP  a b lim NRS-FIXTURE:NRS-2LOOP-N  T= ;

: AGAIN= ( n n -- ) {: k:n lim:n :}
   k lim NRS-FIXTURE:NRS-AGAIN  k lim NRS-FIXTURE:NRS-AGAIN-N  T= ;

: LEAVE= ( n n -- ) {: k:n lim:n :}
   k lim NRS-FIXTURE:NRS-LEAVE  k lim NRS-FIXTURE:NRS-LEAVE-N  T= ;

: LOOP= ( n n -- ) {: k:n lim:n :}
   k lim NRS-FIXTURE:NRS-QLOOP     k lim NRS-FIXTURE:NRS-QLOOP-N     T=
   k lim NRS-FIXTURE:NRS-CARRY     k lim NRS-FIXTURE:NRS-CARRY-N     T=
   k lim NRS-FIXTURE:NRS-WHILE     k lim NRS-FIXTURE:NRS-WHILE-N     T=
   k lim NRS-FIXTURE:NRS-NESTLOOP  k lim NRS-FIXTURE:NRS-NESTLOOP-N  T= ;

\ A plain `do` always runs at least one turn, so it is compared on its own rows:
\ at an equal limit and start the `?do` rows above run none and these run one.
: DLOOP= ( n n -- ) {: k:n lim:n :}
   k lim NRS-FIXTURE:NRS-DLOOP  k lim NRS-FIXTURE:NRS-DLOOP-N  T= ;

: CASE= ( n n -- ) {: k:n sel:n :}
   k sel NRS-FIXTURE:NRS-CASE  k sel NRS-FIXTURE:NRS-CASE-N  T= ;

: CALL= ( n n -- ) {: k:n s:n :}
   k s NRS-FIXTURE:NRS-CALL  k s NRS-FIXTURE:NRS-CALL-N  T= ;

: TWOCALL= ( n n n -- ) {: a:n b:n s:n :}
   a b s NRS-FIXTURE:NRS-2CALL  a b s NRS-FIXTURE:NRS-2CALL-N  T= ;

: CALLLOOP= ( n n n -- ) {: k:n s:n lim:n :}
   k s lim NRS-FIXTURE:NRS-CALLLOOP  k s lim NRS-FIXTURE:NRS-CALLLOOP-N  T= ;

\ ---- the cases ---------------------------------------------------------------
: STRAIGHT-CASE ( -- )
   s" a parked value comes back the value it was" T-LABEL
   0 TOR=  1 TOR=  -1 TOR=  7 TOR=  MIN-INT TOR=  MAX-INT TOR=
   0 0 PAIR=  1 2 PAIR=  2 1 PAIR=  -3 5 PAIR=  MAX-INT MIN-INT PAIR=
   0 0 0 DEEP=  1 2 3 DEEP=  -1 -2 -3 DEEP=  MAX-INT 1 MIN-INT DEEP= ;

\ EVERY INPUT BELOW IS ON ONE SIDE OF A TEST THE BODIES MAKE, and the two tests
\ are `> 3` and `> 4`, so 3, 4 and 5 exercise both arms of both bodies. Without
\ them a suite could take one arm everywhere and prove nothing about the other.
: BRANCH-CASE ( -- )
   s" a parked value survives an if, and both arms may pop it" T-LABEL
   0 BRANCH=  1 BRANCH=  2 BRANCH=  3 BRANCH=  4 BRANCH=  5 BRANCH=
   -1 BRANCH=  MIN-INT BRANCH=  MAX-INT BRANCH=
   0 0 TWOHELD=  1 2 TWOHELD=  2 1 TWOHELD=  4 2 TWOHELD=  2 4 TWOHELD=
   -3 5 TWOHELD=  5 -3 TWOHELD= ;

\ ZERO TURNS, ONE TURN AND SEVERAL, which is what tells a body that lost the
\ parked value on the way INTO the loop from one that lost it on the way OUT: at
\ zero turns the header runs once and the body never does.
: LOOP-CASE ( -- )
   s" a parked value crosses a loop edge every turn" T-LABEL
   0 0 LOOP=  0 1 LOOP=  0 5 LOOP=
   7 0 LOOP=  7 1 LOOP=  7 4 LOOP=  -3 4 LOOP=
   MAX-INT 3 LOOP=  MIN-INT 3 LOOP=
   0 1 DLOOP=  7 1 DLOOP=  7 4 DLOOP=  -3 4 DLOOP=
   0 0 0 TWOLOOP=  1 2 0 TWOLOOP=  1 2 1 TWOLOOP=  2 1 1 TWOLOOP=
   1 2 4 TWOLOOP=  2 1 4 TWOLOOP=  -3 5 4 TWOLOOP=  5 -3 4 TWOLOOP= ;

\ THE INPUTS STRADDLE EACH WORD'S OWN CUT. `again` leaves when the accumulator
\ passes the limit, so a limit at, below and above zero says the exit test is
\ read the way the engine reads it; `leave` cuts at index two, so limits of nought
\ through five run the loop to its end, exactly to the cut, and past it.
: EARLY-EXIT-CASE ( -- )
   s" a parked value survives again and leave" T-LABEL
   0 0 AGAIN=  7 0 AGAIN=  -3 0 AGAIN=  7 1 AGAIN=  7 4 AGAIN=
   7 -1 AGAIN=  -3 4 AGAIN=
   0 0 LEAVE=  7 0 LEAVE=  7 1 LEAVE=  7 3 LEAVE=  7 4 LEAVE=
   7 5 LEAVE=  -3 5 LEAVE= ;

: CASE-CASE ( -- )
   s" a parked value crosses a tag-dispatch form's arms" T-LABEL
   0 0 CASE=  0 1 CASE=  0 2 CASE=  0 3 CASE=
   7 1 CASE=  7 2 CASE=  7 9 CASE=  -3 2 CASE= ;

\ THE CALL IS THE ONE SEAM WHERE THE ORDER OF THE OPERANDS DECIDES THE ANSWER.
\ The callee reads its argument out of the slot one below the pointer it is
\ entered with, so a parked value published AFTER the arguments is the value the
\ callee computes on. Every row here would still answer for a site that published
\ them in front, which is where they go, and reds for one that did not.
: CALL-CASE ( -- )
   s" a parked value survives a call and the arguments stay last" T-LABEL
   0 0 CALL=  1 2 CALL=  -5 7 CALL=  MAX-INT 3 CALL=  MIN-INT 3 CALL=
   0 0 0 TWOCALL=  1 2 3 TWOCALL=  -1 -2 -3 TWOCALL=
   0 0 0 CALLLOOP=  1 2 0 CALLLOOP=  1 2 1 CALLLOOP=  3 5 4 CALLLOOP=
   -3 5 4 CALLLOOP= ;

\ ---- the callee question -----------------------------------------------------
\ THE THREE ANSWERS ARE THE FIXTURE, and no two of them may move together. A
\ reader keyed on the callee's BODY text answers "not neutral" for NRS-BAL and
\ reds the first row; a reader keyed on the PRESENCE of a `| rin -- rout` clause
\ answers "not neutral" for NRS-RVAR and reds the second; a reader that answered
\ neutral for everything reds the third. Only the rows themselves pass all three.
: CALLEE-CASE ( -- )
   s" a callee that moves the caller's return stack is refused, by name" T-LABEL
   NRS-MIGRATED:RC-BAL@ 0 T=
   NRS-MIGRATED:RC-RVAR@ 0 T=
   NRS-MIGRATED:RC-PUSH@ E-HIR-UNMODELED T= ;

\ ---- what this body itself is refused for ------------------------------------
\ THE PAIR IS THE ASSERTION IN BOTH ROWS BELOW. A depth of seventeen is refused
\ and a depth of sixteen is not, so the row measures a CEILING at sixteen rather
\ than a dislike of deep bodies; a parked quotation is refused and the plain cell
\ parked out of that same body is not, so the row measures the MARK rather than
\ the company the `>r` keeps.
: CEILING-CASE ( -- )
   s" a seventeenth parked cell is refused by the vector's own ceiling" T-LABEL
   NRS-MIGRATED:RC-D17@ E-NELAB-CAP T=
   NRS-MIGRATED:RC-D16@ 0 T= ;

: PARKED-QUOT-CASE ( -- )
   s" a parked quotation is refused, by the mark and not by the body" T-LABEL
   NRS-MIGRATED:RC-QUOT@ E-NELAB-BUNDLE T=
   NRS-MIGRATED:RC-CELL@ 0 T= ;

public

: RUN ( -- )
   STRAIGHT-CASE
   BRANCH-CASE
   LOOP-CASE
   EARLY-EXIT-CASE
   CASE-CASE
   CALL-CASE
   CALLEE-CASE
   CEILING-CASE
   PARKED-QUOT-CASE ;

;package

T-RESET
NRS-TEST:RUN
T-REPORT
