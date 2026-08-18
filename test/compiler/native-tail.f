\ native-tail.f - the call a routine leaves through when its own names are still
\ standing, run against the engine's own compilation of the same source.
\ One concern: what a tail site publishes, and what it may leave unpublished.
\
\ THE SHAPE THIS SUITE IS ABOUT IS THE TREE'S MOST ORDINARY ACCESSOR. A checked
\ accessor guards its argument and then hands it to the word that gives the
\ answer its type - `buf CHECK-LIVE  buf LEN-RAW@ N>BLEN` - and a checked
\ constructor validates its fields and then hands them to the family's own MAKE.
\ Both leave through their last call, and in both the local the guard was given
\ is still a name the body can write when that last call is staged. The dialect
\ therefore hands it over at the call, because its honest statement about an
\ ordinary call is that no register survives one; and at the call control does
\ not come back from that statement is about a value no instruction will ever
\ read again.
\
\ WHAT WOULD GO WRONG IF THE SITE PUBLISHED IT ANYWAY, which is why this is not
\ a saving of two instructions. A call site writes the values it carries into
\ slots zero upwards and the callee's arguments ABOVE them, so a site that
\ carried one value would enter the callee one cell too high - and a tail branch
\ has no instruction in front of it to move the pointer with. The callee would
\ read the carried value as its argument. The whole class was refused
\ (E-A64SEL-TAIL) rather than compiled wrongly, and this suite is what says the
\ new site publishes the arguments and nothing else.
\
\ WHY EVERY CASE IS DIFFERENTIAL. What the site leaves out cannot be seen in the
\ shape of the code: a routine that dropped a value it needed, one that published
\ the arguments into the wrong slots, and one that got it right all emit a store
\ run and a branch. Only the ANSWER separates them, so every fixture is compiled
\ twice - once by the engine's own emitter, which never builds a tail branch at
\ all, and once by the native chain - and run against each other on pinned
\ inputs, including both ends of the signed range.
\
\ AND EVERY WIDE CASE IS WEIGHTED. Five values handed over and five answered back
\ come to the same total whichever order they arrive in, so a site that exchanged
\ two of them would agree with the engine and the row would prove only that the
\ right NUMBER of cells came back. Each value is scaled by its own odd factor at
\ both ends, so the answer says which value went where.
\
\ THE TAIL BRANCH ITSELF IS ASSERTED, and that is the other half. A chain that
\ simply stopped calling these bodies tail calls would answer every differential
\ correctly and lose the whole optimisation silently, so each migrated routine is
\ read back off its published code: it leaves by a branch, it has no trailing
\ return, and it holds exactly the calls that are NOT the one it leaves through.
\ The adversarial twin is NTL-NOTAIL, the same body with one number changed so
\ that the last callee's arity is not this routine's own - it must answer the
\ same as the engine AND must not have become a branch.
\
\ THE CALLEES ARE PAST THE COPYING CEILING ON PURPOSE. Both compilers copy a
\ short callee into its caller instead of calling it, and a copied callee is no
\ call at all - so a fixture whose callees were small would prove nothing about a
\ site that is not there. Every callee here is eleven operations or more, which
\ is the same discipline test/compiler/codegen-tail-probe.f keeps for the same
\ reason.
\
\ THE REFUSAL THAT IS LEFT lives in test/compiler/native-select.f, because it
\ needs a module the elaborator cannot produce: a site whose carried value the
\ return really does read. Every body the elaborator writes leaves them dead, so
\ the fail-closed proof is measured where hostile modules are built by hand.
\
\ Run: bin/hb --load test/compiler/native-tail.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/prelude.f
require lib/adt/option.f
require src/compiler/native/dict.f
require src/compiler/native/migrate.f
require tools/codegen-tail-probe.f

\ ---- the engine's compilation: the reference ---------------------------------
package NTL-FIXTURE

public

\ ---- the callees -------------------------------------------------------------
\ A guard: it takes a value and answers nothing, which is what makes the name
\ that was handed to it cross the call while still being live afterwards.
: NTL-GUARD ( n -- )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor drop ;

\ A guard over two values, for the wide rows.
: NTL-GUARD2 ( n n -- )
   NTL-GUARD NTL-GUARD ;

\ The word in the middle: it answers a value the tail call then takes.
: NTL-RAW ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ The tail callee of the narrow rows.
: NTL-ROLE ( n -- n )
   dup 17 * over 19 xor + swap 23 and + dup 29 * + 31 xor ;

\ The tail callee whose arity is NOT the caller's, which is what makes NTL-NOTAIL
\ an ordinary call rather than a branch.
: NTL-ROLE2 ( n n -- n )
   swap NTL-ROLE swap NTL-ROLE 37 * + ;

\ The tail callee of the five-wide rows, each value scaled by its own odd factor
\ so an exchange is a different answer.
: NTL-MAKE5 ( n n n n n -- n n n n n )
   {: a:n b:n c:n d:n e:n :}
   a 3 * b 5 * c 7 * d 11 * e 13 * ;

\ And of the ten-wide row.
: NTL-MAKE10 ( n n n n n n n n n n -- n n n n n n n n n n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :}
   a 3 * b 5 * c 7 * d 11 * e 13 * f 17 * g 19 * h 23 * j 29 * k 31 * ;

\ The tail callee that answers a double, which is the typed-result axis: the
\ result convention is the same cell of the caller's stack either way, and the
\ file it is read out of afterwards is the CALLER's business.
: NTL-REAL ( n -- r )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor
   s>f 1.5 f* 0.25 f+ ;

\ The same in the other direction: a double crosses the guard as the cell it is.
: NTL-SCALE ( r -- r )
   1.5 f* 0.25 f+ 2.0 f* 0.5 f- ;

\ A word that executes what it is handed, which is how a body makes a quotation
\ whose own last operation is a call.
: NTL-TAKE ( [ n -- n ] n -- n )
   swap execute ;

\ ---- the bodies --------------------------------------------------------------
\ lib/byte-buffer.f LEN@ reduced to its shape: a local, a guard the local
\ crosses, and a tail call whose argument is what the middle word answered. The
\ local is dead at the tail call and the dialect hands it over anyway.
: NTL-LEN ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-RAW NTL-ROLE ;

\ lib/map.f MAP-CELLS reduced: the local is the tail call's own argument, so the
\ value handed over and the value published are the same value.
: NTL-SELF ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-ROLE ;

\ The adversarial twin: the same body with a two-argument callee, which is not
\ this routine's arity, so it is an ordinary call and the routine returns.
: NTL-NOTAIL ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-RAW v NTL-ROLE2 ;

\ src/compiler/target.f CONTRACT and src/compiler/numeric-policy.f POLICY reduced:
\ five locals, a guard two of them cross, and a tail call taking all five.
: NTL-WIDE ( n n n n n -- n n n n n ) {: a:n b:n c:n d:n e:n :}
   c d NTL-GUARD2
   a b c d e NTL-MAKE5 ;

\ src/compiler/binding.f BIND reduced: ten values, which is what two five-cell
\ records taken apart come to.
: NTL-TEN ( n n n n n n n n n n -- n n n n n n n n n n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :}
   a k NTL-GUARD2
   a b c d e f g h j k NTL-MAKE10 ;

\ The typed-result axis, both ways round.
: NTL-FRESULT ( n -- r ) {: v:n :}
   v NTL-GUARD  v NTL-REAL ;

: NTL-FARG ( r -- r ) {: v:r :}
   7 NTL-GUARD  v NTL-SCALE ;

\ ---- and the site beside a wide instantiation ---------------------------------
\ WHAT A CONSTRUCTION PUTS IN A BODY THAT ALSO LEAVES THROUGH A CALL. A parametric
\ family instantiated with a multi-cell argument reserves more room than its
\ declaration does, so the chain pushes the missing zero cells at the CALL SITE,
\ in front of the constructor's own call (src/compiler/native/elaborate.f
\ CON-PADS-PUSH). Those cells are ordinary vector values that the call it stands
\ in front of consumes, so nothing of them survives it - but they arrive at a
\ call site, which is this suite's subject, and the two features have to be
\ measured together rather than assumed disjoint.
\
\ A PADDED CONSTRUCTION CANNOT ITSELF BE THE SITE A ROUTINE LEAVES THROUGH, and
\ that is a structural fact rather than a hope. The pads make the definition leave
\ MORE cells than the constructor declares, and the elaborator calls a body a tail
\ caller only when the callee's declared output IS the definition's own - so a
\ padded construction as the last call is an ordinary call and the routine
\ returns. `( n -- option<pt> ) drop OPTION:NONE` compiles that way (measured).
\ The two shapes that CAN be written are below: the tail over a wide bundle, and
\ the padded construction whose bundle feeds an ordinary last call.
\
\ AND THE CHECKER IS WHY THERE IS NO THIRD. A construction resolves its
\ instantiation from the definition's declared OUTPUT, so a body that constructs
\ must declare that bundle; a tail caller must declare the same width in as out;
\ and a bundle cannot be taken apart into cells without a dispatch, which is
\ control flow no tail caller has yet. `( n n n -- n n n )` holding a construction
\ is refused by the checker before the chain sees it (`expected: a actual: pt<>`,
\ measured), which is the honest reason this file has two rows here and not three.
PRODUCT pt 0
   FIELD x n
   FIELD y n
;PRODUCT

\ A maker for the wide-bundle rows' input, and its reader. Both are the engine's
\ on both sides of every differential: what is under test is the routine BETWEEN
\ them.
: NTL-MKO ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NTL--FIXTURE-PT:MAKE OPTION:SOME
         else  drop OPTION:NONE  then ;

: NTL-RD ( option<pt> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NTL--FIXTURE-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ The tail callee over the wide bundle: three cells in, three out, which is what
\ lets the routine below leave through it.
: NTL-BUMP ( option<pt> -- option<pt> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF NTL--FIXTURE-PT:UNMAKE swap 3 * swap 5 * NTL--FIXTURE-PT:MAKE OPTION:SOME ENDOF
   ;MATCH ;

\ A tail site whose cells are a BUNDLE and not three unrelated values.
: NTL-WBUNDLE ( option<pt> -- option<pt> )
   NTL-BUMP ;

\ And the padded construction beside one: both arms build a value of the wide
\ instantiation, and the last call is over the bundle they built. Its arity is not
\ its callee's, so it returns - which is what the shape row asserts.
: NTL-PADDED ( n -- option<pt> ) {: v:n :}
   v NTL-GUARD
   v 0 > if  v 3 *  v 5 *  NTL--FIXTURE-PT:MAKE OPTION:SOME  else  OPTION:NONE  then
   NTL-BUMP ;

;package

\ ---- the chain's compilation -------------------------------------------------
\ The same texts, compiled by the native chain and published under their own
\ names. Each text is written out again rather than shared with the definitions
\ above, because what the two sides have to agree about is the SOURCE: a fixture
\ that built its string from the engine's own record would be comparing one
\ compilation with itself.
package NTL-MIGRATED

private

: LEN ( -- )
   s" : NTL-LEN-N ( n -- n ) {: v:n :} v NTL-FIXTURE:NTL-GUARD  v NTL-FIXTURE:NTL-RAW NTL-FIXTURE:NTL-ROLE ;"
   1 1 NMIGRATE:DEFINE ;

: SELF ( -- )
   s" : NTL-SELF-N ( n -- n ) {: v:n :} v NTL-FIXTURE:NTL-GUARD  v NTL-FIXTURE:NTL-ROLE ;"
   1 1 NMIGRATE:DEFINE ;

: NOTAIL ( -- )
   s" : NTL-NOTAIL-N ( n -- n ) {: v:n :} v NTL-FIXTURE:NTL-GUARD  v NTL-FIXTURE:NTL-RAW v NTL-FIXTURE:NTL-ROLE2 ;"
   1 1 NMIGRATE:DEFINE ;

: WIDE ( -- )
   s" : NTL-WIDE-N ( n n n n n -- n n n n n ) {: a:n b:n c:n d:n e:n :} c d NTL-FIXTURE:NTL-GUARD2 a b c d e NTL-FIXTURE:NTL-MAKE5 ;"
   5 5 NMIGRATE:DEFINE ;

: TEN ( -- )
   s" : NTL-TEN-N ( n n n n n n n n n n -- n n n n n n n n n n ) {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :} a k NTL-FIXTURE:NTL-GUARD2 a b c d e f g h j k NTL-FIXTURE:NTL-MAKE10 ;"
   10 10 NMIGRATE:DEFINE ;

: FRESULT ( -- )
   s" : NTL-FRESULT-N ( n -- r ) {: v:n :} v NTL-FIXTURE:NTL-GUARD  v NTL-FIXTURE:NTL-REAL ;"
   1 1 NMIGRATE:DEFINE ;

: FARG ( -- )
   s" : NTL-FARG-N ( r -- r ) {: v:r :} 7 NTL-FIXTURE:NTL-GUARD  v NTL-FIXTURE:NTL-SCALE ;"
   1 1 NMIGRATE:DEFINE ;

: WBUNDLE ( -- )
   s" : NTL-WBUNDLE-N ( option<pt> -- option<pt> ) NTL-FIXTURE:NTL-BUMP ;"
   3 3 NMIGRATE:DEFINE ;

: PADDED ( -- )
   s" : NTL-PADDED-N ( n -- option<pt> ) {: v:n :} v NTL-FIXTURE:NTL-GUARD v 0 > if v 3 * v 5 * NTL--FIXTURE-PT:MAKE OPTION:SOME else OPTION:NONE then NTL-FIXTURE:NTL-BUMP ;"
   1 3 NMIGRATE:DEFINE ;

public

\ ---- the site belongs to the routine the contract describes -------------------
\ AN EMISSION HOLDS MORE THAN ONE ROUTINE and only the first of them was declared
\ to leave through a callee: the published word's own, and one per quotation its
\ body makes. A quotation body is entered through an address by whoever executes
\ it, so a branch out of ITS last call would return to that executor's caller.
\ This is that shape - a body whose own last call is a tail AND whose quotation's
\ last operation is a call - and the site is function zero's alone.
\
\ IT IS STILL REFUSED, AND NO LONGER BY THE SELECTOR. Before this lane the
\ selector built a second tail branch inside the quotation and refused the
\ emission for holding two (E-A64SEL-TAIL); now it builds one, and
\ src/compiler/native/regalloc-verify.f refuses instead - it asks the emission's
\ CONTRACT of every function, so the quotation's ordinary return is held against
\ a contract that declares a tail call. That is the same defect one pass further
\ on, and the code is asserted here rather than left unmeasured: when the
\ verifier's own lane lands, this row changes and the shape compiles.
: TRY-QUOT ( -- )
   s" : NTL-QUOT-N ( n -- n ) dup [: NTL-FIXTURE:NTL-ROLE ;] swap NTL-FIXTURE:NTL-TAKE drop NTL-FIXTURE:NTL-ROLE ;"
   1 1 NMIGRATE:MEASURE-HELD ;

: RUN ( -- )
   LEN SELF NOTAIL
   WIDE TEN
   FRESULT FARG
   WBUNDLE PADDED ;

;package

package NTL-FIXTURE
public

NTL-MIGRATED:RUN

;package

\ ---- the differentials -------------------------------------------------------
package NTL-TEST

private

\ The ends of the signed range, where arithmetic that is right for small numbers
\ is most likely to disagree.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ A double is compared by its bits and not by `f=`, because the two zeros are one
\ number under `f=` and eight different bytes in the cell a result is published
\ in - and the cell is what a caller of these routines reads.
create FBITS-CELL 8 allot

: FBITS ( r -- n )
   FBITS-CELL !  FBITS-CELL @ ;

\ Five and ten values weighted into one number, so an exchange of any two is a
\ different answer rather than the same total.
: WEIGH5 ( n n n n n -- n )
   {: a:n b:n c:n d:n e:n :}
   a 3 * b 5 * + c 7 * + d 11 * + e 13 * + ;

: WEIGH10 ( n n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :}
   a 3 * b 5 * + c 7 * + d 11 * + e 13 * +
   f 17 * + g 19 * + h 23 * + j 29 * + k 31 * + ;

: LEN= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-LEN  v NTL-FIXTURE:NTL-LEN-N  T= ;

: SELF= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-SELF  v NTL-FIXTURE:NTL-SELF-N  T= ;

: NOTAIL= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-NOTAIL  v NTL-FIXTURE:NTL-NOTAIL-N  T= ;

: WIDE= ( n -- ) {: v:n :}
   v v 1+ v 2 + v 3 + v 4 +  NTL-FIXTURE:NTL-WIDE   WEIGH5
   v v 1+ v 2 + v 3 + v 4 +  NTL-FIXTURE:NTL-WIDE-N WEIGH5  T= ;

: TEN= ( n -- ) {: v:n :}
   v v 1+ v 2 + v 3 + v 4 + v 5 + v 6 + v 7 + v 8 + v 9 +
   NTL-FIXTURE:NTL-TEN   WEIGH10
   v v 1+ v 2 + v 3 + v 4 + v 5 + v 6 + v 7 + v 8 + v 9 +
   NTL-FIXTURE:NTL-TEN-N WEIGH10  T= ;

: FRESULT= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-FRESULT   FBITS
   v NTL-FIXTURE:NTL-FRESULT-N FBITS  T= ;

: FARG= ( r -- ) {: v:r :}
   v NTL-FIXTURE:NTL-FARG   FBITS
   v NTL-FIXTURE:NTL-FARG-N FBITS  T= ;

\ The wide-bundle rows go in and come out through the ENGINE's own maker and
\ reader on both sides, so what the row measures is the routine between them.
: WBUNDLE= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-MKO NTL-FIXTURE:NTL-WBUNDLE   NTL-FIXTURE:NTL-RD
   v NTL-FIXTURE:NTL-MKO NTL-FIXTURE:NTL-WBUNDLE-N NTL-FIXTURE:NTL-RD  T= ;

: PADDED= ( n -- ) {: v:n :}
   v NTL-FIXTURE:NTL-PADDED   NTL-FIXTURE:NTL-RD
   v NTL-FIXTURE:NTL-PADDED-N NTL-FIXTURE:NTL-RD  T= ;

\ ---- what the published code says --------------------------------------------
: BRANCHES? ( ptr u8 n -- bool )
   NTAILPROBE:TAIL-BRANCH? ;

: RETURNS? ( ptr u8 n -- bool )
   NTAILPROBE:TRAILER-RET? ;

: CALLS ( ptr u8 n -- n )
   NTAILPROBE:CALLS ;

\ ---- the cases ---------------------------------------------------------------
\ EVERY INPUT IS A REAL TURN OF THE BODY. These bodies have no branch in them, so
\ what the rows have to cover is arithmetic that wraps and values whose bits
\ differ everywhere - which is what the two ends of the signed range are for.
: NARROW-CASE ( -- )
   s" an accessor that leaves through its typed-result call answers the engine's answer" T-LABEL
   0 LEN=  1 LEN=  -1 LEN=  7 LEN=  -13 LEN=  1000003 LEN=
   MIN-INT LEN=  MAX-INT LEN=
   0 SELF=  1 SELF=  -1 SELF=  7 SELF=  -13 SELF=
   MIN-INT SELF=  MAX-INT SELF= ;

: NOTAIL-CASE ( -- )
   s" and the twin whose callee arity is not the routine's answers it too" T-LABEL
   0 NOTAIL=  1 NOTAIL=  -1 NOTAIL=  7 NOTAIL=  -13 NOTAIL=
   MIN-INT NOTAIL=  MAX-INT NOTAIL= ;

: WIDE-CASE ( -- )
   s" a constructor that leaves through its family's MAKE answers value for value" T-LABEL
   0 WIDE=  1 WIDE=  -1 WIDE=  7 WIDE=  -13 WIDE=
   MIN-INT WIDE=  MAX-INT WIDE=
   0 TEN=  1 TEN=  -1 TEN=  7 TEN=  -13 TEN=
   MIN-INT TEN=  MAX-INT TEN= ;

: REAL-CASE ( -- )
   s" a tail callee's double is published in the caller's own result cell" T-LABEL
   0 FRESULT=  1 FRESULT=  -1 FRESULT=  7 FRESULT=  -13 FRESULT=
   MAX-INT FRESULT=
   0.5 FARG=  -0.5 FARG=  2.25 FARG=  -7.5 FARG=  1024.0 FARG= ;

\ THE TWO ROWS BESIDE A WIDE INSTANTIATION. The first is a tail site whose cells
\ are one BUNDLE - three cells that are one value, with the glue a construction
\ puts on them - and it is where the site's arity is not one. The second holds a
\ padded construction, whose zero cells the chain pushes at the constructor's own
\ call site: its last call is over the bundle those cells helped build, its arity
\ is not that callee's, and it returns. Both answer through the engine's own
\ maker and reader, so a lost or misplaced pad is a wrong NUMBER here.
: BUNDLE-CASE ( -- )
   s" a tail site over a wide bundle answers the engine's answer" T-LABEL
   0 WBUNDLE=  1 WBUNDLE=  -1 WBUNDLE=  7 WBUNDLE=  -13 WBUNDLE=
   MIN-INT WBUNDLE=  MAX-INT WBUNDLE=
   s" and a padded construction in a body whose last call is over the bundle" T-LABEL
   0 PADDED=  1 PADDED=  -1 PADDED=  7 PADDED=  -13 PADDED=
   MAX-INT PADDED= ;

\ THE SHAPE, WHICH IS WHAT SAYS THE OPTIMISATION IS REALLY THERE. NTL-LEN-N holds
\ two calls - the guard and the middle word - and leaves through the third, so
\ its emitted code has exactly two branch-with-links and ends in a plain branch
\ with no return behind it. NTL-NOTAIL-N is the same body with the last callee's
\ arity changed: three calls, and it returns.
: SHAPE-CASE ( -- )
   s" the routine really leaves by a branch and keeps its other calls" T-LABEL
   s" NTL-FIXTURE:NTL-LEN-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-LEN-N" RETURNS? TFALSE
   s" NTL-FIXTURE:NTL-LEN-N" CALLS 2 T=
   s" NTL-FIXTURE:NTL-SELF-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-SELF-N" CALLS 1 T=
   s" NTL-FIXTURE:NTL-WIDE-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-TEN-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-FRESULT-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-FARG-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-NOTAIL-N" BRANCHES? TFALSE
   s" NTL-FIXTURE:NTL-NOTAIL-N" RETURNS? TTRUE
   s" NTL-FIXTURE:NTL-NOTAIL-N" CALLS 3 T=

   s" the wide-bundle site is the whole routine, and the padded one is not" T-LABEL
   s" NTL-FIXTURE:NTL-WBUNDLE-N" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-WBUNDLE-N" RETURNS? TFALSE
   s" NTL-FIXTURE:NTL-WBUNDLE-N" CALLS 0 T=
   s" NTL-FIXTURE:NTL-PADDED-N" BRANCHES? TFALSE
   s" NTL-FIXTURE:NTL-PADDED-N" RETURNS? TTRUE ;

\ The quotation row, whose whole content is WHICH pass answers. A selector that
\ let a quotation's function hold the site refuses it itself, with the tail
\ code; the selector this lane leaves refuses nothing and the verifier answers.
: QUOT-CASE ( -- )
   s" a quotation's own last call is not the emission's tail site" T-LABEL
   [: NTL-MIGRATED:TRY-QUOT ;] E-A64RAV-SHAPE TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   NARROW-CASE
   NOTAIL-CASE
   WIDE-CASE
   REAL-CASE
   BUNDLE-CASE
   SHAPE-CASE
   QUOT-CASE
   T-REPORT ;

;package

NTL-TEST:RUN
