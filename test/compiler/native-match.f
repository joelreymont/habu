\ native-match.f - production MATCH, case, and construct compilation.

require test/compiler/native-eval-fixture.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/adt/option.f
require src/compiler/native/compiler.f
require src/compiler/native/branch.f
require src/compiler/native/codewalk.f
require src/compiler/native/dict.f
require src/compiler/native/trap.f
require test/compiler/native-match-layout.f

package NMX
private

4 constant INSN-BYTES

public

\ ---- the families the cases dispatch over -------------------------------------
\ A payload-free enum, whose value is ONE cell and whose bundle therefore carries
\ no glue at all.
ENUM hue
   red
   green
   blue
;ENUM

\ A sum with payloads of three different widths in one family: the widest
\ variant fixes how many cells every value of it occupies, so the narrower two
\ are padded up to it and each arm drops a different number of pads.
SUMTYPE box 0
   VARIANT nil ;VARIANT
   VARIANT one n ;VARIANT
   VARIANT two n n ;VARIANT
;SUMTYPE

\ A product of two cells, and a family one of whose variants carries it. Its
\ payload is TWO CELLS AND ONE VALUE, where `box`'s widest variant is two cells
\ and two values - which is the whole of what the arm's own glue rule decides,
\ and the pair of cases below is what binds it.
PRODUCT pt 0
   FIELD x n
   FIELD y n
;PRODUCT

SUMTYPE holder 0
   VARIANT empty ;VARIANT
   VARIANT full pt ;VARIANT
;SUMTYPE

\ THREE cells, which `box` and `holder` between them do not reach. The widest
\ payload either of them carries is two, so an arm that drops one pad too many
\ or keeps one cell too few answers the same shape as an arm that is right; a
\ third cell is what makes the drop count and the keep count two separate
\ numbers a fixture can tell apart. `trio` carries three INDEPENDENT cells and
\ `hold3` carries three cells that are ONE value, which is the same pair `box`
\ and `holder` draw one cell narrower.
PRODUCT pt3 0
   FIELD x n
   FIELD y n
   FIELD z n
;PRODUCT

SUMTYPE trio 0
   VARIANT t0 ;VARIANT
   VARIANT t1 n ;VARIANT
   VARIANT t3 n n n ;VARIANT
;SUMTYPE

SUMTYPE hold3 0
   VARIANT empty3 ;VARIANT
   VARIANT full3 pt3 ;VARIANT
;SUMTYPE

\ A PARAMETRIC FAMILY THIS PACKAGE OWNS, which is what the `construct` half of a
\ wide instantiation needs. Minting a value of a family belongs to the package
\ that DECLARED it, so `construct option none` cannot be spelled here at all and
\ the reserved form's side of the story would otherwise be untestable. It is
\ `option` in every respect that matters - one parameter, an empty variant and a
\ one-field one - so the two spellings of one construction can be held against
\ each other: `OPTION:NONE` is a CALL to a routine that pushes what the family
\ declares, and `construct opt2 n2` is this chain pushing the same cells itself,
\ and a wide instantiation has to reach both.
ENUM opt2 1
   VARIANT n2 ;VARIANT
   VARIANT s2 FIELD value a ;VARIANT
;ENUM

\ A FAMILY WHOSE WIDE VARIANT STILL CARRIES A PAYLOAD, which `opt2` and `option`
\ between them cannot reach. In both of those the variant that needs cells added
\ is the EMPTY one, so a construction that needed pads never had a payload under
\ them and a lowering could put the added cells anywhere below the tag and still
\ be right. Here `g1` carries one declared payload term and `g2` carries two, so
\ at `grow<pt>` the instantiation reserves four payload slots where the
\ declaration reserved two: `g1` arrives with a two-cell payload on the stack AND
\ needs a cell added between that payload and its declared pad. It is the shape
\ that says WHERE the added cells go.
SUMTYPE grow 1
   VARIANT g1 a ;VARIANT
   VARIANT g2 a a ;VARIANT
;SUMTYPE

\ AND A FAMILY OF TWO PARAMETERS WHOSE ARMS ARE INSTANTIATED TO DIFFERENT WIDTHS,
\ which is the shape production met first. lib/process.f PROC-CAPTURE>RESULT
\ returns `result<pcap:captured,pcap:failed>` - a two-cell ok and a three-cell
\ err - so its `ok` arm needs a cell added and its `err` arm needs none, and the
\ chain refused it (-8503, measured, and the census row is gone). `pair<pt,pt3>`
\ is that shape with this file's own types.
SUMTYPE pair 2
   VARIANT lo a ;VARIANT
   VARIANT hi b ;VARIANT
;SUMTYPE

\ AND A FAMILY WHOSE WIDEST DECLARED VARIANT IS NOT ITS WIDEST INSTANTIATED ONE.
\ At `narrow<pt3>` the parametric variant needs THREE payload cells and the
\ two-cell one still needs two, so the instantiation reserves three slots where
\ the declaration reserved two - and `p1`, which declares one pad, instantiates
\ none. A lowering that can only ADD cells cannot correct that, so the checker
\ refuses the construction outright rather than certifying a width no emitter can
\ build (src/core/type-family.f TFC-XPAD-NARROW-REJECT). It is the negative of
\ every case below: the same shape, the other sign.
SUMTYPE narrow 1
   VARIANT w2 n n ;VARIANT
   VARIANT p1 a ;VARIANT
;SUMTYPE

\ One variant lets the growth cases put many independent dispatch facts before
\ a widened construction, so losing a suffix cannot hide behind one arm count.
ENUM sol ov ;ENUM

\ Four arms, which is the shape the four-armed selector cost was measured on.
ENUM quad
   q0 q1 q2 q3
;ENUM

\ Seven arms, and sixteen, the selector's former arm ceiling.
ENUM step
   p0 p1 p2 p3 p4 p5 p6
;ENUM

ENUM wide
   w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15
;ENUM

\ Seventeen arms exercise growth beyond that former ceiling.
ENUM over
   v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16
;ENUM

private

\ ---- the programs the production compiler compiles ----------------------------
: E-HUE ( hue -- n )
   MATCH hue
      red OF 10 ENDOF
      green OF 20 ENDOF
      blue OF 30 ENDOF
   ;MATCH ;

: E-BOX ( box -- n )
   MATCH box
      nil OF 0 ENDOF
      one OF ENDOF
      two OF + ENDOF
   ;MATCH ;

: E-UNW ( n option<n> -- n )
   MATCH option
      none OF ENDOF
      some OF nip ENDOF
   ;MATCH ;

: E-SWAPPED ( box -- n )
   MATCH box
      nil OF 0 ENDOF
      one OF ENDOF
      two OF swap - ENDOF
   ;MATCH ;

: E-HOLD ( n holder -- n )
   MATCH holder
      empty OF ENDOF
      full OF NMX-PT:UNMAKE + + ENDOF
   ;MATCH ;

\ The same payload, discarded instead of taken apart. It is two cells and ONE
\ value, so `drop` has to take both - which is a row-wise rename inside an arm
\ (dot habu-rename-rows-row-143c0331). The chain refused this body while a
\ rename counted in cells; now it moves the value whole and this executes the
\ resulting arm.
: E-DROPPED ( n holder -- n )
   MATCH holder
      empty OF ENDOF
      full OF drop ENDOF
   ;MATCH ;

: E-QUAD ( quad -- n )
   MATCH quad
      q0 OF 1 ENDOF
      q1 OF 2 ENDOF
      q2 OF 3 ENDOF
      q3 OF 4 ENDOF
   ;MATCH ;

: E-STEP ( step -- n )
   MATCH step
      p0 OF 100 ENDOF   p1 OF 101 ENDOF   p2 OF 102 ENDOF
      p3 OF 103 ENDOF   p4 OF 104 ENDOF   p5 OF 105 ENDOF
      p6 OF 106 ENDOF
   ;MATCH ;

: E-WIDE ( wide -- n )
   MATCH wide
      w0 OF 200 ENDOF   w1 OF 201 ENDOF   w2 OF 202 ENDOF
      w3 OF 203 ENDOF   w4 OF 204 ENDOF   w5 OF 205 ENDOF
      w6 OF 206 ENDOF   w7 OF 207 ENDOF   w8 OF 208 ENDOF
      w9 OF 209 ENDOF   w10 OF 210 ENDOF  w11 OF 211 ENDOF
      w12 OF 212 ENDOF  w13 OF 213 ENDOF  w14 OF 214 ENDOF
      w15 OF 215 ENDOF
   ;MATCH ;

: E-CASE ( n -- n )
   case
      1 of 10 endof
      2 of 20 endof
      99 swap
   endcase ;

: E-MK ( n -- box )
   construct box one ;

: E-MK2 ( n n -- box )
   construct box two ;

: E-MK0 ( -- box )
   construct box nil ;

\ A dead arm: the path ends at the `throw`, so it hands the join nothing and the
\ dispatch's other arms state the whole of what the join takes.
: E-DEAD ( hue -- n )
   MATCH hue
      red OF 1 ENDOF
      green OF E-A-EMPTY throw ENDOF
      blue OF 3 ENDOF
   ;MATCH ;

\ ---- a parametric family instantiated WIDER than it declares -----------------
\ `option<a>` reserves ONE payload slot and `option<pt>` needs two, so a value of
\ it is three cells where the family's declaration says two. Nothing in the
\ registry can say so - the width is a function of a resolved type term - and the
\ chain used to refuse this body by name for exactly that reason. It compiles now
\ because the checker files the instantiated bundle width and each arm's
\ instantiated pad count under the tokens that publish them.
\
\ THE TWO PAYLOAD CELLS CARRY DISTINCT ODD WEIGHTS, and that is the whole point
\ of the arithmetic: a payload combined with a commutative operator answers the
\ same number whichever cell came back where, so it would prove only that the
\ right NUMBER of cells survived. Exchanging the two weights changes the answer.
\ The value the two bodies below dispatch over. A bare `OPTION:NONE` is
\ `option<a>` and grounds to nothing, so the instantiation has to be stated
\ somewhere; a maker whose declared output names it is where every other caller
\ in the tree states it.
: E-MKI ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

: E-INST ( option<pt> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ The same one cell wider, so the arm's drop count and its keep count are two
\ different numbers: `option<pt3>` is four cells, its `some` arm drops the tag
\ alone and keeps three, and its `none` arm drops all four.
\ The cheapest scrutinee this file can spell, for the ceiling case alone. Its
\ name is two characters for the reason the family's is: every byte of it is
\ spent thirteen times in the source that has to overflow the checker's table
\ while staying inside the recorder's text cap.
: MS ( -- sol ) construct sol ov ;

: E-MKI3 ( n -- option<pt3> )
   dup 0 > if  dup 3 *  over 5 *  rot 7 *  NMX-PT3:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

: E-INST3 ( option<pt3> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ TWO DISPATCHES OF DIFFERENT INSTANTIATED WIDTHS IN ONE BODY. The widths are
\ filed under the tokens that publish them, so this body is what tells a reader
\ keyed on the TOKEN from one keyed on the family, on the definition, or on the
\ order the forms appear in: both forms name `option`, and one pops four cells
\ where the other pops three. A store that answered per family would give one of
\ them the other's width and drop the wrong cells with every count agreeing.
: E-TWOW ( n -- n )
   dup E-MKI3 MATCH option
      none OF 0 ENDOF
      some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH
   swap E-MKI MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 13 * + ENDOF
   ;MATCH
   + ;

\ AND A STRING LITERAL IN FRONT OF ONE, whose body is dispatch grammar. A
\ literal is ONE token and the reader reports it through its own event, so a
\ report path that forgot to step the ordinal would file this form's width under
\ the token before it - and the arms would be compiled against whatever that
\ token published, which here is nothing at all.
: E-STRINST ( n -- n )
   s" MATCH option some OF ;MATCH" 2drop
   E-MKI MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ ---- three payload cells, and what an arm may do with them -------------------
\ Arms of three different widths joining, each cell weighted so an exchange
\ shows in the answer.
: E-TRIO ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF 3 * ENDOF
      t3 OF 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ A payload that crosses a nested `if` INSIDE its arm: the cells are live at the
\ branch, at the join, and after it, so an arm that handed the inner structure
\ the wrong number of them would disagree on one side only.
: E-ARMIF ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF dup 0 > if 3 * else 5 * then ENDOF
      t3 OF
         over 0 > if  7 *  else  11 *  then
         swap 13 * +  swap 17 * + ENDOF
   ;MATCH ;

\ And one that crosses a counted loop inside its arm. The trip count is a
\ constant, so the loop terminates whatever the payload is; the payload's second
\ cell is read on every turn, which is what makes it travel the back edge.
: E-ARMLOOP ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF 3 * ENDOF
      t3 OF  3 0 ?do  over i *  +  loop  nip  swap 5 * + ENDOF
   ;MATCH ;

\ Three cells that are ONE value, which is `holder` one cell wider: the arm
\ keeps a bundle rather than three independent cells, and the glue it puts back
\ is what stops a rename from taking a `pt3` apart.
: E-HOLD3 ( hold3 -- n )
   MATCH hold3
      empty3 OF 0 ENDOF
      full3 OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ ---- BUILDING a value of a wide instantiation ---------------------------------
\ THE OTHER HALF OF THE STORY THE CASES ABOVE TELL. Everything above takes a value
\ of a wide instantiation APART; these put one together, which is the operation a
\ dispatch has nothing to dispatch over without. A construction is lowered by
\ pushing what the family DECLARES - the generated constructor is one routine
\ compiled once, and `construct` reads the same declared pads out of the registry
\ - so at `option<pt>`, where the instantiation reserves two payload slots and the
\ declaration reserves one, both spellings are one zero cell short. The checker
\ files that difference under the construction's own token and the chain adds the
\ cells at the site, exactly where native lowering adds them.
\
\ THE TWO ARMS OF EVERY MAKER BELOW NEED DIFFERENT NUMBERS, which is what makes
\ them a pair rather than one case written twice. `some` at `option<pt>` carries a
\ two-cell payload into a three-cell bundle and needs NOTHING added; `none`
\ carries nothing and needs one cell. A store that answered per FAMILY, or per
\ WORD, would hand one arm the other's number - and the `some` arm would then
\ carry a cell too many while the `none` arm stayed short, with the two errors
\ cancelling in any count that only added the arms up.
: E-MKC ( n -- opt2<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE construct opt2 s2  else  drop construct opt2 n2  then ;

: E-RDC ( opt2<pt> -- n )
   MATCH opt2
      n2 OF 0 ENDOF
      s2 OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ lib/object.f NEXT-LINE'S SHAPE, REDUCED TO WHAT BROKE. That word dispatches over
\ one `option` and builds ANOTHER at a wider instantiation in both arms, and its
\ `none` arm is the row this case exists for: it constructed `option<obj:line>`
\ one cell short and the chain refused the join between the two arms (-8503,
\ measured). Here the scrutinee is `option<pt>` and what both arms build is
\ `option<pt3>`, so the constructed instantiation is wider than the one taken
\ apart and the two arms of the construction need two different numbers - the
\ `some` arm's payload fills the bundle and needs nothing, the `none` arm needs
\ two cells.
: E-RELAY ( option<pt> -- option<pt3> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF NMX-PT:UNMAKE over 3 * NMX-PT3:MAKE OPTION:SOME ENDOF
   ;MATCH ;

\ A WIDE CONSTRUCTION THE LOOP CARRIES ROUND ITS BACK EDGE. The value the body
\ leaves is the value the next turn takes apart, so its pads travel the edge and
\ the join at the top of the loop is where a bundle one cell short would meet one
\ of the right width. The trip count is a constant, so the loop terminates
\ whatever the payload is, and the payload still changes on every turn.
: E-LOOPC ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then
   3 0 ?do
      E-INST i +
      dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then
   loop ;

\ A STRING LITERAL IN FRONT OF A CONSTRUCTION, for the reason E-STRINST puts one
\ in front of a dispatch: a literal is ONE token reported through its own event,
\ so a report path that did not step the ordinal would file this construction's
\ cells under the token before it - which here publishes nothing, and the
\ construction would go back to being one cell short.
: E-STRCON ( n -- option<pt> )
   s" OPTION:NONE construct opt2 n2" 2drop
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

\ TWO CONSTRUCTIONS OF DIFFERENT INSTANTIATED WIDTHS IN ONE BODY, and they are of
\ two FAMILIES because one family cannot be instantiated twice in one declared
\ output - the checker resolves a construction's arguments from the first bundle
\ of that family it finds there, so two `option`s would both be built at the first
\ one's width and it refuses the body (measured). `option<pt>` adds one cell and
\ `opt2<pt3>` adds two, so a reader keyed on the definition, or on the order the
\ constructions appear in, gives one of them the other's number.
: E-TWOC ( -- option<pt> opt2<pt3> )
   OPTION:NONE  construct opt2 n2 ;

: E-RDC3 ( opt2<pt3> -- n )
   MATCH opt2
      n2 OF 0 ENDOF
      s2 OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ A CONSTRUCTION THAT HAS A PAYLOAD AND STILL NEEDS CELLS ADDED. `g1` at
\ `grow<pt>` carries a two-cell payload and the instantiation reserves two more
\ slots than its declaration, so one cell has to go in BETWEEN that payload and
\ the pad the lowering already emits. Every other construction in this file needs
\ cells for a variant that carries nothing, where anything below the tag would
\ have done; this pair is what says the cells go above the payload.
: E-MKG ( n -- grow<pt> )
   dup 3 * swap 5 * NMX-PT:MAKE NMX-GROW:G1 ;

: E-MKGC ( n -- grow<pt> )
   dup 3 * swap 5 * NMX-PT:MAKE construct grow g1 ;

: E-RDG ( grow<pt> -- n )
   MATCH grow
      g1 OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
      g2 OF NMX-PT:UNMAKE 13 * swap 17 * + >r NMX-PT:UNMAKE 19 * swap 23 * + r> + ENDOF
   ;MATCH ;

\ THE TWO PARAMETERS OF ONE FAMILY INSTANTIATED TO DIFFERENT WIDTHS. Both arms
\ carry a payload here - there is no empty variant to hide behind - and only one
\ of them needs a cell added, so a number taken from the family rather than from
\ the arm's own token would be wrong for one of the two whichever way it went.
: E-MKP ( n -- pair<pt,pt3> )
   dup 0 > if  dup 3 * swap 5 * NMX-PT:MAKE NMX-PAIR:LO
          else  dup 3 * over 5 * rot 7 * NMX-PT3:MAKE NMX-PAIR:HI  then ;

: E-RDP ( pair<pt,pt3> -- n )
   MATCH pair
      lo OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
      hi OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ The two answers are combined through the RETURN stack because the value left
\ under the one just read is a BUNDLE, and a rename over a bundle is refused -
\ which is the rule this whole file is about, met from the other side.
: E-RDTWO ( option<pt> opt2<pt3> -- n )
   E-RDC3 >r E-INST r> 3 * + ;

\ The family's own name inside a comment, and inside a string literal. Neither is
\ a token of the dispatch grammar; both bodies must compile and answer.
: E-CMT ( hue -- n )
   MATCH hue
      red OF 1 ENDOF
      green OF ( hue blue OF ENDOF ;MATCH ) 2 ENDOF
      blue OF 3 ENDOF
   ;MATCH ;

: E-STR ( hue -- n )
   MATCH hue
      red OF s" MATCH hue blue OF ENDOF ;MATCH" 2drop 1 ENDOF
      green OF 2 ENDOF
      blue OF 3 ENDOF
   ;MATCH ;


\ ---- production-source boundary cases ----------------------------------------

: EV-RC ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

: TRY ( ptr u8 n -- n )
   NELAB:REFUSED-RESET
   EV-RC ;

: OVER$ ( -- ptr u8 n )
   s" : C-OVER ( over -- n ) MATCH over v0 OF 300 ENDOF v1 OF 301 ENDOF v2 OF 302 ENDOF v3 OF 303 ENDOF v4 OF 304 ENDOF v5 OF 305 ENDOF v6 OF 306 ENDOF v7 OF 307 ENDOF v8 OF 308 ENDOF v9 OF 309 ENDOF v10 OF 310 ENDOF v11 OF 311 ENDOF v12 OF 312 ENDOF v13 OF 313 ENDOF v14 OF 314 ENDOF v15 OF 315 ENDOF v16 OF 316 ENDOF ;MATCH ;" ;

: NONEXH$ ( -- ptr u8 n )
   s" : C-NONEXH ( hue -- n ) MATCH hue red OF 1 ENDOF green OF 2 ENDOF ;MATCH ;" ;

: DUPVAR$ ( -- ptr u8 n )
   s" : C-DUPVAR ( hue -- n ) MATCH hue red OF 1 ENDOF red OF 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: NOFAM$ ( -- ptr u8 n )
   s" : C-NOFAM ( hue -- n ) MATCH nosuchfamily red OF 1 ENDOF ;MATCH ;" ;

: NOTSUM$ ( -- ptr u8 n )
   s" : C-NOTSUM ( hue -- n ) MATCH n red OF 1 ENDOF ;MATCH ;" ;

: NOOF$ ( -- ptr u8 n )
   s" : C-NOOF ( hue -- n ) MATCH hue red 1 ENDOF green OF 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: STRAY$ ( -- ptr u8 n )
   s" : C-STRAY ( n -- n ) 1 + ;match ;" ;

: ROWS24$ ( -- ptr u8 n )
   s" : C-ROWS24 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + ;" ;

: ROWS26$ ( -- ptr u8 n )
   s" : C-ROWS26 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + MS MATCH sol ov OF 25 ENDOF ;MATCH + ;" ;

: CONFIT$ ( -- ptr u8 n )
   s" : C-CONFIT ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + drop OPTION:NONE ;" ;

: CONOVER$ ( -- ptr u8 n )
   s" : C-CONOVER ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + drop OPTION:NONE ;" ;

: NARROWC$ ( -- ptr u8 n )
   s" : C-NARROWC ( n n n -- narrow<pt3> ) NMX-PT3:MAKE construct narrow p1 ;" ;

: NARROWK$ ( -- ptr u8 n )
   s" : C-NARROWK ( n n n -- narrow<pt3> ) NMX-PT3:MAKE NMX-NARROW:P1 ;" ;

variable RC-NONEXH  variable ROW-NONEXH
variable RC-DUPVAR  variable ROW-DUPVAR
variable RC-NOFAM   variable ROW-NOFAM
variable RC-NOTSUM
variable RC-NOOF    variable ROW-NOOF
variable RC-STRAY   variable ROW-STRAY
variable RC-OVER
variable RC-ROWS24  variable RC-ROWS26
variable RC-CONFIT  variable RC-CONOVER
variable RC-NARROWC variable ROW-NARROWC
variable RC-NARROWK variable ROW-NARROWK

: RUN-DYNAMIC-CASES ( -- )
   NONEXH$ TRY RC-NONEXH !  NELAB:REFUSED-ROW ROW-NONEXH !
   DUPVAR$ TRY RC-DUPVAR !  NELAB:REFUSED-ROW ROW-DUPVAR !
   NOFAM$ TRY RC-NOFAM !    NELAB:REFUSED-ROW ROW-NOFAM !
   NOTSUM$ TRY RC-NOTSUM !
   NOOF$ TRY RC-NOOF !      NELAB:REFUSED-ROW ROW-NOOF !
   STRAY$ TRY RC-STRAY !    NELAB:REFUSED-ROW ROW-STRAY !
   OVER$ TRY RC-OVER !
   ROWS24$ TRY RC-ROWS24 !
   ROWS26$ TRY RC-ROWS26 !
   CONFIT$ TRY RC-CONFIT !
   CONOVER$ TRY RC-CONOVER !
   NARROWC$ TRY RC-NARROWC ! NELAB:REFUSED-ROW ROW-NARROWC !
   NARROWK$ TRY RC-NARROWK ! NELAB:REFUSED-ROW ROW-NARROWK ! ;

RUN-DYNAMIC-CASES

\ ---- emitted dispatch shape --------------------------------------------------

\ Source compilation retires its temporary emission. Inspect the code committed
\ to the dictionary; its recorded length omits a trailing return instruction.
variable CODE-BASE
variable CODE-LEN

: CODE! ( ptr u8 n -- )
   get-current XREF-FIND-WL {: rec:ptr :}
   rec XREF-FOUND? 0= if s" native-match: missing published code" 76 die then
   rec XREF-START CODE-BASE !
   rec XREF-LEN CODE-LEN ! ;

TRUSTED: CODE-WORD@ ( n -- n )
   INSN-BYTES * CODE-BASE @ + @ $FFFFFFFF and ;

: CODE-INSNS ( -- n ) CODE-LEN @ INSN-BYTES / ;

\ The engine's stack guards the committed code carries
\ (src/compiler/native/codewalk.f), each eleven instructions of the engine's.
: CODE-GUARDS ( -- n )
   CODE-INSNS [: CODE-WORD@ ;] NWALK:GUARDS ;

: GUARD-BYTES ( n -- n ) {: guards:n :}
   guards NWALK:GUARD-INSNS * INSN-BYTES * ;

: CODE-BRANCHES? ( -- bool )
   CODE-INSNS 0 ?do
      i CODE-WORD@ NBR:COND? if true unloop exit then
   loop false ;

: TRAP-BR? ( n n -- bool ) {: k:n t:n :}
   k CODE-WORD@ NBR:BL? 0= if false exit then
   CODE-BASE @ k INSN-BYTES * + k CODE-WORD@ NBR:BL-TARGET t = ;

: TRAP-BRANCHES ( -- n )
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: t:n :}
   0
   CODE-INSNS 0 ?do
      i t TRAP-BR? if 1+ then
   loop ;

variable EMIT-RC
variable EMIT-BRANCH
variable EMIT-RET
variable EMIT-TRAPS
variable UNW-SIZE
variable QUAD-SIZE
variable UNW-GUARDS
variable QUAD-GUARDS

: PROBE-HUE$ ( -- ptr u8 n )
   s" : PROBE-HUE ( hue -- n ) MATCH hue red OF 10 ENDOF green OF 20 ENDOF blue OF 30 ENDOF ;MATCH ;" ;

: PROBE-UNW$ ( -- ptr u8 n )
   s" : PROBE-UNW ( n option<n> -- n ) MATCH option none OF ENDOF some OF nip ENDOF ;MATCH ;" ;

: PROBE-QUAD$ ( -- ptr u8 n )
   s" : PROBE-QUAD ( quad -- n ) MATCH quad q0 OF 1 ENDOF q1 OF 2 ENDOF q2 OF 3 ENDOF q3 OF 4 ENDOF ;MATCH ;" ;

: CAPTURE-EMISSION ( -- )
   PROBE-HUE$ TRY EMIT-RC !
   s" PROBE-HUE" CODE!
   CODE-BRANCHES? if 1 else 0 then EMIT-BRANCH !
   CODE-INSNS CODE-WORD@ NBR:RET? if 1 else 0 then EMIT-RET !
   TRAP-BRANCHES EMIT-TRAPS !
   PROBE-UNW$ TRY drop s" PROBE-UNW" CODE! CODE-LEN @ UNW-SIZE ! CODE-GUARDS UNW-GUARDS !
   PROBE-QUAD$ TRY drop s" PROBE-QUAD" CODE! CODE-LEN @ QUAD-SIZE ! CODE-GUARDS QUAD-GUARDS ! ;

\ These instruction assertions describe optimizing emission. Other cases keep
\ the caller's tier, including when the suite is loaded directly from the REPL.
TRUSTED: CAPTURE-NATIVE-EMISSION ( -- )
   tier@ {: prior:n :}
   1 set-tier
   ['] CAPTURE-EMISSION catch {: rc:n :}
   prior set-tier
   rc 0<> if rc throw then ;

CAPTURE-NATIVE-EMISSION

\ ---- behavior ---------------------------------------------------------------

: BASIC-CASE ( -- )
   s" empty and payload arms preserve their declared values" T-LABEL
   NMX-HUE:RED E-HUE 10 T=
   NMX-HUE:GREEN E-HUE 20 T=
   NMX-HUE:BLUE E-HUE 30 T=
   NMX-BOX:NIL E-BOX 0 T=
   7 NMX-BOX:ONE E-BOX 7 T=
   3 4 NMX-BOX:TWO E-BOX 7 T=
   9 OPTION:NONE E-UNW 9 T=
   9 42 OPTION:SOME E-UNW 42 T=

   s" four, seven, and sixteen-arm dispatches select every arm" T-LABEL
   NMX-QUAD:Q0 E-QUAD 1 T=
   NMX-QUAD:Q1 E-QUAD 2 T=
   NMX-QUAD:Q2 E-QUAD 3 T=
   NMX-QUAD:Q3 E-QUAD 4 T=
   NMX-STEP:P0 E-STEP 100 T=
   NMX-STEP:P1 E-STEP 101 T=
   NMX-STEP:P2 E-STEP 102 T=
   NMX-STEP:P3 E-STEP 103 T=
   NMX-STEP:P4 E-STEP 104 T=
   NMX-STEP:P5 E-STEP 105 T=
   NMX-STEP:P6 E-STEP 106 T=
   NMX-WIDE:W0 E-WIDE 200 T=
   NMX-WIDE:W1 E-WIDE 201 T=
   NMX-WIDE:W2 E-WIDE 202 T=
   NMX-WIDE:W3 E-WIDE 203 T=
   NMX-WIDE:W4 E-WIDE 204 T=
   NMX-WIDE:W5 E-WIDE 205 T=
   NMX-WIDE:W6 E-WIDE 206 T=
   NMX-WIDE:W7 E-WIDE 207 T=
   NMX-WIDE:W8 E-WIDE 208 T=
   NMX-WIDE:W9 E-WIDE 209 T=
   NMX-WIDE:W10 E-WIDE 210 T=
   NMX-WIDE:W11 E-WIDE 211 T=
   NMX-WIDE:W12 E-WIDE 212 T=
   NMX-WIDE:W13 E-WIDE 213 T=
   NMX-WIDE:W14 E-WIDE 214 T=
   NMX-WIDE:W15 E-WIDE 215 T=

   s" case keeps both arms and its default" T-LABEL
   1 E-CASE 10 T=
   2 E-CASE 20 T=
   5 E-CASE 99 T= ;

: CONSTRUCTOR-CASE ( -- )
   s" construct produces every payload width" T-LABEL
   55 E-MK E-BOX 55 T=
   3 4 E-MK2 E-BOX 7 T=
   E-MK0 E-BOX 0 T= ;

: DEAD-CASE ( -- )
   s" live arms around a dead arm return normally" T-LABEL
   NMX-HUE:RED E-DEAD 1 T=
   NMX-HUE:BLUE E-DEAD 3 T=
   [: NMX-HUE:GREEN E-DEAD drop ;] E-A-EMPTY TTHROWSQ ;

: HIDDEN-CASE ( -- )
   s" family spellings in comments and strings are not dispatch tokens" T-LABEL
   NMX-HUE:RED E-CMT 1 T=
   NMX-HUE:GREEN E-CMT 2 T=
   NMX-HUE:BLUE E-CMT 3 T=
   NMX-HUE:RED E-STR 1 T=
   NMX-HUE:GREEN E-STR 2 T=
   NMX-HUE:BLUE E-STR 3 T= ;

: PAYLOAD-CASE ( -- )
   s" two independent payload cells and one two-cell value keep distinct glue" T-LABEL
   3 4 NMX-BOX:TWO E-SWAPPED 1 T=
   9 NMX-HOLDER:EMPTY E-HOLD 9 T=
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL E-HOLD 16 T=
   9 NMX-HOLDER:EMPTY E-DROPPED 9 T=
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL E-DROPPED 9 T=

   s" instantiated payload widths keep cell order" T-LABEL
   0 E-MKI E-INST 0 T=
   3 E-MKI E-INST 204 T=
   5 E-MKI E-INST 340 T=
   0 E-MKI3 E-INST3 0 T=
   3 E-MKI3 E-INST3 423 T= ;

: ORDINAL-CASE ( -- )
   s" separate dispatch tokens and a preceding string keep their own width rows" T-LABEL
   0 E-TWOW 0 T=
   3 E-TWOW 645 T=
   0 E-STRINST 0 T=
   3 E-STRINST 204 T= ;

: TRIPLE-CASE ( -- )
   s" three-cell arms keep count and order across joins" T-LABEL
   NMX-TRIO:T0 E-TRIO 0 T=
   7 NMX-TRIO:T1 E-TRIO 21 T=
   3 5 9 NMX-TRIO:T3 E-TRIO 151 T=
   9 5 3 NMX-TRIO:T3 E-TRIO 223 T=

   s" three cells survive branch and loop control inside an arm" T-LABEL
   7 NMX-TRIO:T1 E-ARMIF 21 T=
   -7 NMX-TRIO:T1 E-ARMIF -35 T=
   3 5 9 NMX-TRIO:T3 E-ARMIF 179 T=
   3 -5 9 NMX-TRIO:T3 E-ARMIF 85 T=
   3 0 9 NMX-TRIO:T3 E-ARMIF 150 T=
   3 5 9 NMX-TRIO:T3 E-ARMLOOP 39 T=
   NMX-HOLD3:EMPTY3 E-HOLD3 0 T=
   3 5 9 NMX-PT3:MAKE NMX-HOLD3:FULL3 E-HOLD3 151 T= ;

: BUILD-CASE ( -- )
   s" wide constructors preserve empty and payload arms" T-LABEL
   0 E-MKI E-INST 0 T=
   3 E-MKI E-INST 204 T=
   0 E-MKI3 E-INST3 0 T=
   3 E-MKI3 E-INST3 423 T=
   0 E-MKC E-RDC 0 T=
   3 E-MKC E-RDC 204 T=

   s" dispatch and loop constructions preserve the widened value" T-LABEL
   0 E-MKI E-RELAY E-INST3 0 T=
   3 E-MKI E-RELAY E-INST3 453 T=
   0 E-LOOPC E-INST 4760 T=
   1 E-LOOPC E-INST 21386136 T=
   0 E-STRCON E-INST 0 T=
   3 E-STRCON E-INST 204 T=

   s" distinct construction tokens and payload placements stay independent" T-LABEL
   E-TWOC E-RDTWO 0 T=
   3 E-MKG E-RDG 204 T=
   3 E-MKGC E-RDG 204 T=
   3 E-MKP E-RDP 204 T=
   -3 E-MKP E-RDP -423 T= ;

70 constant CHECKER-REJECT

: REFUSED-CASE ( -- )
   s" malformed and non-exhaustive dispatches stop before elaboration" T-LABEL
   RC-NONEXH @ CHECKER-REJECT T=  ROW-NONEXH @ -1 T=
   RC-DUPVAR @ CHECKER-REJECT T=  ROW-DUPVAR @ -1 T=
   RC-NOFAM @ CHECKER-REJECT T=   ROW-NOFAM @ -1 T=
   RC-NOTSUM @ CHECKER-REJECT T=
   RC-NOOF @ CHECKER-REJECT T=    ROW-NOOF @ -1 T=
   RC-STRAY @ CHECKER-REJECT T=   ROW-STRAY @ -1 T=

   s" narrower construction spellings remain checker refusals" T-LABEL
   RC-NARROWC @ CHECKER-REJECT T= ROW-NARROWC @ -1 T=
   RC-NARROWK @ CHECKER-REJECT T= ROW-NARROWK @ -1 T= ;

: GROWTH-CASE ( -- )
   s" dispatch beyond the former sixteen-arm selector ceiling remains executable" T-LABEL
   NMX-WIDE:W15 E-WIDE 215 T=
   RC-OVER @ 0 T=
   NMX-OVER:V0 C-OVER 300 T=
   NMX-OVER:V1 C-OVER 301 T=
   NMX-OVER:V2 C-OVER 302 T=
   NMX-OVER:V3 C-OVER 303 T=
   NMX-OVER:V4 C-OVER 304 T=
   NMX-OVER:V5 C-OVER 305 T=
   NMX-OVER:V6 C-OVER 306 T=
   NMX-OVER:V7 C-OVER 307 T=
   NMX-OVER:V8 C-OVER 308 T=
   NMX-OVER:V9 C-OVER 309 T=
   NMX-OVER:V10 C-OVER 310 T=
   NMX-OVER:V11 C-OVER 311 T=
   NMX-OVER:V12 C-OVER 312 T=
   NMX-OVER:V13 C-OVER 313 T=
   NMX-OVER:V14 C-OVER 314 T=
   NMX-OVER:V15 C-OVER 315 T=
   NMX-OVER:V16 C-OVER 316 T=

   s" dispatch facts grow past twenty-four rows" T-LABEL
   RC-ROWS24 @ 0 T=
   C-ROWS24 144 T=
   RC-ROWS26 @ 0 T=
   C-ROWS26 169 T=

   s" widened constructions remain recorded after many dispatches" T-LABEL
   RC-CONFIT @ 0 T=
   C-CONFIT E-INST 0 T=
   RC-CONOVER @ 0 T=
   C-CONOVER E-INST 0 T= ;

: EMISSION-CASE ( -- )
   s" a compiled dispatch branches, returns, and uses one shared trap edge" T-LABEL
   EMIT-RC @ 0 T=
   EMIT-BRANCH @ 1 T=
   EMIT-RET @ 1 T=
   EMIT-TRAPS @ 1 T=

   \ The bounds are on the dispatch's own code; the guards it carries are
\ the engine's, and their bytes are added back explicitly.
   s" dispatch code stays bounded and grows with its arm count" T-LABEL
   UNW-SIZE @  UNW-GUARDS @ GUARD-BYTES 128 +  < TTRUE
   QUAD-SIZE @  QUAD-GUARDS @ GUARD-BYTES 184 +  < TTRUE
   QUAD-SIZE @ UNW-SIZE @ > TTRUE ;

\ ---- hostile tag ------------------------------------------------------------

$4000 constant CAP-CAP
30000 constant CHILD-MS

create OUT-BUF CAP-CAP allot
create ERR-BUF CAP-CAP allot

variable CHILD-OUT-N
variable CHILD-ERR-N
variable CHILD-RC

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/compiler/native-match-forge.f" >LEN PROC-ARGV+ ;

: CHILD-RUN ( -- )
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

: CHILD-ERR$ ( -- ptr u8 n )
   ERR-BUF CHILD-ERR-N @ ;

: FORGE-CASE ( -- )
   CHILD-RUN
   s" a hostile tag reaches the shared trap and exits" T-LABEL
   CHILD-RC @ ENGINE-ERROR:BAD-TAG T=
   CHILD-ERR$ s" hb: bad hue tag" CONTAINS? TTRUE
   CHILD-ERR$ s" hb: bad box tag" CONTAINS? TFALSE ;

public

: MAIN ( -- )
   T-RESET
   BASIC-CASE
   CONSTRUCTOR-CASE
   DEAD-CASE
   HIDDEN-CASE
   PAYLOAD-CASE
   ORDINAL-CASE
   TRIPLE-CASE
   BUILD-CASE
   REFUSED-CASE
   GROWTH-CASE
   EMISSION-CASE
   FORGE-CASE
   NMX-LAYOUT:TEST
   T-REPORT
   s" native-match: ok" type cr ;

;package

NMX:MAIN
