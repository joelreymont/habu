\ native-match.f - `MATCH`, `case` and `construct` through the whole chain, from
\ source text to executed machine code.
\
\     bin/hb --load test/compiler/native-match.f
\     bin/hb --load test/compiler/native-match.f -- forge   (the trap's own half)
\
\ WHAT IS UNDER TEST. A value of a sum family is W flat cells with its tag on
\ top, so a dispatch over it is a chain of ordinary comparisons and the chain
\ already had every operation one needs. src/compiler/native/elaborate.f reads
\ the family and variant tokens with a pre-pass that mirrors the token machine
\ the engine and the checker already run, and builds each arm as a test block, a
\ mismatch edge and a body; the last arm's mismatch edge ends in the trap
\ terminator, and `construct` is the two constant pushes that turn a payload into
\ a value of its family.
\
\ NOTHING HERE IS A MODEL OF THE CHAIN. Every case states one body TWICE - once
\ for the ENGINE to compile as an ordinary definition, and once as source handed
\ to NMIGRATE:DEFINE, which compiles it through every stage and publishes the
\ chain's code under a second name - and then executes both on the same inputs
\ and compares what they answer. An expected-value table would have been written
\ from whichever of the two its author trusted; comparing the two makes the
\ ENGINE the authority and needs nobody to be right about anything. That is
\ test/compiler/native-rename-rows.f's discipline and this file keeps it.
\
\ WHY THE ADVERSARIAL CASES ASSERT WHOSE REFUSAL IT IS. A non-exhaustive
\ dispatch, a duplicate variant, a family that is not a family, a variant token
\ with no `of` after it and a stray `;match` are all refused by the CHECKER,
\ before the chain is handed anything - so what those cases prove is that the
\ chain did not quietly accept a body its own front end never saw. Each asserts
\ the engine's reject status AND that the elaborator recorded no refusal of its
\ own: a case that only asserted "it did not compile" would pass if the chain
\ refused every one of them for a reason of its own.
\
\ AND WHY TWO OF THEM MUST COMPILE. A pre-pass that read TEXT rather than tokens
\ would find a family name inside a comment and inside a string literal. The tape
\ holds what the checker's reader CONSUMED - a comment is not a token at all and
\ a string literal is one token whose kind says so - so both bodies have to
\ compile and answer what the engine answers, which is what those two cases are.
\
\ THE ARM CEILING IS MEASURED, NOT ASSUMED. Sixteen arms is the largest dispatch
\ the chain compiles today and seventeen is refused - by the SELECTOR's own
\ block-queue capacity, which is sized from the block ceiling every native pass
\ shares (src/compiler/native/frozen.f BMAX). The case names that, so a change
\ that raises the ceiling has a row to move rather than a surprise.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/adt/option.f
require src/compiler/native/migrate.f
require src/compiler/native/publish.f
require src/compiler/native/branch.f
require src/compiler/native/dict.f
require src/compiler/native/inline.f
require src/compiler/native/trap.f

package NMX
private

8 constant REGS
18 constant WIDE-REGS                \ the pool the one row below needs, measured
4 constant INSN-BYTES

\ `evaluate` is the metaprogramming boundary the checker does not model, and the
\ forge below is the only way to compile a caller for a published word from
\ inside a test.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

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

\ ONE VARIANT AND THE SHORTEST SPELLINGS IN THIS FILE, and both are forced by
\ the ceiling case at the foot of the file. That case has to put MORE dispatch
\ rows in one definition than the checker records, while staying inside the
\ recorder's own 512-byte text cap - and the only budget it can buy the extra
\ rows with is the spelling. One variant is two rows per form at seven tokens,
\ which is the cheapest dispatch there is.
ENUM sol ov ;ENUM

\ Four arms, which is the shape the engine's own four-armed cost was measured on.
ENUM quad
   q0 q1 q2 q3
;ENUM

\ Seven arms, and sixteen, which is the largest the chain compiles today.
ENUM step
   p0 p1 p2 p3 p4 p5 p6
;ENUM

ENUM wide
   w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15
;ENUM

\ And seventeen, which is one past it.
ENUM over
   v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16
;ENUM

private

\ ---- the bodies the engine compiles -------------------------------------------
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
\ cells at the site, exactly where the engine adds them.
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

\ ---- the source the chain is given --------------------------------------------
\ Character for character the body above it, so a difference between the two
\ columns can only come from the two compilers and never from two programs.
: HUE$ ( -- ptr u8 n )
   s" : C-HUE ( hue -- n ) MATCH hue red OF 10 ENDOF green OF 20 ENDOF blue OF 30 ENDOF ;MATCH ;" ;

: BOX$ ( -- ptr u8 n )
   s" : C-BOX ( box -- n ) MATCH box nil OF 0 ENDOF one OF ENDOF two OF + ENDOF ;MATCH ;" ;

: UNW$ ( -- ptr u8 n )
   s" : C-UNW ( n option<n> -- n ) MATCH option none OF ENDOF some OF nip ENDOF ;MATCH ;" ;

: SWAPPED$ ( -- ptr u8 n )
   s" : C-SWAPPED ( box -- n ) MATCH box nil OF 0 ENDOF one OF ENDOF two OF swap - ENDOF ;MATCH ;" ;

: HOLD$ ( -- ptr u8 n )
   s" : C-HOLD ( n holder -- n ) MATCH holder empty OF ENDOF full OF NMX-PT:UNMAKE + + ENDOF ;MATCH ;" ;

: QUAD$ ( -- ptr u8 n )
   s" : C-QUAD ( quad -- n ) MATCH quad q0 OF 1 ENDOF q1 OF 2 ENDOF q2 OF 3 ENDOF q3 OF 4 ENDOF ;MATCH ;" ;

: STEP$ ( -- ptr u8 n )
   s" : C-STEP ( step -- n ) MATCH step p0 OF 100 ENDOF p1 OF 101 ENDOF p2 OF 102 ENDOF p3 OF 103 ENDOF p4 OF 104 ENDOF p5 OF 105 ENDOF p6 OF 106 ENDOF ;MATCH ;" ;

: WIDE$ ( -- ptr u8 n )
   s" : C-WIDE ( wide -- n ) MATCH wide w0 OF 200 ENDOF w1 OF 201 ENDOF w2 OF 202 ENDOF w3 OF 203 ENDOF w4 OF 204 ENDOF w5 OF 205 ENDOF w6 OF 206 ENDOF w7 OF 207 ENDOF w8 OF 208 ENDOF w9 OF 209 ENDOF w10 OF 210 ENDOF w11 OF 211 ENDOF w12 OF 212 ENDOF w13 OF 213 ENDOF w14 OF 214 ENDOF w15 OF 215 ENDOF ;MATCH ;" ;

: OVER$ ( -- ptr u8 n )
   s" : C-OVER ( over -- n ) MATCH over v0 OF 300 ENDOF v1 OF 301 ENDOF v2 OF 302 ENDOF v3 OF 303 ENDOF v4 OF 304 ENDOF v5 OF 305 ENDOF v6 OF 306 ENDOF v7 OF 307 ENDOF v8 OF 308 ENDOF v9 OF 309 ENDOF v10 OF 310 ENDOF v11 OF 311 ENDOF v12 OF 312 ENDOF v13 OF 313 ENDOF v14 OF 314 ENDOF v15 OF 315 ENDOF v16 OF 316 ENDOF ;MATCH ;" ;

: CASE$ ( -- ptr u8 n )
   s" : C-CASE ( n -- n ) case 1 of 10 endof 2 of 20 endof 99 swap endcase ;" ;

: INST$ ( -- ptr u8 n )
   s" : C-INST ( option<pt> -- n ) MATCH option none OF 0 ENDOF some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF ;MATCH ;" ;

: INST3$ ( -- ptr u8 n )
   s" : C-INST3 ( option<pt3> -- n ) MATCH option none OF 0 ENDOF some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF ;MATCH ;" ;

: TWOW$ ( -- ptr u8 n )
   s" : C-TWOW ( n -- n ) dup E-MKI3 MATCH option none OF 0 ENDOF some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF ;MATCH swap E-MKI MATCH option none OF 0 ENDOF some OF NMX-PT:UNMAKE 7 * swap 13 * + ENDOF ;MATCH + ;" ;

: STRINST$ ( -- ptr u8 n )
   S\" : C-STRINST ( n -- n ) s\" MATCH option some OF ;MATCH\" 2drop E-MKI MATCH option none OF 0 ENDOF some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF ;MATCH ;" ;

: TRIO$ ( -- ptr u8 n )
   s" : C-TRIO ( trio -- n ) MATCH trio t0 OF 0 ENDOF t1 OF 3 * ENDOF t3 OF 5 * swap 11 * + swap 17 * + ENDOF ;MATCH ;" ;

: ARMIF$ ( -- ptr u8 n )
   s" : C-ARMIF ( trio -- n ) MATCH trio t0 OF 0 ENDOF t1 OF dup 0 > if 3 * else 5 * then ENDOF t3 OF over 0 > if 7 * else 11 * then swap 13 * + swap 17 * + ENDOF ;MATCH ;" ;

: ARMLOOP$ ( -- ptr u8 n )
   s" : C-ARMLOOP ( trio -- n ) MATCH trio t0 OF 0 ENDOF t1 OF 3 * ENDOF t3 OF 3 0 ?do over i * + loop nip swap 5 * + ENDOF ;MATCH ;" ;

: HOLD3$ ( -- ptr u8 n )
   s" : C-HOLD3 ( hold3 -- n ) MATCH hold3 empty3 OF 0 ENDOF full3 OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF ;MATCH ;" ;

\ ---- and the source for the constructions -------------------------------------
: MKI$ ( -- ptr u8 n )
   s" : C-MKI ( n -- option<pt> ) dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE OPTION:SOME else drop OPTION:NONE then ;" ;

: MKI3$ ( -- ptr u8 n )
   s" : C-MKI3 ( n -- option<pt3> ) dup 0 > if dup 3 * over 5 * rot 7 * NMX-PT3:MAKE OPTION:SOME else drop OPTION:NONE then ;" ;

: MKC$ ( -- ptr u8 n )
   s" : C-MKC ( n -- opt2<pt> ) dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE construct opt2 s2 else drop construct opt2 n2 then ;" ;

: RELAY$ ( -- ptr u8 n )
   s" : C-RELAY ( option<pt> -- option<pt3> ) MATCH option none OF OPTION:NONE ENDOF some OF NMX-PT:UNMAKE over 3 * NMX-PT3:MAKE OPTION:SOME ENDOF ;MATCH ;" ;

: LOOPC$ ( -- ptr u8 n )
   s" : C-LOOPC ( n -- option<pt> ) dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE OPTION:SOME else drop OPTION:NONE then 3 0 ?do E-INST i + dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE OPTION:SOME else drop OPTION:NONE then loop ;" ;

: STRCON$ ( -- ptr u8 n )
   S\" : C-STRCON ( n -- option<pt> ) s\" OPTION:NONE construct opt2 n2\" 2drop dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE OPTION:SOME else drop OPTION:NONE then ;" ;

: TWOC$ ( -- ptr u8 n )
   s" : C-TWOC ( -- option<pt> opt2<pt3> ) OPTION:NONE construct opt2 n2 ;" ;

: MKG$ ( -- ptr u8 n )
   s" : C-MKG ( n -- grow<pt> ) dup 3 * swap 5 * NMX-PT:MAKE NMX-GROW:G1 ;" ;

: MKGC$ ( -- ptr u8 n )
   s" : C-MKGC ( n -- grow<pt> ) dup 3 * swap 5 * NMX-PT:MAKE construct grow g1 ;" ;

: MKP$ ( -- ptr u8 n )
   s" : C-MKP ( n -- pair<pt,pt3> ) dup 0 > if dup 3 * swap 5 * NMX-PT:MAKE NMX-PAIR:LO else dup 3 * over 5 * rot 7 * NMX-PT3:MAKE NMX-PAIR:HI then ;" ;

: MK$ ( -- ptr u8 n )
   s" : C-MK ( n -- box ) construct box one ;" ;

: MK2$ ( -- ptr u8 n )
   s" : C-MK2 ( n n -- box ) construct box two ;" ;

: MK0$ ( -- ptr u8 n )
   s" : C-MK0 ( -- box ) construct box nil ;" ;

: DEAD$ ( -- ptr u8 n )
   s" : C-DEAD ( hue -- n ) MATCH hue red OF 1 ENDOF green OF E-A-EMPTY throw ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: CMT$ ( -- ptr u8 n )
   s" : C-CMT ( hue -- n ) MATCH hue red OF 1 ENDOF green OF ( hue blue OF ENDOF ;MATCH ) 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: STR$ ( -- ptr u8 n )
   S\" : C-STR ( hue -- n ) MATCH hue red OF s\" MATCH hue blue OF ENDOF ;MATCH\" 2drop 1 ENDOF green OF 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

\ ---- the two bodies the CHAIN refuses, and what each one binds -----------------
\ A rename over an arm's payload is decided by whether that payload's cells are
\ one VALUE, and the registry answers it by counting: a payload of two fields is
\ two cells and two values, a payload of one field whose type is a product is two
\ cells and ONE value. Both bodies below are well typed - the checker moves a
\ whole bundle for a `drop` exactly as it moves one cell - so the chain is the
\ only thing that can tell them apart, and it must: dropping one CELL of a
\ two-cell value leaves half of it on the stack with every count still agreeing.
: DROPPED$ ( -- ptr u8 n )
   s" : C-DROPPED ( n holder -- n ) MATCH holder empty OF ENDOF full OF drop ENDOF ;MATCH ;" ;

\ ---- the bodies the CHECKER refuses -------------------------------------------
\ Every one of these is rejected before the chain is handed anything. They are
\ written only as source, because a file containing them could not be compiled.
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

\ ---- the two sides of the checker's dispatch-row ceiling ---------------------
\ The checker files one row per `MATCH` family token and one per arm, and it
\ records that it overflowed rather than dropping rows in silence - a dropped
\ arm row would be indistinguishable from an arm with nothing to say, and the
\ chain would then unpack that arm against a pad count nobody proved. So one
\ overflow makes every query answer absent and the whole body is refused by
\ name.
\
\ TWELVE SINGLE-ARM DISPATCHES IS TWENTY-FOUR ROWS, which is exactly the table,
\ and thirteen is twenty-six. Both fit the recorder's 512-byte text cap (459 and
\ 496 bytes) and its 128-token tape, so the pair straddles the ceiling with
\ nothing else refusing either side: the first compiles and the second is
\ refused by the table. Fourteen would be 517 bytes and the recorder's own cap
\ would answer first, which is why the pair is where it is.
: ROWS24$ ( -- ptr u8 n )
   s" : C-ROWS24 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + ;" ;

: ROWS26$ ( -- ptr u8 n )
   s" : C-ROWS26 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + MS MATCH sol ov OF 25 ENDOF ;MATCH + ;" ;

\ ---- and the two sides of that ceiling for a CONSTRUCTION ---------------------
\ THE TWO KINDS OF READER MEET THE FULL TABLE DIFFERENTLY, and this pair is what
\ says so. A dispatch token's reader refuses the body when its row was dropped,
\ because a token it has already recognised as a dispatch operand must have a
\ number. A construction files a row only when its instantiation really adds
\ cells, so ABSENT has to mean "adds nothing" - it is the answer for every
\ construction of a family that is not parametric and for every call that is not a
\ construction at all - and a dropped construction row therefore reads as zero and
\ the site is lowered one bundle short.
\
\ THAT IS STILL NOT A WRONG PROGRAM, AND THIS PAIR IS THE PROOF. The missing cells
\ are CONSERVED: every count the elaborator makes afterwards is against the width
\ the checker instantiated, so the deficit reaches the definition's own declared
\ output and is refused there by name. Eleven single-arm dispatches leave room for
\ the construction's row and the body compiles and answers; twelve fill the table,
\ the construction's row is dropped, and the body is refused for leaving the wrong
\ number of cells. Both are inside the recorder's 512-byte text cap (451 and 489
\ bytes, measured) and its 128-token tape, so nothing else refuses either side.
: CONFIT$ ( -- ptr u8 n )
   s" : C-CONFIT ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + drop OPTION:NONE ;" ;

: CONOVER$ ( -- ptr u8 n )
   s" : C-CONOVER ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + drop OPTION:NONE ;" ;

\ ---- the construction the CHECKER must go on refusing -------------------------
\ `narrow<pt3>` reserves three payload slots where its declaration reserved two,
\ and its `p1` variant fills all three - so where the declaration left one pad the
\ instantiation leaves none, and a lowering that can only ADD cells would emit a
\ pad the certified width does not have. The checker refuses it for both
\ spellings, before the chain is handed anything. It is the one sign of the
\ difference this lane publishes that must never reach an emitter, so both rows
\ assert the CHECKER's rejection and that the elaborator recorded no refusal of
\ its own.
: NARROWC$ ( -- ptr u8 n )
   s" : C-NARROWC ( n n n -- narrow<pt3> ) NMX-PT3:MAKE construct narrow p1 ;" ;

: NARROWK$ ( -- ptr u8 n )
   s" : C-NARROWK ( n n n -- narrow<pt3> ) NMX-PT3:MAKE NMX-NARROW:P1 ;" ;

\ ---- driving one migration where its refusal can be read ----------------------
\ A checked `catch` takes a stack-neutral quotation and a quotation cannot read
\ the enclosing word's locals, so what the migration needs is parked first.
variable M-A   variable M-U   variable M-IN   variable M-OUT   variable M-REGS

: MIGRATE-RC ( -- n )
   [: M-A @ M-U @ M-IN @ M-OUT @ M-REGS @ NMIGRATE:DEFINE ;] catch ;

: STAGE-ONE ( ptr u8 n n n n -- ) {: a:ptr u:n in:n out:n regs:n :}
   a M-A !  u M-U !  in M-IN !  out M-OUT !  regs M-REGS !
   NELAB:REFUSED-RESET ;

: TRY ( ptr u8 n n n -- n ) {: a:ptr u:n in:n out:n :}
   a u in out REGS STAGE-ONE
   MIGRATE-RC ;

\ THE POOL IS THE CALLER'S NUMBER AND ONE ROW NEEDS A BIGGER ONE. Eight scratch
\ registers carry every other migration in this file, and they do not carry an
\ arm that holds THREE payload cells live across a branch: measured, the
\ allocator answers E-A64RA-SPILL there before the case can say anything about
\ the payload at all. That is a fact about the pool the caller offered and not
\ about the dispatch, so the row states its own pool rather than moving
\ everybody else's - the emission sizes the cost case pins are measured at
\ eight and must stay there.
: TRY-WIDE ( ptr u8 n n n -- n ) {: a:ptr u:n in:n out:n :}
   a u in out WIDE-REGS STAGE-ONE
   MIGRATE-RC ;

\ ---- what the chain published, read back off the emission and the seam --------
\ THE WORDLIST IS THIS PACKAGE'S OWN, taken while it is open. The migration
\ publishes by evaluating source text in whatever scope is current, so every `C-`
\ word below lives in this package's private wordlist and the publication seam's
\ log is keyed by that wordlist - asking it about wordlist zero would be asking
\ about a word nobody defined.
variable MY-WID
NDICT:OPEN-PRI MY-WID !

\ The emission is sealed until the next one is made, so these answer about the
\ migration that ran last.
: TRAP-BR? ( n n -- bool ) {: k:n t:n :}
   k A64EMIT:WORD@ NBR:B? 0= if false exit then
   A64EMIT:PLACEMENT  k INSN-BYTES * +  k A64EMIT:WORD@  NBR:B-TARGET  t = ;

: TRAP-BRANCHES ( -- n )
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: t:n :}
   0
   A64EMIT:INSNS 0 ?do
      i t TRAP-BR? if 1+ then
   loop ;

: NEW-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u MY-WID @ NPUB:NEW-LEN ;

: OLD-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u MY-WID @ NPUB:OLD-LEN ;

: NEW-START ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u MY-WID @ NPUB:NEW-START ;

\ ---- the migrations, and what each one answered -------------------------------
\ They run HERE, inside the package block, for the two reasons
\ test/compiler/native-rename-rows.f gives: the chain publishes by evaluating the
\ source text, so a twin migrated after `;package` would land outside the package
\ and a body naming a package family would not resolve at all; and the twins have
\ to exist before the comparisons below are compiled against their names.
variable RC-HUE   variable RC-BOX   variable RC-UNW   variable RC-STEP
variable RC-WIDE  variable RC-OVER  variable RC-CASE  variable RC-MK
variable RC-QUAD  variable RC-SWAPPED variable RC-HOLD
variable RC-DROPPED variable RC-INST variable RC-INST3
variable RC-TWOW  variable RC-STRINST
variable RC-TRIO  variable RC-ARMIF variable RC-ARMLOOP variable RC-HOLD3
variable RC-ROWS24 variable RC-ROWS26
variable RC-MKI   variable RC-MKI3  variable RC-MKC
variable RC-RELAY variable RC-LOOPC variable RC-STRCON variable RC-TWOC
variable RC-CONFIT variable RC-CONOVER variable RC-MKG variable RC-MKGC variable RC-MKP
variable RC-NARROWC variable RC-NARROWK
variable ROW-NARROWC variable ROW-NARROWK
variable RC-MK0   variable RC-MK2   variable RC-DEAD
variable RC-CMT   variable RC-STR
variable RC-NONEXH variable RC-DUPVAR variable RC-NOFAM variable RC-NOTSUM
variable RC-NOOF  variable RC-STRAY
variable ROW-NONEXH variable ROW-DUPVAR variable ROW-NOFAM
variable ROW-NOOF variable ROW-STRAY

\ The two facts the trap loose end needs, taken off the emission of a MATCH the
\ chain has just compiled from SOURCE - which is the first source form that has
\ ever produced a trap.
variable TRAP-N   variable EMIT-SIZE   variable EMIT-BRANCH   variable EMIT-RET

: RUN-HUE ( -- )
   HUE$ 1 1 TRY RC-HUE !
   A64EMIT:SIZE EMIT-SIZE !
   A64EMIT:LEAVES-BY-BRANCH? if 1 else 0 then EMIT-BRANCH !
   A64EMIT:TRAILING-RETURN? if 1 else 0 then EMIT-RET !
   TRAP-BRANCHES TRAP-N ! ;

: RUN-THE-MIGRATIONS ( -- )
   RUN-HUE
   BOX$ 3 1 TRY RC-BOX !
   UNW$ 3 1 TRY RC-UNW !
   QUAD$ 1 1 TRY RC-QUAD !
   SWAPPED$ 3 1 TRY RC-SWAPPED !
   HOLD$ 4 1 TRY RC-HOLD !
   STEP$ 1 1 TRY RC-STEP !
   WIDE$ 1 1 TRY RC-WIDE !
   OVER$ 1 1 TRY RC-OVER !
   CASE$ 1 1 TRY RC-CASE !
   MK$ 1 3 TRY RC-MK !
   MK2$ 2 3 TRY RC-MK2 !
   MK0$ 0 3 TRY RC-MK0 !
   DEAD$ 1 1 TRY RC-DEAD !
   CMT$ 1 1 TRY RC-CMT !
   STR$ 1 1 TRY RC-STR !
   INST$ 3 1 TRY RC-INST !
   INST3$ 4 1 TRY RC-INST3 !
   TWOW$ 1 1 TRY RC-TWOW !
   STRINST$ 1 1 TRY RC-STRINST !
   TRIO$ 4 1 TRY RC-TRIO !
   ARMIF$ 4 1 TRY-WIDE RC-ARMIF !
   ARMLOOP$ 4 1 TRY RC-ARMLOOP !
   HOLD3$ 4 1 TRY RC-HOLD3 !
   ROWS24$ 0 1 TRY RC-ROWS24 !
   MKI$ 1 3 TRY RC-MKI !
   MKI3$ 1 4 TRY RC-MKI3 !
   MKC$ 1 3 TRY RC-MKC !
   RELAY$ 3 4 TRY RC-RELAY !
   LOOPC$ 1 3 TRY-WIDE RC-LOOPC !
   STRCON$ 1 3 TRY RC-STRCON !
   TWOC$ 0 7 TRY RC-TWOC !
   MKG$ 1 5 TRY RC-MKG !
   MKGC$ 1 5 TRY RC-MKGC !
   MKP$ 1 4 TRY RC-MKP !
   CONFIT$ 0 3 TRY RC-CONFIT ! ;

: RUN-THE-REFUSALS ( -- )
   NONEXH$ 1 1 TRY RC-NONEXH !  NELAB:REFUSED-ROW ROW-NONEXH !
   DUPVAR$ 1 1 TRY RC-DUPVAR !  NELAB:REFUSED-ROW ROW-DUPVAR !
   NOFAM$ 1 1 TRY RC-NOFAM !    NELAB:REFUSED-ROW ROW-NOFAM !
   NOTSUM$ 1 1 TRY RC-NOTSUM !
   NOOF$ 1 1 TRY RC-NOOF !      NELAB:REFUSED-ROW ROW-NOOF !
   STRAY$ 1 1 TRY RC-STRAY !    NELAB:REFUSED-ROW ROW-STRAY !
   DROPPED$ 4 1 TRY RC-DROPPED !
   ROWS26$ 0 1 TRY RC-ROWS26 !
   CONOVER$ 0 3 TRY RC-CONOVER !
   NARROWC$ 3 4 TRY RC-NARROWC !  NELAB:REFUSED-ROW ROW-NARROWC !
   NARROWK$ 3 4 TRY RC-NARROWK !  NELAB:REFUSED-ROW ROW-NARROWK ! ;

RUN-THE-MIGRATIONS
RUN-THE-REFUSALS

\ ---- executing both publications ----------------------------------------------
\ Every one of these calls the engine's word and the chain's word on the same
\ input and compares the two answers. Both are ordinary checked calls: the chain
\ published its word before this file's own definitions were compiled.
: AGREE-HUE ( -- )
   s" a payload-free dispatch answers what the engine answers, arm for arm" T-LABEL
   NMX-HUE:RED E-HUE  NMX-HUE:RED C-HUE  T=
   NMX-HUE:GREEN E-HUE  NMX-HUE:GREEN C-HUE  T=
   NMX-HUE:BLUE E-HUE  NMX-HUE:BLUE C-HUE  T=

   s" and the answers really are the three the source names" T-LABEL
   NMX-HUE:RED C-HUE 10 T=
   NMX-HUE:GREEN C-HUE 20 T=
   NMX-HUE:BLUE C-HUE 30 T= ;

: AGREE-BOX ( -- )
   s" a dispatch whose arms keep different payloads agrees with the engine" T-LABEL
   NMX-BOX:NIL E-BOX  NMX-BOX:NIL C-BOX  T=
   7 NMX-BOX:ONE E-BOX  7 NMX-BOX:ONE C-BOX  T=
   3 4 NMX-BOX:TWO E-BOX  3 4 NMX-BOX:TWO C-BOX  T=

   s" and each arm kept the payload its variant declares" T-LABEL
   NMX-BOX:NIL C-BOX 0 T=
   7 NMX-BOX:ONE C-BOX 7 T=
   3 4 NMX-BOX:TWO C-BOX 7 T= ;

: AGREE-UNW ( -- )
   s" the shipped option's eliminator agrees on both of its variants" T-LABEL
   9 OPTION:NONE E-UNW  9 OPTION:NONE C-UNW  T=
   9 42 OPTION:SOME E-UNW  9 42 OPTION:SOME C-UNW  T=

   s" and each variant answered its own value" T-LABEL
   9 OPTION:NONE C-UNW 9 T=
   9 42 OPTION:SOME C-UNW 42 T= ;

: AGREE-STEP ( -- )
   s" four arms agree with the engine, every one of them" T-LABEL
   NMX-QUAD:Q0 E-QUAD  NMX-QUAD:Q0 C-QUAD  T=
   NMX-QUAD:Q1 E-QUAD  NMX-QUAD:Q1 C-QUAD  T=
   NMX-QUAD:Q2 E-QUAD  NMX-QUAD:Q2 C-QUAD  T=
   NMX-QUAD:Q3 E-QUAD  NMX-QUAD:Q3 C-QUAD  T=

   s" seven arms agree with the engine, every one of them" T-LABEL
   NMX-STEP:P0 E-STEP  NMX-STEP:P0 C-STEP  T=
   NMX-STEP:P1 E-STEP  NMX-STEP:P1 C-STEP  T=
   NMX-STEP:P2 E-STEP  NMX-STEP:P2 C-STEP  T=
   NMX-STEP:P3 E-STEP  NMX-STEP:P3 C-STEP  T=
   NMX-STEP:P4 E-STEP  NMX-STEP:P4 C-STEP  T=
   NMX-STEP:P5 E-STEP  NMX-STEP:P5 C-STEP  T=
   NMX-STEP:P6 E-STEP  NMX-STEP:P6 C-STEP  T= ;

: AGREE-WIDE-A ( -- )
   NMX-WIDE:W0 E-WIDE  NMX-WIDE:W0 C-WIDE  T=
   NMX-WIDE:W1 E-WIDE  NMX-WIDE:W1 C-WIDE  T=
   NMX-WIDE:W2 E-WIDE  NMX-WIDE:W2 C-WIDE  T=
   NMX-WIDE:W3 E-WIDE  NMX-WIDE:W3 C-WIDE  T=
   NMX-WIDE:W4 E-WIDE  NMX-WIDE:W4 C-WIDE  T=
   NMX-WIDE:W5 E-WIDE  NMX-WIDE:W5 C-WIDE  T=
   NMX-WIDE:W6 E-WIDE  NMX-WIDE:W6 C-WIDE  T=
   NMX-WIDE:W7 E-WIDE  NMX-WIDE:W7 C-WIDE  T= ;

: AGREE-WIDE-B ( -- )
   NMX-WIDE:W8 E-WIDE  NMX-WIDE:W8 C-WIDE  T=
   NMX-WIDE:W9 E-WIDE  NMX-WIDE:W9 C-WIDE  T=
   NMX-WIDE:W10 E-WIDE  NMX-WIDE:W10 C-WIDE  T=
   NMX-WIDE:W11 E-WIDE  NMX-WIDE:W11 C-WIDE  T=
   NMX-WIDE:W12 E-WIDE  NMX-WIDE:W12 C-WIDE  T=
   NMX-WIDE:W13 E-WIDE  NMX-WIDE:W13 C-WIDE  T=
   NMX-WIDE:W14 E-WIDE  NMX-WIDE:W14 C-WIDE  T=
   NMX-WIDE:W15 E-WIDE  NMX-WIDE:W15 C-WIDE  T= ;

: AGREE-WIDE ( -- )
   s" sixteen arms agree with the engine, every one of them" T-LABEL
   AGREE-WIDE-A
   AGREE-WIDE-B ;

: AGREE-CASE ( -- )
   s" a `case` agrees with the engine on both arms and on its default" T-LABEL
   1 E-CASE  1 C-CASE  T=
   2 E-CASE  2 C-CASE  T=
   5 E-CASE  5 C-CASE  T=

   s" and the default really is the value the source leaves" T-LABEL
   5 C-CASE 99 T=
   1 C-CASE 10 T=
   2 C-CASE 20 T= ;

\ A value the CHAIN constructed, eliminated by the ENGINE's own MATCH: the two
\ compilers have to agree about the cells a value of the family IS, and this is
\ the only case where one of them makes what the other takes apart.
: AGREE-CON ( -- )
   s" a value the chain constructed is what the engine's own MATCH takes apart" T-LABEL
   55 E-MK E-BOX  55 C-MK E-BOX  T=
   55 C-MK E-BOX 55 T=

   s" and so is the widest variant, whose payload leaves no pads at all" T-LABEL
   3 4 E-MK2 E-BOX  3 4 C-MK2 E-BOX  T=
   3 4 C-MK2 E-BOX 7 T=

   s" and so is the payloadless one, which is all pads and a tag" T-LABEL
   E-MK0 E-BOX  C-MK0 E-BOX  T=
   C-MK0 E-BOX 0 T=

   s" and the chain's own eliminator agrees with the engine's on all three" T-LABEL
   55 C-MK C-BOX  55 E-MK E-BOX  T=
   3 4 C-MK2 C-BOX  3 4 E-MK2 E-BOX  T=
   C-MK0 C-BOX  E-MK0 E-BOX  T= ;

: AGREE-DEAD ( -- )
   s" a dispatch with a dead arm compiles, and its live arms answer" T-LABEL
   NMX-HUE:RED E-DEAD  NMX-HUE:RED C-DEAD  T=
   NMX-HUE:BLUE E-DEAD  NMX-HUE:BLUE C-DEAD  T= ;

: AGREE-HIDDEN ( -- )
   s" a family name inside a comment is not a token of the form" T-LABEL
   NMX-HUE:RED E-CMT  NMX-HUE:RED C-CMT  T=
   NMX-HUE:GREEN E-CMT  NMX-HUE:GREEN C-CMT  T=
   NMX-HUE:BLUE E-CMT  NMX-HUE:BLUE C-CMT  T=

   s" and one inside a string literal is not one either" T-LABEL
   NMX-HUE:RED E-STR  NMX-HUE:RED C-STR  T=
   NMX-HUE:GREEN E-STR  NMX-HUE:GREEN C-STR  T=
   NMX-HUE:BLUE E-STR  NMX-HUE:BLUE C-STR  T= ;

\ The dead arm really throws, and it throws the code the source named: a chain
\ that trapped INSTEAD of calling would turn a catchable throw into a process
\ exit and change what the program does.
: DEAD-THROWS ( -- )
   s" and its dead arm throws the code the arm named, catchably" T-LABEL
   [: NMX-HUE:GREEN C-DEAD drop ;] E-A-EMPTY TTHROWSQ ;

: COMPILED-CASE ( -- )
   s" every form the chain models compiled through the whole chain" T-LABEL
   RC-HUE @ 0 T=
   RC-BOX @ 0 T=
   RC-UNW @ 0 T=
   RC-QUAD @ 0 T=
   RC-SWAPPED @ 0 T=
   RC-HOLD @ 0 T=
   RC-STEP @ 0 T=
   RC-WIDE @ 0 T=
   RC-CASE @ 0 T=
   RC-MK @ 0 T=
   RC-MK2 @ 0 T=
   RC-MK0 @ 0 T=
   RC-DEAD @ 0 T=
   RC-CMT @ 0 T=
   RC-STR @ 0 T= ;

\ ---- what the checker refused, and who refused it -----------------------------
\ The engine's reject status is 70, and the elaborator's record says it never
\ reached a body token: these bodies are turned away before the chain sees them.
70 constant RC-REJECT

: REFUSED-CASE ( -- )
   s" a non-exhaustive dispatch is refused before the chain is handed it" T-LABEL
   RC-NONEXH @ RC-REJECT T=
   ROW-NONEXH @ -1 T=

   s" and so is a duplicate variant" T-LABEL
   RC-DUPVAR @ RC-REJECT T=
   ROW-DUPVAR @ -1 T=

   s" and a family name that resolves to nothing" T-LABEL
   RC-NOFAM @ RC-REJECT T=
   ROW-NOFAM @ -1 T=

   s" and a type that is not a sum at all" T-LABEL
   RC-NOTSUM @ RC-REJECT T=

   s" and a variant token with no `of` after it" T-LABEL
   RC-NOOF @ RC-REJECT T=
   ROW-NOOF @ -1 T=

   s" and a `;match` with no dispatch open" T-LABEL
   RC-STRAY @ RC-REJECT T=
   ROW-STRAY @ -1 T= ;

\ ---- what an arm's payload IS -------------------------------------------------
\ THE PAIR THAT BINDS THE GLUE RULE. Both arms keep two cells; one of them is two
\ values and the other is one, and nothing but the registry's two counts says
\ which. The rename compiles over the first and is refused over the second, so a
\ rule that marked every payload would fail the first case and a rule that marked
\ none would fail the second.
: PAYLOAD-CASE ( -- )
   s" a rename over two INDEPENDENT payload cells compiles and agrees" T-LABEL
   RC-SWAPPED @ 0 T=
   3 4 NMX-BOX:TWO E-SWAPPED  3 4 NMX-BOX:TWO C-SWAPPED  T=
   3 4 NMX-BOX:TWO C-SWAPPED 1 T=

   s" a payload that is two cells of ONE value is held, and agrees" T-LABEL
   RC-HOLD @ 0 T=
   9 NMX-HOLDER:EMPTY E-HOLD  9 NMX-HOLDER:EMPTY C-HOLD  T=
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL E-HOLD
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL C-HOLD  T=

   s" and a rename reaching into it is refused by name" T-LABEL
   RC-DROPPED @ E-NELAB-BUNDLE T=

   s" a scrutinee wider than its family declares compiles, and agrees" T-LABEL
   RC-INST @ 0 T=
   -1 E-MKI E-INST   -1 E-MKI C-INST   T=
   0 E-MKI E-INST    0 E-MKI C-INST    T=
   3 E-MKI E-INST    3 E-MKI C-INST    T=
   5 E-MKI E-INST    5 E-MKI C-INST    T=
   3 E-MKI C-INST 204 T=
   5 E-MKI C-INST 340 T=
   0 E-MKI C-INST 0 T=

   s" and one cell wider again, where drop and keep are different numbers" T-LABEL
   RC-INST3 @ 0 T=
   -1 E-MKI3 E-INST3  -1 E-MKI3 C-INST3  T=
   0 E-MKI3 E-INST3   0 E-MKI3 C-INST3   T=
   3 E-MKI3 E-INST3   3 E-MKI3 C-INST3   T=
   7 E-MKI3 E-INST3   7 E-MKI3 C-INST3   T=
   3 E-MKI3 C-INST3 423 T=
   0 E-MKI3 C-INST3 0 T= ;

\ ---- which token a width was filed under -------------------------------------
\ Both rows here answer the same question from opposite sides: is the number the
\ chain reads the one THIS token published? The first body holds two dispatches
\ of different instantiated widths, so a reader keyed on the family, the
\ definition or the form's position gives one of them the other's width. The
\ second puts a string literal - one token, reported through its own event, whose
\ body is dispatch grammar - in front of the form, so a report path that did not
\ step the ordinal files the width one token early.
: ORDINAL-CASE ( -- )
   s" two dispatches of different widths in one body each get their own" T-LABEL
   RC-TWOW @ 0 T=
   -1 E-TWOW  -1 C-TWOW  T=
   0 E-TWOW   0 C-TWOW   T=
   1 E-TWOW   1 C-TWOW   T=
   3 E-TWOW   3 C-TWOW   T=
   9 E-TWOW   9 C-TWOW   T=
   3 C-TWOW 645 T=
   0 C-TWOW 0 T=

   s" and a string literal in front of one does not shift the token" T-LABEL
   RC-STRINST @ 0 T=
   -1 E-STRINST  -1 C-STRINST  T=
   0 E-STRINST   0 C-STRINST   T=
   3 E-STRINST   3 C-STRINST   T=
   5 E-STRINST   5 C-STRINST   T=
   3 C-STRINST 204 T= ;

\ ---- three payload cells -----------------------------------------------------
\ `box` and `holder` stop at two, where an arm that drops one cell too many and
\ one that keeps one too few answer the same shape. Every row below weights each
\ cell with a distinct odd factor, so the ANSWER says which cell came back where.
: TRIPLE-CASE ( -- )
   s" arms of no, one and three payload cells join and agree" T-LABEL
   RC-TRIO @ 0 T=
   NMX-TRIO:T0 E-TRIO  NMX-TRIO:T0 C-TRIO  T=
   7 NMX-TRIO:T1 E-TRIO  7 NMX-TRIO:T1 C-TRIO  T=
   3 5 9 NMX-TRIO:T3 E-TRIO  3 5 9 NMX-TRIO:T3 C-TRIO  T=
   3 5 9 NMX-TRIO:T3 C-TRIO 151 T=
   9 5 3 NMX-TRIO:T3 C-TRIO 223 T=

   s" a payload crossing a nested if inside its arm agrees on both sides" T-LABEL
   RC-ARMIF @ 0 T=
   NMX-TRIO:T0 E-ARMIF  NMX-TRIO:T0 C-ARMIF  T=
   7 NMX-TRIO:T1 E-ARMIF   7 NMX-TRIO:T1 C-ARMIF   T=
   -7 NMX-TRIO:T1 E-ARMIF  -7 NMX-TRIO:T1 C-ARMIF  T=
   3 5 9 NMX-TRIO:T3 E-ARMIF   3 5 9 NMX-TRIO:T3 C-ARMIF   T=
   3 -5 9 NMX-TRIO:T3 E-ARMIF  3 -5 9 NMX-TRIO:T3 C-ARMIF  T=
   3 0 9 NMX-TRIO:T3 E-ARMIF   3 0 9 NMX-TRIO:T3 C-ARMIF   T=

   s" and one crossing a counted loop inside its arm" T-LABEL
   RC-ARMLOOP @ 0 T=
   NMX-TRIO:T0 E-ARMLOOP  NMX-TRIO:T0 C-ARMLOOP  T=
   7 NMX-TRIO:T1 E-ARMLOOP  7 NMX-TRIO:T1 C-ARMLOOP  T=
   3 5 9 NMX-TRIO:T3 E-ARMLOOP  3 5 9 NMX-TRIO:T3 C-ARMLOOP  T=
   9 5 3 NMX-TRIO:T3 E-ARMLOOP  9 5 3 NMX-TRIO:T3 C-ARMLOOP  T=
   3 5 9 NMX-TRIO:T3 C-ARMLOOP 39 T=

   s" and three cells that are ONE value are kept as one" T-LABEL
   RC-HOLD3 @ 0 T=
   NMX-HOLD3:EMPTY3 E-HOLD3  NMX-HOLD3:EMPTY3 C-HOLD3  T=
   3 5 9 NMX-PT3:MAKE NMX-HOLD3:FULL3 E-HOLD3
   3 5 9 NMX-PT3:MAKE NMX-HOLD3:FULL3 C-HOLD3  T=
   3 5 9 NMX-PT3:MAKE NMX-HOLD3:FULL3 C-HOLD3 151 T= ;

\ ---- building a value of a wide instantiation --------------------------------
\ Every row here executes both compilations of a MAKER and compares what they
\ answer through one ENGINE reader, which is the other way round from the cases
\ above: there the maker was the engine's and the reader was under test, and a
\ constructor that pushed the wrong number of cells would have been invisible
\ because nothing in the tree ever compiled one through this chain.
: BUILD-CASE ( -- )
   s" a constructor call at a wider instantiation compiles, and agrees" T-LABEL
   RC-MKI @ 0 T=
   -1 E-MKI E-INST   -1 C-MKI E-INST   T=
   0 E-MKI E-INST    0 C-MKI E-INST    T=
   3 E-MKI E-INST    3 C-MKI E-INST    T=
   5 E-MKI E-INST    5 C-MKI E-INST    T=
   3 C-MKI E-INST 204 T=
   0 C-MKI E-INST 0 T=

   s" and one cell wider again, where its two arms need different numbers" T-LABEL
   RC-MKI3 @ 0 T=
   -1 E-MKI3 E-INST3  -1 C-MKI3 E-INST3  T=
   0 E-MKI3 E-INST3   0 C-MKI3 E-INST3   T=
   3 E-MKI3 E-INST3   3 C-MKI3 E-INST3   T=
   7 E-MKI3 E-INST3   7 C-MKI3 E-INST3   T=
   3 C-MKI3 E-INST3 423 T=
   0 C-MKI3 E-INST3 0 T=

   s" and the reserved `construct` form of the same construction" T-LABEL
   RC-MKC @ 0 T=
   -1 E-MKC E-RDC  -1 C-MKC E-RDC  T=
   0 E-MKC E-RDC   0 C-MKC E-RDC   T=
   3 E-MKC E-RDC   3 C-MKC E-RDC   T=
   5 E-MKC E-RDC   5 C-MKC E-RDC   T=
   3 C-MKC E-RDC 204 T=
   0 C-MKC E-RDC 0 T=

   s" a dispatch over one option that builds a wider one in both arms" T-LABEL
   RC-RELAY @ 0 T=
   0 E-MKI E-RELAY E-INST3   0 E-MKI C-RELAY E-INST3   T=
   3 E-MKI E-RELAY E-INST3   3 E-MKI C-RELAY E-INST3   T=
   5 E-MKI E-RELAY E-INST3   5 E-MKI C-RELAY E-INST3   T=
   0 E-MKI C-RELAY E-INST3 0 T=
   3 E-MKI C-RELAY E-INST3 453 T=

   s" and one the back edge of a loop carries round" T-LABEL
   RC-LOOPC @ 0 T=
   -1 E-LOOPC E-INST  -1 C-LOOPC E-INST  T=
   0 E-LOOPC E-INST   0 C-LOOPC E-INST   T=
   1 E-LOOPC E-INST   1 C-LOOPC E-INST   T=
   4 E-LOOPC E-INST   4 C-LOOPC E-INST   T=

   s" a string literal in front of a construction does not shift the token" T-LABEL
   RC-STRCON @ 0 T=
   0 E-STRCON E-INST  0 C-STRCON E-INST  T=
   3 E-STRCON E-INST  3 C-STRCON E-INST  T=
   3 C-STRCON E-INST 204 T=

   s" and two constructions of different widths in one body each get their own" T-LABEL
   RC-TWOC @ 0 T=
   E-TWOC E-RDTWO  C-TWOC E-RDTWO  T=
   C-TWOC E-RDTWO 0 T=

   s" a construction with a payload UNDER the cells it adds, both spellings" T-LABEL
   RC-MKG @ 0 T=
   RC-MKGC @ 0 T=
   -1 E-MKG E-RDG   -1 C-MKG E-RDG   T=
   3 E-MKG E-RDG    3 C-MKG E-RDG    T=
   5 E-MKG E-RDG    5 C-MKG E-RDG    T=
   3 E-MKGC E-RDG   3 C-MKGC E-RDG   T=
   5 E-MKGC E-RDG   5 C-MKGC E-RDG   T=
   3 C-MKG E-RDG 204 T=
   3 C-MKGC E-RDG 204 T=

   s" a two-parameter family whose arms need different numbers" T-LABEL
   RC-MKP @ 0 T=
   -1 E-MKP E-RDP  -1 C-MKP E-RDP  T=
   3 E-MKP E-RDP   3 C-MKP E-RDP   T=
   7 E-MKP E-RDP   7 C-MKP E-RDP   T=
   3 C-MKP E-RDP 204 T=
   -3 C-MKP E-RDP -423 T=

   s" a narrower-than-declared instantiation stays refused by the CHECKER" T-LABEL
   RC-NARROWC @ RC-REJECT T=
   ROW-NARROWC @ -1 T=
   RC-NARROWK @ RC-REJECT T=
   ROW-NARROWK @ -1 T= ;

\ ---- the construction-row ceiling --------------------------------------------
\ A construction whose row fits the checker's table compiles and answers; one
\ whose row the table had no room for reads as "adds nothing", is lowered a
\ bundle short, and is refused for leaving the wrong number of cells rather than
\ published. That refusal is the whole reason the reader may answer zero on
\ absence, so it is asserted by CODE and not merely as "it did not compile".
: CON-CEILING-CASE ( -- )
   s" a construction inside the checker's row ceiling compiles and answers" T-LABEL
   RC-CONFIT @ 0 T=
   C-CONFIT E-INST 0 T=

   s" and one past it is refused for the cells it left, not published short" T-LABEL
   RC-CONOVER @ E-NELAB-ARITY T= ;

\ ---- the dispatch-row ceiling ------------------------------------------------
\ Twenty-four rows in one body is exactly what the checker records and it
\ compiles; twenty-six overflows the table and is refused by name rather than
\ compiled against the rows that did fit. Neither side is refused for anything
\ else: both are inside the recorder's text cap and its tape, and the arms are
\ one apiece so no block or selector ceiling is anywhere near.
: ROW-CEILING-CASE ( -- )
   s" a body at the checker's dispatch-row ceiling compiles" T-LABEL
   RC-ROWS24 @ 0 T=
   C-ROWS24 144 T=

   s" and one past it is refused by name, not compiled from the rows that fit" T-LABEL
   RC-ROWS26 @ E-NELAB-MATCH T= ;

\ ---- the arm ceiling ----------------------------------------------------------
: CEILING-CASE ( -- )
   s" sixteen arms compile and seventeen are refused by a named capacity" T-LABEL
   RC-WIDE @ 0 T=
   RC-OVER @ E-A64SEL-CAP T= ;

\ ---- what the emission of a compiled dispatch is ------------------------------
\ THIS IS THE MIGRATE HALF OF THE TRAP'S OWN CONTRACT. Until a source form
\ produced a trap, nothing reached src/compiler/native/migrate.f's SIZE-CK with a
\ routine that leaves by branching: the publisher half is pinned in
\ test/compiler/native-trap.f against hand-built modules, and this is the same
\ two questions asked of a definition the chain compiled from source text.
\
\ A MATCH RETURNS ON ITS ARMS AND TRAPS ON ITS MISMATCH EDGE, so it does BOTH:
\ it leaves by branching somewhere in the middle of itself, which is what stops
\ its body from being recorded for copying, AND it ends in the return the
\ recorded length has to leave out.
: EMISSION-CASE ( -- )
   s" a compiled dispatch leaves by branching, so no caller may copy its body" T-LABEL
   EMIT-BRANCH @ 1 T=

   s" and it still ends in a return, so its record is the emission without it" T-LABEL
   EMIT-RET @ 1 T=
   s" C-HUE" NEW-LEN  EMIT-SIZE @ INSN-BYTES -  T=

   s" and no body was recorded for it" T-LABEL
   s" C-HUE" NEW-START NINL:KNOWN? TFALSE

   s" its mismatch edge branches to the one shared trap routine" T-LABEL
   TRAP-N @ 1 T= ;

\ ---- what a dispatch costs in code bytes --------------------------------------
\ THE MEASUREMENT THE DOT TURNS ON, taken through the publication seam's own two
\ readers: OLD-LEN is the code the ENGINE compiled for that definition and
\ NEW-LEN is the code the chain published in its place, so ONE migration answers
\ both columns for one body and nothing has to be lined up by hand. That is
\ test/compiler/native-string.f's instrument, and it is used here rather than a
\ new codegen-comparison corpus for a reason: a corpus is a committed artifact
\ with a baseline table of its own, a table nobody may add a row to, so a sixth
\ one would be six new files whose two columns still have to be matched up by
\ hand - and this reads both columns off one publication.
\
\ WHERE THE ENGINE'S BYTES GO. src/habu/habu2.f copies the whole diagnostic
\ INLINE into every compiled MATCH - `"hb: bad "`, the family name and `" tag\n"`,
\ then a write and an exit - so a dispatch pays the message once per SITE, and
\ the cost carries the family's NAME. That is measurable here and it is the
\ reason the four-armed row below is 188 rather than the 188-minus-four the
\ campaign measured: the dispatch it measured was over a three-character family
\ and this one's name is a byte longer, which the engine rounds up to one more
\ instruction word. The chain pays neither: one constant - the ordinal
\ src/compiler/native/trap.f keyed that name by - and the branch to the one
\ routine that owns the bytes.
\
\ SO THE ENGINE'S NUMBERS ARE PINNED EXACTLY AND THE CHAIN'S ARE PINNED AGAINST
\ THEM. The campaign's two figures - 128 bytes for a two-armed dispatch and 184
\ for a four-armed one - are what the chain had to beat, so they are written here
\ as the bound rather than as a note, and the chain's own figures are asserted
\ under them.
: COST-CASE ( -- )
   s" a two-armed dispatch costs the engine 128 bytes and the chain fewer" T-LABEL
   s" C-UNW" OLD-LEN 128 T=
   s" C-UNW" NEW-LEN  s" C-UNW" OLD-LEN  < TTRUE

   s" and the chain is under the 128 the campaign measured" T-LABEL
   s" C-UNW" NEW-LEN 128 < TTRUE

   s" a four-armed one costs the engine the same shape plus its name" T-LABEL
   s" C-QUAD" OLD-LEN 188 T=
   s" C-QUAD" NEW-LEN  s" C-QUAD" OLD-LEN  < TTRUE

   s" and the chain is under the 184 the campaign measured" T-LABEL
   s" C-QUAD" NEW-LEN 184 < TTRUE

   s" and the chain's cost grows with the arms rather than with the message" T-LABEL
   s" C-QUAD" NEW-LEN  s" C-UNW" NEW-LEN  > TTRUE ;

\ ---- the trap, in a process that dies -----------------------------------------
\ A checked body cannot produce a tag no variant carries - that is what the
\ checker's exhaustiveness rule is - so the forge is an unchecked call, which is
\ the one boundary this file has and is exactly as wide as the death it stages.
\ The process does not come back from it: the trap routine writes the diagnostic
\ and exits, which is what the parent measures.
public

TRUSTED: FORGE ( -- )
   99 C-HUE drop ;

private

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
   s" test/compiler/native-match.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" forge" >LEN PROC-ARGV+ ;

: CHILD-RUN ( -- )
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME       \ ( out-len err-len outcome )
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

: CHILD-ERR$ ( -- ptr u8 n )
   ERR-BUF CHILD-ERR-N @ ;

: FORGE-CASE ( -- )
   CHILD-RUN

   s" a tag no variant carries reaches the trap and ends the process" T-LABEL
   CHILD-RC @ ENGINE-ERROR:BAD-TAG T=

   s" and the diagnostic names the family the dispatch was over" T-LABEL
   CHILD-ERR$ s" hb: bad hue tag" CONTAINS? TTRUE

   s" and it names no other family" T-LABEL
   CHILD-ERR$ s" hb: bad box tag" CONTAINS? TFALSE ;

public

\ ---- the two ways this file is entered ----------------------------------------
\ Loaded with no argument it is the suite. Loaded with `forge` it IS the subject
\ of the suite's last case: it calls a compiled dispatch with a tag no variant
\ carries, which ends the process - so that half cannot be a word the suite
\ calls, and the suite runs it as a child of itself.
: MAIN ( -- )
   T-RESET
   COMPILED-CASE
   REFUSED-CASE
   CEILING-CASE
   AGREE-HUE
   AGREE-BOX
   PAYLOAD-CASE
   ORDINAL-CASE
   TRIPLE-CASE
   BUILD-CASE
   CON-CEILING-CASE
   ROW-CEILING-CASE
   AGREE-UNW
   AGREE-STEP
   AGREE-WIDE
   AGREE-CASE
   AGREE-CON
   AGREE-DEAD
   DEAD-THROWS
   AGREE-HIDDEN
   EMISSION-CASE
   COST-CASE
   FORGE-CASE
   T-REPORT
   s" native-match: ok" type cr ;

: ENTRY ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" forge" STR= if FORGE exit then
   then
   MAIN ;

;package

NMX:ENTRY
