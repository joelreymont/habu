\ native-rename-rows.f - a compile-time stack rename moves whole VALUES, and the
\ boundaries between them survive every seam the value vector crosses
\ (dots habu-rename-over-rows-982167af and habu-rename-rows-row-143c0331).
\
\ WHAT WENT WRONG, AND WHY NOTHING CAUGHT IT. The elaborator's compile-time value
\ vector holds one entry per stack CELL. A value of a layout family occupies
\ several cells, so it occupies several entries, and a rename is nothing but a
\ permutation of that vector. Renaming across such a value therefore reordered its
\ cells - and because the rename's inputs and picks are counted in cells too, the
\ arity still balanced and every later stage saw a well-formed program. Four
\ working definitions were compiled into wrong ones with no diagnostic anywhere.
\
\ STAGE ONE REFUSED THOSE FOUR BY NAME; THIS IS THE CAPABILITY THAT REPLACES THE
\ REFUSAL. A rename now counts in VALUES, as the source language does: the
\ ENGINE moves a two-cell `option<n>` whole and the CHECKER certifies it because
\ its stack holds one item per value, so the elaborator segments its vector the
\ same way and puts whole values back. What makes that possible everywhere rather
\ than only inside straight-line code is the glue bit's meaning: it is about a
\ BOUNDARY - entries i and i-1 are cells of one value - rather than about a cell,
\ so two two-cell values standing next to each other are no longer
\ indistinguishable from one four-cell value, and the same one cell of storage
\ crosses the block arguments at a join, a loop edge, a call and the return.
\
\ SO THIS SUITE IS A DIFFERENTIAL AND NOT A TABLE OF EXPECTED NUMBERS. Each case
\ states one body TWICE: once for the engine to compile as an ordinary definition,
\ and once as source handed to NMIGRATE:DEFINE, which publishes the native chain's
\ code under a second name. Both are then executed on the same inputs and their
\ raw cells compared. An expected-value table would have been written from
\ whichever of the two the author trusted; comparing the two makes the engine the
\ authority and needs no author to be right about anything.
\
\ THE STACK IS READ AS CELLS, DELIBERATELY. A multi-cell value cannot be compared
\ by the checked language - there is no `=` for an `option<n>`, and taking it
\ apart is the very thing under test - so the comparisons are a named unchecked
\ boundary and read the cells the two words left, which is the level the bug lives
\ at: the miscompile produced the right NUMBER of cells with two of them
\ exchanged. That boundary does NOT retire with the row-wise rename, and the first
\ version of this file promised it would: what would retire it is a comparison for
\ a multi-cell value of a PARAMETRIC family, which `DERIVE eq` does not give (it
\ generates for arity-0 families), and nothing in this landing is about equality.
\
\ EVERY VALUE IS WEIGHTED, which is what makes an exchange visible at all. Two
\ values carrying the same number come back the same whichever order they are in,
\ so every payload here is distinct and the two adjacent-value cases carry
\ different ones - the shape a per-cell glue bit could not see is exactly two
\ neighbours swapped, and a differential that could not tell them apart would pass
\ under the very bug this landing closes.
\
\ THE NEGATIVES ARE HALF OF IT. Bodies that hold such a value without renaming
\ across it must still compile; a body whose signature is two INDEPENDENT one-cell
\ variables - which reports the same term count, the same cell count and the same
\ per-term family as the bundled one, and differs only in the slots the checker
\ records - must still compile and agree; a closed enum's value is ONE cell and
\ renaming across it was never wrong; and parking a cell of a multi-cell value on
\ the return stack is still refused by name, because that is a different lowering
\ and a different dot.
\
\ EVERYTHING IS IN ONE PACKAGE, INCLUDING THE MIGRATED TWINS. The chain publishes
\ each twin by evaluating its source text, so the twin lands in whatever wordlist
\ is current when that happens; running the migrations inside the package block is
\ what puts the two columns of every comparison in one scope without a single
\ global name.
\
\ Run: bin/hb --load test/compiler/native-rename-rows.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f
require src/compiler/native/migrate.f

package NRR
private

\ ---- the bodies the engine compiles ------------------------------------------
: MK ( n -- option<n> )                        OPTION:SOME ;

\ A callee with a branch in it, so the call cases really reach a call rather than
\ a copied body: the value then crosses it as a survivor and is renamed on the
\ far side.
: SIDE ( n -- n )                              dup 0 > if 1 + else 2 + then ;

\ The four that took a value apart. Each names its value a different way: from
\ the signature, through a deeper rename, from a generated constructor, and from
\ an ordinary user word whose declared output is the value.
: E-SWAP ( option<n> n -- n option<n> )        swap ;
: E-ROT  ( option<n> n n -- n n option<n> )    rot ;
: E-CTOR ( n n -- option<n> n )                swap OPTION:SOME swap ;
: E-USER ( n n -- option<n> n )                swap MK swap ;

\ The other four rename shapes over a value: one that copies it, one that copies
\ it from underneath, one that discards what is under it, and one that discards
\ what is above it. A copy and a discard are as able to take a value apart as a
\ permutation is - `over` puts three values back from two, off the same table of
\ picks - so each is asked separately.
: E-OVER ( option<n> n -- option<n> n option<n> ) over ;
: E-DUPB ( option<n> -- option<n> option<n> )  dup ;
: E-NIPB ( option<n> n -- n )                  nip ;
: E-DRPB ( option<n> n -- option<n> )          drop ;

\ THE SHAPE A PER-CELL BIT CANNOT SEE. Two values of the same family side by side
\ set exactly the bits one value twice as wide sets, so a reader with only those
\ bits cannot say where the first ends. Exchanged, they are four cells in a
\ different order, and the comparison reads all four.
: E-TWO  ( option<n> option<n> -- option<n> option<n> ) swap ;

\ The seams. A value crossing an `if` join, a loop edge - with a rename inside
\ the body as well as after it - and a call, each renamed on the far side, so the
\ boundary has to have arrived with the value rather than been re-derived.
: E-IF   ( option<n> n -- n option<n> )        dup 0 > if 1 + else 2 + then swap ;
: E-LP   ( option<n> n -- n option<n> )        3 0 ?do over drop loop swap ;
: E-CALL ( option<n> n -- n option<n> )        SIDE swap ;

\ A value crossing a join and RETURNED, with no rename anywhere. Nothing this
\ body emits depends on where its values begin, so no differential over it can
\ see a seam that dropped the boundary - the return's own agreement with the
\ declared row is the only reader, and this is the case that binds it: a seam
\ that stopped carrying the boundary refuses HERE (E-NELAB-JOIN) instead of
\ compiling a routine whose caller is told one thing and whose body believes
\ another.
: E-JRET ( option<n> n -- option<n> )          0 > if 1 else 2 then drop ;

\ A CALL THAT TAKES NOTHING, standing where a taller vector left a boundary bit
\ behind. The glue record is not trimmed when the vector shrinks - the bits above
\ its depth are the previous, taller vector's - and a call with no operands has
\ its result region starting exactly AT that depth, so a reader of the boundary
\ there is reading somebody else's. This body puts one there: matching an
\ `option<n>` leaves the payload one cell deep under the option's own boundary
\ bit, and `TWOC` then takes nothing and leaves two. It was found by the census,
\ not by review - lib/ptx/toolchain.f PARSE-VER is the same shape and stopped
\ compiling (E-NELAB-UNDER, the next rename taking three cells for two values).
: TWOC ( -- n n )                              3 5 ;

: E-STALE ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF TWOC nip + ENDOF
   ;MATCH ;

\ A parked value and a bundle crossing ONE join together. The width, the split
\ into data and parked values, and the boundaries are three columns of the same
\ block record, and this is the case where all three have to agree at once.
: E-PARK ( option<n> n n -- n option<n> n )
   >r  dup 0 > if 1 + else 2 + then  swap  r> ;

\ `MATCH` over the UPPER of two adjacent values. Its subject has to be told from
\ its neighbour, which is the test a run of set bits could not make: a chain whose
\ glue bit is about a cell refuses this body (E-NELAB-MATCH), and the matching arm
\ renames across the value it did not consume.
: E-M2 ( option<n> option<n> -- n )
   MATCH option
      none OF drop 0 ENDOF
      some OF swap drop ENDOF
   ;MATCH ;

\ The ones that never were wrong: a value held but not renamed across, one handed
\ straight back, one made and left alone, one carried across a loop edge, and the
\ two-variable twin of the first bundled signature.
: E-HOLD ( n n option<n> -- n n option<n> ) ;
: E-PASS ( option<n> -- option<n> ) ;
: E-MADE ( n -- option<n> )                    OPTION:SOME ;
: E-LOOP ( option<n> n n -- option<n> )        ?do loop ;
: E-VARS ( a b n -- n a b )                    swap ;
: E-CELL ( n n -- n n )                        swap ;

\ A CLOSED ENUM IS A LAYOUT FAMILY WHOSE VALUES ARE ONE CELL, and it is the case
\ that decides whether the marking is worth having. The checker records such a
\ value cell by cell with its position exactly as it records a wider one, so a
\ reader that asked only "does this term carry a position" would mark every enum
\ in the tree and segment renames over values that cannot be taken apart - it cost
\ sixteen definitions the first time this was measured. A value of one cell is
\ moved whole by moving one cell, so it is not marked and this must still compile.
public
ENUM lamp DERIVE eq
   dim
   bright
;ENUM

\ A PAYLOAD WHOSE VALUES NOBODY CAN PLACE, which is the one shape this landing
\ refuses that the cell-counting chain compiled. A variant payload of SEVERAL
\ fields where one of them is wider than a cell has more cells than fields, and
\ the registry exports no per-field width to say which field the extra cells
\ belong to - so where the arm's values BEGIN cannot be stated. Two orders are
\ declared because they cost differently: with the wide field UNDER the narrow
\ one an arm cannot even reach its payload without a rename, while the other way
\ round it can, and the chain used to compile that one.
PRODUCT pt 0
   FIELD x n
   FIELD y n
;PRODUCT

SUMTYPE mixhi 0
   VARIANT nohi ;VARIANT
   VARIANT hi pt n ;VARIANT
;SUMTYPE

SUMTYPE mixlo 0
   VARIANT nolo ;VARIANT
   VARIANT lo n pt ;VARIANT
;SUMTYPE
private

\ Both are ordinary programs: the ENGINE compiles and runs them, which is what
\ makes the chain's refusal a statement about the chain.
: E-MIXHI ( mixhi -- n )
   MATCH mixhi
      nohi OF 0 ENDOF
      hi OF drop NRR-PT:UNMAKE + ENDOF
   ;MATCH ;

: E-MIXLO ( mixlo -- n )
   MATCH mixlo
      nolo OF 0 ENDOF
      lo OF NRR-PT:UNMAKE + + ENDOF
   ;MATCH ;

: E-ENUM ( lamp n -- n lamp )                  swap ;

\ ---- the source the chain is given -------------------------------------------
\ Character for character the body above it, so a difference between the two
\ columns can only come from the two compilers and never from two programs.
: SWAP$ ( -- ptr u8 n ) s" : C-SWAP ( option<n> n -- n option<n> ) swap ;" ;
: ROT$  ( -- ptr u8 n ) s" : C-ROT ( option<n> n n -- n n option<n> ) rot ;" ;
: CTOR$ ( -- ptr u8 n ) s" : C-CTOR ( n n -- option<n> n ) swap OPTION:SOME swap ;" ;
: USER$ ( -- ptr u8 n ) s" : C-USER ( n n -- option<n> n ) swap MK swap ;" ;

: OVER$ ( -- ptr u8 n ) s" : C-OVER ( option<n> n -- option<n> n option<n> ) over ;" ;
: DUPB$ ( -- ptr u8 n ) s" : C-DUPB ( option<n> -- option<n> option<n> ) dup ;" ;
: NIPB$ ( -- ptr u8 n ) s" : C-NIPB ( option<n> n -- n ) nip ;" ;
: DRPB$ ( -- ptr u8 n ) s" : C-DRPB ( option<n> n -- option<n> ) drop ;" ;

: TWO$  ( -- ptr u8 n ) s" : C-TWO ( option<n> option<n> -- option<n> option<n> ) swap ;" ;
: IF$   ( -- ptr u8 n ) s" : C-IF ( option<n> n -- n option<n> ) dup 0 > if 1 + else 2 + then swap ;" ;
: LP$   ( -- ptr u8 n ) s" : C-LP ( option<n> n -- n option<n> ) 3 0 ?do over drop loop swap ;" ;
: CALL$ ( -- ptr u8 n ) s" : C-CALL ( option<n> n -- n option<n> ) SIDE swap ;" ;
: PARK$ ( -- ptr u8 n ) s" : C-PARK ( option<n> n n -- n option<n> n ) >r dup 0 > if 1 + else 2 + then swap r> ;" ;
: STALE$ ( -- ptr u8 n ) s" : C-STALE ( option<n> -- n ) MATCH option none OF 0 ENDOF some OF TWOC nip + ENDOF ;MATCH ;" ;
: JRET$ ( -- ptr u8 n ) s" : C-JRET ( option<n> n -- option<n> ) 0 > if 1 else 2 then drop ;" ;
: M2$   ( -- ptr u8 n ) s" : C-M2 ( option<n> option<n> -- n ) MATCH option none OF drop 0 ENDOF some OF swap drop ENDOF ;MATCH ;" ;

: HOLD$ ( -- ptr u8 n ) s" : C-HOLD ( n n option<n> -- n n option<n> ) ;" ;
: PASS$ ( -- ptr u8 n ) s" : C-PASS ( option<n> -- option<n> ) ;" ;
: MADE$ ( -- ptr u8 n ) s" : C-MADE ( n -- option<n> ) OPTION:SOME ;" ;
: LOOP$ ( -- ptr u8 n ) s" : C-LOOP ( option<n> n n -- option<n> ) ?do loop ;" ;
: VARS$ ( -- ptr u8 n ) s" : C-VARS ( a b n -- n a b ) swap ;" ;
: CELL$ ( -- ptr u8 n ) s" : C-CELL ( n n -- n n ) swap ;" ;
: ENUM$ ( -- ptr u8 n ) s" : C-ENUM ( lamp n -- n lamp ) swap ;" ;

\ THE ONE THAT MUST STILL BE REFUSED. `>r` parks one CELL where the checker moves
\ one ITEM, so a cell of a multi-cell value parked on its own would be separated
\ from the rest with every count still agreeing - the same silent wrongness the
\ rename had, reached by the other door. That lowering is not what this landing
\ gives, so it stays a refusal by name.
\ AND THE TWO THE ARM CANNOT PLACE. Measured against master before they were
\ written: the first refused there too, as E-NELAB-BUNDLE from the rename, and
\ the SECOND compiled - it has no rename for the old guard to catch, and the
\ marking it compiled under merged the payload's two values into one. That
\ marking is safe only while every consumer answers it with a refusal, and a
\ rename now segments instead, so the arm says by name that it cannot place
\ these values. Nothing in src or lib has the shape (the census moved not one
\ definition into this class), and the capability that retires it is the
\ per-field width, dot habu-publish-the-payload-eb4ae38a.
: MIXHI$ ( -- ptr u8 n ) s" : C-MIXHI ( mixhi -- n ) MATCH mixhi nohi OF 0 ENDOF hi OF drop NRR-PT:UNMAKE + ENDOF ;MATCH ;" ;
: MIXLO$ ( -- ptr u8 n ) s" : C-MIXLO ( mixlo -- n ) MATCH mixlo nolo OF 0 ENDOF lo OF NRR-PT:UNMAKE + + ENDOF ;MATCH ;" ;

: TORB$ ( -- ptr u8 n ) s" : C-TORB ( option<n> -- option<n> ) >r r> ;" ;

\ ---- driving one migration where its refusal can be read ---------------------
\ A checked `catch` takes a stack-neutral quotation and a quotation cannot read
\ the enclosing word's locals, so what the migration needs is parked first - the
\ same shape src/compiler/native/migrate.f uses for the same reason.
variable M-A   variable M-U   variable M-IN   variable M-OUT

: MIGRATE-RC ( -- n )
   [: M-A @ M-U @ M-IN @ M-OUT @ NMIGRATE:DEFINE ;] catch ;

: TRY ( ptr u8 n n n -- n ) {: a:ptr u:n in:n out:n :}
   a M-A !  u M-U !  in M-IN !  out M-OUT !
   MIGRATE-RC ;

\ ---- the migrations ----------------------------------------------------------
\ THESE RUN BEFORE THE COMPARISONS ARE COMPILED, because each publishes the name
\ one half of a comparison calls and a word cannot be compiled against a name
\ nothing has defined yet. The outcomes are parked and asserted with the rest.
variable RC-SWAP  variable RC-ROT   variable RC-CTOR  variable RC-USER
variable RC-OVER  variable RC-DUPB  variable RC-NIPB  variable RC-DRPB
variable RC-TWO   variable RC-IF    variable RC-LP    variable RC-CALL
variable RC-PARK  variable RC-M2   variable RC-JRET  variable RC-STALE
variable RC-HOLD  variable RC-PASS  variable RC-MADE
variable RC-LOOP  variable RC-VARS  variable RC-CELL
variable RC-ENUM  variable RC-TORB
variable RC-MIXHI variable RC-MIXLO

\ Every migration runs HERE, inside the package block, and for two reasons.
\ The chain publishes by evaluating the source text, so a twin migrated after
\ `;package` would land outside the package and a body naming a package word -
\ `MK`, `SIDE` - would not resolve at all, which is a refusal about scope rather
\ than about renames. And the twins that succeed have to exist before the
\ comparisons below are compiled against their names.
: MIGRATE-THE-FOUR ( -- )
   SWAP$ 3 3 TRY RC-SWAP !
   ROT$  4 4 TRY RC-ROT !
   CTOR$ 2 3 TRY RC-CTOR !
   USER$ 2 3 TRY RC-USER ! ;

: MIGRATE-THE-SHAPES ( -- )
   OVER$ 3 5 TRY RC-OVER !
   DUPB$ 2 4 TRY RC-DUPB !
   NIPB$ 3 1 TRY RC-NIPB !
   DRPB$ 3 2 TRY RC-DRPB ! ;

: MIGRATE-THE-SEAMS ( -- )
   TWO$  4 4 TRY RC-TWO !
   IF$   3 3 TRY RC-IF !
   LP$   3 3 TRY RC-LP !
   CALL$ 3 3 TRY RC-CALL !
   PARK$ 4 4 TRY RC-PARK !
   JRET$ 3 2 TRY RC-JRET !
   STALE$ 2 1 TRY RC-STALE !
   M2$   4 1 TRY RC-M2 ! ;

: MIGRATE-THE-REST ( -- )
   HOLD$ 4 4 TRY RC-HOLD !
   PASS$ 2 2 TRY RC-PASS !
   MADE$ 1 2 TRY RC-MADE !
   LOOP$ 4 2 TRY RC-LOOP !
   VARS$ 3 3 TRY RC-VARS !
   CELL$ 2 2 TRY RC-CELL !
   ENUM$ 2 2 TRY RC-ENUM !
   TORB$ 2 2 TRY RC-TORB !
   MIXHI$ 4 1 TRY RC-MIXHI !
   MIXLO$ 4 1 TRY RC-MIXLO ! ;

MIGRATE-THE-FOUR
MIGRATE-THE-SHAPES
MIGRATE-THE-SEAMS
MIGRATE-THE-REST

\ ---- executing both publications ---------------------------------------------
\ Reading the cells two words left is not something the checked language can do -
\ a multi-cell value has no `=` and taking it apart is the bug under test - so
\ this is the suite's one named unchecked boundary, and it is exactly as wide as
\ the comparison needs. The cells go into one row of five, the row is copied
\ aside, the second word fills the row again, and the two rows are compared; a
\ word leaving fewer than five cells zeroes the rest, so a case that lost or
\ gained a cell differs from its twin inside the row rather than off the end of it.
variable D1  variable D2  variable D3  variable D4  variable D5
variable A1  variable A2  variable A3  variable A4  variable A5

TRUSTED: ZERO ( -- )  0 D1 !  0 D2 !  0 D3 !  0 D4 !  0 D5 ! ;
TRUSTED: GRAB1 ( n -- )             ZERO  D1 ! ;
TRUSTED: GRAB2 ( n n -- )           ZERO  D2 !  D1 ! ;
TRUSTED: GRAB3 ( n n n -- )         ZERO  D3 !  D2 !  D1 ! ;
TRUSTED: GRAB4 ( n n n n -- )       ZERO  D4 !  D3 !  D2 !  D1 ! ;
TRUSTED: GRAB5 ( n n n n n -- )     ZERO  D5 !  D4 !  D3 !  D2 !  D1 ! ;

TRUSTED: KEEP ( -- )
   D1 @ A1 !  D2 @ A2 !  D3 @ A3 !  D4 @ A4 !  D5 @ A5 ! ;

TRUSTED: SAME ( -- )
   A1 @ D1 @ T=  A2 @ D2 @ T=  A3 @ D3 @ T=  A4 @ D4 @ T=  A5 @ D5 @ T= ;

\ ---- the four that were miscompiled, now compiled and agreeing ----------------
TRUSTED: AGREE-SWAP ( -- )
   s" a value named by the signature is swapped WHOLE, as the engine swaps it" T-LABEL
   43 OPTION:SOME 7 E-SWAP GRAB3 KEEP
   43 OPTION:SOME 7 C-SWAP GRAB3
   SAME ;

TRUSTED: AGREE-ROT ( -- )
   s" and a deeper rename reaching it moves it whole too" T-LABEL
   43 OPTION:SOME 7 9 E-ROT GRAB4 KEEP
   43 OPTION:SOME 7 9 C-ROT GRAB4
   SAME ;

TRUSTED: AGREE-CTOR ( -- )
   s" a value a generated constructor left is moved whole" T-LABEL
   5 11 E-CTOR GRAB3 KEEP
   5 11 C-CTOR GRAB3
   SAME ;

TRUSTED: AGREE-USER ( -- )
   s" and so is one an ordinary word declared it leaves" T-LABEL
   5 11 E-USER GRAB3 KEEP
   5 11 C-USER GRAB3
   SAME ;

\ ---- the other four rename shapes --------------------------------------------
TRUSTED: AGREE-OVER ( -- )
   s" a value copied from under another comes back whole, twice" T-LABEL
   43 OPTION:SOME 7 E-OVER GRAB5 KEEP
   43 OPTION:SOME 7 C-OVER GRAB5
   SAME ;

TRUSTED: AGREE-DUPB ( -- )
   s" and one copied in place" T-LABEL
   43 OPTION:SOME E-DUPB GRAB4 KEEP
   43 OPTION:SOME C-DUPB GRAB4
   SAME ;

TRUSTED: AGREE-NIPB ( -- )
   s" a value discarded from under another goes whole" T-LABEL
   43 OPTION:SOME 7 E-NIPB GRAB1 KEEP
   43 OPTION:SOME 7 C-NIPB GRAB1
   SAME ;

TRUSTED: AGREE-DRPB ( -- )
   s" and one discarded from above it" T-LABEL
   43 OPTION:SOME 7 E-DRPB GRAB2 KEEP
   43 OPTION:SOME 7 C-DRPB GRAB2
   SAME ;

\ ---- the seams ---------------------------------------------------------------
TRUSTED: AGREE-TWO ( -- )
   s" two ADJACENT values are exchanged, not merged into one four-cell value" T-LABEL
   11 OPTION:SOME 29 OPTION:SOME E-TWO GRAB4 KEEP
   11 OPTION:SOME 29 OPTION:SOME C-TWO GRAB4
   SAME ;

TRUSTED: AGREE-IF ( -- )
   s" a value crossing an if join is still one value on the far side" T-LABEL
   43 OPTION:SOME 7 E-IF GRAB3 KEEP
   43 OPTION:SOME 7 C-IF GRAB3
   SAME ;

TRUSTED: AGREE-LP ( -- )
   s" and one crossing a loop edge, renamed inside the body and after it" T-LABEL
   43 OPTION:SOME 7 E-LP GRAB3 KEEP
   43 OPTION:SOME 7 C-LP GRAB3
   SAME ;

TRUSTED: AGREE-CALL ( -- )
   s" and one that survived a call" T-LABEL
   43 OPTION:SOME 7 E-CALL GRAB3 KEEP
   43 OPTION:SOME 7 C-CALL GRAB3
   SAME ;

TRUSTED: AGREE-PARK ( -- )
   s" a parked value and a bundle cross one join together" T-LABEL
   43 OPTION:SOME 7 9 E-PARK GRAB4 KEEP
   43 OPTION:SOME 7 9 C-PARK GRAB4
   SAME ;

TRUSTED: AGREE-JRET ( -- )
   s" a value crossing a join and returned comes back whole" T-LABEL
   43 OPTION:SOME 7 E-JRET GRAB2 KEEP
   43 OPTION:SOME 7 C-JRET GRAB2
   SAME ;

TRUSTED: AGREE-STALE ( -- )
   s" a call taking nothing does not read a boundary the vector no longer has" T-LABEL
   43 OPTION:SOME E-STALE GRAB1 KEEP
   43 OPTION:SOME C-STALE GRAB1
   SAME
   0 OPTION:NONE E-STALE GRAB1 KEEP
   0 OPTION:NONE C-STALE GRAB1
   SAME ;

TRUSTED: AGREE-M2 ( -- )
   s" MATCH over the upper of two adjacent values dispatches on the right one" T-LABEL
   11 OPTION:SOME 29 OPTION:SOME E-M2 GRAB1 KEEP
   11 OPTION:SOME 29 OPTION:SOME C-M2 GRAB1
   SAME ;

\ ---- the bodies that never broke ---------------------------------------------
TRUSTED: AGREE-HOLD ( -- )
   s" a held value comes back cell for cell as the engine leaves it" T-LABEL
   5 6 43 OPTION:SOME E-HOLD GRAB4 KEEP
   5 6 43 OPTION:SOME C-HOLD GRAB4
   SAME ;

TRUSTED: AGREE-PASS ( -- )
   s" and so does one handed straight back" T-LABEL
   43 OPTION:SOME E-PASS GRAB2 KEEP
   43 OPTION:SOME C-PASS GRAB2
   SAME ;

TRUSTED: AGREE-MADE ( -- )
   s" and one the body made" T-LABEL
   7 E-MADE GRAB2 KEEP
   7 C-MADE GRAB2
   SAME ;

TRUSTED: AGREE-LOOP ( -- )
   s" and one carried across a loop edge" T-LABEL
   43 OPTION:SOME 3 0 E-LOOP GRAB2 KEEP
   43 OPTION:SOME 3 0 C-LOOP GRAB2
   SAME ;

TRUSTED: AGREE-VARS ( -- )
   s" the two-variable twin is renamed, and renamed the same way" T-LABEL
   11 22 33 E-VARS GRAB3 KEEP
   11 22 33 C-VARS GRAB3
   SAME ;

TRUSTED: AGREE-CELL ( -- )
   s" and an ordinary two-cell swap is unchanged" T-LABEL
   1 2 E-CELL GRAB2 KEEP
   1 2 C-CELL GRAB2
   SAME ;

TRUSTED: AGREE-ENUM ( -- )
   s" and a one-cell enum is renamed the way the engine renames it" T-LABEL
   NRR-LAMP:BRIGHT 9 E-ENUM GRAB2 KEEP
   NRR-LAMP:BRIGHT 9 C-ENUM GRAB2
   SAME ;

\ ---- what the outcomes have to be --------------------------------------------
\ The CODE is asserted for the refusal, not merely that something failed: a body
\ refused for running out of registers, or for naming a word the dialect cannot
\ compile, would satisfy "did not compile" while proving nothing about values.
: THE-FOUR-NOW-COMPILE ( -- )
   s" swap across a value named by the signature compiles" T-LABEL
   RC-SWAP @ 0 T=
   s" and so does a deeper rename that reaches one of its cells" T-LABEL
   RC-ROT @ 0 T=
   s" a value a generated constructor left is renamed, not refused" T-LABEL
   RC-CTOR @ 0 T=
   s" and so is one an ordinary word declared it leaves" T-LABEL
   RC-USER @ 0 T= ;

: EVERY-RENAME-SHAPE-COMPILES ( -- )
   s" copying a value from under another compiles" T-LABEL
   RC-OVER @ 0 T=
   s" copying one in place compiles" T-LABEL
   RC-DUPB @ 0 T=
   s" discarding what is under one compiles" T-LABEL
   RC-NIPB @ 0 T=
   s" discarding what is above one compiles" T-LABEL
   RC-DRPB @ 0 T= ;

: EVERY-SEAM-CARRIES-THE-BOUNDARY ( -- )
   s" two adjacent values exchanged compiles" T-LABEL
   RC-TWO @ 0 T=
   s" a value crossing an if join compiles" T-LABEL
   RC-IF @ 0 T=
   s" a value crossing a loop edge compiles" T-LABEL
   RC-LP @ 0 T=
   s" a value surviving a call compiles" T-LABEL
   RC-CALL @ 0 T=
   s" a parked value and a bundle crossing one join compiles" T-LABEL
   RC-PARK @ 0 T=
   s" a value crossing a join and returned compiles, boundary and all" T-LABEL
   RC-JRET @ 0 T=
   s" a call taking no operands after a dispatch compiles" T-LABEL
   RC-STALE @ 0 T=
   s" MATCH over the upper of two adjacent values compiles" T-LABEL
   RC-M2 @ 0 T= ;

: BODIES-THAT-NEVER-BROKE-STILL-COMPILE ( -- )
   s" a value held across a body that renames nothing still compiles" T-LABEL
   RC-HOLD @ 0 T=
   s" a value handed straight back still compiles" T-LABEL
   RC-PASS @ 0 T=
   s" a value made and left alone still compiles" T-LABEL
   RC-MADE @ 0 T=
   s" a value carried across a loop edge still compiles" T-LABEL
   RC-LOOP @ 0 T=
   s" two INDEPENDENT variables read as one value's two cells would still compile" T-LABEL
   RC-VARS @ 0 T=
   s" and an ordinary two-cell rename is untouched" T-LABEL
   RC-CELL @ 0 T=
   s" a closed enum's value is ONE cell, so renaming across it is not segmented" T-LABEL
   RC-ENUM @ 0 T= ;

: PARKING-A-CELL-OF-A-VALUE-IS-STILL-REFUSED ( -- )
   s" parking one cell of a multi-cell value is refused by name" T-LABEL
   RC-TORB @ E-NELAB-BUNDLE T= ;

: AN-ARM-IT-CANNOT-PLACE-IS-REFUSED ( -- )
   s" an arm whose payload has several fields in more cells is refused by name" T-LABEL
   RC-MIXHI @ E-NELAB-MATCH T=
   s" and so is the order that needs no rename, which used to compile" T-LABEL
   RC-MIXLO @ E-NELAB-MATCH T=
   s" the engine compiles both, so the refusal is about the chain" T-LABEL
   3 5 NRR-PT:MAKE 7 NRR-MIXHI:HI E-MIXHI 8 T=
   7 3 5 NRR-PT:MAKE NRR-MIXLO:LO E-MIXLO 15 T= ;

public

: MAIN ( -- )
   T-RESET
   THE-FOUR-NOW-COMPILE
   EVERY-RENAME-SHAPE-COMPILES
   EVERY-SEAM-CARRIES-THE-BOUNDARY
   BODIES-THAT-NEVER-BROKE-STILL-COMPILE
   PARKING-A-CELL-OF-A-VALUE-IS-STILL-REFUSED
   AN-ARM-IT-CANNOT-PLACE-IS-REFUSED
   AGREE-SWAP
   AGREE-ROT
   AGREE-CTOR
   AGREE-USER
   AGREE-OVER
   AGREE-DUPB
   AGREE-NIPB
   AGREE-DRPB
   AGREE-TWO
   AGREE-IF
   AGREE-LP
   AGREE-CALL
   AGREE-PARK
   AGREE-JRET
   AGREE-STALE
   AGREE-M2
   AGREE-HOLD
   AGREE-PASS
   AGREE-MADE
   AGREE-LOOP
   AGREE-VARS
   AGREE-CELL
   AGREE-ENUM
   T-REPORT
   s" native-rename-rows: ok" type cr ;

;package

NRR:MAIN
