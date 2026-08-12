\ judge/corpus4.f - the judged rows of tools/codegen-compare-corpus4.f: twelve
\ shapes chosen because somebody had a reason to believe the native chain
\ handles them WORSE than the engine's emitter. One concern: which subject is
\ judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES AND WHAT IT DOES NOT. It states the twelve subjects,
\ ONE pinned input per subject, the storage each subject reads, and the C symbol
\ that is its twin. It states NOTHING about the programs: the engine compiled
\ them when the corpus file was loaded, and the chain's copies are derived from
\ that same file's bytes by tools/judge/src.f and compiled by
\ tools/judge/chain.f. There is no retyped body here. The measuring is
\ tools/judge/pass.f's, which every corpus shares.
\
\ AND IT STATES NOTHING ABOUT WHAT THE CHAIN CANNOT COMPILE. The comparison this
\ replaces kept a hand-written list of those subjects, and a list cannot notice a
\ row that started refusing or one that stopped. Here the chain is asked, every
\ run, and what it answers - the routine, or the code it declined with - is the
\ row's verdict.
\
\ THE INPUT IS WRITTEN ONCE, WHICH IS THE OTHER DUPLICATE GONE. The old harness
\ writes each row's pinned input three times: tools/codegen-compare-cases4.f for
\ the engine, tools/codegen-compare-new4.f for the chain, and
\ tools/codegen-compare-c4.f for the C twin. Three copies of a number is three
\ chances to measure one column on a different program from the other two. Here
\ the numbers are stated once and tools/judge/cost.f builds each column's body
\ from them.
\
\ THE INPUT IS THE LONGEST PATH THROUGH THE SUBJECT, which is the rule
\ tools/codegen-compare-cases4.f set for the same twelve: LADDER is measured at
\ 1000, the input that runs every one of its eight compares, because timing the
\ short way out would measure the first guard and call it a ladder. Every input
\ here is that file's timed input, unchanged.
\
\ THE POINTER IS NOT WRITTEN OUT, IT IS APPENDED PER WORLD. Two subjects read
\ memory the corpus owns, and the C twins cannot share it - they are a different
\ program, not a second compilation of the same one, so they carry their own,
\ filled from the same constants. A row appends its storage where the subject
\ takes it, in each world's own spelling. The stepped cell is also the one
\ subject that is not idempotent, so its row carries the reset a VALUE body runs
\ first - the timing body does not reset, because a reset inside it would be
\ timing a fill as well as a step - and the reader that says what the step wrote.
\
\ THE STEP'S ANSWER IS NOT EVIDENCE OF THE STEP. STORE-LOAD answers the cell it
\ read back, which a routine that kept the value in a register could answer just
\ as well, and it says nothing at all about the cell AFTER the one it steps. So
\ the row reads both cells back and folds them into its answer: a loop that ran
\ one turn too far, or that stepped the wrong address, changes the row's value
\ rather than nothing.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-corpus4.f
require tools/judge/pass.f

package JUDGE-CORPUS4

private

variable REC-CELL
variable STEP-CELL

public

\ The C twins' own copies of the pinned data, reached through the reference
\ library. Public because a generated reference body names them.
\
\ THE TWO POINTERS ARE ASKED FOR ONCE. They are constants of the mapped
\ library, and a generated TIMING body runs its inputs on every one of a
\ quarter of a million repetitions: resolving a symbol and making a foreign
\ call inside that loop put four hundred nanoseconds into both a row and its
\ floor, where it cancelled but left the row's cost as the difference of two
\ large nearly equal numbers. Cached, the input costs a load.
: C-REC ( -- n )
   REC-CELL @ 0<> if REC-CELL @ exit then
   s" hc4_rec_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup REC-CELL ! ;

: C-STEP ( -- n )
   STEP-CELL @ 0<> if STEP-CELL @ exit then
   s" hc4_step_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup STEP-CELL ! ;

: C-RESET ( -- )
   s" hc4_step_reset" CODEGEN-CABI:FN CODEGEN-CABI:I0 drop ;

private

: C-STEP@ ( n -- n )
   s" hc4_step_get" CODEGEN-CABI:FN CODEGEN-CABI:I1 ;

public

\ The cell AFTER the one a step writes, which the step must not have touched.
\ The cell it does write is the subject's own answer - STORE-LOAD ends by
\ loading it - so the row already compares that one, and what no column's answer
\ can show is a loop that ran one turn too far or stepped the wrong address.
\ A reader is asked for outside every timing loop - a VALUE body runs once - so
\ the symbol lookup here costs nothing measured.
: STEP-READ ( -- n )
   1 CODEGEN-CORPUS4:STEP-CELL@ ;

: C-STEP-READ ( -- n )
   1 C-STEP@ ;

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus4.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J4" ;

\ The package this corpus publishes its subjects in, and therefore the package
\ the derived words are published in too. A derived body may name storage that
\ is PRIVATE to it - the corpus's own cell, which both columns step - and a
\ word compiled anywhere else could not see it. So JUDGE runs with this package
\ open, and the reader that takes a size off a dictionary record is given the
\ qualifier, because that reader resolves a spelling as written.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:" ;

\ ---- the storage two of the twelve read --------------------------------------

: REC+ ( -- )
   s" CODEGEN-CORPUS4:REC " s" JUDGE-CORPUS4:C-REC " JUDGE-PASS:STORE+ ;

: STEP+ ( -- )
   s" CODEGEN-CORPUS4:STEP-AT " s" JUDGE-CORPUS4:C-STEP " JUDGE-PASS:STORE+
   s" CODEGEN-CORPUS4:S-RESET " s" JUDGE-CORPUS4:C-RESET " JUDGE-PASS:SETUP+
   s" JUDGE-CORPUS4:STEP-READ" s" JUDGE-CORPUS4:C-STEP-READ" JUDGE-PASS:READ+ ;

: LEN+ ( -- )
   s" CODEGEN-CORPUS4:LOOP-LEN" JUDGE-PASS:IN+ ;

\ ---- the twelve rows, written once -------------------------------------------
\ Each is: the name the corpus publishes it under, the C symbol that is its twin,
\ and then its pinned input in the order the subject takes it.
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ -- ] -- ) {: row :}
   s" CALL-FAN" s" hc4_call_fan" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" CALL-FAN-BIG" s" hc4_call_fan_big" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" CALL-LOOP-3" s" hc4_call_loop_3" JUDGE-PASS:ROW!
      s" 1 2 3 7 " JUDGE-PASS:IN+  LEN+  row execute
   s" WIDE-ARITY" s" hc4_wide_arity" JUDGE-PASS:ROW!
      s" 1 2 3 4 5 6" JUDGE-PASS:IN+  row execute
   s" LADDER" s" hc4_ladder" JUDGE-PASS:ROW!
      s" 1000" JUDGE-PASS:IN+  row execute
   s" PRESSURE-LOOP" s" hc4_pressure_loop" JUDGE-PASS:ROW!
      REC+  LEN+  row execute
   s" CALL-PRESSURE" s" hc4_call_pressure" JUDGE-PASS:ROW!
      s" 1 2 3 4 5 6 7 9 " JUDGE-PASS:IN+  LEN+  row execute
   s" BIG-CONSTS" s" hc4_big_consts" JUDGE-PASS:ROW!
      LEN+  row execute
   s" MANY-LOCALS" s" hc4_many_locals" JUDGE-PASS:ROW!
      s" 1 2 3 4 5 6 7 8 " JUDGE-PASS:IN+  LEN+  row execute
   s" TINY-CALLEE" s" hc4_tiny_callee" JUDGE-PASS:ROW!
      s" 0 " JUDGE-PASS:IN+  LEN+  row execute
   s" FLOAT-MIX" s" hc4_float_mix" JUDGE-PASS:ROW!
      s" 0 " JUDGE-PASS:IN+  LEN+  row execute
   s" STORE-LOAD" s" hc4_store_load" JUDGE-PASS:ROW!
      STEP+  LEN+  row execute ;

: OPEN-CORPUS ( -- )
   SOURCE$ SUFFIX$ QUALIFIER$ JUDGE-PASS:CORPUS! ;

public

\ Read the corpus source and compile every subject through the chain. Runs at
\ load, from inside the corpus's package.
: PUBLISH-ALL ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:PUBLISH ;] EACH ;

\ Judge every subject of this corpus: the bytes, then the answers, then the
\ times. The source is read again because the reader holds one file at a time
\ and other corpora are judged between the load that published these words and
\ this measurement.
: JUDGE ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:BYTES ;] EACH
   [: JUDGE-PASS:VALUE ;] EACH
   JUDGE-PASS:TIME-PASSES 0 ?do [: JUDGE-PASS:TIME ;] EACH loop
   JUDGE-PASS:FLOOR ;

;package

\ The corpus's package, open around the publication, so the derived words are
\ published where the corpus's own private storage is reachable and land beside
\ the words they are compared against. It is written out here because `package`
\ parses its operand: the package a corpus is judged in cannot be a parameter,
\ so it is a line of this file rather than a cell.
package CODEGEN-CORPUS4
public

JUDGE-CORPUS4:PUBLISH-ALL

;package
