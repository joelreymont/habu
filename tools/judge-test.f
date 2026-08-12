\ judge-test.f - the scheduled half of the code generator judge.
\ Run: bin/hb --load tools/judge-test.f
\
\ WHAT IT ASSERTS, AND WHY IT IS THE SAME WORDS THE COMMAND RUNS. Everything
\ below goes through tools/judge/check.f, which is what
\ `bin/hb --load tools/judge.f -- --check` drives, so a green run here is the
\ command passing rather than a second implementation of it agreeing with
\ itself.
\
\   the artifact          this tree's judgement and the committed file are the
\                         same bytes
\   no larger row         no subject the chain compiled into more bytes than the
\                         engine's emitter wrote for the same body
\   the two refusals      are E-A64RA-SPILL and nothing else. The old comparison
\                         named these two subjects on a list; here the code is
\                         checked, so a refusal that changed its reason would
\                         fail even while the row still read REFUSED
\   the derived text      is the corpus file's own program, checked on the row
\                         whose body the chain refuses - the one row whose text
\                         no compiled artifact can vouch for
\
\ NOTHING HERE READS A CLOCK. Every column of the artifact is exact: a byte
\ count off a dictionary record, a refusal code from the compiler, and a verdict
\ that follows from the two. That is what lets this run in a parallel group
\ beside the other codegen members.

require lib/errors.f
require lib/string.f
require lib/test.f
require src/compiler/native/dict.f
require tools/codegen-compare-corpus.f
require tools/judge/check.f

package JUDGE-TEST

private

: SPILL-ROWS ( -- n )
   0
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REFUSED? if
         i JUDGE-ROW:NEW-RC@ E-A64RA-SPILL = if 1+ then
      then
   loop ;

: ROW-OF ( ptr u8 n -- n )
   JUDGE-ROW:FIND ;

: ARTIFACT-CASES ( -- )
   JUDGE-CHECK:DIFF-AT -1 T=
   JUDGE-ROW:LARGER-ROWS 0 T=
   JUDGE-ROW:DISAGREEING-ROWS 0 T= ;

\ ---- the generated bodies, attacked ------------------------------------------
\ tools/judge/cost.f builds a body out of a row's input text and one column's
\ word. A body that compiles is not a body that measures the row, so every way
\ of getting one wrong is checked here against the shipped generator.

: FAN$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:CALL-FAN" ;

: FAN-J$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:CALL-FAN-J4" ;

: FAN-ENTRY ( -- n )
   FAN$ NDICT:CALL-TARGET ;

: FAN-J-ENTRY ( -- n )
   FAN-J$ NDICT:CALL-TARGET ;

\ THE ONE A COMPARISON OF ANSWERS CANNOT SEE. A body built for the chain's
\ column that names the ENGINE's word is the right program in the wrong column:
\ it computes what the row computes, so every answer agrees, and the cost
\ reported for the chain is the engine's. The address is what catches it.
: WRONG-COLUMN-CASES ( -- )
   FAN-ENTRY 0 T<>
   FAN-J-ENTRY 0 T<>
   FAN-ENTRY FAN-J-ENTRY T<>
   [: FAN$ FAN-J-ENTRY JUDGE-COST:COLUMN-CK ;] E-JUDGE-COST-COLUMN TTHROWSQ
   [: FAN-J$ FAN-ENTRY JUDGE-COST:COLUMN-CK ;] E-JUDGE-COST-COLUMN TTHROWSQ
   [: s" CODEGEN-CORPUS4:NO-SUCH-WORD" FAN-ENTRY JUDGE-COST:COLUMN-CK ;]
      E-JUDGE-COST-COLUMN TTHROWSQ
   FAN$ FAN-ENTRY JUDGE-COST:COLUMN-CK
   FAN-J$ FAN-J-ENTRY JUDGE-COST:COLUMN-CK ;

\ AND THE ONES IT CAN. An input list one number short does not type, so the
\ checker declines the generated body rather than the run timing a program with
\ a stack it never had; an input list with the WRONG number computes something
\ else, which is what the answers column exists to notice.
: WRONG-INPUT-CASES ( -- )
   [: s" " FAN$ 1 JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   [: s" 7 7" FAN$ 1 JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   s" 7" FAN$ 1 JUDGE-COST:VALUE  s" 8" FAN$ 1 JUDGE-COST:VALUE T<> ;

\ THE BOUNDARY INPUTS, THROUGH BOTH CODE GENERATORS. The pinned input a row is
\ timed on runs the longest path through it; these run the arithmetic off the
\ ends of the signed range, where a lost sign or a narrowed width is visible and
\ nowhere else is. Both columns compile the same corpus text, so the two must
\ answer the same cell.
: BOUNDARY-CASES ( -- )
   s" $8000000000000000" FAN$ 1 JUDGE-COST:VALUE
   s" $8000000000000000" FAN-J$ 1 JUDGE-COST:VALUE T=
   s" $7FFFFFFFFFFFFFFF" FAN$ 1 JUDGE-COST:VALUE
   s" $7FFFFFFFFFFFFFFF" FAN-J$ 1 JUDGE-COST:VALUE T=
   s" -1" FAN$ 1 JUDGE-COST:VALUE
   s" -1" FAN-J$ 1 JUDGE-COST:VALUE T= ;

\ Every refusal in the table is the allocator declining to spill inside a loop,
\ which is dot habu-spill-from-a-4145325c. A refusal for some other reason is a
\ different finding and must not read as this one.
: REFUSAL-CASES ( -- )
   JUDGE-ROW:REFUSED-ROWS SPILL-ROWS T=
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ROW-OF JUDGE-ROW:NEW-RC@ E-A64RA-SPILL T=
   s" CODEGEN-CORPUS4:CALL-PRESSURE" ROW-OF JUDGE-ROW:NEW-RC@ E-A64RA-SPILL T= ;

\ A compiled row vouches for its own text: the chain read it, the checker
\ certified it and a routine came out. A REFUSED row has no such witness, so the
\ text the chain was handed is checked here against the corpus's own program.
\ The source is loaded first because the reader holds ONE file at a time and
\ the pass that judged every corpus left the last of them in it.
: TEXT-CASES ( -- )
   s" tools/codegen-compare-corpus4.f" JUDGE-SRC:LOAD
   s" PRESSURE-LOOP" JUDGE-SRC:FIND s" -J4" JUDGE-SRC:TEXT$
   S\" : PRESSURE-LOOP-J4 ( ptr n n -- n ) {: base:ptr len:n :}\n   0\n   len 0 ?do\n      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @\n      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @\n      base 80 + @  base 88 + @  base 96 + @  base 104 + @\n      + + + + + + + + + + + + + +\n   loop ;" T$= ;

\ ---- the storage entry, on real corpus data ----------------------------------
\ A body that names one of its file's storage words needs the migration entry
\ that can express storage. Measured rather than described: corpus 1's
\ CELL-BUMP, which WRITES its cell, is refused with E-A64RAV-DKEEP under the
\ plain entry and compiles under this one; corpus 2's FILL-COPY names TWO and
\ is refused here, because the entry takes one spelling and picking the first
\ would compile a body whose other cell resolved to whatever the scope held.
\
\ THE PUBLICATION IS AT THE FOOT OF THIS FILE, inside corpus 1's own package,
\ because a migration publishes where the interpreter's wordlist points and
\ BUMP-CELL is private to that corpus. What is asserted here is what that line
\ left behind.

variable BUMP-RC

: STORAGE-CASES ( -- )
   BUMP-RC @ 0 T=
   s" tools/codegen-compare-corpus.f" JUDGE-SRC:LOAD
   s" CODEGEN-CORPUS:" JUDGE-CHAIN:QUALIFIER!
   s" -JT1" JUDGE-CHAIN:SUFFIX!
   s" CELL-BUMP" JUDGE-SRC:FIND JUDGE-CHAIN:SIZE 0 T<>

   s" tools/codegen-compare-corpus2.f" JUDGE-SRC:LOAD
   s" -JT2" JUDGE-CHAIN:SUFFIX!
   s" FILL-COPY" JUDGE-SRC:FIND JUDGE-CHAIN:PUBLISH E-JUDGE-CHAIN-DATA T= ;

public

\ Where corpus 1's derived cell-stepping word records what the chain answered.
: BUMP-RC! ( n -- )
   BUMP-RC ! ;

: RUN ( -- )
   T-RESET
   JUDGE-CHECK:JUDGE-ALL
   ARTIFACT-CASES
   REFUSAL-CASES
   TEXT-CASES
   WRONG-COLUMN-CASES
   WRONG-INPUT-CASES
   BOUNDARY-CASES
   STORAGE-CASES
   T-REPORT ;

;package

\ Corpus 1's CELL-BUMP through the storage entry, published where its private
\ cell is reachable. The line is here rather than inside a word because
\ `package` is read at load and a migration lands where the interpreter points.
package JUDGE-TEST
public
: PUBLISH-BUMP ( -- )
   s" CODEGEN-CORPUS:" JUDGE-CHAIN:QUALIFIER!
   s" -JT1" JUDGE-CHAIN:SUFFIX!
   s" tools/codegen-compare-corpus.f" JUDGE-SRC:LOAD
   s" CELL-BUMP" JUDGE-SRC:FIND JUDGE-CHAIN:PUBLISH-CALLING JUDGE-TEST:BUMP-RC! ;
;package

package CODEGEN-CORPUS
public
JUDGE-TEST:PUBLISH-BUMP
;package

JUDGE-TEST:RUN
