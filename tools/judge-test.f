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
\   the refusals          are E-A64RA-SPILL on two named rows, E-NFEED-LITERAL on
\                         one, and NOTHING ELSE. The old comparison named its
\                         refused subjects on a list; here the code is checked,
\                         so a refusal that changed its reason would fail even
\                         while the row still read REFUSED, and a third
\                         capability gap cannot arrive unnamed
\   the derived text      is the corpus file's own program, checked on the rows
\                         whose bodies the chain refuses - the only rows whose
\                         text no compiled artifact can vouch for
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

\ A refusal code the artifact's own head names, with the dot it waits on. A code
\ that is neither is a capability gap nobody has written down, and the count
\ below is what makes that fail rather than read as one more REFUSED row.
: NAMED-CODE? ( n -- bool ) {: rc:n :}
   rc E-A64RA-SPILL = ;

: NAMED-REFUSALS ( -- n )
   0
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REFUSED? if
         i JUDGE-ROW:NEW-RC@ NAMED-CODE? if 1+ then
      then
   loop ;

: ROW-OF ( ptr u8 n -- n )
   JUDGE-ROW:FIND ;

: ARTIFACT-CASES ( -- )
   JUDGE-CHECK:DIFF-AT -1 T=
   JUDGE-ROW:LARGER-ROWS 0 T=
   JUDGE-ROW:DISAGREEING-ROWS 0 T= ;

\ ---- which WAY the artifact moved --------------------------------------------
\ The byte comparison above says the tree and the artifact are the same text. It
\ cannot say what a difference WOULD have been, and a chain row that grew and
\ one that shrank are two different events - a regression against ourselves and
\ progress. tools/judge/base.f adjudicates them apart and `--check` prints that
\ adjudication under its disagreement.
\
\ THE FIXTURE IS THE REAL MEASUREMENT WITH ONE NUMBER MOVED. The committed
\ artifact is the artifact, and the row that is shifted is a row of this run, so
\ what is compared is a whole real table against a whole real file differing in
\ exactly one cell. tools/judge/base-test.f attacks the reader itself with
\ artifacts built to fool it; what is asserted here is that it reads the REAL
\ one - forty-six rows, no malformed line, nothing to report - so a report that
\ changed the shape of a row is caught here rather than leaving those fixtures
\ agreeing with a format nothing writes any more.

: ADJUDICATE-COMMITTED ( -- n )
   JUDGE-CHECK:COMMITTED$ JUDGE-BASE:LOAD-FROM
   JUDGE-BASE:ADJUDICATE ;

: DIRECTION-CASES ( -- )
   JUDGE-BASE:QUIET!

   s" the committed artifact reads back as the rows this run measured" T-LABEL
   ADJUDICATE-COMMITTED 0 T=
   JUDGE-BASE:ROWS JUDGE-ROW:ROWS T=
   JUDGE-BASE:DECLARED@ JUDGE-ROW:ROWS T=
   JUDGE-BASE:MALFORMED@ 0 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=
   JUDGE-BASE:ENGINE-MOVES 0 T=
   JUDGE-BASE:LOST 0 T=
   JUDGE-BASE:GAINED 0 T=

   0 JUDGE-ROW:NEW-BYTES@ {: bytes:n :}
   0 JUDGE-ROW:NEW-TAIL? {: tail:bool :}

   s" a chain row bigger than the artifact's is a regression and a finding"
   T-LABEL
   0 bytes 4 + tail JUDGE-ROW:NEW!
   ADJUDICATE-COMMITTED 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=

   s" a chain row smaller than the artifact's is progress and is not" T-LABEL
   0 bytes 4 - tail JUDGE-ROW:NEW!
   ADJUDICATE-COMMITTED 0 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 1 T=

   0 bytes tail JUDGE-ROW:NEW!
   JUDGE-BASE:LOUD!

   s" and the row is back where the measurement left it" T-LABEL
   ADJUDICATE-COMMITTED 0 T= ;

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
   [: s" " FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   [: s" 7 7" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   s" 7" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" 8" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T<> ;

\ THE BOUNDARY INPUTS, THROUGH BOTH CODE GENERATORS. The pinned input a row is
\ timed on runs the longest path through it; these run the arithmetic off the
\ ends of the signed range, where a lost sign or a narrowed width is visible and
\ nowhere else is. Both columns compile the same corpus text, so the two must
\ answer the same cell.
: BOUNDARY-CASES ( -- )
   s" $8000000000000000" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" $8000000000000000" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T=
   s" $7FFFFFFFFFFFFFFF" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" $7FFFFFFFFFFFFFFF" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T=
   s" -1" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" -1" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T= ;

\ NO ROW OF THE TABLE IS REFUSED. The count is asserted at zero, and the balance
\ between it and the codes the artifact's head names is kept because a refusal
\ carrying some OTHER code would be a capability gap nobody has written down and
\ must not read as the one that closed.
\
\ THE THREE ROWS THAT WERE GAPS EACH KEEP A CASE saying so rather than simply
\ leaving the list. CODEGEN-CORPUS2:SYM-FOLD-C waited on a tape that could
\ record a hexadecimal literal (dot habu-record-the-engine-79c570ed).
\ CODEGEN-CORPUS4:CALL-PRESSURE carried the loop-spill code until the elaborator
\ learnt to leave a crossing local in a register the callee spares
\ (src/compiler/native/elaborate.f CALL-KEEPS?). CODEGEN-CORPUS4:PRESSURE-LOOP
\ was the last one of all and carried the same code until
\ src/compiler/native/loop.f moved the reads that cannot change with the turn out
\ of its body, so it never holds the fourteen values the allocator refused to
\ place.
\
\ THE CASES ARE WRITTEN AS "NOT REFUSED" AND NOT ONLY AS A COUNT, because a count
\ would not notice which row came back. A regression in any of the three puts its
\ row back among the refusals carrying the code this file still names, so the
\ balance would hold and nothing else here would see it.
: REFUSAL-CASES ( -- )
   JUDGE-ROW:REFUSED-ROWS 0 T=
   JUDGE-ROW:REFUSED-ROWS NAMED-REFUSALS T=
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ROW-OF JUDGE-ROW:REFUSED? TFALSE
   s" CODEGEN-CORPUS2:SYM-FOLD-C" ROW-OF JUDGE-ROW:REFUSED? TFALSE
   s" CODEGEN-CORPUS4:CALL-PRESSURE" ROW-OF JUDGE-ROW:REFUSED? TFALSE ;

\ A compiled row vouches for its own text: the chain read it, the checker
\ certified it and a routine came out. PRESSURE-LOOP keeps its case from the
\ years it had no such witness, because the corpus's own program is what the
\ whole row means and a body quietly narrowed to make it compile would be a
\ different program measured under the same name.
\ SYM-FOLD-C keeps its case now that it compiles, for the other half of the
\ argument its row used to carry in the head of the artifact: its constants are
\ written $41, $5A and $20, and a corpus quietly respelled in decimal to make a
\ row green would be a different program measured under the same name.
\ The source is loaded first because the reader holds ONE file at a time and
\ the pass that judged every corpus left the last of them in it.
: TEXT-CASES ( -- )
   s" tools/codegen-compare-corpus4.f" JUDGE-SRC:LOAD
   s" PRESSURE-LOOP" JUDGE-SRC:FIND s" -J4" JUDGE-SRC:TEXT$
   S\" : PRESSURE-LOOP-J4 ( ptr n n -- n ) {: base:ptr len:n :}\n   0\n   len 0 ?do\n      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @\n      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @\n      base 80 + @  base 88 + @  base 96 + @  base 104 + @\n      + + + + + + + + + + + + + +\n   loop ;" T$=

   s" tools/codegen-compare-corpus2.f" JUDGE-SRC:LOAD
   s" SYM-FOLD-C" JUDGE-SRC:FIND s" -J2" JUDGE-SRC:TEXT$
   S\" : SYM-FOLD-C-J2 ( n -- n ) {: c:n :}\n   c $41 < if c exit then\n   c $5A > if c exit then\n   c $20 or ;" T$= ;

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

\ ---- what a generated body did with what the subject left ---------------------
\ Three of the corpora hold a subject whose point is a STORE, and one holds a
\ subject that answers a flag while two hold subjects that answer a double.
\ Every one of those is a value a generated body has to project or read back
\ before two columns can be compared on it, so the projection is pinned here on
\ the numbers themselves rather than left to the columns agreeing about zero.

: WITNESS-CASES ( -- )
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:WITNESSED? TTRUE
   s" CODEGEN-CORPUS:ADD3" ROW-OF JUDGE-ROW:WITNESSED? TFALSE

   \ CELL-BUMP stores its argument, reads it back and stores that plus one, so
   \ on the pinned 7 the cell holds 8 and the answer is the same 8. Folded
   \ together the two would be zero whatever either was; apart, each says what it
   \ measured.
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:OLD-VALUE@ 8 T=
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:OLD-WITNESS@ 8 T=
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:NEW-WITNESS@ 8 T=

   \ VEC-COPY-CELLS and T-SGD! leave nothing at all, so the witness is the whole
   \ of what their rows compare. A zero there would be the reader never running.
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" ROW-OF JUDGE-ROW:OLD-WITNESS@ 0 T<>
   s" CODEGEN-CORPUS3:T-SGD!" ROW-OF JUDGE-ROW:OLD-WITNESS@ 0 T<> ;

\ A flag is not a number and a double is not a cell until each is projected, and
\ both projections are the ones the comparison already records that kind by.
: PROJECTION-CASES ( -- )
   \ WS? on a space answers true, which is 1 through the shared flag projection.
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:OLD-VALUE@ 1 T=
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:NEW-VALUE@ 1 T=

   \ T-SUM over -2.5 0.0 1.5 0.25 is -0.75, whose cell is $BFE8000000000000.
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:OLD-VALUE@
      -0.75 CODEGEN-COMPARE:REAL-BITS T=
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:NEW-VALUE@
      -0.75 CODEGEN-COMPARE:REAL-BITS T=

   \ And the generator refuses what it cannot project or read back rather than
   \ emitting a body that reaches past the top of the stack or records the
   \ subject's own answer under a second name.
   [: 2 JUDGE-COST:P-REAL JUDGE-COST:FOLD+ ;] E-JUDGE-COST-FOLD TTHROWSQ
   [: 2 JUDGE-COST:P-FLAG JUDGE-COST:FOLD+ ;] E-JUDGE-COST-FOLD TTHROWSQ
   [: s" 7" FAN$ s" " 1 JUDGE-COST:WITNESS drop ;] E-JUDGE-COST-WITNESS TTHROWSQ ;

\ ---- the C call shape a reference row is reached through ----------------------
\ A twin that takes or answers a double has a shape the subject's arity cannot
\ give, so the row states it - and a spelling nobody can read an answer kind off
\ is refused rather than guessed at. Run last, because opening a corpus here
\ moves the reader and the chain's suffix.

: SHAPE-CASES ( -- )
   s" tools/codegen-compare-corpus4.f" s" -JS" s" CODEGEN-CORPUS4:"
      JUDGE-PASS:CORPUS!
   [: s" LADDER" s" hc4_ladder" s" IIZ" JUDGE-PASS:ROW-ABI! ;]
      E-JUDGE-PASS-SHAPE TTHROWSQ
   [: s" LADDER" s" hc4_ladder" s" " JUDGE-PASS:ROW-ABI! ;]
      E-JUDGE-PASS-SHAPE TTHROWSQ
   [: s" NO-SUCH-SUBJECT" s" hc4_ladder" JUDGE-PASS:ROW! ;]
      E-JUDGE-SRC-ROW TTHROWSQ
   s" LADDER" s" hc4_ladder" s" IID" JUDGE-PASS:ROW-ABI!
   JUDGE-PASS:NAME$ s" CODEGEN-CORPUS4:LADDER" T$= ;

public

\ Where corpus 1's derived cell-stepping word records what the chain answered.
: BUMP-RC! ( n -- )
   BUMP-RC ! ;

: RUN ( -- )
   T-RESET
   JUDGE-CHECK:JUDGE-ALL
   ARTIFACT-CASES
   DIRECTION-CASES
   REFUSAL-CASES
   WITNESS-CASES
   PROJECTION-CASES
   TEXT-CASES
   WRONG-COLUMN-CASES
   WRONG-INPUT-CASES
   BOUNDARY-CASES
   STORAGE-CASES
   SHAPE-CASES
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
