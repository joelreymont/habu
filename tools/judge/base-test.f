\ judge/base-test.f - the artifact reader that says which way a column moved,
\ attacked. Run: bin/hb --load tools/judge/base-test.f
\
\ WHAT IS UNDER TEST. tools/judge/base.f reads a committed judge artifact back
\ as ROWS and adjudicates each column against what a run measured: the chain
\ bigger is a regression, smaller is progress, the engine moved either way is a
\ finding, and a row on one side and not the other is a finding. Nothing here
\ compiles a corpus - the adjudication is arithmetic over a table and a text, so
\ the tables are stated here and the artifacts are rendered from them by the
\ report the command writes.
\
\ EVERY WORD EXERCISED IS PRODUCTION'S. The table is JUDGE-ROW, the artifact is
\ JUDGE-REPORT:TEXT$ - the same text `bin/hb --load tools/judge.f` prints - and
\ the reading and adjudication are JUDGE-BASE:LOAD-FROM and
\ JUDGE-BASE:ADJUDICATE, which is what `--check` drives on a disagreement.
\ tools/judge-test.f holds the other half of this claim: that the reader parses
\ the REAL forty-six-row artifact, so a renderer that changed shape is caught
\ there rather than leaving these fixtures agreeing with a format nothing
\ writes any more.
\
\ THE FIXTURES ARE BUILT TO FOOL A READER THAT SEARCHED FOR TEXT. The subject's
\ name written inside a sentence that also carries its numbers, and a row-shaped
\ line under the marker where the costs are. A reader that matched substrings,
\ or that read past the marker, fails one of them.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require tools/judge/row.f
require tools/judge/report.f
require tools/judge/base.f

package JUDGE-BASE-TEST

private

$4000 constant FIX-CAP
FIX-CAP BUFFER: FIX
variable FIX-U

: FIX-RESET ( -- )
   0 FIX-U ! ;

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-JUDGE-BASE-CAP throw then
   a  FIX FIX-U @ +  u STR-LEN BYTE-COPY-LEN
   FIX-U @ u + FIX-U ! ;

: FIX$ ( -- ptr u8 n )
   FIX FIX-U @ ;

\ Every fixture below ends in the marker line the artifact really carries, taken
\ from the report rather than retyped: a fixture whose marker did not match
\ would be refused before a single row was read, and the refusal would look like
\ a finding about the rows.
: FIX-END ( -- )
   S\" \n" FIX+
   JUDGE-REPORT:MARK-TEXT$ FIX+
   S\" \n" FIX+ ;

\ ---- a table stated here, and the artifact the report renders from it --------

: TWO-ROW-TABLE ( -- )
   JUDGE-ROW:RESET
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:OPEN {: a:n :}
   a 100 JUDGE-ROW:OLD!
   a 40 false JUDGE-ROW:NEW!
   a 12 JUDGE-ROW:REF!
   a 9 JUDGE-ROW:OLD-TRAFFIC!
   a 3 JUDGE-ROW:NEW-TRAFFIC!
   s" CODEGEN-CORPUSX:TWO" JUDGE-ROW:OPEN {: b:n :}
   b 80 JUDGE-ROW:OLD!
   b 20 false JUDGE-ROW:NEW!
   b 8 JUDGE-ROW:REF!
   b 7 JUDGE-ROW:OLD-TRAFFIC!
   b 2 JUDGE-ROW:NEW-TRAFFIC! ;

: ONE-ROW-TABLE ( -- )
   JUDGE-ROW:RESET
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:OPEN {: a:n :}
   a 100 JUDGE-ROW:OLD!
   a 40 false JUDGE-ROW:NEW!
   a 12 JUDGE-ROW:REF!
   a 9 JUDGE-ROW:OLD-TRAFFIC!
   a 3 JUDGE-ROW:NEW-TRAFFIC! ;

\ The artifact this table renders to, kept so the table can then be changed and
\ the two held against each other.
: SNAPSHOT ( -- )
   FIX-RESET
   JUDGE-REPORT:TEXT$ FIX+ ;

: READ-BACK ( -- n )
   FIX$ JUDGE-BASE:LOAD-FROM
   JUDGE-BASE:ADJUDICATE ;

\ ---- the round trip ---------------------------------------------------------

: ROUND-TRIP-CASES ( -- )
   s" an artifact rendered from a table agrees with that table" T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   READ-BACK 0 T=
   JUDGE-BASE:ROWS 2 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=

   s" and the cells it read back are the cells the table holds" T-LABEL
   s" CODEGEN-CORPUSX:ONE" JUDGE-BASE:FIND {: k:n :}
   k 0 >= TTRUE
   k JUDGE-BASE:OLD@ 100 T=
   k JUDGE-BASE:CHAIN@ 40 T=
   k JUDGE-BASE:DS-OLD@ 9 T=
   k JUDGE-BASE:DS-NEW@ 3 T=
   k JUDGE-BASE:REFUSED@ TFALSE ;

\ ---- the two directions, which is the whole point ---------------------------

: DIRECTION-CASES ( -- )
   s" a chain row that GREW against the artifact is a regression and a finding"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 44 false JUDGE-ROW:NEW!
   READ-BACK 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=

   s" a chain row that SHRANK is progress and is NOT a finding" T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 36 false JUDGE-ROW:NEW!
   READ-BACK 0 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 1 T=

   s" a row that STARTED being refused lost a capability and is a regression"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND E-A64RA-SPILL JUDGE-ROW:REFUSED!
   READ-BACK 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=

   s" a row that STOPPED being refused gained one and is not a finding" T-LABEL
   TWO-ROW-TABLE
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND E-A64RA-SPILL JUDGE-ROW:REFUSED!
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 40 false JUDGE-ROW:NEW!
   READ-BACK 0 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 1 T=

   s" a refusal whose CODE changed is a different gap and is a regression"
   T-LABEL
   TWO-ROW-TABLE
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND E-A64RA-SPILL JUDGE-ROW:REFUSED!
   SNAPSHOT
   TWO-ROW-TABLE
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND E-NFEED-LITERAL JUDGE-ROW:REFUSED!
   READ-BACK 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=

   s" the ENGINE column moved, and either direction of that is a finding"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 96 JUDGE-ROW:OLD!
   READ-BACK 1 T=
   JUDGE-BASE:ENGINE-MOVES 1 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 104 JUDGE-ROW:OLD!
   READ-BACK 1 T=
   JUDGE-BASE:ENGINE-MOVES 1 T=

   s" the reference column moved and that is a fact about a host, not a finding"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 16 JUDGE-ROW:REF!
   READ-BACK 0 T=

   s" the chain touching the caller's stack MORE often is a regression too"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 5 JUDGE-ROW:NEW-TRAFFIC!
   READ-BACK 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=

   s" and touching it less often is progress and is not a finding" T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 1 JUDGE-ROW:NEW-TRAFFIC!
   READ-BACK 0 T=
   JUDGE-BASE:IMPROVEMENTS 1 T=

   s" and the engine's own traffic moving is a finding either way" T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   s" CODEGEN-CORPUSX:ONE" JUDGE-ROW:FIND 8 JUDGE-ROW:OLD-TRAFFIC!
   READ-BACK 1 T=
   JUDGE-BASE:ENGINE-MOVES 1 T= ;

\ ---- a row on one side and not the other ------------------------------------

: MEMBERSHIP-CASES ( -- )
   s" a row the artifact carries that this run did not measure is a finding"
   T-LABEL
   TWO-ROW-TABLE
   SNAPSHOT
   ONE-ROW-TABLE
   READ-BACK 1 T=
   JUDGE-BASE:LOST 1 T=

   s" a row measured that the artifact has no row for is a finding" T-LABEL
   ONE-ROW-TABLE
   SNAPSHOT
   TWO-ROW-TABLE
   READ-BACK 1 T=
   JUDGE-BASE:GAINED 1 T= ;

\ ---- fixtures built to fool a reader that searched for text ------------------
\ Each of these is a whole artifact, handed to the word `--check` hands the
\ committed file to.

: HEAD$ ( -- ptr u8 n )
   S\" habu code generator judge\n=========================\n\nword                                old  chain  clang  ds-old  ds-new  verdict\n--------------------------------  -----  -----  -----  ------  ------  -------\n" ;

: ONE-TRUE-ROW$ ( -- ptr u8 n )
   S\" CODEGEN-CORPUSX:ONE                 100     40     12       9       3  smaller\n" ;

\ Every fixture carries this line, and no fixture asserts anything about it:
\ what it is here for is that `rows:` ends in its colon, so a reader that let it
\ open a row would answer one row too many on every case below.
: TALLY-1$ ( -- ptr u8 n )
   S\" \nrows: 1, refused by the chain: 0, larger than the engine's: 0\n" ;

: FOOL-CASES ( -- )
   ONE-ROW-TABLE

   \ A sentence that names the subject and then carries a WHOLE ROW's fields
   \ behind it. A reader that searched the line for its first qualified token
   \ instead of taking the line's first word would build a second row here, and
   \ the table would answer about one of two programs under one name.
   s" a subject named inside a sentence is prose and not a row" T-LABEL
   FIX-RESET
   HEAD$ FIX+
   S\" the row CODEGEN-CORPUSX:ONE 100 40 12 9 3 smaller was measured today\n"
      FIX+
   ONE-TRUE-ROW$ FIX+
   TALLY-1$ FIX+
   FIX-END
   READ-BACK 0 T=
   JUDGE-BASE:ROWS 1 T=

   \ A line that opens with a subject name and then runs out of fields. It
   \ stores no row, and the point is that this cannot go quiet: the subject this
   \ run measured has no partner in the artifact and is reported as a row the
   \ artifact does not have.
   s" a line that opens with a subject name and lacks a field stores no row"
   T-LABEL
   FIX-RESET
   HEAD$ FIX+
   S\" CODEGEN-CORPUSX:ONE                 100     40     12  smaller\n" FIX+
   TALLY-1$ FIX+
   FIX-END
   READ-BACK 1 T=
   JUDGE-BASE:ROWS 0 T=
   JUDGE-BASE:GAINED 1 T=

   s" and a file with no marker line is refused, never read to its end" T-LABEL
   FIX-RESET
   HEAD$ FIX+
   ONE-TRUE-ROW$ FIX+
   TALLY-1$ FIX+
   [: FIX$ JUDGE-BASE:LOAD-FROM ;] E-JUDGE-CHECK-MARK TTHROWSQ

   \ The costs under the marker are nanoseconds, and a reader that walked past
   \ the marker would meet a line whose first word is a subject name and whose
   \ second is `1.69`. It must never reach them.
   s" nothing under the marker line is read as a row" T-LABEL
   FIX-RESET
   HEAD$ FIX+
   ONE-TRUE-ROW$ FIX+
   TALLY-1$ FIX+
   FIX-END
   S\" CODEGEN-CORPUSX:ONE                  1.69     0.30     0.11     0.02     0.01\n" FIX+
   READ-BACK 0 T=
   JUDGE-BASE:ROWS 1 T= ;

public

: MAIN ( -- )
   T-RESET
   JUDGE-BASE:QUIET!
   ROUND-TRIP-CASES
   DIRECTION-CASES
   MEMBERSHIP-CASES
   FOOL-CASES
   JUDGE-BASE:LOUD!
   T-REPORT ;

;package

JUDGE-BASE-TEST:MAIN
