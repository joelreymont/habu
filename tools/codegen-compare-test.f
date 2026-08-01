\ codegen-compare-test.f - tests for the code generator comparison harness.
\ Run: bin/hb --load tools/codegen-compare-test.f
\
\ The harness's job is to notice when the compiled code changes, so the tests
\ are mostly attempts to make it fail to notice. Each one writes a baseline file
\ that is wrong in one specific way and checks that the comparison reports
\ exactly that, and the tests that matter most are the two the dot asks for:
\
\   a corrupted number in the committed table must be reported, and
\   a word that is deliberately made slower must move the timing column far
\   enough that the comparison reports it.
\
\ The fixtures are built from a real measurement of two real corpus words, so
\ the sizes and outputs they carry are the ones the engine actually produced.
\ Only the specific corruption under test is invented.
\
\ The new code generator's column is checked two ways, because the two questions
\ are different. The comparison logic is checked on rows built to fool it: a new
\ row whose recorded answers are the old row's answers must read as agreement,
\ and the same row with one value moved by one must read as a disagreement and
\ be counted. Those rows are built from a real measurement, so only the moved
\ value is invented, and they prove the thing a comparison harness must never get
\ wrong - reporting a wrong answer as a match.
\
\ The coverage claim is then checked on the real run: every corpus word is either
\ compiled by the new chain or named as a gap with the capabilities it waits for,
\ never both and never neither, no new row disagrees with its old row, and every
\ word the chain did compile came out smaller than the old emitter made it. Those
\ are assertions about the store the real pass left behind, computed here from
\ the public readers rather than by calling the harness's own check.
\
\ Between the two, the file runs CODEGEN-COMPARE-CLI:CHECK-EXACT against the
\ committed baseline in the repository: the same measurement pass, the same
\ report, the same baseline load and the same verdict that
\ `bin/hb --load tools/codegen-compare.f` runs, over the same shared body, with
\ the cost column and the pass budget left out. It ends the process with a
\ non-zero status if the committed table and the live compiler disagree, or if
\ the two code generators do.
\
\ WHY THIS FILE LEAVES THE TIMINGS OUT, AND WHERE THEY ARE STILL CHECKED. This
\ file is scheduled: it runs in the resident stdlib/tail-pure fork group and in
\ the spawned stdlib gate, both of which keep every core busy. A cost is the one
\ column that is a measurement rather than a fact about the compiled code, and it
\ is compared with a number recorded on an idle machine - measured, with the
\ numbers, at the head of tools/codegen-compare-baseline.f, where eight competing
\ processes per core left two per cent of the tolerance band and sixteen went
\ through it. A scheduled run that can fail for host load is worse than no
\ scheduled run at all, so the timings against the committed table are checked by
\ hand with `bin/hb --load tools/codegen-compare.f`, and the run says out loud
\ which comparison it did not make.
\
\ The timing column is still exercised here, and where the answer cannot turn on
\ host load: the deliberately slowed word below is measured and compared against
\ a baseline written from another measurement in the same pass, so both sides of
\ that comparison meet the same busy machine. Those cases prove the cost column
\ reports a real slowdown and reports nothing at normal speed. The cases after
\ them prove that leaving the cost column unchecked drops that comparison and
\ nothing else: a wrong size, a wrong output value and a missing row are all
\ still reported with the timings left out.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cli.f
require tools/codegen-compare-new.f
require tools/codegen-compare-report.f

package CODEGEN-COMPARE-TEST

private

$1000 constant FIX-CAP
256 constant PATH-CAP
32 constant SPACE-BYTE
10 constant NEWLINE-BYTE

\ How many extra calls the deliberately slowed word makes. The comparison's
\ tolerance is CODEGEN-COMPARE:COST-BAND (four times slower); twenty extra calls
\ put the slowed word about twenty times slower, so the finding cannot be an
\ accident of host noise.
20 constant SLOW-REPEATS

FIX-CAP BUFFER: FIX-TEXT
variable FIX-U
PATH-CAP BUFFER: PATH-BUF
variable PATH-U
PATH-CAP BUFFER: DIR-BUF
variable DIR-U
variable BAD-OUTPUT               \ index of the output to corrupt, -1 for none

\ ---- the deliberately slowed word ------------------------------------------

\ Same answer as CODEGEN-CORPUS:ADD3, reached the slow way. It is measured under
\ ADD3's name, so its recorded size is ADD3's real size and the only column that
\ can move is the timing one.
: SLOW-ADD3 ( n n n -- n )
   CODEGEN-CORPUS:ADD3
   SLOW-REPEATS 0 ?do
      1 2 3 CODEGEN-CORPUS:ADD3 drop
   loop ;

\ ---- fixture text ----------------------------------------------------------

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a FIX-TEXT FIX-U @ + u STR-LEN BYTE-COPY-LEN
   FIX-U @ u + FIX-U ! ;

: FIX-C ( n -- ) {: c:n :}
   FIX-U @ 1+ FIX-CAP > if E-CODEGEN-COMPARE-CAP throw then
   c FIX-TEXT FIX-U @ + c!
   FIX-U @ 1+ FIX-U ! ;

: FIX-NL ( -- )
   NEWLINE-BYTE FIX-C ;

: FIX-SP ( -- )
   SPACE-BYTE FIX-C ;

: FIX-NUM ( n -- )
   SB-RESET FMT:SB-INT SB$ FIX+ ;

: FIX-LINE ( ptr u8 n -- )
   FIX+ FIX-NL ;

: FIX-RESET ( -- )
   0 FIX-U !
   -1 BAD-OUTPUT ! ;

: FIX-DECLARED ( n -- )
   s" rows: " FIX+ FIX-NUM FIX-NL ;

: FIX-OUTPUT ( n n -- ) {: k:n j:n :}
   FIX-SP
   k j CODEGEN-COMPARE:OUTPUT
   j BAD-OUTPUT @ = if 1+ then
   FIX-NUM ;

: FIX-OUTPUTS ( n -- ) {: k:n :}
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      dup k swap FIX-OUTPUT
      1+
   repeat drop ;

\ One data row for measured row k, written with the size and cost the caller
\ chooses so a test can put a wrong number in exactly one column.
: FIX-ROW ( n n n -- ) {: k:n size:n cost:n :}
   CODEGEN-COMPARE:PATH-OLD$ FIX+
   FIX-SP k CODEGEN-COMPARE:NAME$ FIX+
   FIX-SP size FIX-NUM
   FIX-SP cost FIX-NUM
   k FIX-OUTPUTS
   FIX-NL ;

: FIX-TRUE-ROW ( n -- ) {: k:n :}
   k k CODEGEN-COMPARE:SIZE k CODEGEN-COMPARE:COST FIX-ROW ;

\ ---- paths -----------------------------------------------------------------

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a PATH-BUF u STR-LEN BYTE-COPY-LEN
   u PATH-U ! ;

: DIR! ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a DIR-BUF u STR-LEN BYTE-COPY-LEN
   u DIR-U ! ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: DIR$ ( -- ptr u8 n )
   DIR-BUF DIR-U @ ;

: PATH-PREFIX ( -- )
   SB-RESET DIR$ SB-APPEND s" /" SB-APPEND ;

: PATH-FOR ( ptr u8 n -- )
   PATH-PREFIX SB-APPEND SB$ PATH! ;

: FIX-WRITE ( -- )
   s" baseline.txt" PATH-FOR
   PATH$ FIX-TEXT FIX-U @ ATOMIC-WRITE-FILE ;

: FINDINGS-AT ( ptr u8 n -- n )
   CODEGEN-BASELINE:LOAD
   CODEGEN-BASELINE:COMPARE ;

: FIX-FINDINGS ( -- n )
   FIX-WRITE
   PATH$ FINDINGS-AT ;

\ ---- measurement -----------------------------------------------------------

: CALIBRATION-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP"
   [: CODEGEN-CORPUS:NOOP ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE
   CODEGEN-COMPARE:CALIBRATE ;

: HONEST-ADD3-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3"
   [: 1 2 3 CODEGEN-CORPUS:ADD3 drop ;]
   [: 1 2 3 CODEGEN-CORPUS:ADD3 CODEGEN-COMPARE:VECTOR
      -5 5 7 CODEGEN-CORPUS:ADD3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: SLOW-ADD3-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3"
   [: 1 2 3 SLOW-ADD3 drop ;]
   [: 1 2 3 SLOW-ADD3 CODEGEN-COMPARE:VECTOR
      -5 5 7 SLOW-ADD3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: MEASURE-HONEST ( -- )
   CODEGEN-COMPARE:RESET
   CODEGEN-COMPARE:PASS-BEGIN
   CALIBRATION-CASE
   HONEST-ADD3-CASE
   CODEGEN-COMPARE:PASS-END
   CODEGEN-COMPARE:NORMALIZE ;

: MEASURE-SLOW ( -- )
   CODEGEN-COMPARE:RESET
   CODEGEN-COMPARE:PASS-BEGIN
   CALIBRATION-CASE
   SLOW-ADD3-CASE
   CODEGEN-COMPARE:PASS-END
   CODEGEN-COMPARE:NORMALIZE ;

\ ---- fixtures --------------------------------------------------------------

: HONEST-FIXTURE ( -- )
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 FIX-TRUE-ROW ;

: TWO-ROW-CASES ( -- )
   s" an honest baseline reports nothing" T-LABEL
   HONEST-FIXTURE
   FIX-FINDINGS 0 T=

   s" one wrong size byte is reported" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 1 CODEGEN-COMPARE:SIZE 4 + 1 CODEGEN-COMPARE:COST FIX-ROW
   FIX-FINDINGS 1 T=

   s" one wrong output value is reported" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 BAD-OUTPUT !
   1 FIX-TRUE-ROW
   FIX-FINDINGS 1 T=

   s" rows in the other order are still the same rows" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   1 FIX-TRUE-ROW
   0 FIX-TRUE-ROW
   FIX-FINDINGS 0 T= ;

: STRUCTURE-CASES ( -- )
   s" a deleted row is reported as missing" T-LABEL
   FIX-RESET
   1 FIX-DECLARED
   0 FIX-TRUE-ROW
   FIX-FINDINGS 1 T=

   s" a row hidden inside a sentence is not a row" T-LABEL
   FIX-RESET
   1 FIX-DECLARED
   0 FIX-TRUE-ROW
   s" the missing row reads old CODEGEN-CORPUS:ADD3 72 1884 6 7 in full" FIX-LINE
   FIX-FINDINGS 1 T=

   s" the same row twice is reported" T-LABEL
   FIX-RESET
   3 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 FIX-TRUE-ROW
   1 FIX-TRUE-ROW
   FIX-FINDINGS 1 T=

   s" a wrong declared row count is reported" T-LABEL
   FIX-RESET
   5 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 FIX-TRUE-ROW
   FIX-FINDINGS 1 T=

   s" the size and cost columns swapped are reported twice" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 1 CODEGEN-COMPARE:COST 1 CODEGEN-COMPARE:SIZE FIX-ROW
   FIX-FINDINGS 2 T=

   s" a size that is not a number is reported, and its row is gone" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   s" old CODEGEN-CORPUS:ADD3 seventy-two 1884 6 7" FIX-LINE
   FIX-FINDINGS 3 T=

   s" a row for a word that was not measured is reported" T-LABEL
   FIX-RESET
   3 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 FIX-TRUE-ROW
   s" old CODEGEN-CORPUS:NOT-A-WORD 16 1000" FIX-LINE
   FIX-FINDINGS 1 T=

   s" a baseline file that does not exist is reported" T-LABEL
   s" no-such-baseline.txt" PATH-FOR
   PATH$ FINDINGS-AT 3 T= ;

\ The falsification the dot asks for: make one word genuinely slower and check
\ that the timing column moves far enough to be reported, then measure the same
\ word honestly again against the same baseline and check that it does not.
: SLOWDOWN-CASES ( -- )
   MEASURE-HONEST
   HONEST-FIXTURE
   FIX-WRITE

   s" a deliberately slowed word is reported as slower" T-LABEL
   MEASURE-SLOW
   PATH$ FINDINGS-AT 1 T=

   s" the same word at its normal speed is not" T-LABEL
   MEASURE-HONEST
   PATH$ FINDINGS-AT 0 T= ;

\ ---- the cost column unchecked ---------------------------------------------

\ What a scheduled run leaves out, and what it must still catch. The row under
\ test carries a cost of one, which no measured row can be within eight times of,
\ so a case that expects nothing is asking whether the cost column was consulted
\ at all, and a case that expects a finding is asking whether everything else
\ still is. The measurement they read is the honest one SLOWDOWN-CASES left
\ behind.
: WILD-COST-ROW ( n -- ) {: k:n :}
   k k CODEGEN-COMPARE:SIZE 1 FIX-ROW ;

: WILD-COST-FIXTURE ( -- )
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 WILD-COST-ROW ;

: COST-MODE-CASES ( -- )
   s" a cost far outside the band is reported while the cost column is checked" T-LABEL
   WILD-COST-FIXTURE
   FIX-FINDINGS 1 T=

   s" and is not reported once the cost column is unchecked" T-LABEL
   CODEGEN-BASELINE:COSTS-UNCHECKED!
   WILD-COST-FIXTURE
   FIX-FINDINGS 0 T=

   s" the deliberately slowed word is not reported either" T-LABEL
   HONEST-FIXTURE
   FIX-WRITE
   MEASURE-SLOW
   PATH$ FINDINGS-AT 0 T=

   s" but a wrong size byte still is" T-LABEL
   MEASURE-HONEST
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 1 CODEGEN-COMPARE:SIZE 4 + 1 FIX-ROW
   FIX-FINDINGS 1 T=

   s" and so does a wrong output value" T-LABEL
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW
   1 BAD-OUTPUT !
   1 WILD-COST-ROW
   FIX-FINDINGS 1 T=

   s" and so does a row the baseline is missing" T-LABEL
   FIX-RESET
   1 FIX-DECLARED
   0 FIX-TRUE-ROW
   FIX-FINDINGS 1 T=

   CODEGEN-BASELINE:COSTS-CHECKED! ;

\ ---- the new column's comparison, on rows built to fool it -----------------

\ The row indices MEASURE-HONEST leaves behind: the calibration call and the
\ corpus word whose answers the fixtures below echo.
0 constant OLD-NOOP-ROW
1 constant OLD-ADD3-ROW

\ The fixtures below build new rows whose ANSWERS are stated, so what they
\ exercise is the head-to-head comparison rather than a compiler. Their byte
\ counts are not stated: a new row's size comes off the record of the word that
\ carries the new chain's code, so these name the real migrated words and read
\ the real sizes, exactly as the production pass does.
: NEW-CALIBRATION-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

\ A new row that answers exactly what the old row answered.
: NEW-HONEST-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:ADD3-N"
   [: ;]
   [: OLD-ADD3-ROW 0 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR
      OLD-ADD3-ROW 1 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The same row with one answer moved by one.
: NEW-WRONG-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:ADD3-N"
   [: ;]
   [: OLD-ADD3-ROW 0 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR
      OLD-ADD3-ROW 1 CODEGEN-COMPARE:OUTPUT 1+ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ And one that answers a value short.
: NEW-SHORT-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:ADD3-N"
   [: ;]
   [: OLD-ADD3-ROW 0 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ typed-local-lint: allow-bare-local - build is the case body being measured.
: MEASURE-WITH ( [ -- ] -- ) {: build :}
   CODEGEN-COMPARE:RESET
   CODEGEN-NEW:RESET
   CODEGEN-COMPARE:PASS-BEGIN
   CALIBRATION-CASE
   HONEST-ADD3-CASE
   NEW-CALIBRATION-CASE
   build execute
   CODEGEN-COMPARE:PASS-END
   CODEGEN-COMPARE:NORMALIZE ;

\ The new row the fixtures above add is always the last one.
: LAST-ROW ( -- n )
   CODEGEN-COMPARE:ROWS 1- ;

: UNKNOWN-GAP ( -- )
   s" CODEGEN-CORPUS:NOT-A-WORD" CODEGEN--NEW-CAP:LOCALS CODEGEN-NEW:GAP ;

: NEW-COLUMN-CASES ( -- )
   s" a new row that answers what the old row answered reads as agreement" T-LABEL
   [: NEW-HONEST-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-NEW:ROW-MATCH? TTRUE
   CODEGEN-NEW:MISMATCHES 0 T=
   CODEGEN-REPORT:SAY-MISMATCHES 0 T=

   s" one answer moved by one is reported as a disagreement" T-LABEL
   [: NEW-WRONG-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-NEW:ROW-MATCH? TFALSE
   CODEGEN-NEW:MISMATCHES 1 T=

   s" a new row one answer short is reported too" T-LABEL
   [: NEW-SHORT-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-NEW:ROW-MATCH? TFALSE
   CODEGEN-NEW:MISMATCHES 1 T=

   s" a new row is compared with the old row of the same name, not with itself" T-LABEL
   [: NEW-WRONG-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-NEW:PARTNER OLD-ADD3-ROW T=

   s" naming a gap for a word the old column never measured is refused" T-LABEL
   [: UNKNOWN-GAP ;] E-CODEGEN-COMPARE-CORPUS TTHROWSQ ;

\ ---- what the real run left behind ------------------------------------------
\ Read back off the store the production path filled, so these say something
\ about the run that just happened rather than about a fixture.

: OLD-ROWS ( -- n )
   CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:ROWS-OF ;

: NEW-ROWS ( -- n )
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:ROWS-OF ;

: COMPILED? ( n -- bool ) {: k:n :}
   CODEGEN-COMPARE:PATH-NEW k CODEGEN-COMPARE:NAME$ CODEGEN-COMPARE:FIND-ROW 0 >= ;

: NAMED-GAP? ( n -- bool ) {: k:n :}
   false
   CODEGEN-NEW:GAPS 0 ?do
      i CODEGEN-NEW:GAP-NAME$ k CODEGEN-COMPARE:NAME$ STR= if drop true leave then
   loop ;

\ How many corpus words are accounted for by neither column - the one number
\ that says a word was quietly skipped.
: UNACCOUNTED ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i COMPILED? 0= i NAMED-GAP? 0= and if 1+ then
      then
   loop ;

\ And how many are claimed by both, which would double-count the corpus.
: DOUBLE-COUNTED ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i COMPILED? i NAMED-GAP? and if 1+ then
      then
   loop ;

: CAPLESS-GAPS ( -- n )
   0
   CODEGEN-NEW:GAPS 0 ?do
      i CODEGEN-NEW:GAP-CAPS@ 0= if 1+ then
   loop ;

\ Every word the new chain compiled, compared byte for byte with the old
\ emitter's answer for the same word.
: NOT-SMALLER ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i CODEGEN-NEW:PARTNER {: b:n :}
         b 0 < if 1+ else
            i CODEGEN-COMPARE:SIZE b CODEGEN-COMPARE:SIZE < 0= if 1+ then
         then
      then
   loop ;

: REAL-RUN-CASES ( -- )
   s" the real run measured the whole pinned corpus" T-LABEL
   OLD-ROWS 11 T=

   s" every corpus word is compiled by the new chain or named a gap" T-LABEL
   UNACCOUNTED 0 T=

   s" and no corpus word is claimed by both accounts" T-LABEL
   DOUBLE-COUNTED 0 T=

   s" the two accounts together are the whole corpus" T-LABEL
   NEW-ROWS CODEGEN-NEW:GAPS + OLD-ROWS T=

   s" every gap names at least one capability it is waiting for" T-LABEL
   CAPLESS-GAPS 0 T=

   s" every word the new chain compiled computes what the old emitter computes" T-LABEL
   CODEGEN-NEW:MISMATCHES 0 T=

   s" and every one of them is fewer bytes of machine code" T-LABEL
   NOT-SMALLER 0 T= ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-codegen-compare" TMPDIR-MKDIR
   2dup CLEANUP-TREE+
   DIR!
   CODEGEN-BASELINE:QUIET! ;

: MAIN ( -- )
   T-RESET
   SETUP
   MEASURE-HONEST
   TWO-ROW-CASES
   STRUCTURE-CASES
   SLOWDOWN-CASES
   COST-MODE-CASES
   NEW-COLUMN-CASES
   CLEANUP-RUN
   CODEGEN-BASELINE:LOUD!
   CODEGEN-COMPARE-CLI:CHECK-EXACT
   REAL-RUN-CASES
   T-REPORT ;

MAIN

;package
