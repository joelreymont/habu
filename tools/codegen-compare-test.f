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
\ The coverage claim is then checked on the real runs - ALL THREE of them,
\ because there are three pinned corpora and each is measured in a pass of its
\ own. Every
\ corpus word is either compiled by the new chain or named as a gap with the
\ capabilities it waits for, never both and never neither, and no new row
\ disagrees with its old row. What the two corpora do NOT share is the byte
\ result: every word of the first that the chain compiled came out smaller than
\ the old emitter made it, one word of the second - T-RES-WALK, the loop whose
\ test is a call - came out LARGER, and so did one of the fourth - CALL-FAN-BIG,
\ five sites over a callee only the chain copies. Both are pinned here by name,
\ so a change in either direction has to be looked at rather than absorbed. These
\ are assertions about the store a pass left behind, computed here from the
\ public readers rather than by calling the harness's own check.
\
\ THE BYTE COLUMNS ARE NOW CHECKED THE WAY THE OUTPUT COLUMNS ARE. A row the
\ chain compiles into more bytes than the engine's emitter wrote is a finding
\ that exits the run non-zero, unless it is one of the two rows
\ tools/codegen-compare-report.f declares a known loss with the dot that closes
\ it. BYTE-COLUMN-CASES below attacks that on fixtures - a smaller row, an equal
\ row, a larger row, and a known loss that has stopped losing - through the
\ production words, so what passes there is what a gate runs.
\
\ The second corpus's committed table is then attacked the way the first one is,
\ on fixtures built from the whole of its old column: a byte count moved by one
\ instruction, on an ordinary row and on a GAP row, a wrong output value, and a
\ missing row. A comparison that read the second table against the first
\ corpus's rows would pass every earlier case in this file and fail those.
\
\ The third corpus is the float benchmark, and its account is all gaps: ten
\ corpus words, ten declarations naming the float capability, and a new column
\ that is nothing but the calibration call. Its table is attacked the same way,
\ with two cases the other two tables cannot ask for - a NaN recorded one bit
\ out and a negative zero written as a positive one - because a float row's
\ outputs are the CELLS the words left and that is the equality the harness
\ compares. Three of its assertions are about the corpus rather than the
\ comparison: that the pinned sum distinguishes one evaluation order from
\ another, that the two zeros stay two values, and that the NaN two different
\ words produce is one cell.
\
\ At the end the file runs CODEGEN-COMPARE-CLI:CHECK-EXACT against the committed
\ baselines in the repository: the same measurement passes, the same reports,
\ the same baseline loads and the same verdict that
\ `bin/hb --load tools/codegen-compare.f` runs, over the same shared body, with
\ the cost column and the pass budget left out. It ends the process with a
\ non-zero status if a committed table and the live compiler disagree, or if the
\ two code generators do.
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
\ NO ASSERTION THIS FILE SCHEDULES READS ONE MEASUREMENT AGAINST ANOTHER, and
\ that is what makes it safe to schedule. The cost column's arithmetic is still
\ checked here, on cases of two kinds, neither of which can flip on host load.
\ Either the fixture carries the row's OWN recorded cost, so the two sides of
\ the comparison are one number; or it carries a STATED number - one, or a row's
\ byte count - which a measured cost would have to come within a factor of eight
\ of to pass, and cannot: a cost is a ratio to the empty call timed beside it in
\ the same pass, 1000 meaning "as expensive as that call", so a row at cost 8
\ would be a hundred and twenty-fifth of the price of calling an empty word.
\ Between them those pin both directions of the tolerance band.
\
\ What used to sit beside them was a genuinely slower word measured against a
\ genuinely honest one, and that comparison has both feet on a wall clock. It is
\ in tools/codegen-compare-timed-test.f now, with the cost-direction assertions
\ that used to be in the corpus sections below (dot
\ habu-sweep-timing-assertions-2282630b). The cases that remain prove that
\ leaving the cost column unchecked drops that comparison and nothing else: a
\ wrong size, a wrong output value and a missing row are all still reported with
\ the timings left out.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cli.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-cases.f
require tools/codegen-compare-cases2.f
require tools/codegen-compare-cases3.f
require tools/codegen-compare-cases4.f
require tools/codegen-compare-report.f
require src/compiler/native/migrate.f

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
variable BAD-ROW                  \ which measured row to write wrongly, -1 for none
variable BAD-OUTPUT               \ index of the output to corrupt in it, -1 for none

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
   -1 BAD-ROW !
   -1 BAD-OUTPUT ! ;

: FIX-DECLARED ( n -- )
   s" rows: " FIX+ FIX-NUM FIX-NL ;

: FIX-OUTPUT ( n n -- ) {: k:n j:n :}
   FIX-SP
   k j CODEGEN-COMPARE:OUTPUT
   k BAD-ROW @ =  j BAD-OUTPUT @ =  and if 1+ then
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

\ Two two-row passes over the production pass machinery: the calibration row
\ every corpus shares, and one corpus word measured honestly or the slow way.
: MEASURE-HONEST ( -- )
   [: CODEGEN-CALIBRATE:OLD HONEST-ADD3-CASE ;] [: ;] CODEGEN-COMPARE:PASS ;

: MEASURE-SLOW ( -- )
   [: CODEGEN-CALIBRATE:OLD SLOW-ADD3-CASE ;] [: ;] CODEGEN-COMPARE:PASS ;

\ ---- fixtures --------------------------------------------------------------

\ The head of an honest two-row table: the declared count and the calibration
\ row written truly, with the second row left for the case under test to write
\ however it needs to. Eight cases below open this way and differ only in what
\ they put in the second row.
: FIRST-ROW-ONLY ( -- )
   FIX-RESET
   2 FIX-DECLARED
   0 FIX-TRUE-ROW ;

\ The same for a table that declares ONE row, which is how a case makes the
\ table miss a row it should have.
: FIRST-ROW-ALONE ( -- )
   FIX-RESET
   1 FIX-DECLARED
   0 FIX-TRUE-ROW ;

\ The whole two-row table, written the way the harness itself would write it.
: HONEST-FIXTURE ( -- )
   FIRST-ROW-ONLY
   1 FIX-TRUE-ROW ;

: TWO-ROW-CASES ( -- )
   s" an honest baseline reports nothing" T-LABEL
   HONEST-FIXTURE
   FIX-FINDINGS 0 T=

   s" one wrong size byte is reported" T-LABEL
   FIRST-ROW-ONLY
   1 1 CODEGEN-COMPARE:SIZE 4 + 1 CODEGEN-COMPARE:COST FIX-ROW
   FIX-FINDINGS 1 T=

   s" one wrong output value is reported" T-LABEL
   FIRST-ROW-ONLY
   1 BAD-ROW ! 1 BAD-OUTPUT !
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
   FIRST-ROW-ALONE
   FIX-FINDINGS 1 T=

   s" a row hidden inside a sentence is not a row" T-LABEL
   FIRST-ROW-ALONE
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
   FIRST-ROW-ONLY
   1 1 CODEGEN-COMPARE:COST 1 CODEGEN-COMPARE:SIZE FIX-ROW
   FIX-FINDINGS 2 T=

   s" a size that is not a number is reported, and its row is gone" T-LABEL
   FIRST-ROW-ONLY
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
\
\ BOTH SIDES OF THIS ARE MEASUREMENTS, so it belongs to the timed run and not to
\ a gate. The slowed word comes out about twenty times its honest cost against a
\ band of eight, and the honest re-measurement about once its own; a host that
\ moved either by the worst factor recorded at the head of
\ tools/codegen-compare-baseline.f - 3.04 - crosses neither margin, but nothing
\ about the comparison makes that a fact rather than a hope. What the gate keeps
\ instead is the same comparison on stated numbers, where the answer cannot turn
\ on a clock: COST-MODE-CASES writes a cost of one against a measured row, which
\ no row can be within eight times of, and TWO-ROW-CASES writes a row's own cost
\ back at it. Those two pin both directions of the band arithmetic. What only
\ this can add is the link between a word that really got slower and a column
\ that really moved, and that link is a fact about a clock.
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
   FIRST-ROW-ONLY
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
   FIRST-ROW-ONLY
   1 1 CODEGEN-COMPARE:SIZE 4 + 1 FIX-ROW
   FIX-FINDINGS 1 T=

   s" and so does a wrong output value" T-LABEL
   FIRST-ROW-ONLY
   1 BAD-ROW ! 1 BAD-OUTPUT !
   1 WILD-COST-ROW
   FIX-FINDINGS 1 T=

   s" and so does a row the baseline is missing" T-LABEL
   FIRST-ROW-ALONE
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

\ ---- and the byte columns, on rows built to fool them ------------------------
\ The size a new row carries is the size of the word NAMED as carrying the new
\ chain's code, read off that word's own dictionary record - which is what lets a
\ fixture put a row of a chosen size in front of the adjudication without
\ inventing a number. These three name real words of the first corpus whose real
\ sizes stand in the three relations there are: ADD3-N is smaller than ADD3, ADD3
\ is exactly equal to itself, and BYTE-FIND is larger. What is being checked is
\ that the adjudication is a strict `>` - a draw is not a loss - and that a row
\ that IS larger is named and counted.

\ A new row for ADD3 whose code is ADD3's own, so the two columns are equal.
: NEW-EQUAL-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:ADD3"
   [: ;]
   [: OLD-ADD3-ROW 0 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR
      OLD-ADD3-ROW 1 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ And one whose code is the corpus's largest word, so the new column is bigger.
: NEW-BIGGER-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3" s" CODEGEN-CORPUS:BYTE-FIND"
   [: ;]
   [: OLD-ADD3-ROW 0 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR
      OLD-ADD3-ROW 1 CODEGEN-COMPARE:OUTPUT CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ A pass that holds the KNOWN loss - the fourth corpus's CALL-FAN-BIG - in which
\ the new column is NOT bigger. That is the day the dot behind the entry lands,
\ and the entry has to go with it; the harness says so rather than quietly
\ keeping an allowance for a row that no longer needs one. It is not a
\ hypothetical: the list's other entry, the second corpus's T-RES-WALK, came off
\ through exactly this finding when the chain reached the engine's 36 bytes.
: SHRUNK-KNOWN-LOSS ( -- )
   s" CODEGEN-CORPUS4:CALL-FAN-BIG"
   [: ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" s" CODEGEN-CORPUS:NOOP"
   [: ;]
   [: ;]
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
   CODEGEN-GAP:RESET
   CODEGEN-COMPARE:PASS-BEGIN
   CODEGEN-CALIBRATE:OLD
   HONEST-ADD3-CASE
   NEW-CALIBRATION-CASE
   build execute
   CODEGEN-COMPARE:PASS-END
   CODEGEN-COMPARE:NORMALIZE ;

\ The new row the fixtures above add is always the last one.
: LAST-ROW ( -- n )
   CODEGEN-COMPARE:ROWS 1- ;

: UNKNOWN-GAP ( -- )
   s" CODEGEN-CORPUS:NOT-A-WORD" CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP ;

\ A new column that measured the calibration row and then simply stopped, with
\ the corpus word the old column measured neither compiled nor declared a gap.
\ CODEGEN-GAP:COVERAGE-CK is what turns that into a refusal rather than a column
\ that is quietly one row shorter, and CODEGEN-GAP:ACCOUNT is what makes a new
\ column unable to reach the end of its pass without it. Both are exercised by
\ the real words, through the real pass machinery, on a column built to be one
\ row short.
: SHORT-COLUMN-CASES ( -- )
   NEW-CALIBRATION-CASE ;

: SHORT-COLUMN-NEW ( -- )
   [: SHORT-COLUMN-CASES ;] CODEGEN-GAP:ACCOUNT ;

: SHORT-COLUMN-PASS ( -- )
   [: CODEGEN-CALIBRATE:OLD HONEST-ADD3-CASE ;]
   [: SHORT-COLUMN-NEW ;]
   CODEGEN-COMPARE:PASS ;

: NEW-COLUMN-CASES ( -- )
   s" a new row that answers what the old row answered reads as agreement" T-LABEL
   [: NEW-HONEST-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-COMPARE:ROW-MATCH? TTRUE
   CODEGEN-COMPARE:MISMATCHES 0 T=
   CODEGEN-REPORT:SAY-MISMATCHES 0 T=

   s" one answer moved by one is reported as a disagreement" T-LABEL
   [: NEW-WRONG-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-COMPARE:ROW-MATCH? TFALSE
   CODEGEN-COMPARE:MISMATCHES 1 T=

   s" a new row one answer short is reported too" T-LABEL
   [: NEW-SHORT-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-COMPARE:ROW-MATCH? TFALSE
   CODEGEN-COMPARE:MISMATCHES 1 T=

   s" a new row is compared with the old row of the same name, not with itself" T-LABEL
   [: NEW-WRONG-CASE ;] MEASURE-WITH
   LAST-ROW CODEGEN-COMPARE:PARTNER OLD-ADD3-ROW T=

   s" naming a gap for a word the old column never measured is refused" T-LABEL
   [: UNKNOWN-GAP ;] E-CODEGEN-COMPARE-CORPUS TTHROWSQ

   s" and so is a new column that left a corpus word out without declaring it" T-LABEL
   [: SHORT-COLUMN-PASS ;] E-CODEGEN-COMPARE-CORPUS TTHROWSQ ;

\ The byte adjudication, on the same fixtures. It reports through the production
\ words - CODEGEN-REPORT:BIGGER? is the one place the question is asked and
\ SAY-BYTE-LOSSES is what the run's exit status is made of - so what passes here
\ is what a gate runs. The third case prints one finding line while it passes,
\ which is the harness saying out loud what the fixture made it find.
: BYTE-COLUMN-CASES ( -- )
   s" a new row of fewer bytes than its old row is not a byte loss" T-LABEL
   [: NEW-HONEST-CASE ;] MEASURE-WITH
   s" CODEGEN-CORPUS:ADD3" CODEGEN-REPORT:BIGGER? TFALSE
   CODEGEN-REPORT:SAY-BYTE-LOSSES 0 T=

   s" and neither is a row of exactly the same size: a draw is not a loss" T-LABEL
   [: NEW-EQUAL-CASE ;] MEASURE-WITH
   s" CODEGEN-CORPUS:ADD3" CODEGEN-REPORT:BIGGER? TFALSE
   CODEGEN-REPORT:SAY-BYTE-LOSSES 0 T=

   s" a new row of MORE bytes than its old row is a loss, named and counted" T-LABEL
   [: NEW-BIGGER-CASE ;] MEASURE-WITH
   s" CODEGEN-CORPUS:ADD3" CODEGEN-REPORT:BIGGER? TTRUE
   CODEGEN-REPORT:SAY-BYTE-LOSSES 1 T=

   s" and a known loss that has stopped losing is a finding of its own" T-LABEL
   [: SHRUNK-KNOWN-LOSS ;] MEASURE-WITH
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" CODEGEN-REPORT:BIGGER? TFALSE
   CODEGEN-REPORT:SAY-BYTE-LOSSES 1 T=

   s" while a known loss this pass does not hold says nothing either way" T-LABEL
   [: NEW-HONEST-CASE ;] MEASURE-WITH
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" CODEGEN-REPORT:BIGGER? TFALSE
   CODEGEN-REPORT:SAY-BYTE-LOSSES 0 T=

   s" the known-loss list is the one row that loses today, named" T-LABEL
   CODEGEN-REPORT:KNOWN-LOSSES 1 T=
   0 CODEGEN-REPORT:KNOWN-LOSS$ s" CODEGEN-CORPUS4:CALL-FAN-BIG" T$= ;

\ ---- the register the drivers walk ------------------------------------------
\ Every driver in tools/codegen-compare-cli.f reads which corpora exist, and
\ what each one's table is, out of tools/codegen-compare-corpora.f. Two things
\ have to hold for `--update <corpus>` to mean one table: a name has to select
\ exactly one corpus, and no two corpora may point at the same file. A register
\ that mapped two names onto one table would let a regeneration rewrite a
\ yardstick nobody asked about, which is the failure the per-table update
\ exists to prevent.
\
\ These read the live register, filled by the four case files as they loaded,
\ so they say something about the declarations production uses.

: PATHS-CLASH ( -- n )
   0
   CODEGEN-CORPORA:COUNT 0 ?do
      CODEGEN-CORPORA:COUNT 0 ?do
         i j <> if
            i CODEGEN-CORPORA:BASELINE$ j CODEGEN-CORPORA:BASELINE$ STR= if 1+ then
         then
      loop
   loop ;

: NAMES-CLASH ( -- n )
   0
   CODEGEN-CORPORA:COUNT 0 ?do
      CODEGEN-CORPORA:COUNT 0 ?do
         i j <> if
            i CODEGEN-CORPORA:NAME$ j CODEGEN-CORPORA:NAME$ STR= if 1+ then
         then
      loop
   loop ;

: REGISTER-CASES ( -- )
   s" four corpora are declared, in the order they are measured" T-LABEL
   CODEGEN-CORPORA:COUNT 4 T=
   0 CODEGEN-CORPORA:NAME$ s" corpus" T$=
   1 CODEGEN-CORPORA:NAME$ s" corpus2" T$=
   2 CODEGEN-CORPORA:NAME$ s" corpus3" T$=
   3 CODEGEN-CORPORA:NAME$ s" corpus4" T$=

   s" each declared name selects its own corpus" T-LABEL
   s" corpus" CODEGEN-CORPORA:FIND 0 T=
   s" corpus2" CODEGEN-CORPORA:FIND 1 T=
   s" corpus3" CODEGEN-CORPORA:FIND 2 T=
   s" corpus4" CODEGEN-CORPORA:FIND 3 T=

   s" a name no corpus was declared under selects none" T-LABEL
   s" corpus9" CODEGEN-CORPORA:FIND -1 T=
   s" " CODEGEN-CORPORA:FIND -1 T=
   s" corpus3 " CODEGEN-CORPORA:FIND -1 T=

   s" and rewriting under such a name writes nothing and says so" T-LABEL
   s" corpus9" CODEGEN-COMPARE-CLI:UPDATE-NAMED TFALSE

   s" no two corpora share a name or a committed table" T-LABEL
   NAMES-CLASH 0 T=
   PATHS-CLASH 0 T=

   s" and every corpus names a table and the source it measures" T-LABEL
   CODEGEN-CORPORA:COUNT 0 ?do
      i CODEGEN-CORPORA:BASELINE$ nip 0 > TTRUE
      i CODEGEN-CORPORA:SOURCE$ nip 0 > TTRUE
   loop ;

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
   CODEGEN-GAP:GAPS 0 ?do
      i CODEGEN-GAP:GAP-NAME$ k CODEGEN-COMPARE:NAME$ STR= if drop true leave then
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
   CODEGEN-GAP:GAPS 0 ?do
      i CODEGEN-GAP:GAP-CAPS@ 0= if 1+ then
   loop ;

\ Every word the new chain compiled, compared byte for byte with the old
\ emitter's answer for the same word.
: NOT-SMALLER ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i CODEGEN-COMPARE:PARTNER {: b:n :}
         b 0 < if 1+ else
            i CODEGEN-COMPARE:SIZE b CODEGEN-COMPARE:SIZE < 0= if 1+ then
         then
      then
   loop ;

\ Is the chain's code for this corpus word fewer bytes than the engine's? Asked
\ by name, because a count alone cannot say WHICH row lost and a corpus with a
\ losing row has to name it.
: SMALLER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-NEW a u CODEGEN-COMPARE:FIND-ROW {: k:n :}
   k 0 < if false exit then
   k CODEGEN-COMPARE:PARTNER {: b:n :}
   b 0 < if false exit then
   k CODEGEN-COMPARE:SIZE b CODEGEN-COMPARE:SIZE < ;

\ Did the new column measure a row for this word at all? A gap that was closed
\ has to show up as a measured row, not merely as an absent declaration.
: MEASURED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-NEW a u CODEGEN-COMPARE:FIND-ROW 0 >= ;

: NAMED-GAP-AMONG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   false
   CODEGEN-GAP:GAPS 0 ?do
      i CODEGEN-GAP:GAP-NAME$ a u STR= if drop true leave then
   loop ;

variable CODE-AT

: CODE-PTR ( -- ptr u8 )
   CODE-AT 0 ptr-field @ ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

\ ---- the traffic a call site makes with the caller's own data stack ----------
\ What a call costs the chain beyond the branch is the values it puts into its
\ own data-stack slots and reads back. Since dot habu-narrow-what-a-5d6a0845 a
\ site saves only what the callee's recorded destroyed set covers, and the whole
\ of that change shows up here: these are the Str and Ldr forms of a whole cell
\ against the register the running engine keeps its data-stack pointer in
\ (src/compiler/a64-effect.f DSTACK-GPR), counted in a word's own compiled code.
\ A byte count moves for any reason; this moves for one.
\
\ AND THERE ARE TWO SPELLINGS OF ONE ACCESS, which is why each counter below adds
\ two forms rather than one. Since dot habu-place-the-data-9f128e58 the chain
\ stands its data-stack pointer where the fewest adjustments are needed, so a
\ cell it reaches can be UNDER the pointer as well as over it - and under it is
\ the unscaled signed form, Ldur and Stur, which is a different encoding of the
\ same access. A counter that knew only the scaled forms would report an access
\ the routine really makes as no access at all, and would read a change of
\ addressing mode as a saving. Both forms are counted, so what these numbers
\ measure stays "how often the caller's stack is touched" whatever the placement
\ chose.
$FFC00000 constant MEM-MASK
$F9000000 constant STR-OP
$F9400000 constant LDR-OP
$F8000000 constant STUR-OP
$F8400000 constant LDUR-OP

: DSTACK-AT? ( n n -- bool ) {: w:n op:n :}
   w MEM-MASK and op =
   w 5 rshift $1F and  A64EFF:DSTACK-GPR =  and ;

: DS-COUNT ( ptr u8 n n -- n ) {: a:ptr u:n op:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-COMPARE-SUBJECT throw then
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len 4 / 0 ?do
      CODE-PTR i 4 * + U32@ op DSTACK-AT? if 1+ then
   loop ;

: DS-STORES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u STR-OP DS-COUNT
   a u STUR-OP DS-COUNT + ;

: DS-LOADS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LDR-OP DS-COUNT
   a u LDUR-OP DS-COUNT + ;

\ ---- and how many calls a word's own code makes ------------------------------
\ The other exact number, and the one a claim about copying a callee rests on:
\ AArch64's branch-with-link, top six bits 100101 - the same encoding
\ src/habu/habu2.f names `$94000000 constant C-CALL-BL-IMM` and scans for when it
\ decides whether a span is safe to copy. A byte count can be explained away; a
\ branch either is in the emitted code or is not. It stands here beside the
\ data-stack counters because the third and fourth corpora both read it.
$FC000000 constant BL-MASK
$94000000 constant BL-OP

\ How many call instructions a live word's compiled code contains. The code start
\ and the code length come off the word's own dictionary record, which is where
\ every other size in this harness comes from.
: BL-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-COMPARE-SUBJECT throw then
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len 4 / 0 ?do
      CODE-PTR i 4 * + U32@ BL-MASK and BL-OP = if 1+ then
   loop ;

\ ---- and how many of a word's branches a predictor can get wrong -------------
\ The third exact number, and the one an if-conversion claim rests on. A
\ conditional branch is B.cond, top eight bits 01010100 with bit 4 clear, which
\ is the form formal/Common/Insn.v's row_Bcond decodes; a Cbz or Cbnz over a
\ tested register is the other shape a two-way branch takes and is counted with
\ it, because a conversion that turned one into the other would have removed
\ nothing. A byte count can be explained away and a timing moves for host load;
\ a branch is in the emitted code or it is not.
\
\ AND WHAT REPLACED IT, counted the same way, because "no branch" alone is also
\ what a body that lost an arm would report. Fcsel is the D-file conditional
\ select - src/arch/arm64/asm.f ENC-FCSEL, the same opcode mask
\ formal/Common/Insn.v gives row_Fcsel - and a converted float selection has one
\ per value it chooses.
$FF000010 constant BCOND-MASK
$54000000 constant BCOND-OP
$7E000000 constant CBZ-MASK
$34000000 constant CBZ-OP
$FFE00C00 constant FCSEL-MASK
$1E600C00 constant FCSEL-OP

\ AND THE THREE THAT SAY WHETHER A FLOAT COMPARISON WAS FOLDED INTO THE SELECT
\ OR ONLY STOOD IN FRONT OF IT. A body that materialises a Habu flag and then
\ chooses on it emits an Fcmp, a Cset and a select; one that folds the
\ comparison in emits the Fcmp and the select and no Cset. So the Cset is the
\ exact witness, and the Fcmp beside it says the select really is reading an
\ Fcmp's flags rather than an integer compare's.
\
\   Csel  is the general-file select, src/arch/arm64/asm.f ENC-CSEL, and the
\         same 11-bit base as Fcsel with the op2 field zero - which is what
\         separates it from the Csinc a Cset is.
\   Cset  is that Csinc with both source registers the zero register, which is
\         why its mask covers the rm and rn fields as well as the opcode.
\   Fcmp  is the float compare, in both its forms: two D registers, and one
\         against the immediate zero. A body may use either, so both are
\         counted - what is being asserted is that the comparison happened in
\         this word's own code and not behind a call.
$FFE00C00 constant CSEL-MASK
$9A800000 constant CSEL-OP
$FFFF0FE0 constant CSET-MASK
$9A9F07E0 constant CSET-OP
$FFE0FC1F constant FCMP-MASK
$1E602000 constant FCMP-OP
$1E602008 constant FCMP0-OP

: FORM-COUNT ( ptr u8 n n n -- n ) {: a:ptr u:n mask:n op:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-COMPARE-SUBJECT throw then
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len 4 / 0 ?do
      CODE-PTR i 4 * + U32@ mask and op = if 1+ then
   loop ;

: BCOND-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u BCOND-MASK BCOND-OP FORM-COUNT
   a u CBZ-MASK CBZ-OP FORM-COUNT + ;

: FCSEL-COUNT ( ptr u8 n -- n )
   FCSEL-MASK FCSEL-OP FORM-COUNT ;

: CSEL-COUNT ( ptr u8 n -- n )
   CSEL-MASK CSEL-OP FORM-COUNT ;

: CSET-COUNT ( ptr u8 n -- n )
   CSET-MASK CSET-OP FORM-COUNT ;

: FCMP-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FCMP-MASK FCMP-OP FORM-COUNT
   a u FCMP-MASK FCMP0-OP FORM-COUNT + ;

\ ---- what each column spends on the caller's own data stack ------------------
\ A COST is a measurement; a count of instructions is not. What the timing rows
\ of this harness were really about is the traffic each column makes with the
\ caller's own data stack: the engine moves every intermediate through a stack
\ slot in memory, and the chain holds it in a register and touches the slot only
\ at the edges of the routine. That difference is what the timings showed, and
\ unlike a timing it is exact, moves for one reason, and does not turn on host
\ load. It is what the assertions below pin.
\
\ BE CLEAR ABOUT WHAT THAT IS NOT. It is not the cost claim restated: a chain
\ that made the same number of accesses and took twice as long would pass every
\ one of them. The cost claim itself is an assertion in
\ tools/codegen-compare-timed-test.f, run by hand on a quiet machine, and the
\ two columns' nanoseconds are printed side by side in every report the timed
\ entry writes.

\ The word that carries the new chain's code for a corpus word. The migration
\ publishes each body under its own name with `-N` on the end, which is the one
\ place that spelling is derived rather than written out again.
: NEW-WORD$ ( ptr u8 n -- ptr u8 n )
   SB-RESET SB-APPEND s" -N" SB-APPEND SB$ ;

: DS-TRAFFIC ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u DS-STORES a u DS-LOADS + ;

\ Does the chain's code for this corpus word touch the caller's data stack more
\ often than the engine's code for the same body?
: DS-HEAVIER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u NEW-WORD$ DS-TRAFFIC
   a u DS-TRAFFIC > ;

\ How many rows of the new column do. Over every compiled row of the pass, so
\ this is a statement about the whole corpus rather than about the rows somebody
\ thought to name.
: DS-HEAVIER-ROWS ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i CODEGEN-COMPARE:NAME$ DS-HEAVIER? if 1+ then
      then
   loop ;


\ The account every measured pass has to keep, whichever corpus it measured.
: ACCOUNT-CASES ( n -- ) {: rows:n :}
   s" the real run measured the whole pinned corpus" T-LABEL
   OLD-ROWS rows T=

   s" every corpus word is compiled by the new chain or named a gap" T-LABEL
   UNACCOUNTED 0 T=

   s" and no corpus word is claimed by both accounts" T-LABEL
   DOUBLE-COUNTED 0 T=

   s" the two accounts together are the whole corpus" T-LABEL
   NEW-ROWS CODEGEN-GAP:GAPS + OLD-ROWS T=

   s" every gap names at least one capability it is waiting for" T-LABEL
   CAPLESS-GAPS 0 T=

   s" every word the new chain compiled computes what the old emitter computes" T-LABEL
   CODEGEN-COMPARE:MISMATCHES 0 T= ;

: REAL-RUN-CASES ( -- )
   11 ACCOUNT-CASES

   s" the first corpus has no gaps at all" T-LABEL
   CODEGEN-GAP:GAPS 0 T=

   s" and every word the new chain compiled is fewer bytes of machine code" T-LABEL
   NOT-SMALLER 0 T=

   s" and no row of it touches the caller's data stack more often" T-LABEL
   DS-HEAVIER-ROWS 0 T= ;

\ The second corpus, whose account was not all wins and now is. All eight of its
\ words are compiled - VEC-COPY-CELLS was a gap until dot
\ habu-save-the-loop-5f07e0c3 made a call inside a counted loop save the loop's
\ own state - and the one row that took MORE bytes than the engine's code for the
\ same body, T-RES-WALK, is now a DRAW at 36 bytes each. That number is pinned
\ here so that the day it changes, in either direction, somebody has to look at
\ it: it is the engine's own code for the same body, instruction for instruction.
\
\ THREE MECHANISMS CLOSED IT AND NONE OF THEM REACHED 36 ALONE. The residency
\ pass in src/compiler/native/select.f took the row from 76 bytes to 56: the value
\ the walker carries round its loop never leaves the data-stack cell it arrived
\ in, so the entry load, the call's store of the same value into the same cell,
\ the result load, the return's store and the edge copy that only existed because
\ the value was in a register are all gone. The block order in
\ src/compiler/native/emit.f took it to 48 by laying the loop body next to its
\ header. And the placement in src/compiler/native/select.f took it to 36 by
\ standing the routine's data-stack pointer where the loop's own call already
\ wants it, which left one adjustment in the whole routine - the same one the
\ engine writes, `sub x19, x19, #8`, to pop the flag.
\
\ NO ROW OF THIS CORPUS TOUCHES THE CALLER'S DATA STACK MORE OFTEN than the
\ engine any more, which is why the assertion below is zero and names the row it
\ used to be about.
: REAL-RUN-CASES2 ( -- )
   8 ACCOUNT-CASES

   s" the second corpus declares no gap at all" T-LABEL
   CODEGEN-GAP:GAPS 0 T=

   s" and the word that was one is compiled and measured" T-LABEL
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" MEASURED? TTRUE

   s" exactly one compiled word is not fewer bytes than the engine's" T-LABEL
   NOT-SMALLER 1 T=

   s" and it is the loop whose test is a call" T-LABEL
   s" CODEGEN-CORPUS2:T-RES-WALK" SMALLER? TFALSE

   s" it is a DRAW and not a loss, which the harness adjudicates by name" T-LABEL
   s" CODEGEN-CORPUS2:T-RES-WALK" CODEGEN-REPORT:BIGGER? TFALSE
   s" CODEGEN-CORPUS2:COUNT-CHAR" CODEGEN-REPORT:BIGGER? TFALSE

   s" and the corpus carries no known loss at all any more" T-LABEL
   CODEGEN-REPORT:SAY-BYTE-LOSSES 0 T=

   s" every other compiled word of it is fewer bytes" T-LABEL
   s" CODEGEN-CORPUS2:TAG" SMALLER? TTRUE
   s" CODEGEN-CORPUS2:WS?" SMALLER? TTRUE
   s" CODEGEN-CORPUS2:SYM-FOLD-C" SMALLER? TTRUE
   s" CODEGEN-CORPUS2:MAX-DIM" SMALLER? TTRUE
   s" CODEGEN-CORPUS2:COUNT-CHAR" SMALLER? TTRUE
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" SMALLER? TTRUE

   s" and no row of it touches the caller's data stack more often" T-LABEL
   DS-HEAVIER-ROWS 0 T=
   s" CODEGEN-CORPUS2:T-RES-WALK" DS-HEAVIER? TFALSE
   s" CODEGEN-CORPUS2:COUNT-CHAR" DS-HEAVIER? TFALSE ;

\ The third corpus, whose account is now eleven compiled rows and no gap at all.
\ It is the float benchmark, measured and committed before the chain had a single
\ float capability, and it took three leaves to close: scalar float arithmetic
\ over a locals frame, the five comparisons and the branch they feed, and the
\ placement of a double where a straight line does not reach - across a block
\ edge, across a call, and round a loop's back edge. The assertions below are
\ about that account, about the three float facts the table rests on - that a
\ recorded output is the whole cell, that the sign of a zero survives the
\ recording, and that the pinned sum distinguishes one evaluation order from
\ another - and about every compiled row agreeing with the old column on every
\ pinned input.
\
\ NO GAP NAMES ANY CAPABILITY, and each of the three the float campaign used is
\ asked for by name rather than only counted: a row still naming `floats`,
\ `comparison` or `float-place` would mean the account had not been brought
\ forward with the chain, and counting alone cannot tell one wrong name from
\ another.
\
\ NO ROW OF THIS CORPUS COSTS MORE THAN THE ENGINE'S CODE ANY MORE. That is a
\ statement about a clock, so it is asserted in
\ tools/codegen-compare-timed-test.f and run by hand; what is pinned HERE is the
\ thing behind it, which a wall clock cannot blur. T-SGD!'s loop
\ body is THREE calls - two loads and a store - and its four locals, two counters
\ and accumulator are live across all of them; every one of those values used to
\ go out through a data-stack slot and come back at every call, because nothing
\ in a Habu word's convention is callee-saved. That is still the rule for a
\ callee nobody knows anything about, and it is no longer the rule for one the
\ chain published: such a routine records which registers its own allocation
\ writes, and the site saves only the live values that set can reach. So what is
\ pinned is the traffic itself - how many stores and loads against the caller's
\ data-stack pointer the emitted word contains - which is exact, moves for one
\ reason, and does not turn on host load. The row was 340 bytes with 24 stores
\ and 23 loads, then 204 with seven and six, and is 140 with none and four.
\
\ AND THE KERNELS NOW CARRY NO CALL AT ALL, WHICH IS WHY THOSE COUNTS MOVED
\ AGAIN. Every one of the five reaches an element through `T-GET` or `T-SET`, and
\ each of those two is written as `T-AT @` or `T-AT !` - a call inside a call. The
\ chain copied T-AT-N's body into them from the start, so the routines it
\ published for T-GET-N and T-SET-N contain no branch; what it would not do was
\ RECORD such a routine, because the record was judged on the source token rather
\ than on the routine. It is judged on the routine now (dot
\ habu-copy-a-callee-711e74eb, src/compiler/native/inline.f), so a kernel copies
\ `cells + @` into its own loop and the per-element call is gone.
\
\ THIS IS THE CORPUS WHERE THE TWO COLUMNS DIFFER ON THAT, and that is what makes
\ the Bl counts below worth pinning rather than restating the fourth corpus's
\ result. The ENGINE copies a callee of at most forty body bytes and refuses any
\ span with a branch-with-link in it, so T-GET - which HAS one, to T-AT - is a
\ word it calls; its own kernels therefore still branch once, twice or three
\ times per turn of the loop, and they are pinned here so that a change to either
\ compiler shows up as a moved number in the column it belongs to.
: GAP-WANTS? ( n CODEGEN-GAP:cap -- bool ) {: k:n c:CODEGEN-GAP:cap :}
   false
   k CODEGEN-GAP:GAP-CAPS@ 0 ?do
      k i CODEGEN-GAP:GAP-CAP@ c CODEGEN--GAP-CAP:EQ if
         drop true leave
      then
   loop ;

: GAPS-WANTING ( CODEGEN-GAP:cap -- n ) {: c:CODEGEN-GAP:cap :}
   0
   CODEGEN-GAP:GAPS 0 ?do
      i c GAP-WANTS? if 1+ then
   loop ;

\ One recorded output of an old row, by the row's name.
: OLD-OUTPUT ( ptr u8 n n -- n ) {: a:ptr u:n j:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW {: k:n :}
   k 0 < if E-CODEGEN-COMPARE-CORPUS throw then
   k j CODEGEN-COMPARE:OUTPUT ;

\ Whether the new column's row for this word costs more than the old column's for
\ the same word. Cost is the harness's own measure, taken in the same pass on the
\ same host, so the two numbers are comparable in the one way a wall clock is -
\ and only in that way, which is why every caller of this is inside TIMED and
\ TIMED is reached only from tools/codegen-compare-timed-test.f.
: COSTLIER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-NEW a u CODEGEN-COMPARE:FIND-ROW {: k:n :}
   k 0 < if false exit then
   k CODEGEN-COMPARE:PARTNER {: b:n :}
   b 0 < if false exit then
   k CODEGEN-COMPARE:COST  b CODEGEN-COMPARE:COST  > ;

: REAL-RUN-CASES3 ( -- )
   11 ACCOUNT-CASES

   s" the third corpus compiles every float row and declares no gap" T-LABEL
   CODEGEN-GAP:GAPS 0 T=
   NEW-ROWS 11 T=

   s" the rows the float campaign closed last are measured rows now" T-LABEL
   s" CODEGEN-CORPUS:NOOP" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:SGD" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:SEG-1/SQRT" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:MAX-F" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:RELU-F" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:FROUND" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:T-SUM" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:T-DIST2" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:T-NORM2" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:T-SGD!" MEASURED? TTRUE
   s" CODEGEN-CORPUS3:T-REL-L2" MEASURED? TTRUE

   s" and not one of them is left standing as a declaration" T-LABEL
   s" CODEGEN-CORPUS3:T-SUM" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS3:FROUND" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS3:RELU-F" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS3:T-REL-L2" NAMED-GAP-AMONG? TFALSE

   s" no row waits for any of the three capabilities the float campaign built" T-LABEL
   CODEGEN--GAP-CAP:FLOAT-PLACE GAPS-WANTING 0 T=
   CODEGEN--GAP-CAP:FLOATS GAPS-WANTING 0 T=
   CODEGEN--GAP-CAP:COMPARISON GAPS-WANTING 0 T=

   s" every compiled row is fewer bytes than the code the old emitter wrote" T-LABEL
   NOT-SMALLER 0 T=
   s" CODEGEN-CORPUS3:RELU-F" SMALLER? TTRUE
   s" CODEGEN-CORPUS3:FROUND" SMALLER? TTRUE
   s" CODEGEN-CORPUS3:T-SUM" SMALLER? TTRUE
   s" CODEGEN-CORPUS3:T-SGD!" SMALLER? TTRUE

   s" the loop written with three calls in it now makes none of them" T-LABEL
   s" CODEGEN-CORPUS3:T-SGD!-N" DS-STORES 0 T=
   s" CODEGEN-CORPUS3:T-SGD!-N" DS-LOADS 4 T=

   s" exactly one row of this corpus touches the caller's data stack more often" T-LABEL
   DS-HEAVIER-ROWS 1 T=

   s" and it is the smallest float body, by the one store the chain makes" T-LABEL
   s" CODEGEN-CORPUS3:SGD" DS-HEAVIER? TTRUE
   s" CODEGEN-CORPUS3:SGD" DS-STORES 0 T=
   s" CODEGEN-CORPUS3:SGD" DS-LOADS 3 T=
   s" CODEGEN-CORPUS3:SGD-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS3:SGD-N" DS-LOADS 3 T=

   s" and the loop rows, which the timings were about, spend its own arguments" T-LABEL
   s" CODEGEN-CORPUS3:T-SUM" DS-STORES 6 T=
   s" CODEGEN-CORPUS3:T-SUM-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS3:T-SUM" DS-LOADS 6 T=
   s" CODEGEN-CORPUS3:T-SUM-N" DS-LOADS 2 T=
   s" CODEGEN-CORPUS3:T-DIST2" DS-HEAVIER? TFALSE
   s" CODEGEN-CORPUS3:T-NORM2" DS-HEAVIER? TFALSE
   s" CODEGEN-CORPUS3:T-REL-L2" DS-HEAVIER? TFALSE
   s" CODEGEN-CORPUS3:RELU-F" DS-HEAVIER? TFALSE

   s" the engine still branches once per element, because T-GET itself calls" T-LABEL
   s" CODEGEN-CORPUS3:T-GET" BL-COUNT 1 T=
   s" CODEGEN-CORPUS3:T-SUM" BL-COUNT 1 T=
   s" CODEGEN-CORPUS3:T-NORM2" BL-COUNT 1 T=
   s" CODEGEN-CORPUS3:T-DIST2" BL-COUNT 2 T=
   s" CODEGEN-CORPUS3:T-SGD!" BL-COUNT 3 T=

   s" and the chain's code for the same five kernels carries no call at all" T-LABEL
   s" CODEGEN-CORPUS3:T-GET-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS3:T-SET-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS3:T-SUM-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS3:T-NORM2-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS3:T-DIST2-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS3:T-SGD!-N" BL-COUNT 0 T=

   s" while the row whose callees are whole loops still calls them, in both" T-LABEL
   s" CODEGEN-CORPUS3:T-REL-L2" BL-COUNT 2 T=
   s" CODEGEN-CORPUS3:T-REL-L2-N" BL-COUNT 2 T=

   s" the sum row's pinned input distinguishes one evaluation order from another" T-LABEL
   CODEGEN-CORPUS3:SUM-REVERSED CODEGEN-COMPARE:REAL-BITS
   s" CODEGEN-CORPUS3:T-SUM" 1 OLD-OUTPUT T<>

   s" a recorded float keeps the sign of a zero, so the two zeros are two rows" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F" 1 OLD-OUTPUT
   s" CODEGEN-CORPUS3:RELU-F" 2 OLD-OUTPUT T<>
   s" CODEGEN-CORPUS3:RELU-F" 1 OLD-OUTPUT 0 T=

   s" and the NaN two different words produce is the same cell" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F" 4 OLD-OUTPUT
   s" CODEGEN-CORPUS3:T-REL-L2" 3 OLD-OUTPUT T=

\ ---- the two branching float bodies, and what each of them hands its join ----
\ Both are a float comparison with two arms meeting at one block, and they are
\ NOT the same shape to this chain, which is what makes the pair worth pinning.
\ MAX-F's arms hand on the two argument CELLS - the doubles are read out of them
\ by the comparison and never cross the edge - so its join carries cells and a
\ general Csel has always been able to choose between them. RELU-F's arms hand
\ on a DOUBLE, the float literal against the argument, so its join carries one,
\ and until src/compiler/native/select.f gained the D-file select there was no
\ instruction to choose with and the region stayed branched. The counts below
\ are that difference: RELU-F-N loses its branch and gains an Fcsel, and
\ MAX-F-N, which never had a branch to lose, gains nothing - which is what says
\ the cell path was left alone.
\
\ AND THE COMPARISON ITSELF IS NOW FOLDED INTO THE SELECT IN BOTH. The chain's
\ code for each of them is one Fcmp and one conditional select and nothing else:
\ the Cset and the negation that turned the comparison into a Habu flag are gone,
\ and so is the compare-against-zero that tested that flag. RELU-F-N's select
\ reads the flags an `Fcmp d,#0.0` left and MAX-F-N's the flags an `Fcmp d,d`
\ left, which is what a64.fcmpselzd and a64.fcmpsel are.
\
\ WHAT THAT RESTS ON, AND WHY IT IS SAFE. A select that reads an Fcmp's flags
\ takes its arm from a condition, and after an Fcmp a NaN raises the unordered
\ flag - so the condition is the whole of the NaN behaviour. The conditions
\ src/compiler/native/select.f gives the float comparisons are `mi`, `gt` and
\ `equal`, which are exactly the three that are FALSE under the unordered flag,
\ so an unordered comparison takes the same arm the engine's own flag-and-branch
\ takes. The derivation is written out at that table.
\
\ THAT LAST SENTENCE IS WHAT THE OUTPUT ROWS ABOVE MEASURE AND THESE COUNTS DO
\ NOT. The corpus runs RELU-F on a NaN and on a negative zero, and MAX-F on a
\ NaN in each position, and the report compares every recorded cell against the
\ interpreted word's - so a conversion that took the other arm for an unordered
\ comparison is a finding and not a slower row. What is pinned here is the other
\ half: that the branch really left, that the flag is never materialised, and
\ that what stands in its place chooses in the file its arms live in.
\
\ THE OLD COLUMN'S TWO ROWS DO NOT AGREE WITH EACH OTHER, and that is the engine
\ and not a mistake here. RELU-F's `f0<` is a LEAF primitive with no call in its
\ body, so the engine copies it inline and the row really does hold an Fcmp and
\ the Cset that turns it into a number; MAX-F's `f<` is reached by a call, so its
\ own code holds neither and one Bl instead. Both are pinned, because between
\ them they say what the chain's two rows replaced.
   s" the engine branches through both float selections and the chain does not" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F" BCOND-COUNT 1 T=
   s" CODEGEN-CORPUS3:MAX-F" BCOND-COUNT 1 T=
   s" CODEGEN-CORPUS3:RELU-F-N" BCOND-COUNT 0 T=
   s" CODEGEN-CORPUS3:MAX-F-N" BCOND-COUNT 0 T=

   s" and the body whose join carries a double chose it with the D-file select"
   T-LABEL
   s" CODEGEN-CORPUS3:RELU-F-N" FCSEL-COUNT 1 T=
   s" CODEGEN-CORPUS3:MAX-F-N" FCSEL-COUNT 0 T=
   s" CODEGEN-CORPUS3:RELU-F" FCSEL-COUNT 0 T=
   s" CODEGEN-CORPUS3:MAX-F" FCSEL-COUNT 0 T=

   s" while the one whose join carries cells chose with the general select" T-LABEL
   s" CODEGEN-CORPUS3:MAX-F-N" CSEL-COUNT 1 T=
   s" CODEGEN-CORPUS3:RELU-F-N" CSEL-COUNT 0 T=

   s" neither of them materialises the comparison's flag any more" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F-N" CSET-COUNT 0 T=
   s" CODEGEN-CORPUS3:MAX-F-N" CSET-COUNT 0 T=

   s" because each select reads the flags an Fcmp in its own code left" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F-N" FCMP-COUNT 1 T=
   s" CODEGEN-CORPUS3:MAX-F-N" FCMP-COUNT 1 T=

   s" where the engine either inlines the compare and sets a flag, or calls out"
   T-LABEL
   s" CODEGEN-CORPUS3:RELU-F" FCMP-COUNT 1 T=
   s" CODEGEN-CORPUS3:RELU-F" CSET-COUNT 1 T=
   s" CODEGEN-CORPUS3:MAX-F" FCMP-COUNT 0 T=
   s" CODEGEN-CORPUS3:MAX-F" BL-COUNT 1 T= ;

\ ---- the claim the second corpus's two respelled rows rest on ----------------
\ tools/codegen-compare-migrated2.f compiles two of the second corpus's bodies
\ with a constant written as the number behind it: WS?'s four named byte
\ constants, because a migrated body may name at most one word outside the
\ dialect (dot habu-let-a-migrated-77d34d82), and SYM-FOLD-C's three hexadecimal
\ literals, because the tape cannot read a hexadecimal spelling back (dot
\ habu-record-the-engine-79c570ed). Both files say so in prose. This is the
\ measurement behind the prose.
\
\ The twins below are the corpus bodies with the substitution applied, compiled
\ by the ENGINE - the same emitter that compiled the corpus words - so the
\ comparison is between two spellings of one program under one compiler. If they
\ are the same size and answer the same on every pinned input, the substitution
\ is a spelling; if a future engine ever compiled a named constant or a
\ hexadecimal literal into anything else, these red and the note in
\ tools/codegen-compare-migrated2.f is what they point at.

\ The twins are PUBLIC because their sizes are read the way every other size in
\ this harness is read - off the dictionary record, by name - and a private word
\ has no name the dictionary answers to. Nothing outside this file calls them.
public

: WS-SPELLED-OUT ( n -- bool )
   dup 32 = over 9 = or over 10 = or swap 13 = or ;

: FOLD-SPELLED-OUT ( n -- n ) {: c:n :}
   c 65 < if c exit then
   c 90 > if c exit then
   c 32 or ;

private

\ A flag as the number the assertion vocabulary compares. `T=` takes two
\ numbers, and two flags are not two numbers.
: FLAG# ( bool -- n )
   if 1 else 0 then ;

: WORD-BYTES ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if E-CODEGEN-COMPARE-SUBJECT throw then
   XREF-LEN ;

: SPELLING-CASES ( -- )
   s" a named constant compiles to the same code as the number behind it" T-LABEL
   s" CODEGEN-COMPARE-TEST:WS-SPELLED-OUT" WORD-BYTES
   s" CODEGEN-CORPUS2:WS?" WORD-BYTES T=

   s" and answers the same on every pinned input" T-LABEL
   32 WS-SPELLED-OUT FLAG#  32 CODEGEN-CORPUS2:WS? FLAG# T=
   9 WS-SPELLED-OUT FLAG#  9 CODEGEN-CORPUS2:WS? FLAG# T=
   10 WS-SPELLED-OUT FLAG#  10 CODEGEN-CORPUS2:WS? FLAG# T=
   13 WS-SPELLED-OUT FLAG#  13 CODEGEN-CORPUS2:WS? FLAG# T=
   97 WS-SPELLED-OUT FLAG#  97 CODEGEN-CORPUS2:WS? FLAG# T=

   s" a hexadecimal literal compiles to the same code as its decimal spelling" T-LABEL
   s" CODEGEN-COMPARE-TEST:FOLD-SPELLED-OUT" WORD-BYTES
   s" CODEGEN-CORPUS2:SYM-FOLD-C" WORD-BYTES T=

   s" and answers the same on every pinned input" T-LABEL
   64 FOLD-SPELLED-OUT 64 CODEGEN-CORPUS2:SYM-FOLD-C T=
   65 FOLD-SPELLED-OUT 65 CODEGEN-CORPUS2:SYM-FOLD-C T=
   90 FOLD-SPELLED-OUT 90 CODEGEN-CORPUS2:SYM-FOLD-C T=
   91 FOLD-SPELLED-OUT 91 CODEGEN-CORPUS2:SYM-FOLD-C T=
   97 FOLD-SPELLED-OUT 97 CODEGEN-CORPUS2:SYM-FOLD-C T= ;

\ ---- the second corpus's committed table, on fixtures built to fool it -------
\ The cases near the top of this file build their fixtures from a two-row
\ measurement of the FIRST corpus. These build one from the whole of the SECOND
\ corpus's old column - eight rows, five of which the chain also compiles and two
\ of which are gaps - and then break it in one place at a time. A comparison that
\ read the second table against the first corpus's rows, or that stopped after
\ the rows it recognised, passes every case above and fails these.
\
\ The measurement they read is whatever pass the caller ran last, so MAIN runs
\ the second corpus immediately before calling them.

: OLD-ROW ( ptr u8 n -- n ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW ;

\ Every old row of the measured pass, with the named row's byte count moved by
\ `delta`. A delta of zero writes the table the harness itself would write.
: FIX-OLD-ROWS ( n n -- ) {: bad:n delta:n :}
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i bad = if
            i  i CODEGEN-COMPARE:SIZE delta +  i CODEGEN-COMPARE:COST FIX-ROW
         else
            i FIX-TRUE-ROW
         then
      then
   loop ;

: WHOLE-OLD-COLUMN ( n n -- ) {: bad:n delta:n :}
   FIX-RESET
   OLD-ROWS FIX-DECLARED
   bad delta FIX-OLD-ROWS ;

\ Four bytes is one AArch64 instruction, which is the smallest a compiled word's
\ size can move by, so a check that catches this catches any real regression.
4 constant ONE-INSN

\ THE FIVE WAYS A COMMITTED TABLE IS ATTACKED, WRITTEN ONCE. Each corpus after
\ the first is broken in the same places - the honest table, a byte count moved
\ by one instruction either way, a recorded output moved by one, and a row left
\ out - and the sets below used to write all five out again for every corpus,
\ with only the row names differing. What is genuinely per corpus is which row
\ each case names and why that row is the interesting one, and that is what the
\ three sets keep.
\
\ Every one of them writes a whole table from the pass the caller ran last and
\ asks the real comparison how many findings it reports, so a set proves
\ something about that corpus's own table and not about a fixture.

\ The table the harness itself would write, which must report nothing.
: HONEST-TABLE-CK ( -- )
   -1 0 WHOLE-OLD-COLUMN
   FIX-FINDINGS 0 T= ;

\ One named row's byte count moved by `delta` bytes, and nothing else touched.
: BYTE-DELTA-CK ( ptr u8 n n -- ) {: a:ptr u:n delta:n :}
   a u OLD-ROW delta WHOLE-OLD-COLUMN
   FIX-FINDINGS 1 T= ;

\ One recorded output of one named row moved by one, and nothing else touched.
\ Which output is named because a row's outputs are not interchangeable: the
\ interesting one is the edge the pinned inputs were chosen to reach.
: BAD-OUTPUT-CK ( ptr u8 n n -- ) {: a:ptr u:n j:n :}
   FIX-RESET
   OLD-ROWS FIX-DECLARED
   a u OLD-ROW BAD-ROW ! j BAD-OUTPUT !
   -1 0 FIX-OLD-ROWS
   FIX-FINDINGS 1 T= ;

\ The whole table with one named row left out of it, and the declared count
\ lowered to match, so what is caught is the missing row and not the count.
: MISSING-ROW-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-RESET
   OLD-ROWS 1- FIX-DECLARED
   a u OLD-ROW {: gone:n :}
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i gone <> if i FIX-TRUE-ROW then
      then
   loop
   FIX-FINDINGS 1 T= ;

: CORPUS2-TABLE-CASES ( -- )
   s" the second corpus's own table, written honestly, reports nothing" T-LABEL
   HONEST-TABLE-CK

   s" one instruction added to one row's byte count is reported" T-LABEL
   s" CODEGEN-CORPUS2:COUNT-CHAR" ONE-INSN BYTE-DELTA-CK

   s" and so is one taken off a different row" T-LABEL
   s" CODEGEN-CORPUS2:T-RES-WALK" ONE-INSN negate BYTE-DELTA-CK

   s" a byte regression on a GAP row's old column is reported too" T-LABEL
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" ONE-INSN BYTE-DELTA-CK

   s" one wrong output value in the second table is reported" T-LABEL
   s" CODEGEN-CORPUS2:WS?" 4 BAD-OUTPUT-CK

   s" a second table missing a row is reported" T-LABEL
   s" CODEGEN-CORPUS2:MAX-DIM" MISSING-ROW-CK ;

\ ---- the third corpus's committed table, on fixtures built to fool it --------
\ The same attack as the second corpus's, over the whole of the float table -
\ eleven old rows, every one of them a gap in the new column - and then broken
\ in one place at a time. Two of the cases are about float outputs in
\ particular: a NaN whose recorded cell is moved by one bit has to be reported
\ like any other wrong answer, because a NaN with another payload is another
\ value, and a table missing a float row has to be reported rather than passing
\ because the remaining rows all matched.
\
\ The measurement they read is whatever pass the caller ran last, so MAIN runs
\ the third corpus immediately before calling them.
: CORPUS3-TABLE-CASES ( -- )
   s" the third corpus's own table, written honestly, reports nothing" T-LABEL
   HONEST-TABLE-CK

   s" one instruction added to a float row's byte count is reported" T-LABEL
   s" CODEGEN-CORPUS3:T-SGD!" ONE-INSN BYTE-DELTA-CK

   s" and so is one taken off the row that reaches its answer through calls" T-LABEL
   s" CODEGEN-CORPUS3:T-REL-L2" ONE-INSN negate BYTE-DELTA-CK

   s" a NaN recorded one bit out is reported" T-LABEL
   s" CODEGEN-CORPUS3:T-REL-L2" 3 BAD-OUTPUT-CK

   s" and so is a negative zero written as a positive one" T-LABEL
   s" CODEGEN-CORPUS3:RELU-F" 2 BAD-OUTPUT-CK

   s" a third table missing a row is reported" T-LABEL
   s" CODEGEN-CORPUS3:MAX-F" MISSING-ROW-CK ;

\ ---- the fourth corpus, the one built to make the new chain lose --------------
\ Every row of the fourth corpus is a shape somebody had a reason to believe the
\ new chain handles WORSE than the engine's emitter, and the head of
\ tools/codegen-compare-corpus4.f gives the reason for each. What the assertions
\ below pin is the answer that corpus got, so a change in either direction has to
\ be looked at rather than absorbed.
\
\ TEN COMPILED ROWS AND TWO GAPS. PRESSURE-LOOP and CALL-PRESSURE are refused,
\ and they are the first gaps in this harness whose cause is neither a missing
\ operation nor a missing type: the chain has every capability either body needs
\ and will not put a loop-carried value in a frame slot. The `loop-spill`
\ capability names that, and REFUSAL-CASES below hands the corpus's own text to
\ the real migration entry and checks that each refusal is E-A64RA-SPILL and not
\ some other code, with the largest accepted neighbour beside it so that
\ "refused" cannot mean "the register budget was too small".
\
\ WHICH ROWS COST MORE. NONE of the nine does any more. TINY-CALLEE was the last
\ one that did - its loop body is nothing but calls, and the engine copied the
\ callee where the chain branched to it - and the chain now copies the same body
\ for the same reason (src/compiler/native/inline.f), which took it from about
\ 1.20x slower to about eight times faster. CALL-LOOP-3 and CALL-FAN went the
\ same way. That is a claim about a clock, so it is asserted in
\ tools/codegen-compare-timed-test.f and run by hand; LADDER is left out of it
\ even there, because the two columns measured within a twentieth of each other
\ and each came out ahead at least once over five passes, and an assertion that
\ fails for host noise is worse than none.
\
\ WHAT IS PINNED HERE INSTEAD, FOR THE ROWS THAT TURN ON A CALL, is the traffic
\ itself: how many stores and loads against the caller's own data-stack pointer
\ each emitted word contains. Those numbers are exact and they do not move for
\ host load. They now say something stronger than the narrowing did: each of the
\ three call rows contains exactly its own arguments and its own result and
\ nothing else, because there is no call left in it to publish anything for. A
\ change that stopped copying puts the callee's arguments, results and every live
\ value back into those counts and fails a gate here rather than being absorbed
\ into a timing. Being exact, they are also stricter than a direction: a row that
\ kept its speed by some other route would still have to make the same accesses.
\
\ EVERY COMPILED ROW BUT ONE IS FEWER BYTES, and the one is CALL-FAN-BIG. Its
\ callee is two operations, which is past the forty bytes of body the engine
\ copies and still inside the chain's own rule, so the engine emits five Bl - one
\ instruction each, because residency and placement leave those sites nothing
\ else to write - and the chain copies four instructions of body into all five
\ sites: 88 bytes against 36, and about twenty times faster. That is the row the
\ corpus did not have while every callee in it was one BOTH generators copy, and
\ it is pinned here in both directions - the byte loss by name through the
\ harness's own adjudication, the Bl counts on both sides - so that a change to
\ either inlining rule has to be looked at. It is a declared known loss in
\ tools/codegen-compare-report.f, which says why the chain's size rule accepts
\ it rather than naming a dot that removes it.
\
\ IT IS ALSO THE ONE ROW WHERE THE DATA-STACK COUNT GOES THE OTHER WAY, and that
\ is worth reading carefully rather than as the chain being wasteful. The counts
\ are of a word's OWN emitted code, and the engine's CALL-FAN-BIG makes no
\ data-stack access at all: it is five Bl and a frame, and every load and store
\ happens inside the five callees, which are five other words. The chain's word
\ has no callee to hide anything in, so its one load and one store - its own
\ argument and its own result - are more than nothing. Both numbers are asserted,
\ so what the assertion says is exactly this and not a claim about total work.

\ ---- who copies a small callee, read off the emitted machine code -------------
\ The call rows of the fourth corpus rested on one claim: the engine COPIES a
\ small callee into its caller and emits no call instruction, while the chain
\ emitted a Bl per call site. The first half is unchanged and the second half is
\ what an earlier leaf's work removed - both columns now carry no call
\ instruction at all for these three bodies, each under its own rule, and the
\ counts below say so on both sides. A byte count could be explained away; the
\ instruction cannot. BL-COUNT is up beside the data-stack counters, because the
\ third corpus reads it too, and reads it for a case this corpus does not have:
\ there the ENGINE still branches and only the chain does not.

\ src/habu/habu2.f, `$28 constant INL-MAX`: the most bytes of BODY the engine
\ copies into a caller. A callee that opens with the standard two-instruction
\ prologue is measured as clen-16, so a callee of exactly this many body bytes
\ occupies this many plus sixteen.
40 constant INL-MAX
16 constant FRAME-BYTES

: REAL-RUN-CASES4 ( -- )
   13 ACCOUNT-CASES

   s" the fourth corpus compiles ten rows and declares two gaps" T-LABEL
   CODEGEN-GAP:GAPS 2 T=
   NEW-ROWS 11 T=

   s" and both gaps are loops that hold a value the allocator cannot place" T-LABEL
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" NAMED-GAP-AMONG? TTRUE
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" MEASURED? TFALSE
   s" CODEGEN-CORPUS4:CALL-PRESSURE" NAMED-GAP-AMONG? TTRUE
   s" CODEGEN-CORPUS4:CALL-PRESSURE" MEASURED? TFALSE
   CODEGEN--GAP-CAP:LOOP-SPILL GAPS-WANTING 2 T=

   s" no other row of it waits for anything" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS4:CALL-LOOP-3" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS4:TINY-CALLEE" NAMED-GAP-AMONG? TFALSE
   s" CODEGEN-CORPUS4:MANY-LOCALS" NAMED-GAP-AMONG? TFALSE

   s" and the ten it does compile are measured rows" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:CALL-LOOP-3" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:TINY-CALLEE" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:WIDE-ARITY" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:LADDER" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:BIG-CONSTS" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:MANY-LOCALS" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:FLOAT-MIX" MEASURED? TTRUE
   s" CODEGEN-CORPUS4:STORE-LOAD" MEASURED? TTRUE

   s" exactly one compiled row is not fewer bytes than the engine's" T-LABEL
   NOT-SMALLER 1 T=
   s" CODEGEN-CORPUS4:CALL-FAN" SMALLER? TTRUE
   s" CODEGEN-CORPUS4:CALL-LOOP-3" SMALLER? TTRUE
   s" CODEGEN-CORPUS4:TINY-CALLEE" SMALLER? TTRUE
   s" CODEGEN-CORPUS4:LADDER" SMALLER? TTRUE

   s" and it is the five sites over the callee only the chain copies" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" SMALLER? TFALSE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" CODEGEN-REPORT:BIGGER? TTRUE
   s" CODEGEN-CORPUS4:CALL-FAN" CODEGEN-REPORT:BIGGER? TFALSE

   s" which the harness adjudicates as a KNOWN loss and not as a finding" T-LABEL
   CODEGEN-REPORT:SAY-BYTE-LOSSES 0 T=

   s" exactly one row touches the caller's data stack more often, and is that one" T-LABEL
   DS-HEAVIER-ROWS 1 T=
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" DS-HEAVIER? TTRUE

   s" and the data-stack traffic in a call row is its own arguments and result" T-LABEL
   s" CODEGEN-CORPUS4:CALL-LOOP-3-N" DS-LOADS 5 T=
   s" CODEGEN-CORPUS4:CALL-LOOP-3-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS4:TINY-CALLEE-N" DS-LOADS 2 T=
   s" CODEGEN-CORPUS4:TINY-CALLEE-N" DS-STORES 1 T=

   s" and the row with five call sites in a straight line the same" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-N" DS-LOADS 1 T=
   s" CODEGEN-CORPUS4:CALL-FAN-N" DS-STORES 1 T=

   s" and the four rows with no call in them spend one access on their whole body" T-LABEL
   s" CODEGEN-CORPUS4:BIG-CONSTS-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS4:BIG-CONSTS" DS-STORES 19 T=
   s" CODEGEN-CORPUS4:MANY-LOCALS-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS4:MANY-LOCALS" DS-STORES 19 T=
   s" CODEGEN-CORPUS4:FLOAT-MIX-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS4:STORE-LOAD-N" DS-STORES 1 T=

   s" each of the four callees is one the engine copies rather than calls" T-LABEL
   s" CODEGEN-CORPUS4:C-ADD1" WORD-BYTES INL-MAX FRAME-BYTES + T=
   s" CODEGEN-CORPUS4:C-MUL2" WORD-BYTES INL-MAX FRAME-BYTES + T=
   s" CODEGEN-CORPUS4:C-AND7" WORD-BYTES INL-MAX FRAME-BYTES + T=
   s" CODEGEN-CORPUS4:C-XOR5" WORD-BYTES INL-MAX FRAME-BYTES + T=

   s" so the engine's code for the three call rows carries no call at all" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN" BL-COUNT 0 T=
   s" CODEGEN-CORPUS4:CALL-LOOP-3" BL-COUNT 0 T=
   s" CODEGEN-CORPUS4:TINY-CALLEE" BL-COUNT 0 T=

   s" and the chain's code for the same three bodies carries none either" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS4:CALL-LOOP-3-N" BL-COUNT 0 T=
   s" CODEGEN-CORPUS4:TINY-CALLEE-N" BL-COUNT 0 T=

   s" while the two callees past that ceiling are called and not copied" T-LABEL
   s" CODEGEN-CORPUS4:C-MAD" WORD-BYTES INL-MAX FRAME-BYTES + > TTRUE
   s" CODEGEN-CORPUS4:C-LONG" WORD-BYTES INL-MAX FRAME-BYTES + > TTRUE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" BL-COUNT 5 T=
   s" CODEGEN-CORPUS4:CALL-PRESSURE" BL-COUNT 1 T=

   s" and the chain copies the smaller of the two, which is the whole loss" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" BL-COUNT 0 T=

   s" so the engine's caller makes no data-stack access of its own at all" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" DS-STORES 0 T=
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" DS-LOADS 0 T=

   s" and the chain's makes its own entry and exit and nothing else" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" DS-STORES 1 T=
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" DS-LOADS 1 T=

   s" and it reaches that with a fraction of the engine's bytes" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-N" WORD-BYTES
   s" CODEGEN-CORPUS4:CALL-FAN" WORD-BYTES 2 /  < TTRUE
   s" CODEGEN-CORPUS4:TINY-CALLEE-N" WORD-BYTES
   s" CODEGEN-CORPUS4:TINY-CALLEE" WORD-BYTES 2 /  < TTRUE

   s" the five copied bodies are the whole of the five-call row's code" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN" WORD-BYTES  INL-MAX 5 * FRAME-BYTES + T=

   s" a loop of zero turns runs its body no times, in both columns" T-LABEL
   s" CODEGEN-CORPUS4:TINY-CALLEE" 1 OLD-OUTPUT 5 T=
   s" CODEGEN-CORPUS4:MANY-LOCALS" 1 OLD-OUTPUT 0 T=

   s" and the step that writes one cell leaves the cell after it alone" T-LABEL
   s" CODEGEN-CORPUS4:STORE-LOAD" 2 OLD-OUTPUT -7 T= ;

\ ---- the fourth corpus's committed table, on fixtures built to fool it --------
\ The same attack as the second and third corpora's, over the whole of the fourth
\ table - thirteen old rows, two of which are gaps in the new column - broken in
\ one place at a time. The measurement they read is whatever pass the caller ran last,
\ so MAIN runs the fourth corpus immediately before calling them.
: CORPUS4-TABLE-CASES ( -- )
   s" the fourth corpus's own table, written honestly, reports nothing" T-LABEL
   HONEST-TABLE-CK

   s" one instruction added to a call row's byte count is reported" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN" ONE-INSN BYTE-DELTA-CK

   s" and so is one taken off the row the chain refuses" T-LABEL
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ONE-INSN negate BYTE-DELTA-CK

   s" a wrong sixty-four-bit output is reported" T-LABEL
   s" CODEGEN-CORPUS4:BIG-CONSTS" 0 BAD-OUTPUT-CK

   s" and so is a wrong answer on the last rung of the ladder" T-LABEL
   s" CODEGEN-CORPUS4:LADDER" 7 BAD-OUTPUT-CK

   s" a fourth table missing a row is reported" T-LABEL
   s" CODEGEN-CORPUS4:TINY-CALLEE" MISSING-ROW-CK ;

\ ---- the ceilings the fourth corpus was designed around -----------------------
\ tools/codegen-compare-corpus4.f says two shapes were left out of the corpus
\ because the migration entry cannot carry them, and gives an error code for each;
\ tools/codegen-compare-new4.f says two of the corpus's own rows are refused and
\ gives a third code for both. These run the real entry - the same NMIGRATE the
\ whole new column goes through - and check the codes, so each account is a
\ measurement rather than a sentence.
\
\ THE REFUSED ROWS ARE THE ONES THAT MATTER. "The chain refuses PRESSURE-LOOP" is
\ a claim about a compiler, and the honest way to check it is to hand the compiler
\ the corpus's own text. Each body below is the corpus's, character for character,
\ under the `-N` name tools/codegen-compare-migrated4.f would have given it, at
\ EIGHTEEN registers - the largest pool src/compiler/a64-effect.f allows, x18
\ being platform-reserved. Each has a control beside it - the same body one value
\ smaller, which compiles at the same budget - because without the control
\ "refused" could mean "the number was too small".
\
\ THE TWO REFUSALS REACH ONE RULE BY TWO ROADS. PRESSURE-LOOP holds fourteen
\ values live in a loop body and nothing else; CALL-PRESSURE holds seven across a
\ call the loop makes, which is a shape programs really have. Their walls are at
\ different counts and the reason is one: MB-SPILLABLE? will not place a value
\ defined or read in a block that is neither the entry nor the exit, and a call
\ costs the allocator registers it then cannot get back inside the loop.

PTR-VARIABLE TRY-SRC
variable TRY-U
variable TRY-IN
variable TRY-REGS

\ The migration a case is asking about, run where its failure can be read as a
\ code. A quotation cannot see the enclosing word's locals, so what it needs is
\ parked - the shape src/compiler/native/migrate.f uses for the same reason.
: MIGRATE-RC ( -- n )
   [: TRY-SRC @ TRY-U @ TRY-IN @ 1 TRY-REGS @ NMIGRATE:DEFINE ;] catch ;

: TRY ( ptr u8 n n n -- n ) {: a:ptr u:n in:n regs:n :}
   a TRY-SRC ! u TRY-U ! in TRY-IN ! regs TRY-REGS !
   MIGRATE-RC ;

\ The same for a body that CALLS, with the callee staged first. A migration
\ clears its staged callees whether it succeeded or was refused
\ (src/compiler/native/migrate.f, RUN), so a refusal here leaves nothing behind
\ for the cases after it.
: MIGRATE-CALLING-RC ( -- n )
   [: TRY-SRC @ TRY-U @ TRY-IN @ 1 TRY-REGS @ NMIGRATE:DEFINE-CALLING ;] catch ;

\ The corpus's PRESSURE-LOOP body, character for character.
: SPILL-14$ ( -- ptr u8 n )
   s" : PRESSURE-LOOP-N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do base @ base 8 + @ base 16 + @ base 24 + @ base 32 + @ base 40 + @ base 48 + @ base 56 + @ base 64 + @ base 72 + @ base 80 + @ base 88 + @ base 96 + @ base 104 + @ + + + + + + + + + + + + + + loop ;" ;

\ The same body with one field fewer, which is the control.
: SPILL-13$ ( -- ptr u8 n )
   s" : PRESSURE-13-N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do base @ base 8 + @ base 16 + @ base 24 + @ base 32 + @ base 40 + @ base 48 + @ base 56 + @ base 64 + @ base 72 + @ base 80 + @ base 88 + @ base 96 + @ + + + + + + + + + + + + + loop ;" ;

\ Eleven arguments, which is one more than a routine's place list holds.
: WIDE-11$ ( -- ptr u8 n )
   s" : WIDE-11-N ( n n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n m:n :} a b + c + d + e + f + g + h + j + k + m + ;" ;

\ Ten, which is what it does hold.
: WIDE-10$ ( -- ptr u8 n )
   s" : WIDE-10-N ( n n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :} a b + c + d + e + f + g + h + j + k + ;" ;

\ The corpus's CALL-PRESSURE body, character for character but for the `-N` the
\ migration's convention puts on a name: seven values live across a call the loop
\ really makes, to a callee neither generator copies. The callee is named as the
\ package publishes it, which is a spelling the migration's own resolver has to
\ take (test/compiler/native-inline.f pins that it does).
: SPILL-CALL-7$ ( -- ptr u8 n )
   s" : CALL-PRESSURE-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop a + b + c + d + e + f + g + ;" ;

\ The same body with one value fewer live across the call, which is the control.
: SPILL-CALL-6$ ( -- ptr u8 n )
   s" : CALL-PRESSURE-6-N ( n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n seed:n len:n :} seed len 0 ?do CODEGEN-CORPUS4:C-LONG-N loop a + b + c + d + e + f + ;" ;

\ One staged callee, named the way a migrated body would spell it and addressed
\ off its own dictionary record.
: CALLEE-AT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-COMPARE-SUBJECT throw then
   XREF-START 1 1 NMIGRATE:CALLEE ;

\ A calling body tried against the callee the corpus's own gap row calls. The
\ callee is the chain's published C-LONG-N, so a refusal is about the body and
\ not about a routine the chain has never seen.
: TRY-CALLING ( ptr u8 n n n -- n ) {: a:ptr u:n in:n regs:n :}
   a TRY-SRC ! u TRY-U ! in TRY-IN ! regs TRY-REGS !
   s" CODEGEN-CORPUS4:C-LONG-N" CALLEE-AT
   MIGRATE-CALLING-RC ;

\ Stage a fifth callee for one definition. The entry holds four
\ (src/compiler/native/migrate.f, `4 constant CALLEES-MAX`), so the fifth staging
\ is the refusal and nothing after it runs.
: STAGE-FIVE ( -- )
   s" CODEGEN-CORPUS4:C-ADD1-N" CALLEE-AT
   s" CODEGEN-CORPUS4:C-MUL2-N" CALLEE-AT
   s" CODEGEN-CORPUS4:C-AND7-N" CALLEE-AT
   s" CODEGEN-CORPUS4:C-XOR5-N" CALLEE-AT
   s" CODEGEN-CORPUS4:C-ADD1-N" CALLEE-AT ;

\ The four rows the refused fifth left staged belong to the next migration
\ whatever happens, so they are spent here on a real one rather than left for some
\ later caller to be refused for. It is also the other half of the claim: four is
\ a ceiling because four WORK.
: SPEND-FOUR ( -- )
   s" : FAN-CEILING-N ( n -- n ) CODEGEN-CORPUS4:C-ADD1-N CODEGEN-CORPUS4:C-MUL2-N CODEGEN-CORPUS4:C-AND7-N CODEGEN-CORPUS4:C-XOR5-N ;"
   1 1 8 NMIGRATE:DEFINE-CALLING ;

: FIVE-RC ( -- n )
   [: STAGE-FIVE ;] catch ;

: SPEND-RC ( -- n )
   [: SPEND-FOUR ;] catch ;

: REFUSAL-CASES ( -- )
   s" the corpus's own pressure loop is refused at the largest register pool" T-LABEL
   SPILL-14$ 2 18 TRY E-A64RA-SPILL T=

   s" and it is the loop and not the budget: one field fewer compiles there" T-LABEL
   SPILL-13$ 2 18 TRY 0 T=

   s" the corpus's own call-pressure loop is refused at the same pool" T-LABEL
   SPILL-CALL-7$ 9 18 TRY-CALLING E-A64RA-SPILL T=

   s" and it is the seventh live value and not the budget: six compiles there" T-LABEL
   SPILL-CALL-6$ 8 18 TRY-CALLING 0 T=

   s" a routine of eleven arguments is refused by the place list" T-LABEL
   WIDE-11$ 11 18 TRY E-A64EFF-SEQ T=

   s" and ten, which is what it holds, is not" T-LABEL
   WIDE-10$ 10 18 TRY 0 T=

   s" a fifth callee staged for one definition is refused" T-LABEL
   FIVE-RC E-NMIGRATE-STATE T=

   s" and the four the refusal left staged make a definition that compiles" T-LABEL
   SPEND-RC 0 T= ;

\ ---- the cost assertions, which are not scheduled ----------------------------
\ Everything below turns on a wall clock, so nothing below is reached from MAIN.
\ These are reached only through TIMED, and TIMED only from
\ tools/codegen-compare-timed-test.f, whose head gives the measurements behind
\ the decision and what the gate keeps in their place.

: COST-DIRECTION-CASES3 ( -- )
   s" no row of the third corpus costs more than the engine's code" T-LABEL
   s" CODEGEN-CORPUS3:T-SUM" COSTLIER? TFALSE
   s" CODEGEN-CORPUS3:T-DIST2" COSTLIER? TFALSE
   s" CODEGEN-CORPUS3:T-NORM2" COSTLIER? TFALSE
   s" CODEGEN-CORPUS3:T-REL-L2" COSTLIER? TFALSE
   s" CODEGEN-CORPUS3:RELU-F" COSTLIER? TFALSE ;

\ LADDER is deliberately absent: the two columns measured within a twentieth of
\ each other and each came out ahead at least once over five passes, so it is a
\ draw and pinning it in either direction would be inventing a result.
\
\ CALL-FAN-BIG is here despite being the row the chain LOSES on bytes, and the
\ two facts are one fact: copying the callee is what makes its word larger and
\ what makes it about twenty-five times faster - 12.8 ns against 0.58 on an idle
\ host. A margin that size is not a draw, so the direction is worth pinning.
: COST-DIRECTION-CASES4 ( -- )
   s" no row of the fourth corpus costs more than the engine's code" T-LABEL
   s" CODEGEN-CORPUS4:TINY-CALLEE" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:CALL-LOOP-3" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:CALL-FAN" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:BIG-CONSTS" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:MANY-LOCALS" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:FLOAT-MIX" COSTLIER? TFALSE
   s" CODEGEN-CORPUS4:STORE-LOAD" COSTLIER? TFALSE ;

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
   COST-MODE-CASES
   NEW-COLUMN-CASES
   BYTE-COLUMN-CASES
   REGISTER-CASES
   \ Each account and each fixture is read off a pass this file runs BY NAME.
   \ Reading whatever the previous run happened to leave in the store would make
   \ every assertion below depend on the order of the file rather than on the
   \ corpus it names.
   CODEGEN-CASES:RUN
   REAL-RUN-CASES
   CODEGEN-CASES2:RUN
   REAL-RUN-CASES2
   SPELLING-CASES
   CORPUS2-TABLE-CASES
   CODEGEN-CASES3:RUN
   REAL-RUN-CASES3
   CORPUS3-TABLE-CASES
   CODEGEN-CASES4:RUN
   REAL-RUN-CASES4
   CORPUS4-TABLE-CASES
   REFUSAL-CASES
   CLEANUP-RUN
   CODEGEN-BASELINE:LOUD!
   CODEGEN-COMPARE-CLI:CHECK-EXACT
   T-REPORT ;

MAIN

public

\ The assertions that turn on a clock, run on a quiet machine by
\ bin/hb --load tools/codegen-compare-timed-test.f. Public so that exactly one
\ caller can reach them and no gate does; the scheduled MAIN above does not
\ call it.
: TIMED ( -- )
   T-RESET
   SETUP
   \ The scheduled MAIN above ends in CHECK-EXACT, which leaves the cost column
   \ declared unchecked. Everything below is about that column, so it is turned
   \ back on here rather than assumed.
   CODEGEN-BASELINE:COSTS-CHECKED!
   SLOWDOWN-CASES
   CODEGEN-CASES3:RUN
   COST-DIRECTION-CASES3
   CODEGEN-CASES4:RUN
   COST-DIRECTION-CASES4
   CLEANUP-RUN
   CODEGEN-BASELINE:LOUD!
   T-REPORT ;

;package
