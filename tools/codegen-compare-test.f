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
\ The last thing the file does is run CODEGEN-COMPARE-CLI:CHECK - the exact word
\ `bin/hb --load tools/codegen-compare.f` runs - against the committed baseline
\ in the repository. That is the real production path, and it ends the process
\ with a non-zero status if the committed table and the live compiler disagree.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require tools/codegen-compare-cli.f

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
   CLEANUP-RUN
   CODEGEN-BASELINE:LOUD!
   CODEGEN-COMPARE-CLI:CHECK
   T-REPORT ;

MAIN

;package
