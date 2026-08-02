\ codegen-compare-baseline.f - reading the committed baseline table back and
\ comparing it with a fresh measurement.
\ One concern: the structural comparison.
\
\ The comparison is structural, not textual. The committed file is read, split
\ into lines, and every line whose first word is old or new is parsed into a row
\ with a path, a word name, a size, a cost and a list of outputs. Prose is
\ everything else. That means the expected text hidden inside a sentence cannot
\ satisfy anything - it is either a well-formed row, in which case it is an extra
\ row and reported, or it is not, in which case it is reported as a malformed
\ row. Nothing is matched by searching the file for a substring.
\
\ The file also declares its own row count on a "rows: N" line, so deleting a
\ row is a finding even when the measurement side lost the same word.
\
\ ONLY THE OLD ROWS ARE COMMITTED. The measurement pass also produces a new row
\ for every corpus word the native chain can compile, and none of them are
\ written here: the new chain is still growing, its byte counts move with every
\ capability it gains, and pinning them would turn a real advance into a red
\ gate. The new column is recomputed on every run and compared with the old one
\ live - exactly, on the values the two routines produce - so it is checked
\ without being frozen. Rows of that path are therefore skipped on both sides of
\ the comparison below, and the reader still understands the word `new` so a
\ hand-added new row is reported as an extra row rather than silently ignored.
\
\ What is compared exactly, and what is compared with a tolerance:
\
\   size, outputs   exactly. These are facts about the emitted code and about
\                   what it computes; either they match or the compiler changed.
\   cost            with a stated tolerance. A cost is a measurement, so a row is
\                   reported only when it comes out more than COST-BAND times
\                   slower than the committed number. See the band's own note in
\                   tools/codegen-compare-core.f for the load measurements it is
\                   derived from.
\
\ THE COST COLUMN CAN BE LEFT OUT, AND SAYS SO WHEN IT IS. A gate runs many
\ suites at once, and a timing taken on a loaded host is compared here with a
\ number recorded on an idle one. Measured on the twelve-core host this harness
\ was written on: with eight competing busy processes per core the pass still
\ agreed, but CODEGEN-CORPUS:BYTE-FIND came out 7.93 times its recorded cost
\ against a band of eight - two per cent of margin - and with sixteen per core
\ the run reported four slower rows and the pass budget and exited non-zero.
\ None of those was a compiler change. So a run may declare the cost column
\ unchecked: COSTS-UNCHECKED! leaves the cost comparison and the pass budget out
\ - the budget exists only to say the timings are untrustworthy, so it belongs
\ with them - and COMPARE prints one line naming what it did not compare and
\ why. A run that skips the timings therefore cannot be mistaken for a run that
\ compared them and found nothing. Everything else is compared exactly as
\ before, because none of it is a measurement. COSTS-CHECKED! is the default.
\
\ Every disagreement is a finding: the run reports all of them and returns the
\ count, rather than stopping at the first.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fmt.f
require lib/adt/option.f
require tools/codegen-compare-core.f

package CODEGEN-BASELINE

$4000 constant FILE-CAP
32 constant SPACE-BYTE
10 constant NEWLINE-BYTE

FILE-CAP BUFFER: FILE-TEXT
variable FILE-U

CODEGEN-COMPARE:ROW-MAX CODEGEN-COMPARE:NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS CODEGEN-COMPARE:ROW-MAX cells allot
create PATHS CODEGEN-COMPARE:ROW-MAX cells allot
create SIZES CODEGEN-COMPARE:ROW-MAX cells allot
create COSTS CODEGEN-COMPARE:ROW-MAX cells allot
create OUT-COUNTS CODEGEN-COMPARE:ROW-MAX cells allot
create OUT-VALUES CODEGEN-COMPARE:ROW-MAX CODEGEN-COMPARE:OUTPUT-MAX * cells allot

variable ROW-N
variable DECLARED-ROWS
variable FINDINGS
variable REPORT?
variable COSTS?                   \ compare the cost column, and with it the pass budget
variable CURSOR
variable TEXT-CURSOR
PTR-VARIABLE LINE-A
variable LINE-U

: SLOT ( ptr a n -- ptr a )
   cells + ;

: TRUE? ( -- bool )
   0 0= ;

: FALSE? ( -- bool )
   TRUE? 0= ;

: NAME-AT ( n -- ptr u8 )
   CODEGEN-COMPARE:NAME-MAX * NAME-BYTES + ;

\ ---- findings --------------------------------------------------------------

: FIND+ ( -- )
   FINDINGS @ 1+ FINDINGS ! ;

: SAY ( ptr u8 n -- )
   REPORT? @ if type else 2drop then ;

: SAY-NUM ( n -- )
   REPORT? @ if FMT:.INT else drop then ;

: SAY-END ( -- )
   REPORT? @ if cr then ;

: FINDING ( ptr u8 n -- )
   s" codegen-compare: " SAY SAY
   s"  " SAY ;

\ ---- line and token cursor -------------------------------------------------

: LINE! ( ptr u8 n -- )
   LINE-U ! LINE-A !
   0 CURSOR ! ;

: LINE$ ( -- ptr u8 n )
   LINE-A @ LINE-U @ ;

: SKIP-SPACES ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ SPACE-BYTE <> if exit then
      1+
   repeat ;

: TOKEN ( ptr u8 n n -- ptr u8 n n bool ) {: a:ptr u:n start:n :}
   a u start SKIP-SPACES {: from:n :}
   from u >= if a 0 u FALSE? exit then
   from begin dup u < while
      dup a + c@ SPACE-BYTE = if
         a from +  over from -  rot  TRUE? exit
      then
      1+
   repeat drop
   a from +  u from -  u  TRUE? ;

: NEXT$ ( -- ptr u8 n bool )
   LINE$ CURSOR @ TOKEN
   swap CURSOR ! ;

: NEXT-NUMBER ( -- n bool )
   NEXT$ 0= if 2drop 0 FALSE? exit then
   STR>NUMBER? MATCH option
     none OF 0 FALSE? ENDOF
     some OF TRUE? ENDOF
   ;MATCH ;

\ ---- parsed row store ------------------------------------------------------

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a  ROW-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS ROW-N @ SLOT ! ;

: ROW-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k NAME-AT NAME-LENS k SLOT @ ;

: ROW-PATH ( n -- n ) {: k:n :}
   PATHS k SLOT @ ;

: ROW-SIZE ( n -- n ) {: k:n :}
   SIZES k SLOT @ ;

: ROW-COST ( n -- n ) {: k:n :}
   COSTS k SLOT @ ;

: ROW-OUTPUTS ( n -- n ) {: k:n :}
   OUT-COUNTS k SLOT @ ;

: BASELINE-OUTPUT ( n n -- n ) {: b:n j:n :}
   OUT-VALUES b CODEGEN-COMPARE:OUTPUT-MAX * j + SLOT @ ;

: OUTPUT+ ( n -- ) {: value:n :}
   OUT-COUNTS ROW-N @ SLOT @ {: j:n :}
   j CODEGEN-COMPARE:OUTPUT-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   value OUT-VALUES ROW-N @ CODEGEN-COMPARE:OUTPUT-MAX * j + SLOT !
   j 1+ OUT-COUNTS ROW-N @ SLOT ! ;

: SAME-ROW? ( n n -- bool ) {: k:n j:n :}
   k ROW-PATH j ROW-PATH <> if FALSE? exit then
   k ROW-NAME$ j ROW-NAME$ STR= ;

: DUPLICATE-CHECK ( -- )
   0 begin dup ROW-N @ < while
      dup ROW-N @ SAME-ROW? if
         s" DUPLICATE-ROW" FINDING
         ROW-N @ ROW-NAME$ SAY
         s"  appears twice in the baseline" SAY SAY-END
         FIND+
         drop exit
      then
      1+
   repeat drop ;

: MALFORMED ( ptr u8 n -- )
   s" MALFORMED-ROW" FINDING
   SAY SAY-END FIND+ ;

\ ---- parsing ---------------------------------------------------------------

: OUTPUT-TAIL ( -- )
   begin
      NEXT-NUMBER 0= if drop exit then
      OUTPUT+
   again ;

: PARSE-ROW ( n -- ) {: path:n :}
   ROW-N @ CODEGEN-COMPARE:ROW-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   path PATHS ROW-N @ SLOT !
   0 OUT-COUNTS ROW-N @ SLOT !
   NEXT$ 0= if
      2drop s" a row with no word name" MALFORMED exit
   then
   NAME!
   NEXT-NUMBER 0= if
      drop ROW-N @ ROW-NAME$ MALFORMED exit
   then
   SIZES ROW-N @ SLOT !
   NEXT-NUMBER 0= if
      drop ROW-N @ ROW-NAME$ MALFORMED exit
   then
   COSTS ROW-N @ SLOT !
   OUTPUT-TAIL
   DUPLICATE-CHECK
   ROW-N @ 1+ ROW-N ! ;

: PARSE-DECLARED ( -- )
   NEXT-NUMBER 0= if
      drop s" the rows: line carries no number" MALFORMED exit
   then
   DECLARED-ROWS ! ;

: SCAN-LINE ( ptr u8 n -- )
   LINE!
   NEXT$ 0= if 2drop exit then
   2dup CODEGEN-COMPARE:PATH-OLD$ STR= if 2drop CODEGEN-COMPARE:PATH-OLD PARSE-ROW exit then
   2dup CODEGEN-COMPARE:PATH-NEW$ STR= if 2drop CODEGEN-COMPARE:PATH-NEW PARSE-ROW exit then
   s" rows:" STR= if PARSE-DECLARED then ;

: NEXT-LINE ( -- ptr u8 n bool )
   FILE-TEXT FILE-U @ NEWLINE-BYTE TEXT-CURSOR @ SPLIT-NEXT
   swap TEXT-CURSOR ! ;

: SCAN-TEXT ( -- )
   0 TEXT-CURSOR !
   begin
      NEXT-LINE 0= if 2drop exit then
      SCAN-LINE
   again ;

: READ-BASELINE ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u EXISTS? 0= if
      s" NO-BASELINE" FINDING
      a u SAY
      s"  does not exist; regenerate it with --update" SAY SAY-END
      FIND+ FALSE? exit
   then
   a u FILE-TEXT FILE-CAP READ-ALL FILE-U !
   TRUE? ;

public

: LOUD! ( -- )
   -1 REPORT? ! ;

: QUIET! ( -- )
   0 REPORT? ! ;

\ Compare the cost column and the pass budget, or leave both out. See the note
\ at the head of this file for what a loaded host does to a timing.
: COSTS-CHECKED! ( -- )
   -1 COSTS? ! ;

: COSTS-UNCHECKED! ( -- )
   0 COSTS? ! ;

\ Whether this run compares timings at all. The caller that decides also has to
\ leave every other timed check out of an untimed run, so the decision is read
\ back here rather than taken twice.
: COSTS-CHECKED? ( -- bool )
   COSTS? @ 0<> ;

\ Where a committed table lives is a fact about the corpus it is the measurement
\ of, so each corpus states its own path beside its cases - CODEGEN-CASES and
\ CODEGEN-CASES2 - and this reader is handed one. It reads whichever it is
\ given, which is what lets one reader serve both tables.

\ Read and parse the committed table. Resets the finding count: a malformed or
\ missing file is itself a finding, and COMPARE adds to the same count.
: LOAD ( ptr u8 n -- )
   0 FINDINGS !
   0 ROW-N !
   -1 DECLARED-ROWS !
   0 FILE-U !
   READ-BASELINE 0= if exit then
   SCAN-TEXT
   DECLARED-ROWS @ ROW-N @ <> if
      s" ROW-COUNT" FINDING
      s" the file declares " SAY DECLARED-ROWS @ SAY-NUM
      s"  rows but carries " SAY ROW-N @ SAY-NUM SAY-END
      FIND+
   then ;

: ROWS ( -- n )
   ROW-N @ ;

private

: FIND-BASELINE ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup ROW-N @ < while
      dup ROW-PATH CODEGEN-COMPARE:PATH-OLD = if
         dup ROW-NAME$ a u STR= if exit then
      then
      1+
   repeat drop -1 ;

: SIZE-CHECK ( n n -- ) {: k:n b:n :}
   k CODEGEN-COMPARE:SIZE b ROW-SIZE = if exit then
   s" SIZE" FINDING
   k CODEGEN-COMPARE:NAME$ SAY
   s"  is " SAY k CODEGEN-COMPARE:SIZE SAY-NUM
   s"  bytes of machine code, baseline says " SAY b ROW-SIZE SAY-NUM
   SAY-END FIND+ ;

: OUTPUT-VALUE-CHECK ( n n n -- bool ) {: k:n b:n j:n :}
   k j CODEGEN-COMPARE:OUTPUT b j BASELINE-OUTPUT = ;

: OUTPUT-COUNT-SAME? ( n n -- bool ) {: k:n b:n :}
   k CODEGEN-COMPARE:OUTPUTS b ROW-OUTPUTS = ;

: OUTPUT-VALUES-SAME? ( n n -- bool ) {: k:n b:n :}
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      dup k swap b swap OUTPUT-VALUE-CHECK 0= if drop FALSE? exit then
      1+
   repeat drop TRUE? ;

: SAY-BASELINE-OUTPUTS ( n -- ) {: b:n :}
   0 begin dup OUT-COUNTS b SLOT @ < while
      s"  " SAY
      dup b swap BASELINE-OUTPUT SAY-NUM
      1+
   repeat drop ;

: SAY-MEASURED-OUTPUTS ( n -- ) {: k:n :}
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      s"  " SAY
      dup k swap CODEGEN-COMPARE:OUTPUT SAY-NUM
      1+
   repeat drop ;

: OUTPUT-CHECK ( n n -- ) {: k:n b:n :}
   k b OUTPUT-COUNT-SAME? if
      k b OUTPUT-VALUES-SAME? if exit then
   then
   s" OUTPUT" FINDING
   k CODEGEN-COMPARE:NAME$ SAY
   s"  produced" SAY k SAY-MEASURED-OUTPUTS
   s" , baseline says" SAY b SAY-BASELINE-OUTPUTS
   SAY-END FIND+ ;

: COST-CHECK ( n n -- ) {: k:n b:n :}
   b ROW-COST CODEGEN-COMPARE:COST-BAND * {: ceiling:n :}
   k CODEGEN-COMPARE:COST ceiling <= if exit then
   s" SLOWER" FINDING
   k CODEGEN-COMPARE:NAME$ SAY
   s"  costs " SAY k CODEGEN-COMPARE:COST SAY-NUM
   s" , baseline " SAY b ROW-COST SAY-NUM
   s" , allowed up to " SAY ceiling SAY-NUM
   SAY-END FIND+ ;

\ A new row is measured live and committed nowhere, so it has nothing to be
\ compared with here. Its outputs are checked against the old row's by
\ tools/codegen-compare-new.f, which is where the head-to-head comparison lives.
: MEASURED-ROW-CHECK ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD <> if exit then
   k CODEGEN-COMPARE:NAME$ FIND-BASELINE {: b:n :}
   b 0 < if
      s" MISSING-ROW" FINDING
      k CODEGEN-COMPARE:NAME$ SAY
      s"  was measured but the baseline has no row for it" SAY SAY-END
      FIND+ exit
   then
   k b SIZE-CHECK
   k b OUTPUT-CHECK
   COSTS? @ if k b COST-CHECK then ;

: EXTRA-ROW-CHECK ( n -- ) {: b:n :}
   b ROW-PATH CODEGEN-COMPARE:PATH-OLD = if
      CODEGEN-COMPARE:PATH-OLD b ROW-NAME$ CODEGEN-COMPARE:FIND-ROW 0 >= if exit then
   then
   s" EXTRA-ROW" FINDING
   b ROW-NAME$ SAY
   s"  is in the baseline but was not measured" SAY SAY-END
   FIND+ ;

: BUDGET-CHECK ( -- )
   CODEGEN-COMPARE:OVER-BUDGET? 0= if exit then
   s" BUDGET" FINDING
   s" the measurement pass took " SAY CODEGEN-COMPARE:PASS-MS@ SAY-NUM
   s"  ms, past its " SAY CODEGEN-COMPARE:BUDGET-MS SAY-NUM
   s"  ms budget; the timings are not trustworthy on this host" SAY
   SAY-END FIND+ ;

\ What a run that left the timings out says instead. It is not a finding: it is
\ the one line that keeps an unchecked cost column from reading as a checked one.
: COST-NOTE ( -- )
   s" codegen-compare: COST-UNCHECKED the cost column and the pass budget were" SAY
   s"  not compared: host load under a parallel gate moves a timing further than" SAY
   s"  the tolerance band. The timed check is bin/hb --load tools/codegen-compare.f" SAY
   SAY-END ;

public

\ Compare the rows just measured with the rows LOAD parsed. Adds to the finding
\ count LOAD started and returns the total.
: COMPARE ( -- n )
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup MEASURED-ROW-CHECK
      1+
   repeat drop
   0 begin dup ROW-N @ < while
      dup EXTRA-ROW-CHECK
      1+
   repeat drop
   COSTS? @ if BUDGET-CHECK else COST-NOTE then
   FINDINGS @ ;

: FINDINGS@ ( -- n )
   FINDINGS @ ;

private

: INIT ( -- )
   -1 REPORT? !
   -1 COSTS? !
   0 FINDINGS !
   0 ROW-N ! ;

INIT

;package
