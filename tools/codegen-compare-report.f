\ codegen-compare-report.f - rendering for the codegen comparison.
\ One concern: turning recorded rows into text a person can read.
\
\ Two renderings, because they answer different questions:
\
\   BASELINE$  a committed table - there is one per pinned corpus, and which one
\              is being written is the corpus source path the caller hands over.
\              It carries only the OLD code generator's
\              rows, and only the numbers that are stable across runs and
\              machines - the compiled size, the outputs, and the cost expressed
\              as a multiple of an empty call - so the file in the repository
\              changes when that emitter changes and at no other time. The new
\              chain's rows are deliberately not written: it is still gaining
\              capabilities, its numbers move with each one, and pinning them
\              would turn an advance into a red gate. They are recomputed every
\              run and checked against the old rows live instead.
\   PRINT      the report a person reads after a run. It is the head-to-head
\              table - each corpus word's old and new bytes, costs and whether
\              the two agree about what the word computes - followed by the
\              words the new chain cannot compile yet and what each is waiting
\              for, and then the measurement detail that is true of this run
\              only: absolute nanoseconds per call, how far apart the timed runs
\              were, and how long the whole pass took.
\
\ Absolute nanoseconds are deliberately absent from the committed table. They
\ are a property of the machine that measured them, so storing them would make
\ the file disagree with itself on every other host, while the ratio to an empty
\ call measured in the same run stays comparable.
\
\ THE TWO COST COLUMNS ARE THE SAME KIND OF NUMBER, AND THE REPORT SAYS SO WITH A
\ MEASUREMENT RATHER THAN A PARAGRAPH. Both columns are ordinary words of the
\ dictionary now - the old ones the engine's emitter compiled, the new ones the
\ native chain compiled and the publication seam republished - so both are
\ entered by whatever the engine does for a call, and each column's empty call
\ measures the same thing. The report prints both floors on one line, and
\ FLOOR-GAP compares them: while they agree the two ratios are comparable with
\ each other, and if they ever stop agreeing that is a finding naming the two
\ numbers rather than a caveat asking the reader to allow for it. The byte and
\ result columns never needed such an allowance: they are exact.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpora.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-baseline.f

package CODEGEN-REPORT

$2000 constant TEXT-CAP
TEXT-CAP BUFFER: TEXT
variable TEXT-U

4 constant PATH-COL
28 constant NAME-COL
5 constant SIZE-COL
6 constant COST-COL
2 constant GAP-COL
9 constant WIDE-COL                  \ a head-to-head column, wide enough for a cost
6 constant VERDICT-COL
10 constant NEWLINE-BYTE
32 constant SPACE-BYTE
45 constant DASH-BYTE

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   TEXT-U @ u + TEXT-CAP > if E-CODEGEN-COMPARE-CAP throw then
   a  TEXT TEXT-U @ +  u STR-LEN BYTE-COPY-LEN
   TEXT-U @ u + TEXT-U ! ;

: APPEND-C ( n -- ) {: c:n :}
   TEXT-U @ 1+ TEXT-CAP > if E-CODEGEN-COMPARE-CAP throw then
   c TEXT TEXT-U @ + c!
   TEXT-U @ 1+ TEXT-U ! ;

: NL ( -- )
   NEWLINE-BYTE APPEND-C ;

: LINE ( ptr u8 n -- )
   APPEND NL ;

: NUM$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

: NUM ( n -- )
   NUM$ APPEND ;

: NUM-WIDTH ( n -- n )
   NUM$ nip ;

: PAD ( n -- )
   dup 0 <= if drop exit then
   0 ?do SPACE-BYTE APPEND-C loop ;

: PAD-RIGHT ( ptr u8 n n -- ) {: width:n :}
   dup {: used:n :}
   APPEND
   width used - PAD ;

: PAD-LEFT ( n n -- ) {: value:n width:n :}
   width value NUM-WIDTH - PAD
   value NUM ;

\ ---- the committed table ---------------------------------------------------

: TITLE ( -- )
   s" habu code generator comparison - baseline" LINE
   s" =========================================" LINE
   NL ;

\ Which corpus this table is of, and the command that rewrites THIS table. The
\ measurement is the same for every corpus, so the text is one text and the
\ corpus tells a reader which table is in front of them. Both come from the
\ corpus's own declaration, so the instruction a reader follows regenerates the
\ file the instruction is printed in and leaves the other three alone.
: WHAT-IT-IS ( n -- ) {: k:n :}
   s" What this file is" LINE
   s" -----------------" LINE
   s" This is the recorded measurement of the code generator bin/hb uses today," LINE
   s" over the pinned word corpus in " APPEND
   k CODEGEN-CORPORA:SOURCE$ APPEND s" . The harness" LINE
   s" recreates this table on every run and compares it with this committed copy," LINE
   s" field by field. Nothing here is written by hand; regenerate it with" LINE
   NL
   s"     bin/hb --load tools/codegen-compare.f -- --update " APPEND
   k CODEGEN-CORPORA:NAME$ APPEND NL
   NL
   s" The new compiler chain compiles the corpus words it can express in the same" LINE
   s" run, and the harness prints the two side by side - bytes, cost, and whether" LINE
   s" the two answers agree. Those rows are not written here. The chain is still" LINE
   s" gaining capabilities and its numbers move with each one, so pinning them" LINE
   s" would turn an advance into a red gate; they are recomputed every run and" LINE
   s" compared with the old rows live instead. Every row in this file is therefore" LINE
   s" marked old. The reader still understands the word new, so a hand-added new" LINE
   s" row is reported as an extra row rather than quietly ignored." LINE
   NL ;

: HOW-TO-READ ( -- )
   s" How to read a row" LINE
   s" -----------------" LINE
   s" Every line that begins with the word old or new is a data row; every other" LINE
   s" line is prose. A row carries, in order:" LINE
   NL
   s"   path     which code generator produced the row" LINE
   s"   word     the corpus word that was compiled, executed and timed" LINE
   s"   bytes    how many bytes of machine code the word occupies, read from the" LINE
   s"            word's own dictionary record - the record holds its code start" LINE
   s"            address and its code length" LINE
   s"   cost     how long one call takes, in thousandths of the cost of calling" LINE
   s"            the empty word CODEGEN-CORPUS:NOOP in the same run. 1000 means" LINE
   s"            an ordinary empty call; 4000 means four times that. Absolute" LINE
   s"            nanoseconds are printed by the harness but not stored here," LINE
   s"            because they differ from machine to machine while this ratio" LINE
   s"            largely does not." LINE
   s"   outputs  the values the word left on the stack when it ran on its pinned" LINE
   s"            inputs, in the order the harness recorded them" LINE
   NL
   s" Sizes and outputs are compared exactly: one byte or one value out of place" LINE
   s" is a finding. A cost is a measurement, so it is compared with a stated" LINE
   s" tolerance instead. A row is reported only when it measures more than" LINE
   CODEGEN-COMPARE:COST-BAND NUM
   s"  times slower than the cost recorded here. That tolerance is measured," LINE
   s" not chosen; tools/codegen-compare-core.f records the load it was taken" LINE
   s" under, and says plainly what a timing check of this shape can and cannot" LINE
   s" catch." LINE
   NL ;

: TABLE-HEAD ( -- )
   s" rows: " APPEND CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:ROWS-OF NUM NL
   NL
   s" path  word                          bytes    cost  outputs" LINE
   s" ----  ----------------------------  -----  ------  -------" LINE ;

: OUTPUT-LIST ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:OUTPUTS 0= if exit then
   GAP-COL PAD
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      dup 0 > if 1 PAD then
      dup k swap CODEGEN-COMPARE:OUTPUT NUM
      1+
   repeat drop ;

: ROW ( n -- ) {: k:n :}
   CODEGEN-COMPARE:PATH-OLD$ PATH-COL PAD-RIGHT
   GAP-COL PAD
   k CODEGEN-COMPARE:NAME$ NAME-COL PAD-RIGHT
   GAP-COL PAD
   k CODEGEN-COMPARE:SIZE SIZE-COL PAD-LEFT
   GAP-COL PAD
   k CODEGEN-COMPARE:COST COST-COL PAD-LEFT
   k OUTPUT-LIST
   NL ;

: TABLE-ROWS ( -- )
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if dup ROW then
      1+
   repeat drop ;

public

\ The committed table of one declared corpus. Nothing else about the corpora
\ differs here: all four are measured the same way and read back the same way,
\ and what tells a reader which table is in front of them is the corpus the
\ register names. The corpus is given by its register index rather than by its
\ path, so the file's own prose and the argument that regenerates it come from
\ one declaration.
: BASELINE$ ( n -- ptr u8 n ) {: k:n :}
   0 TEXT-U !
   TITLE
   k WHAT-IT-IS
   HOW-TO-READ
   TABLE-HEAD
   TABLE-ROWS
   TEXT TEXT-U @ ;

private

\ ---- the head-to-head table ------------------------------------------------
\ One line per corpus word: what the old emitter made of it, what the new chain
\ made of it, and whether the two compute the same thing. A word the new chain
\ cannot compile yet carries a dash in its columns and is named again, with the
\ capabilities it is waiting for, in the list underneath.

: DASH-COL ( -- )
   WIDE-COL 1- PAD DASH-BYTE APPEND-C ;

: VERDICT ( ptr u8 n -- )
   VERDICT-COL over - PAD APPEND ;

: PAIR-HEAD ( -- )
   s" word                          old bytes  new bytes   old cost   new cost  result" LINE
   s" ----------------------------  ---------  ---------  ---------  ---------  ------" LINE ;

: COVERED-PAIR ( n n -- ) {: k:n j:n :}
   k CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   j CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   k CODEGEN-COMPARE:COST WIDE-COL PAD-LEFT
   GAP-COL PAD
   j CODEGEN-COMPARE:COST WIDE-COL PAD-LEFT
   GAP-COL PAD
   j CODEGEN-COMPARE:ROW-MATCH? if s" same" VERDICT else s" DIFFERS" VERDICT then ;

: UNCOVERED-PAIR ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   DASH-COL
   GAP-COL PAD
   k CODEGEN-COMPARE:COST WIDE-COL PAD-LEFT
   GAP-COL PAD
   DASH-COL
   GAP-COL PAD
   s" -" VERDICT ;

: PAIR-ROW ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:NAME$ NAME-COL PAD-RIGHT
   GAP-COL PAD
   CODEGEN-COMPARE:PATH-NEW k CODEGEN-COMPARE:NAME$ CODEGEN-COMPARE:FIND-ROW {: j:n :}
   j 0 < if k UNCOVERED-PAIR else k j COVERED-PAIR then
   NL ;

: PAIR-ROWS ( -- )
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if dup PAIR-ROW then
      1+
   repeat drop ;

: GAP-CAP-LIST ( n -- ) {: k:n :}
   0 begin dup k CODEGEN-GAP:GAP-CAPS@ < while
      dup 0 > if s" , " APPEND then
      dup k swap CODEGEN-GAP:GAP-CAP@ CODEGEN-GAP:CAP$ APPEND
      1+
   repeat drop ;

: GAP-ROW ( n -- ) {: k:n :}
   s"   " APPEND
   k CODEGEN-GAP:GAP-NAME$ NAME-COL PAD-RIGHT
   GAP-COL PAD
   k GAP-CAP-LIST
   NL ;

: GAP-LIST ( -- )
   CODEGEN-GAP:GAPS 0= if exit then
   NL
   s" not yet compiled by the new chain, and what each is waiting for:" LINE
   0 begin dup CODEGEN-GAP:GAPS < while
      dup GAP-ROW
      1+
   repeat drop ;

: COUNTS ( -- )
   s" corpus words: " APPEND
   CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:ROWS-OF NUM
   s" , compiled by the new chain: " APPEND
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:ROWS-OF NUM
   s" , not yet: " APPEND
   CODEGEN-GAP:GAPS NUM
   NL ;

: PAIRS$ ( -- ptr u8 n )
   0 TEXT-U !
   COUNTS
   NL
   PAIR-HEAD
   PAIR-ROWS
   GAP-LIST
   TEXT TEXT-U @ ;

\ ---- the measurement detail ------------------------------------------------

: FRACTION. ( n -- ) {: frac:n :}
   frac 100 < if s" 0" type then
   frac 10 < if s" 0" type then
   frac FMT:.U ;

: NANOS. ( n -- ) {: picos:n :}
   picos CODEGEN-COMPARE:PICOS-PER-NS / FMT:.U
   s" ." type
   picos CODEGEN-COMPARE:PICOS-PER-NS mod FRACTION. ;

: PRINT-OUTPUTS ( n -- ) {: k:n :}
   0 begin dup k CODEGEN-COMPARE:OUTPUTS < while
      s"  " type
      dup k swap CODEGEN-COMPARE:OUTPUT FMT:.INT
      1+
   repeat drop ;

: PRINT-ROW ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH$ type
   s"   " type
   k CODEGEN-COMPARE:NAME$ type
   s"   bytes " type k CODEGEN-COMPARE:SIZE FMT:.U
   s"   cost " type k CODEGEN-COMPARE:COST FMT:.U
   s"   ns/call " type k CODEGEN-COMPARE:PICOSECONDS NANOS.
   s"   run spread " type k CODEGEN-COMPARE:SPREAD FMT:.U s" /1000" type
   s"   outputs" type k PRINT-OUTPUTS
   cr ;

\ ---- the cost of the routine, with the call taken off ------------------------
\ Both columns are entered the same way now, so the floor each path measures is
\ the same call and taking it off leaves the emitted code - which is the number
\ the comparison is about. Each path still measures its own floor rather than
\ sharing one, because the agreement between them is a fact about the publication
\ seam that can regress, and a shared floor could not report it.
: PATH-FLOOR ( n -- ) {: path:n :}
   path CODEGEN-COMPARE:PATH-PICOS NANOS. ;

\ A row that measured faster than its own empty call is host noise, and it is
\ printed as the negative number it is rather than clamped to zero: a reader who
\ sees a minus sign knows the row is at the resolution of the measurement, and a
\ clamp would have hidden exactly that.
: BODY-NS ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:PICOSECONDS
   k CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-PICOS - {: d:n :}
   d 0 < if s" -" type d negate NANOS. exit then
   d NANOS. ;

: BODY-ROW ( n n -- ) {: k:n j:n :}
   s"   " type
   k CODEGEN-COMPARE:NAME$ type
   s"   old " type k BODY-NS
   s"  ns   new " type j BODY-NS
   s"  ns" type cr ;

\ ---- which rows the new column costs more on ---------------------------------
\ The one line a reader of a timed run is really after, printed rather than left
\ to be worked out from a column of eleven pairs. It is printed ONLY when the
\ cost column is being compared - that is, by the hand-run timed entry and not
\ by a gate - because on a loaded host the direction of a nine-per-cent gap is
\ host noise and printing it would invite somebody to read it as a result. Dot
\ habu-sweep-timing-assertions-2282630b moved the assertions that used to state
\ this out of the scheduled suite for the same reason.

: COSTLIER-ROW? ( n n -- bool ) {: k:n j:n :}
   j CODEGEN-COMPARE:COST  k CODEGEN-COMPARE:COST  > ;

: SAY-COSTLIER ( -- )
   CODEGEN-BASELINE:COSTS-CHECKED? 0= if exit then
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW {: j:n :}
         j 0 >= if
            i j COSTLIER-ROW? if
               dup 0= if s" rows the new column costs MORE on:" type cr then
               s"   " type i CODEGEN-COMPARE:NAME$ type cr
               1+
            then
         then
      then
   loop
   0= if s" rows the new column costs more on: none" type cr then ;

: BODIES ( -- )
   s" Cost of the emitted code, with the entry taken off. Both columns are entered" type cr
   s" the way the engine enters any word, so each path's own empty call is the same" type cr
   s" floor and what is printed here is the row minus that floor." type cr
   s" empty call: old " type CODEGEN-COMPARE:PATH-OLD PATH-FLOOR
   s"  ns, new " type CODEGEN-COMPARE:PATH-NEW PATH-FLOOR
   s"  ns" type cr
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW {: j:n :}
         j 0 >= if i j BODY-ROW then
      then
   loop
   SAY-COSTLIER ;

\ ---- do the two floors still measure the same call? --------------------------
\ The comparison's two ratio columns are only comparable with each other while
\ they are divided by the same thing. That used to be false and was written down
\ as a caveat; it is true now because both columns are entered as ordinary words,
\ and what makes it a claim rather than an assumption is this check.
\
\ THE BAND. Two measurements of the same call differ by host noise, which the
\ recorded run spread puts in the low percent. The difference this check exists
\ to catch is an entry that goes back to being indirect, which cost the new
\ column about two and a half times its floor when it did. Half again is
\ therefore far above the noise and far below the defect.
500 constant FLOOR-BAND              \ permille each floor may differ from the other

: FLOOR-DELTA ( -- n )
   CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:PATH-PICOS {: old:n :}
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:PATH-PICOS {: new:n :}
   old new - dup 0 < if negate then ;

: FLOOR-SMALLER ( -- n )
   CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:PATH-PICOS {: old:n :}
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:PATH-PICOS {: new:n :}
   old new < if old exit then
   new ;

public

\ Report the two floors as one finding if they have stopped measuring the same
\ call, and answer how many findings that was.
: SAY-FLOOR-GAP ( -- n )
   FLOOR-SMALLER {: base:n :}
   base 0= if E-CODEGEN-COMPARE-CLOCK throw then
   FLOOR-DELTA CODEGEN-COMPARE:COST-UNIT * base / {: permille:n :}
   permille FLOOR-BAND <= if 0 exit then
   s" codegen-compare: ENTRY the two empty calls no longer measure the same call: old " type
   CODEGEN-COMPARE:PATH-OLD PATH-FLOOR
   s"  ns, new " type CODEGEN-COMPARE:PATH-NEW PATH-FLOOR
   s"  ns" type cr
   1 ;

private

\ ---- the head-to-head finding ----------------------------------------------
\ The one thing the comparison must never let past: the same corpus word
\ compiled two ways computing two different answers on the same pinned inputs.
\ Reported by name, with both answers, and counted into the run's findings.

: SAY-OUTPUTS ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:OUTPUTS 0= if s"  nothing" type exit then
   k PRINT-OUTPUTS ;

: SAY-MISMATCH ( n -- ) {: j:n :}
   s" codegen-compare: RESULT " type
   j CODEGEN-COMPARE:NAME$ type
   s"  compiled by the new chain produces" type j SAY-OUTPUTS
   s" , the old emitter produces" type
   j CODEGEN-COMPARE:PARTNER {: k:n :}
   k 0 < if s"  no row at all" type else k SAY-OUTPUTS then
   cr ;

public

\ Name every corpus word the two code generators disagree about, and answer how
\ many there were.
: SAY-MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i CODEGEN-COMPARE:ROW-MATCH? 0= if i SAY-MISMATCH 1+ then
      then
   loop ;

: PRINT ( -- )
   s" code generator comparison: old (the emitter bin/hb uses today)" type cr
   s"                            new (the native chain)" type cr
   s" repetitions per timed run: " type CODEGEN-COMPARE:REPS FMT:.U
   s" , timed runs per word: " type CODEGEN-COMPARE:RUNS FMT:.U
   s"  (the fastest run is the one recorded)" type cr
   s" measurement pass: " type CODEGEN-COMPARE:PASS-MS@ FMT:.U
   s"  ms, budget " type CODEGEN-COMPARE:BUDGET-MS FMT:.U s"  ms" type cr
   cr
   PAIRS$ type
   cr
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup PRINT-ROW
      1+
   repeat drop
   cr
   BODIES ;

;package
