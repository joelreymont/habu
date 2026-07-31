\ codegen-compare-report.f - rendering for the codegen comparison.
\ One concern: turning recorded rows into text a person can read.
\
\ Two renderings, because they answer different questions:
\
\   BASELINE$  the committed table. It carries only the OLD code generator's
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
\ READ THE TWO COST COLUMNS WITH THE TWO NOTES THIS FILE PRINTS UNDER THEM. Both
\ paths are entered by a branch now: an old row is an ordinary Habu word call,
\ and a new row is that same branch through `execute`, because the routines the
\ chain emits take their arguments off the data stack the way a word does. One
\ difference is left - the new column's address is on the stack rather than
\ compiled into the call site, which is one indirect branch more - so each column
\ is still a multiple of an empty call of its OWN kind and the two floors are not
\ the same number. Rather than leave a reader to guess what that is worth, the
\ report prints both floors and every covered word's cost with its floor taken
\ off, which is the emitted code and nothing else. The byte and result columns
\ carry no such caveat: they are exact.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f
require tools/codegen-compare-new.f

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

: WHAT-IT-IS ( -- )
   s" What this file is" LINE
   s" -----------------" LINE
   s" This is the recorded measurement of the code generator bin/hb uses today," LINE
   s" over the pinned word corpus in tools/codegen-compare-corpus.f. The harness" LINE
   s" recreates this table on every run and compares it with this committed copy," LINE
   s" field by field. Nothing here is written by hand; regenerate it with" LINE
   NL
   s"     bin/hb --load tools/codegen-compare.f -- --update" LINE
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

: BASELINE$ ( -- ptr u8 n )
   0 TEXT-U !
   TITLE
   WHAT-IT-IS
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
   j CODEGEN-NEW:ROW-MATCH? if s" same" VERDICT else s" DIFFERS" VERDICT then ;

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
   0 begin dup k CODEGEN-NEW:GAP-CAPS@ < while
      dup 0 > if s" , " APPEND then
      dup k swap CODEGEN-NEW:GAP-CAP@ CODEGEN-NEW:CAP$ APPEND
      1+
   repeat drop ;

: GAP-ROW ( n -- ) {: k:n :}
   s"   " APPEND
   k CODEGEN-NEW:GAP-NAME$ NAME-COL PAD-RIGHT
   GAP-COL PAD
   k GAP-CAP-LIST
   NL ;

: GAP-LIST ( -- )
   CODEGEN-NEW:GAPS 0= if exit then
   NL
   s" not yet compiled by the new chain, and what each is waiting for:" LINE
   0 begin dup CODEGEN-NEW:GAPS < while
      dup GAP-ROW
      1+
   repeat drop ;

: COUNTS ( -- )
   s" corpus words: " APPEND
   CODEGEN-COMPARE:PATH-OLD CODEGEN-COMPARE:ROWS-OF NUM
   s" , compiled by the new chain: " APPEND
   CODEGEN-COMPARE:PATH-NEW CODEGEN-COMPARE:ROWS-OF NUM
   s" , not yet: " APPEND
   CODEGEN-NEW:GAPS NUM
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
\ Both columns are entered by a branch now: an old row is an ordinary Habu word
\ call and a new row is that same branch through `execute`, because the routines
\ the chain emits take their arguments off the data stack the way a word does.
\ One difference is left, and it is why each path still keeps its own calibration
\ row: the new column's address is on the stack rather than compiled into the
\ call site, so its entry is one indirect branch more. That difference is a
\ constant, it is exactly what an empty call of each kind measures, and
\ subtracting it leaves the emitted code - which is the number the comparison is
\ about. Both the floor and the difference are printed, so nothing is hidden in a
\ ratio.
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

: BODIES ( -- )
   s" Cost of the emitted code, with the entry taken off. Both columns are entered" type cr
   s" by a branch; the new column's goes through the address on the stack, which is" type cr
   s" one indirect branch more, so each path's own empty call is the floor and what" type cr
   s" is printed here is the row minus that floor." type cr
   s" empty call: old " type CODEGEN-COMPARE:PATH-OLD PATH-FLOOR
   s"  ns, new " type CODEGEN-COMPARE:PATH-NEW PATH-FLOOR
   s"  ns" type cr
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW {: j:n :}
         j 0 >= if i j BODY-ROW then
      then
   loop ;

\ What the two ratio columns of the table do and do not say. Printed with every
\ run, because a reader who takes them for a like-for-like race is reading them
\ wrongly: each is a multiple of an empty call of its own kind, and the two
\ floors are not the same number.
: CAVEAT ( -- )
   s" How to read the two cost columns of the table. Each is a multiple of an empty" type cr
   s" call of its OWN kind, and the two floors differ - an old row is entered by a" type cr
   s" branch the engine compiled into the call site, a new row by a branch through" type cr
   s" an address on the stack. So the ratios are not comparable with each other;" type cr
   s" the nanoseconds above and the entry-subtracted figures below are. The bytes" type cr
   s" and the results carry no such caveat: they are exact." type cr ;

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
   j CODEGEN-NEW:PARTNER {: k:n :}
   k 0 < if s"  no row at all" type else k SAY-OUTPUTS then
   cr ;

public

\ Name every corpus word the two code generators disagree about, and answer how
\ many there were.
: SAY-MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i CODEGEN-NEW:ROW-MATCH? 0= if i SAY-MISMATCH 1+ then
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
   BODIES
   cr
   CAVEAT ;

;package
