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
\              for, then the rows the new column is BIGGER on, which is the byte
\              column adjudicated instead of left in a table for a reader to
\              work out, and then the measurement detail that is true of this run
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
\ result columns never needed such an allowance: they are exact. THAT IS ALSO
\ WHY THE THREE COLUMNS ARE NOT ADJUDICATED ALIKE. The two exact columns are
\ findings - a wrong answer by SAY-MISMATCHES, a bigger word by SAY-BYTE-LOSSES,
\ and either exits the run non-zero - while the cost direction is printed and
\ counted nowhere, because a gate runs on a machine whose load can move it.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpora.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-baseline.f
require tools/codegen-compare-clang.f
require tools/codegen-compare-ns.f

package CODEGEN-REPORT

$2000 constant TEXT-CAP
TEXT-CAP BUFFER: TEXT
variable TEXT-U

variable WRITE-PATH                  \ which column the committed table being written is of

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
   NL ;

\ How a difference is adjudicated. It is the one part of the row prose that is
\ NOT the same for the two tables - the engine's is a frozen artifact and the
\ chain's is a baseline being beaten - so the shared part above stops here and
\ each table says its own.
: HOW-TO-JUDGE ( -- )
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
   s" rows: " APPEND WRITE-PATH @ CODEGEN-COMPARE:ROWS-OF NUM NL
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
   WRITE-PATH @ CODEGEN-COMPARE:PATH$ PATH-COL PAD-RIGHT
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
      dup CODEGEN-COMPARE:PATH@ WRITE-PATH @ = if dup ROW then
      1+
   repeat drop ;

\ ---- the chain's own committed table -----------------------------------------
\ THE SECOND REFERENCE, AND WHY THERE IS ONE. The clang column says how far the
\ chain is from a production compiler. It does not say whether the chain got
\ better or worse since the last time anybody looked, and a lane that closed a
\ gap by two bytes while opening another by ten would have shown up as neither.
\ So the chain's own numbers are pinned too, in a table of their own per corpus,
\ and every run is read against it: a row that grew is a regression against
\ OURSELVES and a finding, a row that shrank is progress and is named so the
\ baseline gets re-pinned deliberately.
\
\ WHY IT IS A SEPARATE FILE FROM THE ENGINE'S TABLE. They are two yardsticks
\ measuring two different things and they move for two different reasons: the
\ engine's table changes when bin/hb's emitter does, which is almost never, and
\ the chain's changes whenever the chain gets better, which is the point of the
\ work. One file with both in it would have made every improvement a diff
\ against the frozen artifact as well.

: CHAIN-TITLE ( -- )
   s" habu code generator comparison - the chain's own baseline" LINE
   s" =========================================================" LINE
   NL ;

: CHAIN-WHAT-IT-IS ( n -- ) {: k:n :}
   s" What this file is" LINE
   s" -----------------" LINE
   s" This is the recorded measurement of the NATIVE CHAIN - the code generator" LINE
   s" being built - over the pinned word corpus in " APPEND
   k CODEGEN-CORPORA:SOURCE$ APPEND s" . It is the" LINE
   s" second of the comparison's two references. The first is the clang column," LINE
   s" which says how far the chain is from a production optimising compiler and is" LINE
   s" measured live because it is a fact about the host's toolchain. This one says" LINE
   s" how far the chain has come from where it was, and it is committed, because a" LINE
   s" baseline you are trying to beat has to be a thing that does not move." LINE
   NL
   s" The bytes below were measured on master cd2d7cf6, and they are the chain as" LINE
   s" it stood at the parity campaign's baseline commit, 5dcb2867. That second" LINE
   s" sentence is a checkable claim and not a date. The four migrated corpora are" LINE
   s" unchanged between the two commits, and the ONE difference in src/compiler is" LINE
   s" the enum member rename in a64-effect.f - `result` became `delivered`, because" LINE
   s" the old spelling collided with the result<T,E> family and stopped the file" LINE
   s" loading. Re-measuring across that rename moved no byte count in this file," LINE
   s" which is how the claim is checked rather than asserted: a rename that had" LINE
   s" touched code generation would have shown up here as a row that grew or shrank," LINE
   s" and the run that pinned these numbers would have said so." LINE
   NL
   s" Regenerate it, deliberately, with" LINE
   NL
   s"     bin/hb --load tools/codegen-compare.f -- --update-chain " APPEND
   k CODEGEN-CORPORA:NAME$ APPEND NL
   NL
   s" How a difference is read, and it is not the way the engine's table is read:" LINE
   NL
   s"   bytes bigger   a REGRESSION against ourselves. It is a finding and the run" LINE
   s"                  exits non-zero. We do not get bigger without saying so." LINE
   s"   bytes smaller  progress. It is NAMED and not counted, so that re-pinning" LINE
   s"                  this file is a decision somebody takes after reading the" LINE
   s"                  diff rather than something that happens quietly." LINE
   s"   cost           informational in both directions, under the tolerance the" LINE
   s"                  engine's table states. A cost is a measurement." LINE
   s"   a row missing  the chain no longer compiles a word it used to. A finding." LINE
   s"   a row added    the chain compiles something new. Named, not counted." LINE
   NL
   s" Outputs are compared exactly in both tables: a row that answers something" LINE
   s" else is not a smaller row, it is a wrong one." LINE
   NL ;

public

\ The committed table of one declared corpus. Nothing else about the corpora
\ differs here: all four are measured the same way and read back the same way,
\ and what tells a reader which table is in front of them is the corpus the
\ register names. The corpus is given by its register index rather than by its
\ path, so the file's own prose and the argument that regenerates it come from
\ one declaration.
: BASELINE$ ( n -- ptr u8 n ) {: k:n :}
   CODEGEN-COMPARE:PATH-OLD WRITE-PATH !
   0 TEXT-U !
   TITLE
   k WHAT-IT-IS
   HOW-TO-READ
   HOW-TO-JUDGE
   TABLE-HEAD
   TABLE-ROWS
   TEXT TEXT-U @ ;

\ The chain's own committed table of one declared corpus, written by the same
\ renderer over the same store with the column selector moved: two tables from
\ one piece of machinery, so a change to how a row is written reaches both.
: CHAIN-BASELINE$ ( n -- ptr u8 n ) {: k:n :}
   CODEGEN-COMPARE:PATH-NEW WRITE-PATH !
   0 TEXT-U !
   CHAIN-TITLE
   k CHAIN-WHAT-IT-IS
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

: NANOS. ( n -- )
   CODEGEN-NS:.NS ;

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

\ A row that measured faster than its own floor is host noise, and it is
\ printed as the negative number it is rather than clamped to zero: a reader who
\ sees a minus sign knows the row is at the resolution of the measurement, and a
\ clamp would have hidden exactly that. Which floor a row is divided by is the
\ store's decision and not this file's - the two habu columns share a path
\ calibration and every reference row carries a floor of its own arity - so this
\ prints what CODEGEN-COMPARE:BODY-PICOS answers and does no arithmetic of its
\ own.
: BODY-NS ( n -- ) {: k:n :}
   k CODEGEN-COMPARE:BODY-PICOS NANOS. ;

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
   s"  ns" type
   CODEGEN-COMPARE:PATH-CLANG CODEGEN-COMPARE:ROWS-OF 0 > if
      s" , clang (an empty foreign call) " type
      CODEGEN-COMPARE:PATH-CLANG PATH-FLOOR s"  ns" type
   then
   cr
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
   s"  in the " type j CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH$ type
   s"  column produces" type j SAY-OUTPUTS
   s" , the old emitter produces" type
   j CODEGEN-COMPARE:PARTNER {: k:n :}
   k 0 < if s"  no row at all" type else k SAY-OUTPUTS then
   cr ;

public

\ Name every corpus word a column disagrees with the engine about, and answer
\ how many there were. EVERY column that is not the engine's is checked, not
\ only the chain's: the clang reference is a reference exactly to the extent
\ that it computes the same function on the same pinned inputs, and a twin that
\ answered something else would be a faster number for a different program. A
\ wrong answer is a finding in either column and the run exits non-zero on it.
: SAY-MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD <> if
         i CODEGEN-COMPARE:ROW-MATCH? 0= if i SAY-MISMATCH 1+ then
      then
   loop ;

private

\ ---- which rows the new column is BIGGER on ----------------------------------
\ The other question the head-to-head table asks and used to leave to the reader.
\ It is answered here the way the RESULT question above it is answered, and NOT
\ the way the cost question below it is: a row the new chain compiles into more
\ bytes than the engine's emitter wrote for the same body is a FINDING, named and
\ counted, and the run exits non-zero on it.
\
\ WHY THE TWO DIRECTIONS ARE TREATED DIFFERENTLY, WHICH IS THE WHOLE DECISION.
\ The head of this file already draws the line: a cost is a measurement and the
\ byte and result columns are exact. SAY-COSTLIER therefore prints and counts
\ nothing, and is printed only by the timed entry, because on a loaded host the
\ direction of a small gap is host noise and a gate that acted on it would fail
\ for the machine it ran on. A byte count is read off the word's own dictionary
\ record; it is the same number on every host, in every run, and moves only when
\ a compiler moves it. So it belongs with the outputs, which are also exact and
\ are also a finding. Treating it like the cost column is how CODEGEN-CORPUS2:
\ T-RES-WALK's 36-against-76 sat green under "rows the new column costs more on:
\ none" - a line that was only ever about a clock.

\ The new column's row for the corpus word an old row names, or -1.
: NEW-PARTNER ( n -- n ) {: k:n :}
   CODEGEN-COMPARE:PATH-NEW k CODEGEN-COMPARE:NAME$ CODEGEN-COMPARE:FIND-ROW ;

: BIGGER-ROW? ( n n -- bool ) {: k:n j:n :}
   j CODEGEN-COMPARE:SIZE  k CODEGEN-COMPARE:SIZE  > ;

\ The same question of an OLD row alone: is there a new row for this word, and is
\ it bigger? A word the chain cannot compile yet has no new row and is not a loss.
: BIGGER-PAIR? ( n -- bool ) {: k:n :}
   k NEW-PARTNER {: j:n :}
   j 0 < if false exit then
   k j BIGGER-ROW? ;

\ ---- the rows that lose on bytes today, and why each one is allowed to --------
\ ONE row of the four corpora is bigger in the new column right now. A row gets
\ on this list for one of two reasons - a capability the chain has not got yet,
\ or a trade a rule of the chain makes deliberately and states - and either way
\ it is written down HERE, by name, with which of the two it is, and the check
\ subtracts it. A gate that went red on it would be red for work that is already
\ scheduled, or for a decision that has already been made and argued.
\
\ THE LIST WAS TWO ROWS AND IS NOW ONE. CODEGEN-CORPUS2:T-RES-WALK - the loop
\ whose test is a call - came off it when the three mechanisms its diagnosis
\ named landed: the pass-through residency (habu-keep-a-pass-8025401f), the block
\ order (habu-order-blocks-to-f6d89653) and the data-stack placement
\ (habu-place-the-data-9f128e58). It compiles to 36 bytes, which is the engine's
\ own number for the same body, instruction for instruction.
\
\ AND THE ONE THAT IS LEFT IS OF THE SECOND KIND, WHICH IS WHY IT HAS NO DOT.
\ CODEGEN-CORPUS4:CALL-FAN-BIG was on this list waiting for
\ habu-measure-inline-cost-031e817e to re-derive the inliner's size rule. That
\ landed, and the honest re-derivation kept the row: a copy costs the callee's
\ body and a call site costs anything from the branch alone to the whole of what
\ its arity allows, so no rule read while the CALLEE is published can promise
\ that a copy is never bigger - the only bound that would is a one-instruction
\ body, which refuses
\ every routine these corpora show copying wins on. The rule therefore bounds
\ what a copy ADDS and says so, and this row is that bound at its widest: five
\ sites whose calls are one instruction each against copies of four, 88 bytes
\ against 36, in about a twentieth of the time. src/compiler/native/inline.f carries
\ the derivation. The row comes off this list if the rule ever tightens past
\ C-MAD's body, and the stale check below is what would say so.
\
\ WHY A LIST OF ROWS AND NOT A TOLERANCE. A tolerance - "a row may be up to so
\ much bigger" - would also absorb the next loss, and the next loss is the whole
\ reason this check exists. A named row absorbs itself and nothing else.
\
\ AND WHY AN ENTRY THAT HAS STOPPED LOSING IS ITSELF A FINDING. Without that the
\ list would outlive the defect: the day the dot lands the row stops being bigger,
\ the entry stays, and the row is free to lose again with nobody told. So the
\ list is checked in both directions and empties itself - which is exactly how
\ the entry above came off.
1 constant KNOWN-LOSSES

: KNOWN-LOSS$ ( n -- ptr u8 n ) {: e:n :}
   e 0 = if s" CODEGEN-CORPUS4:CALL-FAN-BIG" exit then
   E-CODEGEN-COMPARE-ROW throw ;

\ Why each entry is allowed to lose, printed beside the row so a reader of a
\ report does not have to come here to find out.
: KNOWN-WHY$ ( n -- ptr u8 n ) {: e:n :}
   e 0 = if s" five copies of a callee the engine calls; the inline size rule's stated trade, src/compiler/native/inline.f" exit then
   E-CODEGEN-COMPARE-ROW throw ;

: KNOWN-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup KNOWN-LOSSES < while
      dup KNOWN-LOSS$ a u STR= if exit then
      1+
   repeat drop -1 ;

\ ---- the printed adjudication ------------------------------------------------
\ The byte column's own version of the line SAY-COSTLIER prints for the cost
\ column, and printed on EVERY run rather than only on a timed one, for the
\ reason the section head gives: there is nothing about a byte count that a
\ loaded host can move.

: SAY-BIGGER-ROW ( n -- ) {: k:n :}
   s"   " type k CODEGEN-COMPARE:NAME$ type
   s"  " type k NEW-PARTNER CODEGEN-COMPARE:SIZE FMT:.U
   s"  bytes against " type k CODEGEN-COMPARE:SIZE FMT:.U
   k CODEGEN-COMPARE:NAME$ KNOWN-AT {: e:n :}
   e 0 < if cr exit then
   s"  (known: " type e KNOWN-WHY$ type s" )" type cr ;

: BIGGER-ROWS ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i BIGGER-PAIR? if 1+ then
      then
   loop ;

: SAY-BIGGER ( -- )
   BIGGER-ROWS 0= if
      s" rows the new column is BIGGER on: none" type cr exit
   then
   s" rows the new column is BIGGER on:" type cr
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i BIGGER-PAIR? if i SAY-BIGGER-ROW then
      then
   loop ;

\ ---- and the finding it counts -----------------------------------------------

: SAY-LOSS ( n -- ) {: k:n :}
   s" codegen-compare: BIGGER " type
   k CODEGEN-COMPARE:NAME$ type
   s"  compiled by the new chain is " type k NEW-PARTNER CODEGEN-COMPARE:SIZE FMT:.U
   s"  bytes of machine code, the old emitter's is " type k CODEGEN-COMPARE:SIZE FMT:.U
   cr ;

: UNKNOWN-LOSSES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         i BIGGER-PAIR? if
            i CODEGEN-COMPARE:NAME$ KNOWN-AT 0 < if i SAY-LOSS 1+ then
         then
      then
   loop ;

\ A known loss this pass measured in both columns and that is no longer bigger.
\ A pass that does not hold the row at all says nothing about it: the store holds
\ one corpus at a time and the two entries belong to two of them.
: STALE-ENTRY? ( n -- bool ) {: e:n :}
   CODEGEN-COMPARE:PATH-OLD e KNOWN-LOSS$ CODEGEN-COMPARE:FIND-ROW {: k:n :}
   k 0 < if false exit then
   k NEW-PARTNER 0 < if false exit then
   k BIGGER-PAIR? 0= ;

: SAY-STALE ( n -- ) {: e:n :}
   s" codegen-compare: KNOWN-LOSS " type
   e KNOWN-LOSS$ type
   s"  is no longer bigger than the old emitter's code: take its entry off the" type
   s"  known-loss list in tools/codegen-compare-report.f" type cr ;

: STALE-ENTRIES ( -- n )
   0
   KNOWN-LOSSES 0 ?do
      i STALE-ENTRY? if i SAY-STALE 1+ then
   loop ;

public

\ Is the chain's code for this corpus word more bytes than the engine's? The one
\ authority for that question: the report asks it, the check asks it, and
\ tools/codegen-compare-test.f names rows through it rather than comparing two
\ sizes of its own.
: BIGGER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW {: k:n :}
   k 0 < if false exit then
   k BIGGER-PAIR? ;

\ How many rows the known-loss list holds, and what each one is. Published so
\ that the scheduled test can pin the list itself: an entry added to buy a gate a
\ quiet life then has to survive an assertion that names it. They are the private
\ words themselves rather than wrappers around them, so the list the report reads
\ and the list the test reads cannot become two lists.
EXPORT KNOWN-LOSSES
EXPORT KNOWN-LOSS$

\ Name every row the new chain compiled into more bytes than the engine's
\ emitter wrote, apart from the known losses above, and every known-loss entry
\ that has stopped losing. Answer how many findings that was.
: SAY-BYTE-LOSSES ( -- n )
   UNKNOWN-LOSSES STALE-ENTRIES + ;

private

\ ---- the chain against the reference -----------------------------------------
\ The one question the third column exists to answer, per row: how far is the
\ chain's code from what a production optimising compiler makes of the same
\ program. It is printed and it is NOT counted - parity with clang is the goal
\ being measured, not a gate - and the ranking over all four corpora, which is
\ the priority list the optimisation lanes work from, is printed once at the end
\ of a run by tools/codegen-compare-gaps.f.
\
\ THE TWO TIME COLUMNS ARE EACH DIVIDED BY THEIR OWN ENTRY. A reference row is
\ reached through a foreign call and a chain row through an ordinary one, and
\ the two cost very different amounts, so neither raw number means anything
\ against the other. What is printed is each side's cost with its own floor
\ taken off, which is the emitted code - and the reference's floor is the row's
\ own arity, measured by running the row's own timing body against an empty C
\ function of the same signature.

: NS-COL ( n n -- ) {: picos:n width:n :}
   picos CODEGEN-NS:NS$ {: a:ptr u:n :}
   width u - PAD
   a u APPEND ;

: REF-HEAD ( -- )
   s" word                          chain byt  clang byt    gap byt   chain ns   clang ns     gap ns" LINE
   s" ----------------------------  ---------  ---------  ---------  ---------  ---------  ---------" LINE ;

: REF-COVERED ( n n -- ) {: j:n c:n :}
   j CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   c CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   j CODEGEN-COMPARE:SIZE c CODEGEN-COMPARE:SIZE - WIDE-COL PAD-LEFT
   GAP-COL PAD
   j CODEGEN-COMPARE:BODY-PICOS WIDE-COL NS-COL
   GAP-COL PAD
   c CODEGEN-COMPARE:BODY-PICOS WIDE-COL NS-COL
   GAP-COL PAD
   j CODEGEN-COMPARE:BODY-PICOS c CODEGEN-COMPARE:BODY-PICOS -
   WIDE-COL NS-COL ;

\ A row the chain cannot compile yet still has a reference: what clang makes of
\ a shape the chain has not reached is exactly the number that says how much is
\ waiting on that capability, so the row is printed with dashes where the chain
\ would be rather than dropped.
: REF-UNCOVERED ( n -- ) {: c:n :}
   DASH-COL
   GAP-COL PAD
   c CODEGEN-COMPARE:SIZE WIDE-COL PAD-LEFT
   GAP-COL PAD
   DASH-COL
   GAP-COL PAD
   DASH-COL
   GAP-COL PAD
   c CODEGEN-COMPARE:BODY-PICOS WIDE-COL NS-COL
   GAP-COL PAD
   DASH-COL ;

: REF-ROW ( n -- ) {: k:n :}
   CODEGEN-COMPARE:PATH-CLANG k CODEGEN-COMPARE:NAME$
   CODEGEN-COMPARE:FIND-ROW {: c:n :}
   c 0 < if exit then
   k CODEGEN-COMPARE:NAME$ NAME-COL PAD-RIGHT
   GAP-COL PAD
   CODEGEN-COMPARE:PATH-NEW k CODEGEN-COMPARE:NAME$
   CODEGEN-COMPARE:FIND-ROW {: j:n :}
   j 0 < if c REF-UNCOVERED else j c REF-COVERED then
   NL ;

: REF-ROWS ( -- )
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if dup REF-ROW then
      1+
   repeat drop ;

: REF$ ( -- ptr u8 n )
   0 TEXT-U !
   s" chain against the clang reference. Informational: a gap is a priority, not" LINE
   s" a finding. clang flags: " APPEND CODEGEN-CLANG:FLAGS$ APPEND NL
   s" reference object: " APPEND CODEGEN-CLANG:TEXT-BYTES NUM
   s"  bytes of code, plus " APPEND CODEGEN-CLANG:POOL-BYTES NUM
   s"  bytes of literal pool that belongs to no one twin" LINE
   NL
   REF-HEAD
   REF-ROWS
   TEXT TEXT-U @ ;

public

\ The reference column, or one line naming what this host has not got. An
\ absent column is a result and says so; it is never silence.
: SAY-REFERENCE ( -- )
   CODEGEN-COMPARE:PATH-CLANG CODEGEN-COMPARE:ROWS-OF 0= if
      s" codegen-compare: NO-REFERENCE there is no clang column on this host: " type
      CODEGEN-CLANG:ABSENT-WHY$ type cr exit
   then
   REF$ type ;

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
   SAY-BIGGER
   cr
   SAY-REFERENCE
   cr
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup PRINT-ROW
      1+
   repeat drop
   cr
   BODIES ;

;package
