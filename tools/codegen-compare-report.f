\ codegen-compare-report.f - rendering for the codegen comparison.
\ One concern: turning recorded rows into text a person can read.
\
\ Two renderings, because they answer different questions:
\
\   BASELINE$  the committed table. It carries only the numbers that are stable
\              across runs and machines - the compiled size, the outputs, and
\              the cost expressed as a multiple of an empty call - so the file
\              in the repository changes when the compiler changes and at no
\              other time.
\   PRINT      the report a person reads after a run. It carries the same rows
\              plus the measurement detail that is true of this run only:
\              absolute nanoseconds per call, how far apart the timed runs were,
\              and how long the whole measurement pass took.
\
\ Absolute nanoseconds are deliberately absent from the committed table. They
\ are a property of the machine that measured them, so storing them would make
\ the file disagree with itself on every other host, while the ratio to an empty
\ call measured in the same run stays comparable.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f

package CODEGEN-REPORT

$2000 constant TEXT-CAP
TEXT-CAP BUFFER: TEXT
variable TEXT-U

4 constant PATH-COL
28 constant NAME-COL
5 constant SIZE-COL
6 constant COST-COL
2 constant GAP-COL
10 constant NEWLINE-BYTE
32 constant SPACE-BYTE

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
   s" When the new compiler chain starts emitting machine code it will add a second" LINE
   s" set of rows, marked new, for the same corpus words and the same inputs, and" LINE
   s" the two sets become the head-to-head comparison this harness exists for." LINE
   s" Until then every row is marked old and there is no new column to show." LINE
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
   s" rows: " APPEND CODEGEN-COMPARE:ROWS NUM NL
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
      dup ROW
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

\ ---- the report a person reads ---------------------------------------------

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
   k CODEGEN-COMPARE:NAME$ type
   s"   bytes " type k CODEGEN-COMPARE:SIZE FMT:.U
   s"   cost " type k CODEGEN-COMPARE:COST FMT:.U
   s"   ns/call " type k CODEGEN-COMPARE:PICOSECONDS NANOS.
   s"   run spread " type k CODEGEN-COMPARE:SPREAD FMT:.U s" /1000" type
   s"   outputs" type k PRINT-OUTPUTS
   cr ;

public

: PRINT ( -- )
   s" code generator: old (the emitter bin/hb uses today)" type cr
   s" corpus words measured: " type CODEGEN-COMPARE:ROWS FMT:.U cr
   s" repetitions per timed run: " type CODEGEN-COMPARE:REPS FMT:.U
   s" , timed runs per word: " type CODEGEN-COMPARE:RUNS FMT:.U
   s"  (the fastest run is the one recorded)" type cr
   s" measurement pass: " type CODEGEN-COMPARE:PASS-MS@ FMT:.U
   s"  ms, budget " type CODEGEN-COMPARE:BUDGET-MS FMT:.U s"  ms" type cr
   cr
   0 begin dup CODEGEN-COMPARE:ROWS < while
      dup PRINT-ROW
      1+
   repeat drop ;

;package
