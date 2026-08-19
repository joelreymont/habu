\ codegen-compare-core.f - the measurement engine of the codegen comparison.
\ One concern: turning one runnable piece of machine code into one recorded row.
\
\ For each subject the engine records three facts:
\
\   size      the number of bytes of machine code the subject occupies.
\   outputs   the values the subject leaves on the stack when it is executed on
\             its pinned inputs. The caller runs it and hands each result to
\             VECTOR, which appends it to the row in order.
\   cost      how long one call takes. See the timing note below.
\
\ TWO CODE GENERATORS, ONE STORE. A row records which code generator produced
\ it. Both are words of the running dictionary now - the old ones compiled by
\ the engine when tools/codegen-compare-corpus.f was loaded, the new ones
\ recompiled by the native chain and republished by
\ tools/codegen-compare-migrated.f - so MEASURE and MEASURE-NEW read a size the
\ same way, out of the subject's own dictionary record. The record keeps the code
\ start address and the code length, so the size is the code that will really
\ run: not an estimate, not a re-count, and not a number some emitter reported
\ about a routine nothing published. The one thing that differs is which record
\ the size is read from, because a new row is named after the corpus word it is
\ compared against and its code lives under the word that carries it.
\
\ EACH PATH STILL DECLARES ITS OWN EMPTY CALL, AND THE TWO NOW AGREE. Both
\ columns are entered the way the engine enters any word - the caller's compiled
\ call site, or the caller with the callee's body copied into it - so the entry
\ cost is the same on both sides and neither ratio is a different kind of number
\ from the other. The two calibration rows are kept, and measured, because that
\ agreement is a fact about the publication seam that can regress: the report
\ prints both floors and the check compares them. Every cost remains a multiple
\ of its own path's empty call, which now means the same thing on both sides.
\
\ Nothing here compiles anything or reproduces any part of a compiler. The old
\ subjects were compiled by the engine when their source file was loaded and the
\ new ones by the native chain; this file only runs them and times them.
\
\ Timing discipline (no sleeps anywhere, and a named budget).
\
\ One timed run executes the caller's timing body REPS times back to back and
\ divides the elapsed monotonic nanoseconds by REPS. Each case is timed RUNS
\ times and the FASTEST of those runs is the number kept: a run can only be made
\ slower by interference from the rest of the machine, never faster, so the
\ minimum is the closest estimate of the real cost available. The spread between
\ the fastest and slowest run is kept as well and printed, so a reader can see
\ how noisy the host was while the row was measured.
\
\ Per-call costs here are a few nanoseconds, so the row value is kept in
\ picoseconds per call: one thousandth of a nanosecond, which leaves three
\ significant digits where whole nanoseconds would leave one.
\
\ Why seven runs and not three. Three runs of a million repetitions each was
\ tried first and measured worse on both counts. On a 12-core host running 16
\ competing busy processes, one case (LERP) had all three of its runs hit by the
\ same sustained scheduling delay and came out 4.1 times its idle cost, which is
\ a false alarm no tolerance short of useless would absorb. Seven shorter runs
\ give the fastest-run rule more chances to find a clean window, and the whole
\ pass also got faster: 0.50 s idle against 0.90 s. The spread across all seven
\ runs is still reported, which is the variance a reader needs to judge a row.
\
\ Measured 2026-07-30 on a 12-core Apple Silicon host, eleven cases at the
\ settings below: the whole measurement pass costs 0.50 s with the machine idle
\ and 2.1 to 4.3 s with 24 competing busy processes, twice as many as the host
\ has cores. WORST-PASS-MS records that busiest measurement rounded up, and
\ PASS-MARGIN keeps the budget an order of magnitude above it, so ordinary host
\ load cannot reach the budget and a pass that does reach it is genuinely broken
\ rather than merely slow. This mirrors the deadlock-guard budgets in
\ test/compiler/ir-id.f.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-tail-probe.f

package CODEGEN-COMPARE

\ The measurement settings are public because the report prints them and the
\ comparison applies them; everything else in this file is package-private.
public

250000 constant REPS
7 constant RUNS
1000 constant COST-UNIT           \ a cost of COST-UNIT means "as expensive as the calibration call"
1000 constant PICOS-PER-NS        \ the unit PICOSECONDS reports in

\ The store's shape. These are public because the reader of a written table has
\ to allocate the same shape to read it back, and two files guessing the same
\ numbers separately is how a wider row silently loses its tail.
\
\ ROW-MAX HOLDS THREE COLUMNS OF THE WIDEST CORPUS. The fourth corpus measures
\ thirteen subjects, and a pass holds an engine row, a chain row and a clang
\ reference row for each of them, so a store of thirty-two - which held two
\ columns - would have refused the third the day it arrived.
64 constant ROW-MAX
64 constant NAME-MAX
8 constant OUTPUT-MAX

\ Which code generator produced a row. The committed table spells these out in
\ its first column. The reader that took those spellings back went with the
\ comparison harness; what is left of this file is the measuring the judge uses.
0 constant PATH-OLD
1 constant PATH-NEW
2 constant PATH-CLANG

\ How much slower than its recorded baseline a row may measure before the
\ comparison calls it a regression.
\
\ This number is a measurement of host noise, not a preference. Twelve full
\ passes were run on a 12-core host with 24 competing busy processes - twice as
\ many as the machine has cores - and compared with the same host's idle
\ numbers. The worst row came out 3.04 times its idle cost and three more rows
\ passed 2x, because a case whose body takes tens of nanoseconds cannot find a
\ scheduling window that clean while the calibration call, at two nanoseconds,
\ still can; normalising against the calibration therefore does not cancel
\ sustained load. Eight keeps a margin of two and a half over that worst
\ measurement, and still reports the deliberately slowed word in
\ the deliberately slowed word the old timed member measured, about eighteen
\ times slower.
\
\ Be honest about what this buys: the timing column catches a code generator
\ that became catastrophically slower, not one that lost a fifth of its speed.
\ The exact checks in this harness are the compiled size and the outputs.
8 constant COST-BAND

private

3 constant PATH-N                 \ how many code generators a row can name

1000000 constant NS-PER-MS
$7FFFFFFFFFFFFFFF constant PICOS-MAX

4300 constant WORST-PASS-MS
10 constant PASS-MARGIN

public

WORST-PASS-MS PASS-MARGIN * constant BUDGET-MS

private

ROW-MAX NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS ROW-MAX cells allot
create PATHS ROW-MAX cells allot
create CALIBRATIONS PATH-N cells allot
create SIZES ROW-MAX cells allot
create TAILS ROW-MAX cells allot
create PICOS ROW-MAX cells allot
create FLOORS ROW-MAX cells allot
create SPREADS ROW-MAX cells allot
create COSTS ROW-MAX cells allot
create OUT-COUNTS ROW-MAX cells allot
create OUT-VALUES ROW-MAX OUTPUT-MAX * cells allot

variable ROW-N
variable OUT-N                    \ outputs recorded so far for the row being measured
variable FASTEST
variable SLOWEST
variable PASS-NS
variable PASS-MS
variable NORMALIZED

: SLOT ( ptr a n -- ptr a )
   cells + ;

: ROW-OK ( n -- n )
   dup 0 < over ROW-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: PATH-OK ( n -- n )
   dup 0 < over PATH-N >= or if E-CODEGEN-COMPARE-ROW throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAME-BYTES + ;

\ ---- one timed run ---------------------------------------------------------

\ typed-local-lint: allow-bare-local - q is the timing body; its effect is in
\ the stack signature and a local annotation cannot carry a quotation effect.
: RUN-ONCE ( [ -- ] -- n ) {: q :}
   mono-ns {: t0:n :}
   REPS 0 ?do q execute loop
   mono-ns t0 - PICOS-PER-NS * REPS / ;

: SAMPLE ( n -- ) {: picos:n :}
   picos FASTEST @ < if picos FASTEST ! then
   picos SLOWEST @ > if picos SLOWEST ! then ;

\ typed-local-lint: allow-bare-local - q is the timing body, as in RUN-ONCE.
: TIME-RUNS ( [ -- ] -- ) {: q :}
   PICOS-MAX FASTEST !
   0 SLOWEST !
   RUNS 0 ?do q RUN-ONCE SAMPLE loop
   FASTEST @ 0= if E-CODEGEN-COMPARE-CLOCK throw then ;

: SPREAD-PERMILLE ( -- n )
   SLOWEST @ FASTEST @ - COST-UNIT * FASTEST @ / ;

\ ---- the compiled size of a live word --------------------------------------
\ THE SIZE IS THE WHOLE ROUTINE, TRAILING RETURN AND ALL, and that is a fix and
\ not a definition. The size used to be the word's recorded length, which is the
\ span a caller may COPY - everything before the trailing return - so every habu
\ row was four bytes short of the code that runs while the reference column's
\ rows, being whole C functions, were not. The two columns were not counting the
\ same thing. CODEGEN-CORPUS:NOOP said it plainly: an empty word is a return and
\ nothing else, and the chain's row for it read zero bytes.
\
\ The four are not simply added. A routine that leaves by a branch has no
\ trailing return, and its recorded length is already the whole emission; adding
\ to it would invent an instruction. tools/codegen-tail-probe.f decides which
\ shape a record is by reading the code - it owns that reading, and the tail rows
\ of the fifth corpus are what made the distinction matter - so the size here is
\ its answer and this file does no arithmetic of its own.

: SUBJECT-SIZE ( ptr u8 n -- n )
   NTAILPROBE:CODE-BYTES ;

\ Does this subject leave by a branch to a callee? Recorded per row because it
\ decides whether the row's byte count is the whole of what the row's program
\ costs: a routine that tail-branches carries four bytes here and the callee it
\ reaches carries the rest. The report marks such a row rather than adding the
\ callee in - see the note there for why a shared callee cannot be added to
\ every row that reaches it.
: SUBJECT-TAIL? ( ptr u8 n -- bool )
   NTAILPROBE:TAIL-BRANCH? ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a  ROW-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS ROW-N @ SLOT ! ;

public

\ Record one value the subject word left on the stack. Called from inside a
\ case's correctness body, once per recorded output, in order.
: VECTOR ( n -- ) {: value:n :}
   ROW-N @ ROW-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   OUT-N @ OUTPUT-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   value OUT-VALUES ROW-N @ OUTPUT-MAX * OUT-N @ + SLOT !
   OUT-N @ 1+ OUT-N ! ;

\ A flag, as the number a row can hold. A row holds numbers and a Habu flag is
\ not one, so a flag has to become a number somewhere; it becomes one HERE,
\ because two columns turning a flag into a number by two routes of their own
\ would let a difference in the routes read as a difference in the compiled
\ code. It is public for the reason REAL-BITS below is: the code generator
\ judge's generated bodies cross the same boundary and must cross it the same
\ way.
: FLAG-BITS ( bool -- n )
   if 1 else 0 then ;

\ Record one flag the subject word left on the stack, through that one
\ projection.
: VECTOR-FLAG ( bool -- )
   FLAG-BITS VECTOR ;

\ A double, as the cell it already is. The engine keeps a double unboxed in one
\ data-stack cell as its raw IEEE754 bit pattern - the survey at the head of
\ tools/codegen-compare-corpus3.f establishes that from the engine's own source
\ and from what it prints - so this is a retype and not a conversion, and it is
\ the CAST: form, which the checker certifies, rather than a trusted boundary.
\ It is public because a caller that wants to compare a float of its own with a
\ value a row recorded has to reach it by this route and not by a second one.
CAST: REAL-BITS ( r -- n ) ;

\ Record one float the subject word left on the stack. It goes through the one
\ projection above for the reason VECTOR-FLAG gives: two columns crossing from a
\ float to a recorded cell by two routes of their own would let a difference in
\ the routes read as a difference in the compiled code.
\
\ WHAT THIS MAKES THE COMPARISON MEAN. The recorded value is the whole cell, so
\ two rows agree only if every bit agrees. That is the strictest comparison
\ available and the honest one for doubles: two cells that differ by a bit are
\ two different numbers. It is finer than float equality in exactly two places,
\ both deliberate - +0.0 and -0.0 are equal numbers in different cells, and a
\ NaN is equal to nothing at all while two NaNs of one payload are one cell. A
\ code generator that lost the sign of a zero, or that produced a NaN with
\ another payload, is reported here rather than absorbed.
: VECTOR-REAL ( r -- )
   REAL-BITS VECTOR ;

: RESET ( -- )
   0 ROW-N !
   0 OUT-N !
   PATH-N 0 ?do -1 CALIBRATIONS i SLOT ! loop
   0 PASS-NS !
   0 PASS-MS !
   0 NORMALIZED ! ;

: PASS-BEGIN ( -- )
   mono-ns PASS-NS ! ;

: PASS-END ( -- )
   mono-ns PASS-NS @ - NS-PER-MS / PASS-MS ! ;

: PASS-MS@ ( -- n )
   PASS-MS @ ;

: OVER-BUDGET? ( -- bool )
   PASS-MS @ BUDGET-MS > ;

private

\ Record one row: which code generator produced it, its name, its size in bytes,
\ whether the routine leaves by a branch to a callee whose bytes that size does
\ not hold, a body that calls the subject once for timing, and a body that calls
\ it on its pinned inputs and hands every result to VECTOR.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies,
\ and a local annotation cannot carry a quotation effect.
: RECORD ( ptr u8 n n n bool [ -- ] [ -- ] -- )
   {: a:ptr u:n path:n size:n tail?:bool timing vectors :}
   ROW-N @ ROW-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   a u NAME!
   path PATH-OK PATHS ROW-N @ SLOT !
   size SIZES ROW-N @ SLOT !
   tail? if 1 else 0 then TAILS ROW-N @ SLOT !
   0 OUT-N !
   vectors execute
   OUT-N @ OUT-COUNTS ROW-N @ SLOT !
   timing TIME-RUNS
   FASTEST @ PICOS ROW-N @ SLOT !
   SPREAD-PERMILLE SPREADS ROW-N @ SLOT !
   0 COSTS ROW-N @ SLOT !
   0 FLOORS ROW-N @ SLOT !
   ROW-N @ 1+ ROW-N ! ;

public

\ Measure a word the engine compiled: its size comes from its own dictionary
\ record, so a subject this image does not hold stops the pass.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies.
: MEASURE ( ptr u8 n [ -- ] [ -- ] -- ) {: a:ptr u:n timing vectors :}
   a u PATH-OLD  a u SUBJECT-SIZE  a u SUBJECT-TAIL?  timing vectors RECORD ;

\ Measure a routine whose byte count cannot be read off a dictionary record
\ because it is not a word of this dictionary: the clang reference column's C
\ twins, whose size comes out of the object file clang wrote. The size is a
\ parameter here for that reason and for no other - there is still exactly one
\ authority for it, and it is not this file.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies.
\
\ A REFERENCE ROW IS NEVER MARKED AS REACHING CODE ITS SIZE DOES NOT HOLD, and
\ what entitles it to that is a refusal in another file rather than a hope. Every
\ helper twins.c keeps to itself is static, and tools/codegen-compare-macho.f
\ refuses the whole reading if any non-external symbol starts a stretch of
\ __text - so a run that produced this column at all is a run in which clang
\ inlined every one of them, and a twin's symbol size is the whole of its
\ program. What that refusal does NOT cover is a twin that tail-branches to
\ ANOTHER TWIN, which is external and would be read normally; dot
\ habu-check-no-c-019014f8 carries that check.
: MEASURE-CLANG ( ptr u8 n n [ -- ] [ -- ] -- ) {: a:ptr u:n size:n timing vectors :}
   a u PATH-CLANG size false timing vectors RECORD ;

\ Measure a word the native chain compiled and the publication seam republished.
\ The row is named after the corpus word it is the head-to-head partner of, and
\ the size is read off the record of the word that actually carries the new
\ code - the same reader, on the same kind of record, as the old column's. There
\ is no way to state a size here: a new row's byte count is whatever the engine
\ recorded for the word a caller would enter.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies.
: MEASURE-NEW ( ptr u8 n ptr u8 n [ -- ] [ -- ] -- )
   {: a:ptr u:n na:ptr nu:n timing vectors :}
   a u PATH-NEW  na nu SUBJECT-SIZE  na nu SUBJECT-TAIL?  timing vectors RECORD ;

\ Where a published word's code starts, read off its own dictionary record. It
\ is here, beside the reader that takes a size off the same record, so that the
\ address a migrated call site is given and the bytes the table reports come
\ from one authority rather than from three copies of one reader.
: CODE-ENTRY ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if E-NPUB-NAME throw then
   XREF-START ;

\ Time one body the way a row is timed - the same repetitions, the same number
\ of runs, the same fastest-run rule - without recording a row. This is what the
\ reference column measures its per-row entry floor with: the row's OWN timing
\ body, run against an empty C function of the same signature, so what the
\ subtraction leaves is the emitted code and not the marshalling.
\ typed-local-lint: allow-bare-local - q is a timing body, as in RUN-ONCE.
: TIME-ONLY ( [ -- ] -- n ) {: q :}
   q TIME-RUNS
   FASTEST @ ;

\ What the row just measured has to have taken off it before its cost means the
\ emitted code. Rows whose path enters every subject the same way leave this at
\ zero and are divided by their path's calibration row instead; a row that is
\ entered through a call shape of its own records that shape's floor here.
: FLOOR! ( n -- ) {: picos:n :}
   ROW-N @ 1- ROW-OK {: k:n :}
   picos FLOORS k SLOT ! ;

\ Declare the row just measured to be its path's calibration row: every other
\ row of that path expresses its cost as a multiple of this one. Declared
\ explicitly rather than assumed to be the path's first row, so a reordered case
\ list cannot silently divide by the wrong measurement.
: CALIBRATE ( -- )
   ROW-N @ 1- ROW-OK {: k:n :}
   k CALIBRATIONS  PATHS k SLOT @  SLOT ! ;

private

\ The picoseconds an empty call of this path costs. A path with rows and no
\ calibration row would otherwise divide every one of them by nothing.
: BASE-PICOS ( n -- n ) {: path:n :}
   CALIBRATIONS path PATH-OK SLOT @ {: base-row:n :}
   base-row 0 < if E-CODEGEN-COMPARE-STAGE throw then
   PICOS base-row ROW-OK SLOT @ ;

public

\ What an empty call of this path costs, in picoseconds. The report subtracts it
\ from each row of the same path, which is the one honest way to compare two
\ paths whose entry costs differ: what is left is the routine and not the call.
: PATH-PICOS ( n -- n )
   BASE-PICOS ;

: NORMALIZE ( -- )
   0 begin dup ROW-N @ < while
      dup {: k:n :}
      PATHS k SLOT @ BASE-PICOS {: base:n :}
      base 0= if E-CODEGEN-COMPARE-CLOCK throw then
      PICOS k SLOT @ COST-UNIT * base / COSTS k SLOT !
      1+
   repeat drop
   -1 NORMALIZED ! ;

\ ONE MEASURED PASS OVER ONE CORPUS, AND THE SIX STEPS IT IS MADE OF: the store
\ cleared, the clock started, the old column's cases, the new column's, the
\ clang reference column's, the clock stopped, and the normalisation that turns
\ picoseconds into costs. Every corpus needs all of them in that order and none
\ of them differ between corpora, so the order is stated here once and each case
\ file hands in the three bodies that are its own. A pass assembled by hand could
\ leave a step out, and the one that costs least to leave out - NORMALIZE - is
\ the one that would hand every comparison a zero and fail nothing.
\
\ The old column runs before the new one because the new column checks every
\ name it writes down against a row the old column measured. The reference
\ column runs last and is empty on a host with no C compiler, which is why a
\ pass with two columns is a pass and not a failure.
\ typed-local-lint: allow-bare-local - old, new and ref are the three columns'
\ bodies, and a local annotation cannot carry a quotation effect.
: PASS ( [ -- ] [ -- ] [ -- ] -- ) {: old new ref :}
   RESET
   PASS-BEGIN
   old execute
   new execute
   ref execute
   PASS-END
   NORMALIZE ;

\ The word that opens a data row in the baseline table, and names which code
\ generator produced it. The reference column has a spelling too, so a report
\ that prints a path never prints a number instead - but no committed table
\ carries a clang row: what the host's C compiler emits is a measurement of that
\ host's toolchain, and pinning it would turn a compiler upgrade into a red gate.
: PATH-OLD$ ( -- ptr u8 n )
   s" old" ;

: PATH-NEW$ ( -- ptr u8 n )
   s" new" ;

: PATH-CLANG$ ( -- ptr u8 n )
   s" clang" ;

: PATH$ ( n -- ptr u8 n ) {: path:n :}
   path PATH-OK PATH-NEW = if PATH-NEW$ exit then
   path PATH-CLANG = if PATH-CLANG$ exit then
   PATH-OLD$ ;

: ROWS ( -- n )
   ROW-N @ ;

: PATH@ ( n -- n ) {: k:n :}
   PATHS k ROW-OK SLOT @ ;

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-OK NAME-AT
   NAME-LENS k SLOT @ ;

: SIZE ( n -- n ) {: k:n :}
   SIZES k ROW-OK SLOT @ ;

\ Does this row's routine leave by a branch to a callee? Such a row's SIZE is the
\ branch and not the program: the callee's bytes are real, they are simply
\ somewhere else. The report says so beside the row.
: TAIL? ( n -- bool ) {: k:n :}
   TAILS k ROW-OK SLOT @ 0<> ;

: PICOSECONDS ( n -- n ) {: k:n :}
   PICOS k ROW-OK SLOT @ ;

\ What one call of this row costs with its entry taken off: the cost of the
\ emitted code and nothing else. A row that recorded a floor of its own is
\ divided by that; every other row is divided by its path's calibration, which
\ is the same call every subject of that path is entered through. One reader, so
\ the three columns' body costs are the same kind of number.
\
\ A row that measured faster than its own floor comes back negative rather than
\ clamped: that is a row at the resolution of the measurement saying so.
: BODY-PICOS ( n -- n ) {: k:n :}
   k ROW-OK drop
   FLOORS k SLOT @ {: own:n :}
   own 0<> if k PICOSECONDS own - exit then
   k PICOSECONDS  k PATH@ PATH-PICOS  - ;

: SPREAD ( n -- n ) {: k:n :}
   SPREADS k ROW-OK SLOT @ ;

\ A cost only exists once NORMALIZE has divided the whole pass by the
\ calibration row. Reading one before that would hand every caller a zero,
\ which no comparison could ever fail, so it fails closed instead.
: COST ( n -- n ) {: k:n :}
   NORMALIZED @ 0= if E-CODEGEN-COMPARE-STAGE throw then
   COSTS k ROW-OK SLOT @ ;

: OUTPUTS ( n -- n ) {: k:n :}
   OUT-COUNTS k ROW-OK SLOT @ ;

: OUTPUT ( n n -- n ) {: k:n j:n :}
   k ROW-OK drop
   j 0 < j k OUTPUTS >= or if E-CODEGEN-COMPARE-ROW throw then
   OUT-VALUES k OUTPUT-MAX * j + SLOT @ ;

\ The row of this path with this name, or -1. Two paths measure the same corpus
\ word under the same name, so a search that ignored the path would answer with
\ whichever row it met first.
: FIND-ROW ( n ptr u8 n -- n ) {: path:n a:ptr u:n :}
   path PATH-OK drop
   0 begin dup ROW-N @ < while
      dup PATH@ path = if
         dup NAME$ a u STR= if exit then
      then
      1+
   repeat drop -1 ;

: ROWS-OF ( n -- n ) {: path:n :}
   path PATH-OK drop
   0
   ROW-N @ 0 ?do
      i PATH@ path = if 1+ then
   loop ;

\ Did two rows leave exactly the same values on the stack? This is the equality
\ the head-to-head comparison turns on: the same corpus word compiled two ways
\ has to compute the same answer on the same pinned inputs.
: SAME-OUTPUTS? ( n n -- bool ) {: k:n j:n :}
   k ROW-OK drop
   j ROW-OK drop
   k OUTPUTS j OUTPUTS <> if false exit then
   true
   k OUTPUTS 0 ?do
      k i OUTPUT  j i OUTPUT  <> if drop false leave then
   loop ;

\ ---- the head-to-head pairing ------------------------------------------------
\ A new row and the old row it is the partner of are two rows of this store with
\ one name, so pairing them is store navigation and lives here beside the search
\ that finds them. Which corpus is being measured makes no difference to any of
\ it: a pass holds one corpus at a time and these three read whatever is there.

\ The old row this new row is the head-to-head partner of, or -1.
: PARTNER ( n -- n ) {: k:n :}
   PATH-OLD k NAME$ FIND-ROW ;

\ Did the routine the new chain emitted compute what the old word computes? This
\ is the equality the whole comparison turns on.
: ROW-MATCH? ( n -- bool ) {: k:n :}
   k PARTNER {: b:n :}
   b 0 < if false exit then
   k b SAME-OUTPUTS? ;

\ How many rows of the new column disagree with their partners.
: MISMATCHES ( -- n )
   0
   ROW-N @ 0 ?do
      i PATH@ PATH-NEW = if
         i ROW-MATCH? 0= if 1+ then
      then
   loop ;

;package
