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
\ it. An old row is a word the engine compiled when its source file was loaded:
\ MEASURE looks it up and reads its size out of its own dictionary record - the
\ record keeps the code start address and the code length - so the size is the
\ code the engine actually emitted, not an estimate and not a re-count. A new
\ row is a routine the native chain emitted and the harness published into code
\ space: it has no dictionary record, so MEASURE-EMITTED is told the size the
\ emitter reported. Everything else about the two is measured identically, by
\ the same words, in the same pass.
\
\ EACH PATH IS NORMALIZED AGAINST ITS OWN EMPTY CALL. The two paths are entered
\ differently - an old row is called as an ordinary Habu word, a new row through
\ the C-ABI call, because the convention the chain binds is the C one and a Habu
\ word's own data-stack entry does not exist yet (dot
\ habu-enter-and-leave-2684e515) - so
\ the call overhead is not the same on both sides and no subtraction can make it
\ so. Each path therefore declares its own calibration row, an empty call
\ entered the same way its other rows are, and every cost is a multiple of that.
\ A cost is then "how much more than an empty call of this kind", which is the
\ only sense in which the two columns are comparable; the report prints absolute
\ nanoseconds for both as well, so a reader can see what was actually measured.
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
32 constant ROW-MAX
64 constant NAME-MAX
8 constant OUTPUT-MAX

\ Which code generator produced a row. The committed table spells these out in
\ its first column, and tools/codegen-compare-baseline.f reads the spelling back
\ into the same code, so the two files cannot disagree about what a row is.
0 constant PATH-OLD
1 constant PATH-NEW

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
\ tools/codegen-compare-test.f, which comes out about eighteen times slower.
\
\ Be honest about what this buys: the timing column catches a code generator
\ that became catastrophically slower, not one that lost a fifth of its speed.
\ The exact checks in this harness are the compiled size and the outputs.
8 constant COST-BAND

private

2 constant PATH-N                 \ how many code generators a row can name

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
create PICOS ROW-MAX cells allot
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

: SUBJECT-SIZE ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop E-CODEGEN-COMPARE-SUBJECT throw
   then
   XREF-LEN ;

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
\ a body that calls the subject once for timing, and a body that calls it on its
\ pinned inputs and hands every result to VECTOR.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies,
\ and a local annotation cannot carry a quotation effect.
: RECORD ( ptr u8 n n n [ -- ] [ -- ] -- ) {: a:ptr u:n path:n size:n timing vectors :}
   ROW-N @ ROW-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   a u NAME!
   path PATH-OK PATHS ROW-N @ SLOT !
   size SIZES ROW-N @ SLOT !
   0 OUT-N !
   vectors execute
   OUT-N @ OUT-COUNTS ROW-N @ SLOT !
   timing TIME-RUNS
   FASTEST @ PICOS ROW-N @ SLOT !
   SPREAD-PERMILLE SPREADS ROW-N @ SLOT !
   0 COSTS ROW-N @ SLOT !
   ROW-N @ 1+ ROW-N ! ;

public

\ Measure a word the engine compiled: its size comes from its own dictionary
\ record, so a subject this image does not hold stops the pass.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies.
: MEASURE ( ptr u8 n [ -- ] [ -- ] -- ) {: a:ptr u:n timing vectors :}
   a u PATH-OLD  a u SUBJECT-SIZE  timing vectors RECORD ;

\ Measure a routine the native chain emitted and the caller published. It has no
\ dictionary record, so the size is the one the emitter reported.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies.
: MEASURE-EMITTED ( ptr u8 n n [ -- ] [ -- ] -- ) {: a:ptr u:n size:n timing vectors :}
   a u PATH-NEW size timing vectors RECORD ;

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

: NORMALIZE ( -- )
   0 begin dup ROW-N @ < while
      dup {: k:n :}
      PATHS k SLOT @ BASE-PICOS {: base:n :}
      base 0= if E-CODEGEN-COMPARE-CLOCK throw then
      PICOS k SLOT @ COST-UNIT * base / COSTS k SLOT !
      1+
   repeat drop
   -1 NORMALIZED ! ;

\ The word that opens a data row in the baseline table, and names which code
\ generator produced it.
: PATH-OLD$ ( -- ptr u8 n )
   s" old" ;

: PATH-NEW$ ( -- ptr u8 n )
   s" new" ;

: PATH$ ( n -- ptr u8 n ) {: path:n :}
   path PATH-OK PATH-NEW = if PATH-NEW$ exit then
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

: PICOSECONDS ( n -- n ) {: k:n :}
   PICOS k ROW-OK SLOT @ ;

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

;package
