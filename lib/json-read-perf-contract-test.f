\ json-read-perf-contract-test.f - focused coverage for MEASURE and REPORT.
\
\ The file reopens package JSON-READ-PERF-TEST so it can drive the same private
\ sample table the provider's own verdicts read. Nothing here re-implements a
\ budget, a median or a verdict: every probe runs the real words and only reads
\ back how many assertions failed. What it proves:
\   - MEASURE really stores a full round-major table - one sample per round for
\     each of the six workloads AND the reference - none of them zero, and its
\     warm-up correctness probes pass on the production path.
\   - REPORT fails all six verdicts closed while the sample table is incomplete,
\     so a skipped MEASURE can never look like a pass.
\   - A complete, fast sample set passes all six, and one slowed workload turns
\     exactly one verdict red.
\   - The red verdict is the slowed workload's own, checked for all six.
\   - MULTIPLYING EVERY SLOT, workloads and reference alike, by 2 and by 97
\     moves no verdict. That is the property the ratio exists to provide - a
\     slower machine must not be able to fail a tree - and the old absolute
\     budgets could not have survived either factor.
\   - Each workload is judged against its own recorded ratio, which the known
\     order of those ratios pins.
\   - Every verdict is published as an evidence line, not just counted.
\   - Storing a sample out of order, an extra sample past the table, or a read outside the
\     table is refused with a named code.
\
\ Run: bin/hb --load lib/json-read-perf-contract-test.f

require lib/json-read-perf-test.f

package JSON-READ-PERF-TEST
private

\ ---- synthetic sample values ----------------------------------------------
\ Verdicts are ratios now, so a synthetic table needs a reference to be a ratio
\ AGAINST. Every row's value is chosen relative to this one number, which is why
\ it is named and not spelled out at each use.
1000000 constant REF-NS               \ the reference's synthetic sample
WORK-N constant NO-SLOW               \ "slow workload" argument naming no workload

: ROW-RATIO ( n -- n ) {: work:n :}   \ this row's recorded ratio
   work ROW {: base:n pct:n :}
   2drop                              \ the workload name is not needed here
   base ;

\ A passing time for this row: the one that reproduces its RECORDED ratio
\ exactly. Deliberately not "1". A trivially small sample divides to a ratio of
\ zero, which is under every ceiling by construction - it would make the fast
\ table and the scaling cases below pass without ever exercising the division
\ they exist to test. This value makes the arithmetic do real work and leaves
\ exactly the headroom margin between the row and its ceiling.
: FAST-NS ( n -- n ) {: work:n :}
   work ROW-RATIO REF-NS * RATIO-SCALE / ;

\ A sample this large is over budget for EVERY row: it is one part past the
\ largest ceiling the provider can compute (the largest recorded ratio widened
\ by the headroom), converted back into a time against REF-NS. Derived from the
\ table rather than written down, so a re-recorded ratio cannot leave a
\ hand-picked constant behind that no longer clears the bar.
: SLOW-NS ( -- n )
   ESC-RATIO HEADROOM-PCT * PCT-DEN / 1+
   REF-NS * RATIO-SCALE / ;

: SAMPLE-VALUE ( n n -- n ) {: work:n slow:n :}
   work REF-ID = if REF-NS exit then
   work slow = if SLOW-NS exit then
   work FAST-NS ;

\ Complete table in the store's own round-major order: each round stores one
\ sample of every slot, the reference included, exactly the order MEASURE takes
\ them in.
: FILL ( n -- ) {: slow:n :}          \ complete table; workload `slow` is over budget
   SAMPLES-CLEAR
   SAMPLE-N 0 ?do
      SLOT-N 0 ?do
         i slow SAMPLE-VALUE  i SAMPLE+
      loop
   loop ;

\ The same table with every slot - workloads AND reference - multiplied by the
\ same factor, which is exactly what a slower machine does to a measurement.
\ Every ratio is unchanged by construction, so every verdict must be too. This
\ is the one property the whole design exists for, and it is asserted rather
\ than assumed.
: FILL-SCALED ( n -- ) {: k:n :}
   SAMPLES-CLEAR
   SAMPLE-N 0 ?do
      SLOT-N 0 ?do
         i NO-SLOW SAMPLE-VALUE k *  i SAMPLE+
      loop
   loop ;

\ ---- probes ---------------------------------------------------------------
variable MEASURE-TAKEN                \ samples MEASURE stored
variable MEASURE-RED                  \ assertions MEASURE's warm-up failed
variable POSITIVE-N                   \ stored samples with a positive elapsed time
variable EMPTY-RED                    \ verdicts REPORT failed on an empty table
variable FAST-RED                     \ verdicts REPORT failed on a complete fast table
variable ONE-RED                      \ verdicts REPORT failed with one workload slowed
variable RED-WORK                     \ the last workload whose own verdict failed
variable RED-N                        \ how many workloads failed their own verdict
create SEEN WORK-N cells allot        \ per slowed workload: the verdict that went red
create SEEN-N WORK-N cells allot      \ per slowed workload: how many verdicts went red

: POSITIVE-WORK ( n -- ) {: work:n :}
   SAMPLE-N 0 ?do
      work i SAMPLE@ 0 > if POSITIVE-N @ 1+ POSITIVE-N ! then
   loop ;

: PROBE-MEASURE ( -- )                \ the real six workloads and the reference, once
   T-RESET
   MEASURE
   T-FAILURES MEASURE-RED !
   TAKEN @ MEASURE-TAKEN !
   0 POSITIVE-N !
   SLOT-N 0 ?do i POSITIVE-WORK loop ;

: REPORT-RED ( -- n )                 \ verdicts REPORT fails against the current table
   T-RESET
   REPORT
   T-FAILURES ;

: JUDGE-RED? ( n -- bool )            \ does this workload's own verdict fail?
   T-RESET
   JUDGE
   T-FAILURES 0 > ;

: SCAN ( -- )                         \ which single verdict fails against this table
   0 RED-N !
   NO-SLOW RED-WORK !
   WORK-N 0 ?do
      i JUDGE-RED? if
         i RED-WORK !
         RED-N @ 1+ RED-N !
      then
   loop ;

: PROBE-ROW ( n -- ) {: slow:n :}
   slow FILL
   SCAN
   RED-WORK @ SEEN slow cells + !
   RED-N @ SEEN-N slow cells + ! ;

: PROBE-ROWS ( -- )
   WORK-N 0 ?do i PROBE-ROW loop ;

: PROBE-REPORT ( -- )
   SAMPLES-CLEAR REPORT-RED EMPTY-RED !
   NO-SLOW FILL REPORT-RED FAST-RED !
   ESC-ID FILL REPORT-RED ONE-RED ! ;

\ ---- sample-table misuse --------------------------------------------------
: BAD-EXTRA ( -- )                    \ a sample past the full table belongs to no workload
   NO-SLOW FILL
   REF-NS MISS-ID SAMPLE+ ;

: BAD-ORDER ( -- )                    \ a round stores the workloads in order; a jump skips one
   SAMPLES-CLEAR
   REF-NS LONG-ID SAMPLE+ ;

: BAD-WORK ( -- )
   SLOT-N 0 SAMPLE@ drop ;

: BAD-SLOT ( -- )
   SMALL-ID SAMPLE-N SAMPLE@ drop ;

\ ---- assertions -----------------------------------------------------------
: CHECK-ROW ( n -- ) {: work:n :}
   s" the slowed workload's own verdict is the red one" T-LABEL
   SEEN work cells + @ work T=
   s" no other workload's verdict goes red with it" T-LABEL
   SEEN-N work cells + @ 1 T= ;

: CHECK-MEASURE ( -- )
   s" MEASURE stores a full round-major table for the workloads and the reference" T-LABEL
   MEASURE-TAKEN @ SAMPLE-TOTAL T=
   s" every stored sample is a positive elapsed time" T-LABEL
   POSITIVE-N @ SAMPLE-TOTAL T=
   s" the warm-up correctness probes pass on the production path" T-LABEL
   MEASURE-RED @ 0 T= ;

: CHECK-REPORT ( -- )
   s" REPORT fails every verdict while the sample table is empty" T-LABEL
   EMPTY-RED @ WORK-N T=
   s" REPORT passes every verdict for a complete fast sample set" T-LABEL
   FAST-RED @ 0 T=
   s" REPORT reports exactly one red verdict for one slowed workload" T-LABEL
   ONE-RED @ 1 T= ;

\ ---- the property the ratio exists for -------------------------------------
\ A slower machine multiplies every timed slot by the same factor. If a verdict
\ can be moved by that, the ratchet is still measuring the box. Two very
\ different factors are used because one could pass by arithmetic accident;
\ SLOW-FACTOR is far past the calibration clamp the old absolute budgets used to
\ need, so a run this slow could not have been judged at all before.
2 constant SLOW-FACTOR
97 constant ODD-FACTOR                \ not a power of two, so no shift hides a bug

variable SCALED-RED                   \ verdicts REPORT fails on a uniformly slowed table
variable ODD-RED

: PROBE-SCALE ( -- )
   SLOW-FACTOR FILL-SCALED REPORT-RED SCALED-RED !
   ODD-FACTOR FILL-SCALED REPORT-RED ODD-RED ! ;

: CHECK-SCALE ( -- )
   s" a machine twice as slow moves no verdict" T-LABEL
   SCALED-RED @ 0 T=
   s" a machine 97 times as slow moves no verdict" T-LABEL
   ODD-RED @ 0 T= ;

\ The recorded ratios are all different and their order is known, so reading
\ each workload's budget back through the real ROW and BUDGET words pins which
\ ratio each workload is judged against: swap any two rows of the table and
\ one of these comparisons turns false.
: WORK-BUDGET ( n -- n ) {: work:n :}
   work ROW {: base:n pct:n :}
   2drop                              \ the workload name is not needed here
   base pct BUDGET ;

\ The order is ESC > RAW > LONG > MISS > HIT > SMALL. The long stream sits high
\ for a structural reason worth naming: every other slot's sub-run is a fraction
\ of its iteration count, but one 10,000-value parse cannot be cut in half, so
\ the long row's sub-run is a whole parse while the reference's shrank with
\ SLOT-CHUNKS. Its ratio is therefore against a smaller denominator than the
\ others and is not comparable to them - only to its own recorded value.
: CHECK-BUDGET-ORDER ( -- )
   s" escape-heavy decode is judged against the largest ratio" T-LABEL
   ESC-ID WORK-BUDGET RAW-ID WORK-BUDGET > TTRUE
   s" raw string decode is judged above the long stream" T-LABEL
   RAW-ID WORK-BUDGET LONG-ID WORK-BUDGET > TTRUE
   s" the long stream is judged above both key searches" T-LABEL
   LONG-ID WORK-BUDGET MISS-ID WORK-BUDGET > TTRUE
   s" the key-search miss is judged above the key-search hit" T-LABEL
   MISS-ID WORK-BUDGET HIT-ID WORK-BUDGET > TTRUE
   s" the key-search hit is judged above the small documents" T-LABEL
   HIT-ID WORK-BUDGET SMALL-ID WORK-BUDGET > TTRUE ;

\ REPORT renders each evidence line through the shared string builder, so the
\ line for the last workload is still there when REPORT returns. Reading it back
\ proves the verdicts are published as evidence and not just counted.
: LINE-PREFIX$ ( -- ptr u8 n )
   s" json-read-perf: " ;

: LINE-PASS$ ( -- ptr u8 n )
   s"  pass=true" ;

: BUILT-HEAD$ ( n -- ptr u8 n ) {: k:n :}
   SB$ {: a:ptr u:n :}
   u k < if a u exit then
   a k ;

: BUILT-TAIL$ ( n -- ptr u8 n ) {: k:n :}
   SB$ {: a:ptr u:n :}
   u k < if a u exit then
   a u k - + k ;

: CHECK-LINE ( -- )
   NO-SLOW FILL
   REPORT                                   \ six passing verdicts, six evidence lines
   s" the evidence line carries the json-read-perf tag" T-LABEL
   LINE-PREFIX$ {: tag:ptr tagu:n :}
   tagu BUILT-HEAD$ tag tagu T$=
   s" the evidence line ends with the verdict it asserted" T-LABEL
   LINE-PASS$ {: verdict:ptr verdictu:n :}
   verdictu BUILT-TAIL$ verdict verdictu T$= ;

: CHECK-MISUSE ( -- )
   s" a sample past the full table is rejected" T-LABEL
   [: BAD-EXTRA ;] E-JRP-SAMPLE TTHROWSQ
   s" a sample stored out of workload order is rejected" T-LABEL
   [: BAD-ORDER ;] E-JRP-SAMPLE TTHROWSQ
   s" a read outside the workloads is rejected" T-LABEL
   [: BAD-WORK ;] E-JRP-RANGE TTHROWSQ
   s" a read outside a workload's runs is rejected" T-LABEL
   [: BAD-SLOT ;] E-JRP-RANGE TTHROWSQ ;

: CHECK-ALL ( -- )
   CHECK-MEASURE
   CHECK-REPORT
   WORK-N 0 ?do i CHECK-ROW loop
   CHECK-SCALE
   CHECK-BUDGET-ORDER
   CHECK-LINE
   CHECK-MISUSE ;

\ The probes reset the assertion counters to read them, so they all run first and
\ every real assertion happens afterwards against a clean counter.
PROBE-MEASURE
PROBE-REPORT
PROBE-ROWS
PROBE-SCALE

T-RESET
CHECK-ALL
T-REPORT

;package

s" json-read-perf-contract-test: ok" type cr
