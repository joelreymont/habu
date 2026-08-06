\ json-read-perf-contract-test.f - focused coverage for MEASURE and REPORT.
\
\ The file reopens package JSON-READ-PERF-TEST so it can drive the same private
\ sample table the provider's own verdicts read. Nothing here re-implements a
\ budget, a median or a verdict: every probe runs the real words and only reads
\ back how many assertions failed. What it proves:
\   - MEASURE really stores eighteen samples, three per workload, none of them
\     zero, and its warm-up correctness probes pass on the production path.
\   - REPORT fails all six verdicts closed while the sample table is incomplete,
\     so a skipped MEASURE can never look like a pass.
\   - A complete, fast sample set passes all six, and one slowed workload turns
\     exactly one verdict red.
\   - The red verdict is the slowed workload's own, checked for all six.
\   - Each workload is judged against its own recorded baseline, which the known
\     order of those baselines pins.
\   - Every verdict is published as an evidence line, not just counted.
\   - Storing a sample out of order, an extra sample past the table, or a read outside the
\     table is refused with a named code.
\
\ Run: bin/hb --load lib/json-read-perf-contract-test.f

require lib/json-read-perf-test.f

package JSON-READ-PERF-TEST
private

\ ---- synthetic sample values ----------------------------------------------
1 constant FAST-NS                    \ one nanosecond is under every budget
WORK-N constant NO-SLOW               \ "slow workload" argument naming no workload

\ A sample this large is over budget under any calibration: the calibration
\ factor is clamped at T-BUDGET-MAX-PCT and the widest headroom any workload
\ carries is HEADROOM-PCT, so this is one nanosecond past the largest budget the
\ provider can compute for its largest baseline.
: SLOW-NS ( -- n )
   ESC-BASE HEADROOM-PCT * PCT-DEN /
   T-BUDGET-MAX-PCT * PCT-DEN / 1+ ;

: SAMPLE-VALUE ( n n -- n ) {: work:n slow:n :}
   work slow = if SLOW-NS exit then
   FAST-NS ;

\ Complete table in the store's own round-major order: each round stores one
\ sample of every workload, exactly the order MEASURE takes them in.
: FILL ( n -- ) {: slow:n :}          \ complete table; workload `slow` is over budget
   SAMPLES-CLEAR
   SAMPLE-N 0 ?do
      WORK-N 0 ?do
         i slow SAMPLE-VALUE  i SAMPLE+
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

: PROBE-MEASURE ( -- )                \ the real six workloads, once
   T-RESET
   MEASURE
   T-FAILURES MEASURE-RED !
   TAKEN @ MEASURE-TAKEN !
   0 POSITIVE-N !
   WORK-N 0 ?do i POSITIVE-WORK loop ;

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
   FAST-NS MISS-ID SAMPLE+ ;

: BAD-ORDER ( -- )                    \ a round stores the workloads in order; a jump skips one
   SAMPLES-CLEAR
   FAST-NS LONG-ID SAMPLE+ ;

: BAD-WORK ( -- )
   WORK-N 0 SAMPLE@ drop ;

: BAD-SLOT ( -- )
   SMALL-ID SAMPLE-N SAMPLE@ drop ;

\ ---- assertions -----------------------------------------------------------
: CHECK-ROW ( n -- ) {: work:n :}
   s" the slowed workload's own verdict is the red one" T-LABEL
   SEEN work cells + @ work T=
   s" no other workload's verdict goes red with it" T-LABEL
   SEEN-N work cells + @ 1 T= ;

: CHECK-MEASURE ( -- )
   s" MEASURE stores a full round-major sample table for the six workloads" T-LABEL
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

\ The recorded baselines are all different and their order is known, so reading
\ each workload's budget back through the real ROW and BUDGET words pins which
\ baseline each workload is judged against: swap any two rows of the table and
\ one of these comparisons turns false.
: WORK-BUDGET ( n -- n ) {: work:n :}
   work ROW {: base:n pct:n :}
   2drop                              \ the workload name is not needed here
   base pct BUDGET ;

: CHECK-BUDGET-ORDER ( -- )
   s" escape-heavy decode is judged against the largest baseline" T-LABEL
   ESC-ID WORK-BUDGET RAW-ID WORK-BUDGET > TTRUE
   s" raw string decode is judged above both key searches" T-LABEL
   RAW-ID WORK-BUDGET MISS-ID WORK-BUDGET > TTRUE
   s" the key-search miss is judged above the key-search hit" T-LABEL
   MISS-ID WORK-BUDGET HIT-ID WORK-BUDGET > TTRUE
   s" the key-search hit is judged above the small documents" T-LABEL
   HIT-ID WORK-BUDGET SMALL-ID WORK-BUDGET > TTRUE
   s" the small documents are judged above the long stream" T-LABEL
   SMALL-ID WORK-BUDGET LONG-ID WORK-BUDGET > TTRUE ;

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
   CHECK-BUDGET-ORDER
   CHECK-LINE
   CHECK-MISUSE ;

\ The probes reset the assertion counters to read them, so they all run first and
\ every real assertion happens afterwards against a clean counter.
PROBE-MEASURE
PROBE-REPORT
PROBE-ROWS

T-RESET
CHECK-ALL
T-REPORT

;package

s" json-read-perf-contract-test: ok" type cr
