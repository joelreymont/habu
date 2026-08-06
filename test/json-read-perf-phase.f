\ json-read-perf-phase.f - the gate's quiescent JSON-reader ratchet phase.
\
\ The six wall-clock ratchets in lib/json-read-perf-test.f only mean anything
\ when nothing else on the box is competing for the cores, so they cannot run
\ beside the parallel test groups. This package owns the one place in the gate
\ where they do run: START, called from test/run.f after every scheduled phase
\ has drained and before TEST:COMPLETE decides the run.
\
\ START admits itself by construction rather than by convention. A gate-pool
\ fork child stamps its own slot identity (test/gate-pool.f GT-POOL-FORK-CHILD
\ calls GS-CHILD-LABEL!), so a nonzero GS-CHILD-U proves the caller is already
\ inside a forked worker and START refuses. GT-POOL-LIVE is only incremented
\ AFTER the fork returns in the parent, so it cannot see a child; it is the
\ second, parent-side half of the same admission rule - a caller that still has
\ workers in flight is not quiescent either. Both refusals throw a named code.
\
\ The measurement itself runs in ONE ordinary gate-pool fork. That is what makes
\ a failure surface: the fork is a standard pool entry, its body dies on any
\ failed assertion, and its nonzero exit goes through the same red collector as
\ every other phase, so a slowed workload turns the whole gate red with no extra
\ wiring. Draining right after the fork keeps the box to a single working
\ process while the ratchets run.
\
\ Calibration brackets the measurement: one fixed calibration spin before the
\ workloads sets the budget factor, one after checks that the host did not
\ change speed underneath them, and CAL-SPIN:DRIFT-OK? - the single owner of
\ that tolerance - decides. A bracket that moved, or a box so slow that the
\ budget compensation has saturated at its clamp, means the numbers describe the
\ machine rather than the tree.
\
\ Such an attempt is INADMISSIBLE, not a verdict. The phase re-measures up to
\ ATTEMPT-MAX times, and if the box never goes quiet it refuses to report at all
\ and exits with CONTENDED-RC, a status of its own. That distinction is the
\ point: a benchmark that missed its budget reds the gate the ordinary way,
\ while a box too busy to be measured says so in its own words and asks for a
\ rerun in a quiet window. Neither outcome can be mistaken for the other, and
\ ambient load can no longer fail a tree that did not change.

require lib/errors.f
require lib/fmt.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/test/assert.f
require lib/test/budget.f
require lib/test/runner.f
require test/cal-spin-lib.f
require test/gate-stats.f
require test/gate-pool.f
require test/run-lib.f                \ TEST:CAL-REF-MS: the owner of the per-profile spin reference
require lib/json-read-perf-test.f

package JSON-READ-PERF-PHASE

using GATE-POOL
private

\ MEASURE is ~1.7s of workloads on a quiet box; the timeout carries enough
\ headroom that only a genuinely stuck worker reaches it.
30000 constant TIMEOUT-MS

\ The "fork " stem is the gate-stats registered prefix for a forked slice
\ (test/gate-stats.f GS-STRAY-SLICE?), so this slot's completion span is
\ attributed as expected drill-down detail rather than an unexplained stray.
: LABEL$ ( -- ptr u8 n )
   s" fork json-read-perf ratchets" ;

\ ---- admission ------------------------------------------------------------
: CHILD? ( -- bool )                  \ are we inside a gate-pool fork worker?
   GS-CHILD-U @ 0 <> ;

: POOL-BUSY? ( -- bool )              \ does the caller still have workers in flight?
   GT-POOL-LIVE @ 0 <> ;

: ADMIT ( -- )
   CHILD? if E-JRPP-CHILD throw then
   POOL-BUSY? if E-JRPP-BUSY throw then ;

variable RAN

\ The ratchets are quiescent measurements, so running them twice in one gate
\ would mean the second set measured a box that had just spent two seconds
\ saturating a core. Claimed only AFTER admission: a refused call ran nothing,
\ so it must not consume the phase's one turn.
: CLAIM ( -- )
   RAN @ 0 <> if E-JRPP-REPEAT throw then
   1 RAN ! ;

\ ---- one calibration-bracketed measurement --------------------------------
\ The spin measurement is deferred so a fixture can script a known pair of
\ calibration readings and drive the real drift rule, the real diagnostic line
\ and the real re-measure without owning a clock. Production installs the shared
\ spin below.
defer PROBE-MS ( -- n )               \ one fixed calibration spin, wall ms

: PROBE-MS-DEFAULT! ( -- )
   [: CAL-SPIN:MS ;] is PROBE-MS ;

PROBE-MS-DEFAULT!

\ The factor has to come from the SAME per-profile reference the gate driver
\ calibrates against - test/run-lib.f TEST:CAL-REF-MS, which owns the committed
\ GB10 performance-core figure - and NOT from lib/test/budget.f's reference,
\ which is 0 anywhere but macOS. With that one every Linux and Spark factor
\ collapsed to the 100% floor, so this word measured the spin and then threw the
\ measurement away: a worker forked onto a slower efficiency core would run
\ every workload about half again as slow against unscaled budgets and red the
\ gate. T-BUDGET-CAL-PCT still owns the arithmetic and the 100..300 clamp, so
\ neither the reference nor the clamp is copied here.
\
\ TEST:PREPARE already installed the driver's own factor before this phase runs.
\ Replacing it is the point: the driver measured once at gate start, in the
\ driver process, while this measures in the worker that is about to run the
\ workloads, on the core it is about to run them on.
: FACTOR ( n -- n )                   \ spin ms -> this host's budget factor, percent
   TEST:CAL-REF-MS T-BUDGET-CAL-PCT ;

\ ---- machine load samples --------------------------------------------------
\ These are AUDIT CONTEXT, not the admissibility decision. The one-minute load
\ average is an exponentially damped average, so it lags a three-second phase
\ badly; the runnable-process count beside it moves within a scheduler tick and
\ says how many things wanted a CPU while the workloads ran. Recording both is
\ what makes a verdict auditable after the fact. The DECISION is ADMISSIBLE?
\ below, which reads the contended resource itself at the moment it is used.
$40 constant LOAD-CAP
-1 constant LOAD-NONE                 \ this host exposes no load file
100 constant LOAD-CENTI               \ hundredths, so a sample stays an integer
3 constant LOAD-RUN-FIELD             \ "0.31 2.59 2.82 4/702 2013010" - field 3 is 4/702
$20 constant LOAD-SP
$2E constant LOAD-DOT
$2F constant LOAD-SLASH

create LOAD-BUF LOAD-CAP allot
variable LOAD-POS

: LOAD-PATH$ ( -- ptr u8 n )
   s" /proc/loadavg" ;

: SKIP-TO ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}   \ index of first byte c, else u
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat ;

: LOAD-NUM ( ptr u8 n -- n )          \ decimal value, LOAD-NONE when unparseable
   STR>NUMBER? MATCH option
      none OF LOAD-NONE ENDOF
      some OF ENDOF
   ;MATCH ;

\ Index of the next separator at or after `from`, else u.
\
\ The bounds test has to DECIDE before any dereference. Forth `and` evaluates
\ both of its operands, so the tempting one-line form
\    begin pos u <  a pos BYTE+ c@ LOAD-SP <>  and while
\ reads the byte even when the position is already at the end. The verdict still
\ comes out right, because and-with-false discards it, but the read happened:
\ one byte past the input on every exit at end-of-buffer, and past the mapping
\ itself whenever a read filled LOAD-BUF to exactly LOAD-CAP. This is the shape
\ SKIP-TO above already uses - bounds in the `while`, dereference in the body.
: LOAD-NEXT-SP ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ LOAD-SP = if exit then
      1+
   repeat ;

: LOAD-SKIP-FIELDS ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n skip:n :}
   0 LOAD-POS !
   skip 0 ?do
      a u LOAD-POS @ LOAD-NEXT-SP 1+ LOAD-POS !
   loop
   LOAD-POS @ u >= if a 0 exit then
   a LOAD-POS @ BYTE+  u LOAD-POS @ - ;

: LOAD-AVG-X100 ( ptr u8 n -- n ) {: a:ptr u:n :}   \ one-minute average in hundredths
   a u LOAD-DOT SKIP-TO {: dot:n :}
   dot 3 + u > if LOAD-NONE exit then
   a dot LOAD-NUM {: whole:n :}
   whole LOAD-NONE = if LOAD-NONE exit then
   a dot 1+ BYTE+ 2 LOAD-NUM {: frac:n :}
   frac LOAD-NONE = if LOAD-NONE exit then
   whole LOAD-CENTI * frac + ;

: LOAD-RUNNABLE ( ptr u8 n -- n ) {: a:ptr u:n :}   \ processes wanting a CPU right now
   a u LOAD-RUN-FIELD LOAD-SKIP-FIELDS {: fa:ptr fu:n :}
   fu 0= if LOAD-NONE exit then
   fa fu LOAD-SLASH SKIP-TO {: sl:n :}
   sl fu >= if LOAD-NONE exit then
   fa sl LOAD-NUM ;

: LOAD-READ ( -- ptr u8 n )           \ the load file's bytes, zero length when absent
   LOAD-PATH$ EXISTS? 0= if LOAD-BUF 0 exit then
   LOAD-PATH$ LOAD-BUF LOAD-CAP READ-ALL {: got:n :}
   got 0 < if LOAD-BUF 0 exit then
   LOAD-BUF got ;

: LOAD-NOW ( -- n n )                 \ one-minute average x100, runnable count
   LOAD-READ {: a:ptr u:n :}
   u 0= if LOAD-NONE LOAD-NONE exit then
   a u LOAD-AVG-X100  a u LOAD-RUNNABLE ;

\ ---- admissibility --------------------------------------------------------
\ Two structural conditions, neither of them a tuned threshold:
\   - the bracket must hold. CAL-SPIN:DRIFT-OK? is the single owner of the
\     ten-percent rule, and a bracket that moved means the box changed speed
\     underneath the workloads, so the budgets they were judged against were
\     never the budgets they ran under.
\   - the compensation must not be saturated. FACTOR scales every budget by the
\     measured slowdown, but T-BUDGET-CLAMP stops at T-BUDGET-MAX-PCT, so a box
\     past that ceiling runs the workloads against budgets that no longer
\     describe it. The clamp is an existing recorded constant, not a new one.
\ Both read the very resource the workloads compete for, at the time they run,
\ which is why neither is a load heuristic.
: SATURATED? ( n -- bool ) {: pre:n :}
   pre FACTOR T-BUDGET-MAX-PCT >= ;

: ADMISSIBLE? ( n n -- bool ) {: pre:n post:n :}
   pre post CAL-SPIN:DRIFT-OK?
   pre SATURATED? 0= and ;

\ ---- the calibration evidence line -----------------------------------------
: SB-TF ( bool -- )
   if s" true" else s" false" then SB-APPEND ;

: SB-NUM ( n -- )                     \ a sample, or n/a on a host with no load file
   dup LOAD-NONE = if drop s" n/a" SB-APPEND exit then
   FMT:SB-U ;

: CAL-HEAD ( n n -- ) {: pre:n post:n :}
   SB-RESET
   s" json-read-perf-phase: calibration pre=" SB-APPEND pre FMT:SB-U
   s"  post=" SB-APPEND post FMT:SB-U
   s"  stable=" SB-APPEND pre post CAL-SPIN:DRIFT-OK? SB-TF
   s"  saturated=" SB-APPEND pre SATURATED? SB-TF ;

: CAL-LOAD ( n n n n -- ) {: lpre:n lpost:n rpre:n rpost:n :}
   s"  load1-pre-x100=" SB-APPEND lpre SB-NUM
   s"  load1-post-x100=" SB-APPEND lpost SB-NUM
   s"  runnable-pre=" SB-APPEND rpre SB-NUM
   s"  runnable-post=" SB-APPEND rpost SB-NUM ;

: CAL-TAIL ( bool -- ) {: ok:bool :}
   s"  admissible=" SB-APPEND ok SB-TF
   SB$ type cr ;

: ATTEMPT ( -- bool )                 \ measure once; true when the box stayed quiet
   LOAD-NOW {: lpre:n rpre:n :}
   PROBE-MS {: pre:n :}
   pre FACTOR TEST-BUDGET:PERF-SET
   T-RESET
   JSON-READ-PERF-TEST:MEASURE
   PROBE-MS {: post:n :}
   LOAD-NOW {: lpost:n rpost:n :}
   pre post ADMISSIBLE? {: ok:bool :}
   pre post CAL-HEAD
   lpre lpost rpre rpost CAL-LOAD
   ok CAL-TAIL
   ok ;

\ ---- refuse rather than judge a contended box ------------------------------
\ A contended attempt is re-measured, never turned into a verdict: the ratchets
\ are wall-clock numbers, so a box that is busy tells us nothing about the tree.
\ The bound is small and named because the phase is quiescent by construction -
\ a box that cannot produce one clean bracket in three tries will not produce
\ one by trying longer, and the honest report is that the measurement could not
\ be taken. That outcome exits with its OWN status, distinct from a benchmark
\ that genuinely missed its budget, so the two can never be read as each other.
3 constant ATTEMPT-MAX
68 constant CONTENDED-RC              \ measurement impossible; NOT a ratchet failure

variable TRIES

: RETRY-LINE ( -- )
   s" json-read-perf-phase: box contended; re-measuring" type cr ;

: IMPOSSIBLE-LINE ( -- )
   s" json-read-perf-phase: measurement impossible after " type
   ATTEMPT-MAX FMT:.U
   s"  contended attempts" type cr ;

: MEASURE-ADMISSIBLE ( -- )           \ bounded retries, then refuse to measure
   0 TRIES !
   begin TRIES @ ATTEMPT-MAX < while
      ATTEMPT if exit then
      TRIES @ 1+ TRIES !
      TRIES @ ATTEMPT-MAX < if RETRY-LINE then
   repeat
   IMPOSSIBLE-LINE
   CONTENDED-RC GT-POOL-FORK-EXIT ;

\ The forked worker's whole body. T-REPORT dies on any failed verdict, which is
\ what makes the slot exit nonzero and red the gate through the pool's own
\ collector.
: WORKER ( -- )
   MEASURE-ADMISSIBLE
   JSON-READ-PERF-TEST:REPORT
   T-REPORT ;

\ ---- publishing the worker's evidence -------------------------------------
\ A passing pool slot prints only its PASS line, so the six evidence lines the
\ worker wrote would stay in the capture file. Echo them here. A failing slot
\ has already had its output echoed by the pool's own failure path, so this
\ echoes exactly once either way.
: PUBLISH ( idx -- ) {: idx:idx :}
   idx GT-POOL-OK? 0= if exit then
   idx GT-POOL-OUTPUT ;

\ The worker refused to measure a contended box. That is not a slow tree and it
\ must never be reported as one, so it leaves by its own exit status instead of
\ joining the red phases. Dying here keeps the gate root in place, which is what
\ a rerun in a quiet window wants to look at.
: CONTENDED? ( idx -- bool ) {: idx:idx :}
   idx GT-POOL-EXITED-PTR @
   idx GT-POOL-CODE-PTR @ CONTENDED-RC = and ;

: IMPOSSIBLE-DIE ( -- )
   s" json-read-perf-phase: measurement impossible under machine contention; rerun in a quiet window"
   CONTENDED-RC die ;

public

\ Run the ratchets once, quiescent, as a single pool fork, and publish what it
\ measured. A benchmark that missed its budget reaches the gate as that fork's
\ nonzero exit; a box too busy to measure exits with CONTENDED-RC instead.
: START ( -- )
   ADMIT
   CLAIM
   GT-POOL-FIND-FREE {: idx:idx :}
   LABEL$ TIMEOUT-MS idx [: WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-DRAIN-SOFT
   idx CONTENDED? if IMPOSSIBLE-DIE then
   idx PUBLISH ;

;package
