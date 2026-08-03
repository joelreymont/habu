\ codegen-workload-run.f - the choreography: put one program into the dictionary
\ twice, once on each side of a migration, and leave both halves runnable. One
\ concern: the ORDER, which is the whole experiment.
\
\ WHAT HAS TO HAPPEN, AND WHY IT HAS TO HAPPEN IN THIS ORDER. The engine decides
\ at the moment it compiles a caller whether that caller will CALL a word or
\ carry a copy of the word's body, and either way the decision is baked into the
\ caller's machine code. Republishing the callee afterwards changes nothing about
\ a caller that already exists. So a measurement of "did the system get faster"
\ has to compile its callers on BOTH sides of the migration, and this file is the
\ load-order that does it:
\
\   1  the data is generated.
\   2  the subjects are published twice by the engine: into HOT-ENGINE, which
\      the migration will replace, and into HOT-FIXED, which it will not.
\   3  the before-arm drivers are compiled - the three workloads against
\      HOT-ENGINE, and the three controls against HOT-FIXED.
\   4  the compile-shaped workload's before-arm is measured. It has to be here:
\      its arms are separated by the migration itself.
\   5  the subjects are published a third time, by the native chain, into
\      HOT-CHAIN.
\   6  the after-arm drivers are compiled - the same three bodies against
\      HOT-CHAIN, and the same three controls against HOT-FIXED again.
\   7  the compile-shaped workload's after-arm is measured and its row closed.
\
\ Steps 3 and 6 compile the same text under a different search order, which is
\ the entire difference between the two arms. Steps 3 and 6 also compile the
\ CONTROL drivers, against words nothing touched in between: whatever step 5
\ changed about the process that is not the subjects' code - the code region grew
\ by a few hundred bytes, every later definition sits at a different address, the
\ caches hold different things - lands on the control rows in full.
\
\ THE MEASUREMENT IS NOT HERE. This file publishes and it measures the one row
\ that cannot be measured anywhere else; MEASURE below runs the other six, and
\ nothing calls it. tools/codegen-workload.f calls it and prints the report;
\ tools/codegen-workload-test.f does not call it at all, because a scheduled
\ suite must not contain a number that a busy host can move.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-workload-scan.f
require tools/codegen-workload-time.f
require tools/codegen-workload-hot.f

package CODEGEN-RUN

public

\ ---- how much work a measured round does ------------------------------------
\ Each of the three compute workloads is sized so that one timed run takes a
\ millisecond or two: long enough that the clock's own resolution is nothing next
\ to it, short enough that the fastest-run rule has a real chance of finding a
\ window with no interference in it. The counts differ because the three bodies
\ cost very different amounts per unit of data.
100 constant SCAN-REPS
60 constant COUNT-REPS
80 constant TERM-REPS
1 constant COUNT-INNER          \ passes over the buffer inside one COUNT driver call
31 constant ROUNDS

\ The compile-shaped workload is measured in whole batches, and a batch is
\ already tens of milliseconds, so one batch is one run. One batch ahead of the
\ timed ones is not timed: the first compilation of a process pays for pages the
\ dictionary and the code region have not touched yet, and that cost is paid once
\ per arm rather than per round. Leaving it in the measurement made the arm's
\ spread larger than anything the arm could ever be asked to show.
1 constant CHECK-REPS
7 constant CHECK-ROUNDS

private

variable CHECK-ROUND
variable REC0

: NDICT ( -- n )
   ndict@ ;

\ One round of the compile-shaped workload: the next batch of the current arm.
\ The round number is kept here rather than passed, because a timing body is a
\ quotation and a quotation cannot read the enclosing word's locals - the shape
\ src/compiler/native/migrate.f uses for the same reason.
: CHECK-STEP ( n -- ) {: arm:n :}
   arm CHECK-ROUND @ CODEGEN-HOT:CHECK-BATCH
   CHECK-ROUND @ 1+ CHECK-ROUND ! ;

\ How many dictionary records an arm published. This is the compile-shaped
\ workload's ANSWER: the two arms compile the same generated text the same
\ number of times, so they must publish the same number of records, and a row
\ whose two answers differ compiled different amounts of code and its delta
\ means nothing.
variable REC-BEFORE

: ARM-BEGIN ( -- )
   0 CHECK-ROUND !
   NDICT REC0 ! ;

: PUBLISHED ( -- n )
   NDICT REC0 @ - ;

: STEP-BEFORE ( -- )
   CODEGEN-HOT:ARM-BEFORE CHECK-STEP ;

: STEP-AFTER ( -- )
   CODEGEN-HOT:ARM-AFTER CHECK-STEP ;

public

\ The compile-shaped workload's two halves, as two words the load order calls at
\ the two moments they belong at. They are words rather than top-level lines
\ because a timing body is a quotation and a quotation is a compile-time
\ construction: the interpreter has no `[:` to give.
: CHECK-BEFORE ( -- )
   CODEGEN-CLOCK:RESET
   s" check-batch" CODEGEN-CLOCK:OPEN
   ARM-BEGIN
   STEP-BEFORE
   CHECK-REPS CHECK-ROUNDS [: STEP-BEFORE ;] CODEGEN-CLOCK:ARM-OLD
   PUBLISHED REC-BEFORE ! ;

: CHECK-AFTER ( -- )
   ARM-BEGIN
   STEP-AFTER
   CHECK-REPS CHECK-ROUNDS [: STEP-AFTER ;] CODEGEN-CLOCK:ARM-NEW
   REC-BEFORE @ PUBLISHED CODEGEN-CLOCK:ANSWERS
   CODEGEN-CLOCK:CLOSE ;

;package

\ ---- 1. the data ------------------------------------------------------------
CODEGEN-HOT:FILL-DATA

\ ---- 2. the subjects, twice, as the engine compiles them --------------------
\ HOT-ENGINE is what the migration replaces. HOT-FIXED is the same four strings
\ compiled by the same engine at the same moment, and nothing ever replaces it:
\ it is the control's subject.
package HOT-ENGINE
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-FIXED
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

\ ---- 3. the before-arm drivers ----------------------------------------------
\ Compiled from CODEGEN-HOT's three body strings, under a search order in which a
\ bare subject name resolves to the engine's code.
package WORKLOAD
public

using HOT-ENGINE
s" SCAN-OLD"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-OLD" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-OLD"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
;using

using HOT-FIXED
s" SCAN-CTL-A"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-CTL-A" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-CTL-A"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
;using

;package

\ ---- 4. the compile-shaped workload's before-arm ----------------------------
CODEGEN-RUN:CHECK-BEFORE

\ ---- 5. the migration -------------------------------------------------------
package HOT-CHAIN
public
CODEGEN-HOT:PUBLISH-CHAIN
;package

\ ---- 6. the after-arm drivers -----------------------------------------------
\ The same three strings, and the same three control strings, under a search
\ order in which a bare subject name resolves to the chain's code.
package WORKLOAD
public

using HOT-CHAIN
s" SCAN-NEW"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-NEW" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-NEW"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
;using

using HOT-FIXED
s" SCAN-CTL-B"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-CTL-B" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-CTL-B"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
;using

;package

\ ---- 7. the compile-shaped workload's after-arm -----------------------------
CODEGEN-RUN:CHECK-AFTER

\ ---- the six compute rows, and the words that run them ----------------------
\ Everything below is compiled after the drivers exist, so it can name them. The
\ answers are taken once, outside the timing, and the timing bodies drop them:
\ a timed run must do the workload and nothing else.
package CODEGEN-RUN
public

: SCAN-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD ;

: SCAN-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-NEW ;

: COUNT-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD ;

: COUNT-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-NEW ;

: TERM-OLD-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD ;

: TERM-NEW-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-NEW ;

: SCAN-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-A ;

: SCAN-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-B ;

: COUNT-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-A ;

: COUNT-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-B ;

: TERM-CTL-A-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-A ;

: TERM-CTL-B-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-B ;

\ ---- the floor rows ---------------------------------------------------------
\ A row that compares OLD CODE WITH OLD CODE. Its two arms run the same body,
\ compiled by the same code generator, over the same data - and reach it through
\ two different PUBLICATIONS of the subject: HOT-ENGINE for one arm and HOT-FIXED
\ for the other, which are the same four strings compiled by the same engine, one
\ after the other, and differ only in the addresses they landed at.
\
\ WHY A REPORT NEEDS THIS. The first run of this harness measured the scan
\ workload's two arms three per cent apart and its control five per cent apart,
\ and both looked like noise around zero. They were not: a floor row for the same
\ workload came out THIRTY per cent, which says that for a body whose inner loop
\ calls a small word millions of times, WHERE the callee was published moves the
\ workload by ten times more than the code generator does. A delta smaller than
\ its own floor row is not a small effect; it is an effect this measurement
\ cannot see, and the report says so instead of printing a number and a hedge.
\ The control row and the floor row measure different confounds - the control
\ isolates the DRIVER's side of the migration, the floor isolates the CALLEE's
\ address - so both are printed and a workload delta has to clear both.
: MEASURE-FLOOR ( -- )
   s" scan-floor"  SCAN-REPS ROUNDS  SCAN-OLD-SUM SCAN-CTL-A-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-A drop ;] CODEGEN-CLOCK:PAIR
   s" count-floor" COUNT-REPS ROUNDS  COUNT-OLD-SUM COUNT-CTL-A-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-A drop ;] CODEGEN-CLOCK:PAIR
   s" term-floor"  TERM-REPS ROUNDS  TERM-OLD-SUM TERM-CTL-A-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-A drop ;] CODEGEN-CLOCK:PAIR ;

: MEASURE ( -- )
   s" scan"         SCAN-REPS ROUNDS  SCAN-OLD-SUM SCAN-NEW-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-NEW drop ;] CODEGEN-CLOCK:PAIR
   s" scan-control" SCAN-REPS ROUNDS  SCAN-CTL-A-SUM SCAN-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-B drop ;] CODEGEN-CLOCK:PAIR
   s" count"         COUNT-REPS ROUNDS  COUNT-OLD-SUM COUNT-NEW-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-NEW drop ;] CODEGEN-CLOCK:PAIR
   s" count-control" COUNT-REPS ROUNDS  COUNT-CTL-A-SUM COUNT-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-B drop ;] CODEGEN-CLOCK:PAIR
   s" term"         TERM-REPS ROUNDS  TERM-OLD-SUM TERM-NEW-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-NEW drop ;] CODEGEN-CLOCK:PAIR
   s" term-control" TERM-REPS ROUNDS  TERM-CTL-A-SUM TERM-CTL-B-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-A drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-B drop ;] CODEGEN-CLOCK:PAIR
   MEASURE-FLOOR ;

;package
