\ codegen-workload-run.f - the choreography: put one program into the dictionary
\ several times, on both sides of a migration, and leave every copy runnable. One
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
\   2  the subjects are published six times by the engine: into HOT-ENGINE,
\      which the migration will replace, into HOT-FIXED, which it will not, and
\      into HOT-F1..HOT-F4, which it will not either.
\   3  the before-arm drivers are compiled - one per workload against
\      HOT-ENGINE, one against HOT-FIXED for the control, and one against each
\      of the four extra publications.
\   4  the compile-shaped workload's null draws are measured, and then its
\      before-arm. They have to be here: its arms are separated by the migration
\      itself.
\   5  the subjects are published a seventh time, by the native chain, into
\      HOT-CHAIN.
\   5b the CHECKER's own fold is migrated into HOT-REACH and every call
\      instruction that entered its old code is moved onto the new routine
\      (src/compiler/native/reach.f). This is the only step that reaches a
\      caller nothing can recompile, and it is the one the compile-shaped row
\      exists to see: the checker's callers were compiled into this binary.
\   6  the after-arm drivers are compiled - the same bodies against HOT-CHAIN,
\      and the same control bodies against HOT-FIXED again.
\   7  the compile-shaped workload's after-arm is measured and its row closed.
\
\ Steps 3 and 6 compile the same text under a different search order, which is
\ the entire difference between the two arms. Steps 3 and 6 also compile the
\ CONTROL drivers, against words nothing touched in between: whatever step 5
\ changed about the process that is not the subjects' code - the code region grew
\ by a few hundred bytes, every later definition sits at a different address, the
\ caches hold different things - lands on the control rows in full.
\
\ WHY FOUR EXTRA PUBLICATIONS. A control row answers "what does crossing the
\ migration do to a driver that should not care?", and one floor row used to
\ answer "what does reaching a DIFFERENT publication of the same subject do?".
\ One row is one sample, and the second question does not have a small symmetric
\ answer: two byte-identical publications of the same body have measured
\ thirty-five per cent apart on the scan shape, and the same pair has measured
\ two per cent apart on another run. A bar built from one draw of that is a
\ coin toss, and it produced three consecutive runs in which a workload was
\ reported as a REAL LOSS against a two per cent bar. With five publications in
\ hand, each workload's placement row times all five against each other and
\ reports the widest gap any two of them showed; that gap is the bar, and a
\ delta has to clear THAT.
\
\ THE MEASUREMENT IS NOT HERE. This file publishes and it measures the rows that
\ cannot be measured anywhere else; tools/codegen-workload-rows.f holds the
\ timed rows of the five compute workloads, and nothing here calls them.
\ tools/codegen-workload.f runs them and prints the report;
\ tools/codegen-workload-test.f does not, because a scheduled suite must not
\ contain a number that a busy host can move - what it checks about the rows
\ measured HERE is the fact their delta would be meaningless without: that both
\ arms compiled the same amount.

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
\ Each of the compute workloads is sized so that one timed run takes a
\ millisecond or two: long enough that the clock's own resolution is nothing next
\ to it, short enough that the fastest-run rule has a real chance of finding a
\ window with no interference in it. The counts differ because the bodies cost
\ very different amounts per unit of data - a mix run makes three passes over the
\ buffer where a count run makes one, so it is given a third of the repetitions.
100 constant SCAN-REPS
60 constant COUNT-REPS
80 constant TERM-REPS
20 constant MIX-REPS
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
variable CHECK-ARM
variable ARM-NEXT               \ the next unused batch-arm letter
variable REC0

: NDICT ( -- n )
   ndict@ ;

\ One round of the compile-shaped workload: the next batch of the current arm.
\ The arm and the round number are kept here rather than passed, because a timing
\ body is a quotation and a quotation cannot read the enclosing word's locals -
\ the shape src/compiler/native/migrate.f uses for the same reason.
: CHECK-STEP ( -- )
   CHECK-ARM @ CHECK-ROUND @ CODEGEN-HOT:CHECK-BATCH
   CHECK-ROUND @ 1+ CHECK-ROUND ! ;

\ How many dictionary records an arm published. This is the compile-shaped
\ workload's ANSWER: the two arms compile the same generated text the same
\ number of times, so they must publish the same number of records, and a row
\ whose two answers differ compiled different amounts of code and its delta
\ means nothing.
variable REC-BEFORE

: PUBLISHED ( -- n )
   NDICT REC0 @ - ;

\ Start one arm: its own letter, so its generated package names collide with
\ nothing, its round counter back to zero, and one untimed batch.
: ARM-BEGIN ( n -- ) {: arm:n :}
   arm CHECK-ARM !
   0 CHECK-ROUND !
   NDICT REC0 !
   CHECK-STEP ;

: TAKE-ARM ( -- n )
   ARM-NEXT @
   ARM-NEXT @ 1+ ARM-NEXT ! ;

: CHECK-OLD ( n -- )
   ARM-BEGIN
   CHECK-REPS CHECK-ROUNDS [: CHECK-STEP ;] CODEGEN-CLOCK:ARM-OLD
   PUBLISHED REC-BEFORE ! ;

: CHECK-NEW ( n -- )
   ARM-BEGIN
   CHECK-REPS CHECK-ROUNDS [: CHECK-STEP ;] CODEGEN-CLOCK:ARM-NEW ;

: CHECK-CLOSE ( -- )
   REC-BEFORE @ PUBLISHED CODEGEN-CLOCK:ANSWERS
   CODEGEN-CLOCK:CLOSE ;

\ One null draw for the compile-shaped workload: two batch sequences, one after
\ the other, both compiled by the engine with nothing at all in between. Its
\ delta is therefore entirely what compiling a few hundred definitions does to
\ the compilation of the few hundred after them - the dictionary is longer, and
\ every name in the next sequence is looked up in it. That is the same gap the
\ real row's two arms are separated by, plus the migration, so it is the honest
\ bar for the real row and the reason the compile-shaped row can be judged at
\ all. The draws run BEFORE the real row: the growth per sequence is a larger
\ fraction of a smaller total, so a bar taken here is if anything the
\ conservative one.
: DRIFT-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" check" CODEGEN-CLOCK:OPEN-NULL
   TAKE-ARM CHECK-OLD
   TAKE-ARM CHECK-NEW
   CHECK-CLOSE ;

public

\ The compile-shaped workload's null draws and its two halves, as words the load
\ order calls at the moments they belong at. They are words rather than top-level
\ lines because a timing body is a quotation and a quotation is a compile-time
\ construction: the interpreter has no `[:` to give.
\
\ FOUR DRAWS, for the reason the file header gives: one draw of a confound this
\ size is a coin toss, and the report's bar is the largest of them.
: CHECK-DRIFT ( -- )
   CODEGEN-CLOCK:RESET
   CODEGEN-HOT:ARM-AFTER 1+ ARM-NEXT !
   s" check-drift-1" DRIFT-ROW
   s" check-drift-2" DRIFT-ROW
   s" check-drift-3" DRIFT-ROW
   s" check-drift-4" DRIFT-ROW ;

: CHECK-BEFORE ( -- )
   s" check-batch" s" check" CODEGEN-CLOCK:OPEN-REAL
   CODEGEN-HOT:ARM-BEFORE CHECK-OLD ;

: CHECK-AFTER ( -- )
   CODEGEN-HOT:ARM-AFTER CHECK-NEW
   CHECK-CLOSE ;

;package

\ ---- 1. the data ------------------------------------------------------------
CODEGEN-HOT:FILL-DATA

\ ---- 2. the subjects, six times, as the engine compiles them ----------------
\ HOT-ENGINE is what the migration replaces. The other five are the same four
\ strings compiled by the same engine at the same moment, and nothing ever
\ replaces any of them: HOT-FIXED is the control's subject, and HOT-F1..HOT-F4
\ are the four the placement row sweeps - identical code at four other addresses.
package HOT-ENGINE
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-FIXED
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-F1
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-F2
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-F3
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

package HOT-F4
public
CODEGEN-HOT:PUBLISH-ENGINE
;package

\ ---- 3. the before-arm drivers ----------------------------------------------
\ Compiled from CODEGEN-HOT's body strings, each group under a search order in
\ which a bare subject name resolves to one publication's code. The two mix
\ bodies also name HOT-FIXED's subject outright, for the passes the migration is
\ not meant to reach; that name is the same in every group.
package WORKLOAD
public

using HOT-ENGINE
s" SCAN-OLD"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-OLD" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-OLD"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-OLD" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-OLD" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-FIXED
s" SCAN-CTL-A"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-CTL-A" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-CTL-A"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-CTL-A" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-CTL-A" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-F1
s" SCAN-F1"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-F1" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-F1"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-F1" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-F1" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-F2
s" SCAN-F2"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-F2" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-F2"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-F2" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-F2" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-F3
s" SCAN-F3"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-F3" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-F3"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-F3" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-F3" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-F4
s" SCAN-F4"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-F4" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-F4"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-F4" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-F4" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

;package

\ ---- 4. the compile-shaped workload's null draws and before-arm -------------
CODEGEN-RUN:CHECK-DRIFT
CODEGEN-RUN:CHECK-BEFORE

\ ---- 5. the migration -------------------------------------------------------
package HOT-CHAIN
public
CODEGEN-HOT:PUBLISH-CHAIN
;package

\ ---- 5b. the checker's own fold, and the callers already in the image -------
\ The four subjects above are a program published twice to be timed. This is the
\ engine's own word: SYM-FOLD-C, run once per byte of every symbol the checker
\ compares. Its callers are the checker's, compiled into bin/hb and never
\ recompiled, so no republication can reach them - which is why the
\ compile-shaped row below used to report nothing whatever the chain emitted.
\ The body is migrated into a package of its own and every call instruction that
\ entered the old code is moved onto the new routine, so the after-arm compiles
\ its batches with the chain's fold in the checker's own path. The move refuses
\ rather than answering zero, so this step cannot silently do nothing.
package HOT-REACH
public
CODEGEN-HOT:PUBLISH-CHECKER-FOLD
;package

CODEGEN-HOT:REACH-CHECKER-FOLD

\ ---- 6. the after-arm drivers -----------------------------------------------
\ The same strings, and the same control strings, under a search order in which a
\ bare subject name resolves to the chain's code.
package WORKLOAD
public

using HOT-CHAIN
s" SCAN-NEW"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-NEW" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-NEW"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-NEW" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-NEW" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

using HOT-FIXED
s" SCAN-CTL-B"  CODEGEN-HOT:SCAN-BODY$  CODEGEN-HOT:DEFINE-AS
s" COUNT-CTL-B" CODEGEN-HOT:COUNT-BODY$ CODEGEN-HOT:DEFINE-AS
s" TERM-CTL-B"  CODEGEN-HOT:TERM-BODY$  CODEGEN-HOT:DEFINE-AS
s" MIX66-CTL-B" CODEGEN-HOT:MIX66-BODY$ CODEGEN-HOT:DEFINE-AS
s" MIX33-CTL-B" CODEGEN-HOT:MIX33-BODY$ CODEGEN-HOT:DEFINE-AS
;using

;package

\ ---- 7. the compile-shaped workload's after-arm -----------------------------
CODEGEN-RUN:CHECK-AFTER

require tools/codegen-workload-rows.f
