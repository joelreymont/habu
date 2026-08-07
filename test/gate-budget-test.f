\ gate-budget-test.f - the gate's load factor: who gets it, and what it buys.
\
\ Two claims, one mechanism. The runner measures a load factor at startup and
\ scales per-suite wall clock budgets by it. This file pins that a SPAWNED phase
\ is handed that factor at all - the resident phases get it through
\ PROC-ENV-DEFAULT+ and a spawned child inherits nothing but the environment
\ test/run-lib.f writes - and that the stdlib gate's per-suite wall is derived
\ from it rather than frozen.
\
\ The stdlib gate spawns one child engine per suite and gives it a wall clock
\ deadline. That wall used to be a fixed constant, and a fixed wall in a
\ process-spawning suite cannot tell a slow box from a hung child: on 2026-08-07
\ compiler-insn-proof, which runs 99543ms on an idle box, was killed at 120145ms
\ against a fixed 120000 while a second full gate shared the machine. What this
\ file pins is the repair - the deadline is now the nominal wall put through
\ lib/test/budget.f's T-BUDGET-MS - and the two properties that keep the hang
\ detector meaningful: the deadline is never below nominal, and it is bounded.
\
\ Mutation check: revert the wall to a bare `120000 constant` and the stretch
\ claims fail; drop the clamp in lib/test/budget.f and the bound claim fails.
\
\ This file has to BE a top-level process, which is why its registration sits in
\ the tail slice: it starts the runner for real (TEST:PREPARE reads the process's
\ own script arguments and calibrates the host), and a forked member of a gate
\ slice would hand it the slice's arguments and stamp on the live runner state it
\ was forked from.
\
\ Run: bin/hb --load test/gate-budget-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test.f
require lib/test/budget.f
require test/run-lib.f
require test/gate-stdlib-lib.f

package GATE-BUDGET-TEST

private

variable GBT-SAVED-PCT                  \ the live factor, restored before we leave

\ The gate sets T-BUDGET-PCT for the whole image (test/run-lib.f PREPARE), so a
\ suite that drives the factor has to put it back. Reading it through the same
\ cell the budget word reads is the point: this file must not keep a private copy
\ of the factor, or it would stop measuring the shipped one.
: GBT-SAVE ( -- )
   T-BUDGET-PCT @ GBT-SAVED-PCT ! ;

: GBT-RESTORE ( -- )
   GBT-SAVED-PCT @ T-BUDGET-PCT ! ;

: GBT-PCT! ( n -- ) {: pct:n :}
   pct T-BUDGET-PCT ! ;

: GBT-SCALED ( n n -- n ) {: nominal:n pct:n :}
   nominal pct * 100 / ;

\ ---- the stdlib gate's per-suite wall ----------------------------------------

: GBT-STDLIB-NOMINAL ( -- )
   s" an idle box gets the stdlib gate's nominal per-suite wall" T-LABEL
   T-BUDGET-MIN-PCT GBT-PCT!
   STDLIB-GATE:SUITE-TIMEOUT-MS STDLIB-GATE:SUITE-TIMEOUT-NOMINAL-MS T= ;

: GBT-STDLIB-STRETCH ( -- )
   s" a loaded box stretches it by the measured factor" T-LABEL
   200 GBT-PCT!
   STDLIB-GATE:SUITE-TIMEOUT-MS
   STDLIB-GATE:SUITE-TIMEOUT-NOMINAL-MS 200 GBT-SCALED T=
   s" and the stretch is bounded by the clamp, so a hang still dies" T-LABEL
   T-BUDGET-MAX-PCT GBT-PCT!
   STDLIB-GATE:SUITE-TIMEOUT-MS
   STDLIB-GATE:SUITE-TIMEOUT-NOMINAL-MS T-BUDGET-MAX-PCT GBT-SCALED T= ;

\ ---- the clamp the two walls rest on -----------------------------------------

\ The walls are only honest because the factor cannot fall below 100 or rise
\ above 300. Asked through the shipped clamp, not restated here.
: GBT-CLAMP ( -- )
   s" the factor never shrinks a budget below its nominal" T-LABEL
   0 T-BUDGET-CLAMP T-BUDGET-MIN-PCT T=
   T-BUDGET-MIN-PCT 1- T-BUDGET-CLAMP T-BUDGET-MIN-PCT T=
   s" and never stretches one past three times its nominal" T-LABEL
   T-BUDGET-MAX-PCT 1+ T-BUDGET-CLAMP T-BUDGET-MAX-PCT T=
   $7FFFFFFF T-BUDGET-CLAMP T-BUDGET-MAX-PCT T= ;

\ ---- the factor reaching a spawned phase ------------------------------------

\ TEST:PHASE-BASE builds the whole environment a spawned phase inherits. The
\ factor has to be IN it: a spawned child re-reads HB_LOAD_PCT from its
\ environment, and with the name absent lib/test/budget.f falls back to measuring
\ an already-loaded box against an idle reference and comes out at nominal. That
\ is what killed compiler-insn-proof at 120145ms against a 120000 wall.
: GBT-PHASE-ENV ( idx -- ) {: idx:idx :}
   idx TEST:PHASE-BASE
   s" HB_LOAD_PCT" >LEN PROC-ENV-HAS-NAME? TTRUE
   s" HB_CAL_PCT" >LEN PROC-ENV-HAS-NAME? TTRUE ;

: GBT-SPAWNED-ENV ( -- )
   s" a spawned stdlib slice inherits the gate's load factor" T-LABEL
   TEST:PHASE-DEBUG >IDX GBT-PHASE-ENV
   s" and so does every other phase the runner spawns" T-LABEL
   TEST:PHASE-ENGINE-BUILD >IDX GBT-PHASE-ENV ;

: GBT-MAIN ( -- )
   T-RESET
   TEST:PREPARE
   GBT-SAVE
   GBT-SPAWNED-ENV
   GBT-STDLIB-NOMINAL
   GBT-STDLIB-STRETCH
   GBT-CLAMP
   GBT-RESTORE
   T-REPORT
   s" gate-budget-test: ok" type cr ;

GBT-MAIN

;package
