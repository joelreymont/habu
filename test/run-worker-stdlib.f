\ run-worker-stdlib.f - resident stdlib phase worker.

require test/gate-stdlib-lib.f
require test/gate-stdlib-inline-lib.f

package STDLIB-WORKER

TEST:TRW-LOAD-DONE

: SETUP ( -- )
   TEST:TIMINGS? if GSI-TIMINGS! then ;

: TOOL ( -- )
   STDLIB-GATE:SKIP-SEMANTIC!
   GSI-TOOL-TRUST ;

: TAIL-RUNNER ( -- )
   GSI-TAIL-RUNNER
   s" test/gate-pool-test.f" GSI-INCLUDE
   s" test/gate-pool-orphan-test.f" GSI-INCLUDE
   s" test/json-read-perf-phase-test.f" GSI-INCLUDE
   TEST:INSTALL-POOL-HOOKS ;

: RUN ( -- )
   SETUP
   TEST:RESIDENT case
      2 of TOOL endof
      3 of CHECK-CLI-GATE:RUN endof
      17 of GSI-LINT-TOOLS endof
      21 of GSI-TOOL-REPAIR endof
      22 of GSI-TOOL-DOC endof
      23 of GSI-TOOL-LINT-PHASE endof
      24 of GSI-TOOL-TYPED endof
      25 of GSI-TAIL-FAST endof
      26 of TAIL-PURE:RUN endof
      27 of TAIL-RUNNER endof
      28 of GSI-TAIL-BUILD endof
      30 of GSI-LINT-LIBS-CORE endof
      31 of GSI-LINT-LIBS-PTX endof
      32 of GSI-LINT-LIBS-PTX-NEG endof
      33 of GSI-LINT-LIBS-PTX-TOOL endof
      34 of GSI-LINT-ARTIFACTS-FAST endof
      35 of TAIL-PROCESS:RUN endof
      36 of GSI-TOOL-LINT-REPL-PHASE endof
      37 of GSI-TOOL-LINT-AOT endof
      38 of GSI-TOOL-LINT-NAMES endof
      39 of GSI-TOOL-LINT-BUNDLE endof
      E-TBL-BOUNDS throw
   endcase ;

: ACTION ( -- [ -- ] )
   [: RUN ;] ;

ACTION

;package

execute
