\ run-worker-stdlib.f - resident stdlib phase worker.

require test/gate-stdlib-lib.f
require test/gate-stdlib-inline-lib.f

: TRWS-SETUP ( -- )
   TR-TIMINGS @ 0 <> if GSI-TIMINGS! then ;

: TRWS-STDLIB ( -- )
   s" test/gate-stdlib-cases.f" included ;

: TRWS-TOOL ( -- )
   SUITE-SKIP-TOOL-SEMANTIC!
   GSI-TOOL-TRUST ;

: TRWS-RUN ( -- )
   TRWS-SETUP
   TR-RESIDENT-ID @ case
      2 of TRWS-TOOL endof
      3 of GSI-CHECK-CLI endof
      17 of GSI-LINT-TOOLS endof
      18 of GSI-LINT-MANIFEST endof
      22 of GSI-TOOL-REPAIR endof
      23 of GSI-TOOL-DOC endof
      24 of GSI-TOOL-LINT-PHASE endof
      25 of GSI-TOOL-TYPED endof
      26 of GSI-TAIL-FAST endof
      27 of GSI-TAIL-PURE endof
      28 of GSI-TAIL-RUNNER endof
      29 of GSI-TAIL-BUILD endof
      31 of GSI-LINT-LIBS-CORE endof
      32 of GSI-LINT-LIBS-PTX endof
      33 of GSI-LINT-LIBS-PTX-NEG endof
      34 of GSI-LINT-LIBS-PTX-TOOL endof
      35 of GSI-LINT-ARTIFACTS-FAST endof
      36 of GSI-TAIL-PROCESS endof
      37 of GSI-TOOL-LINT-REPL-PHASE endof
      38 of GSI-TOOL-LINT-AOT endof
      39 of GSI-TOOL-LINT-NAMES endof
      40 of GSI-TOOL-LINT-BUNDLE endof
      E-TBL-BOUNDS throw
   endcase ;

TRWS-RUN
