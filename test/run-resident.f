\ run-resident.f - late-loaded resident test suite dispatch.

require test/gate-runner-support.f

: TR-PHASE-GR-ID ( idx -- n ) {: idx:idx :}
   idx IDX>N case
      2 of GR-ID-TOOL endof
      3 of GR-ID-CHECK-CLI endof
      4 of GR-ID-TAIL endof
      5 of GR-ID-REPAIR endof
      6 of GR-ID-DEBUG endof
      9 of GR-ID-FIXTURES endof
      10 of GR-ID-DIAG-REPAIR endof
      11 of GR-ID-DIAG-UNDEF-PRIMARY endof
      12 of GR-ID-DIAG-ALL-STRICT endof
      13 of GR-ID-DIAG-FILE-UNSAFE endof
      14 of GR-ID-DICTIONARY endof
      17 of GR-ID-LINT-TOOLS endof
      18 of GR-ID-LINT-MANIFEST endof
      19 of GR-ID-LINT-ARTIFACTS endof
      20 of GR-ID-LINT-LIBS endof
      22 of GR-ID-TOOL-REPAIR endof
      23 of GR-ID-TOOL-DOC endof
      24 of GR-ID-TOOL-LINTS endof
      25 of GR-ID-TOOL-TYPED endof
      26 of GR-ID-TAIL-FAST endof
      27 of GR-ID-TAIL-PURE endof
      28 of GR-ID-TAIL-RUNNER endof
      29 of GR-ID-TAIL-BUILD endof
      30 of GR-ID-TAIL-WARM endof
      31 of GR-ID-LINT-LIBS-CORE endof
      32 of GR-ID-LINT-LIBS-PTX endof
      33 of GR-ID-LINT-LIBS-PTX-NEG endof
      34 of GR-ID-LINT-LIBS-PTX-TOOL endof
      35 of GR-ID-LINT-ARTIFACTS-FAST endof
      36 of GR-ID-TAIL-PROCESS endof
      37 of GR-ID-TOOL-LINT-REPL endof
      38 of GR-ID-TOOL-LINT-AOT endof
      39 of GR-ID-TOOL-LINT-NAMES endof
      40 of GR-ID-TOOL-LINT-BUNDLE endof
      E-TBL-BOUNDS throw
   endcase ;

: TR-RESIDENT-UNDER! ( idx -- ) {: idx:idx :}
   TR-UNDER-READY @ 0= if exit then
   idx TR-PHASE-UNDER? if
      TR-UNDER$ GE-HB!
      exit
   then
   0 GE-HB-U ! ;

: TR-PHASE-RESIDENT-SETUP ( idx -- ) {: idx:idx :}
   PROC-ENV-DEFAULT-RESET
   TR-TMP-DEFAULT+
   TR-PERSIST-DEFAULT+
   TR-BUILD-CACHE-DEFAULT+
   TR-STATS-DEFAULT+
   TR-TOOLS-DEFAULT+
   TR-CHECK-DEFAULT+
   TR-PERSIST$ GE-WARM-ROOT!
   idx TR-UNDER-DEFAULT+
   idx TR-RESIDENT-UNDER!
   TR-PERSIST$ CK-CACHE-ROOT!
   TR-NESTED-POOL @ GT-POOL-SLOTS!
   TR-TIMINGS @ 0 <> if GSI-TIMINGS! then ;

: TR-RESIDENT-RUN-IMPL ( idx -- ) {: idx:idx :}
   idx TR-PHASE-RESIDENT-SETUP
   idx TR-PHASE-GR-ID GR-RUN-ID ;

: TR-PHASE-RESIDENT-RUN-CURRENT ( -- )
   TR-RESIDENT-ID @ >IDX TR-RESIDENT-RUN-IMPL ;

: TR-PHASE-START-RESIDENT ( idx -- ) {: idx:idx :}
   s" top-phase-fork" GS-EVENT
   s" runner-phase-fork" GS-EVENT
   idx TR-PHASE-TEST
   idx IDX>N TR-RESIDENT-ID !
   idx TR-PHASE-LABEL TR-TIMEOUT-MS [: TR-PHASE-RESIDENT-RUN-CURRENT ;] GT-POOL-START-FORK ;

: TR-R-PHASE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-RESIDENT? if idx TR-PHASE-START-RESIDENT exit then
   idx TR-PHASE-START ;

: TR-R-PHASE-START-ONCE ( idx -- ) {: idx:idx :}
   idx TR-PRE? if exit then
   idx TR-R-PHASE-START ;

: TR-R-GROUP-START ( idx -- ) {: idx:idx :}
   idx TR-GROUP-SEQ? if
      GT-POOL-DRAIN
      idx TR-R-PHASE-START
      GT-POOL-DRAIN
      exit
   then
   idx TR-R-PHASE-START ;

: TR-R-READY-CANDIDATE-START ( -- )
   TR-UNDER-READY @ 0= if exit then
   3 >IDX TR-R-PHASE-START-ONCE
   21 >IDX TR-R-PHASE-START-ONCE
   16 >IDX TR-R-PHASE-START-ONCE
   14 >IDX TR-R-PHASE-START-ONCE
   9 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-EARLY-HOST-START ( -- )
   TR-DRAIN-UNTIL-WARM
   TR-DRAIN-UNTIL-AOT-RUNNER
   TR-R-READY-CANDIDATE-START
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ TR-R-PHASE-START-ONCE
      1+
   repeat drop ;

: TR-R-LATE-START ( -- )
   TR-UNDER-READY @ 0 <> if exit then
   0 begin dup TR-LATE-PHASES < while
      dup >IDX TR-LATE-ORDER@
      TR-R-GROUP-START
      1+
   repeat drop ;

: TR-R-CHECK-WARM-START ( -- )
   TR-UNDER-READY @ 0 <> if exit then
   0 begin dup TR-CHECK-WARM-PHASES < while
      dup >IDX TR-CHECK-WARM-ORDER@ TR-R-PHASE-START
      1+
   repeat drop ;

: TR-R-CANDIDATE-WORK-START ( -- )
   TR-DRAIN-UNTIL-CHECK-WARM
   TR-R-CHECK-WARM-START
   TR-R-LATE-START ;

: TR-DAG-RUN-REST ( -- )
   6 >IDX TR-R-PHASE-START
   TR-R-EARLY-HOST-START
   TR-DRAIN-UNTIL-UNDER
   TR-R-CANDIDATE-WORK-START
   GT-POOL-DRAIN ;
