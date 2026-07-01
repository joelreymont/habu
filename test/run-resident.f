\ run-resident.f - late-loaded resident test suite dispatch.

variable TR-R-SETUP-START-NS

: TR-R-SETUP-START ( -- )
   mono-ns TR-R-SETUP-START-NS ! ;

: TR-R-SETUP-MS ( -- n )
   mono-ns TR-R-SETUP-START-NS @ - PROC-NS-PER-MS / ;

: TR-R-SHARED-BASE ( -- )
   s" test/run-shared-stdlib.f" included ;

: TR-R-SHARED-BASE-SPAN ( -- )
   s" setup/shared-stdlib" TR-R-SETUP-MS GS-SPAN ;

: TR-R-SHARED-SETUP ( -- )
   TR-R-SETUP-START
   TR-R-SHARED-BASE
   TR-R-SHARED-BASE-SPAN ;

: TR-PHASE-RESIDENT-SETUP ( idx -- ) {: idx:idx :}
   PROC-ENV-DEFAULT-RESET
   TR-TMP-DEFAULT+
   TR-BUILD-CACHE-DEFAULT+
   TR-STATS-DEFAULT+
   idx TR-UNDER-DEFAULT+
   TR-PERSIST$ CK-CACHE-ROOT!
   TR-NESTED-POOL @ GT-POOL-SLOTS! ;

: TR-RESIDENT-RUN-IMPL ( idx -- ) {: idx:idx :}
   idx TR-PHASE-RESIDENT-SETUP
   s" test/run-worker.f" included ;

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
   idx TR-R-PHASE-START
   idx TR-PRE-MARK ;

: TR-R-GROUP-START ( idx -- ) {: idx:idx :}
   idx TR-GROUP-SEQ? if
      GT-POOL-DRAIN
      idx TR-R-PHASE-START
      GT-POOL-DRAIN
      exit
   then
   idx TR-R-PHASE-START ;

: TR-R-GROUP-START-ONCE ( idx -- ) {: idx:idx :}
   idx TR-PRE? if exit then
   idx TR-R-GROUP-START
   idx TR-PRE-MARK ;

: TR-R-READY-CANDIDATE-START ( -- )
   TR-UNDER-READY @ 0= if exit then
   3 >IDX TR-R-PHASE-START-ONCE
   9 >IDX TR-R-PHASE-START-ONCE
   14 >IDX TR-R-PHASE-START-ONCE
   21 >IDX TR-R-PHASE-START-ONCE
   16 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-READY-CANDIDATE-START-NON-STDLIB ( -- )
   TR-UNDER-READY @ 0= if exit then
   9 >IDX TR-R-PHASE-START-ONCE
   14 >IDX TR-R-PHASE-START-ONCE
   21 >IDX TR-R-PHASE-START-ONCE
   16 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-READY-CANDIDATE-START-STDLIB ( -- )
   TR-UNDER-READY @ 0= if exit then
   3 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-EARLY-HOST-START-NON-STDLIB ( -- )
   TR-R-READY-CANDIDATE-START-NON-STDLIB
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ {: idx:idx :}
      idx TR-STDLIB-SLICE? 0= if idx TR-R-PHASE-START-ONCE then
      1+
   repeat drop ;

: TR-R-EARLY-HOST-START-STDLIB ( -- )
   TR-R-READY-CANDIDATE-START-STDLIB
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ {: idx:idx :}
      idx TR-STDLIB-SLICE? if idx TR-R-PHASE-START-ONCE then
      1+
   repeat drop ;

: TR-R-EARLY-HOST-START ( -- )
   TR-R-READY-CANDIDATE-START
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ TR-R-PHASE-START-ONCE
      1+
   repeat drop ;

: TR-R-LATE-START ( -- )
   TR-UNDER-READY @ 0= if exit then
   0 begin dup TR-LATE-PHASES < while
      dup >IDX TR-LATE-ORDER@
      TR-R-GROUP-START-ONCE
      1+
   repeat drop ;

: TR-R-CANDIDATE-HOST-START ( -- )
   TR-UNDER-READY @ 0= if exit then
   0 begin dup TR-CANDIDATE-HOST-PHASES < while
      dup >IDX TR-CANDIDATE-HOST-ORDER@ TR-R-PHASE-START-ONCE
      1+
   repeat drop ;

: TR-R-CANDIDATE-WORK-START ( -- )
   TR-R-CANDIDATE-HOST-START
   TR-R-LATE-START ;

: TR-DAG-RUN-REST ( -- )
   6 >IDX TR-R-PHASE-START
   TR-R-EARLY-HOST-START-NON-STDLIB
   TR-R-SHARED-SETUP
   TR-R-EARLY-HOST-START-STDLIB
   TR-DRAIN-UNTIL-UNDER
   TR-R-CANDIDATE-WORK-START
   GT-POOL-DRAIN ;
