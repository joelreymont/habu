\ run-resident.f - late-loaded resident test suite dispatch.

variable TR-R-SETUP-START-NS

: TR-R-SETUP-START ( -- )
   mono-ns TR-R-SETUP-START-NS ! ;

: TR-R-SETUP-MS ( -- n )
   mono-ns TR-R-SETUP-START-NS @ - PROC-NS-PER-MS / ;

: TR-R-SHARED-BASE ( -- )
   s" test/run-shared-stdlib.f" included ;

: TR-R-SHARED-BASE-SPAN ( -- )
   s" setup/shared-stdlib" TR-R-SETUP-MS GS-SPAN-LOAD ;

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
   idx TR-RERUN-SKIP? if exit then
   idx TR-RESULT-CACHED? if idx TR-RESULT-SKIP exit then
   idx TR-PHASE-RESIDENT? if idx TR-PHASE-START-RESIDENT exit then
   idx TR-PHASE-START ;

: TR-R-PHASE-START-ONCE ( idx -- ) {: idx:idx :}
   idx TR-PRE? if exit then
   idx TR-R-PHASE-START
   idx TR-PRE-MARK ;

: TR-R-GROUP-START ( idx -- ) {: idx:idx :}
   idx TR-GROUP-SEQ? if
      GT-POOL-DRAIN-SOFT
      idx TR-R-PHASE-START
      GT-POOL-DRAIN-SOFT
      exit
   then
   idx TR-R-PHASE-START ;

: TR-R-GROUP-START-ONCE ( idx -- ) {: idx:idx :}
   idx TR-PRE? if exit then
   idx TR-R-GROUP-START
   idx TR-PRE-MARK ;

: TR-R-READY-CANDIDATE-START-DIRECT ( -- )
   TR-UNDER-READY @ 0= if exit then
   9 >IDX TR-R-PHASE-START-ONCE
   21 >IDX TR-R-PHASE-START-ONCE
   16 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-READY-CANDIDATE-START-SHARED ( -- )
   TR-UNDER-READY @ 0= if exit then
   3 >IDX TR-R-PHASE-START-ONCE
   14 >IDX TR-R-PHASE-START-ONCE ;

: TR-R-EARLY-HOST-START-DIRECT ( -- )
   TR-R-READY-CANDIDATE-START-DIRECT
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ {: idx:idx :}
      idx TR-SHARED-BASE? 0= if idx TR-R-PHASE-START-ONCE then
      1+
   repeat drop ;

: TR-R-EARLY-HOST-START-SHARED ( -- )
   TR-R-READY-CANDIDATE-START-SHARED
   0 begin dup TR-EARLY-HOST-PHASES < while
      dup >IDX TR-EARLY-HOST-ORDER@ {: idx:idx :}
      idx TR-SHARED-BASE? if idx TR-R-PHASE-START-ONCE then
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

\ Direct phases fork before the serial shared setup so their work overlaps
\ it; shared-base phases fork after it and inherit the tool base
\ copy-on-write, so their require lists load only the family deltas.
: TR-DAG-RUN-REST ( -- )
   6 >IDX TR-R-PHASE-START
   TR-R-EARLY-HOST-START-DIRECT
   TR-R-SHARED-SETUP
   TR-R-EARLY-HOST-START-SHARED
   TR-DRAIN-UNTIL-UNDER
   TR-R-CANDIDATE-WORK-START
   GT-POOL-DRAIN-SOFT ;
