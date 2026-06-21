\ drive-array-habu-test.f - focused tests for native Habu array driver.

: DAHT-CONFIG-COMMON ( -- )
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" arrays" DS-CATEGORY!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   8 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS ! ;

: DAHT-CONFIG-SUM ( ptr u8 n -- ) {: arm:ptr armu :}
   DAHT-CONFIG-COMMON
   46 DS-ID !
   s" ARR-SUM" DS-NAME!
   s" ptr a n -- i64" DS-SIG!
   s" Sum the array." DS-SPEC!
   s" as" DAH-CONV!
   s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS!
   DAH-VECTORS$ DS-TESTS!
   arm armu DAH-ARM! ;

: DAHT-CONFIG-REVERSE ( ptr u8 n -- ) {: arm:ptr armu :}
   DAHT-CONFIG-COMMON
   51 DS-ID !
   s" REVERSE" DS-NAME!
   s" ptr a n --" DS-SIG!
   s" Reverse the array in place." DS-SPEC!
   s" aa" DAH-CONV!
   s" [1 2 3] -> [3 2 1]; [7] -> [7]" DAH-VECTORS!
   DAH-VECTORS$ DS-TESTS!
   arm armu DAH-ARM! ;

T-RESET

s" lib" DAHT-CONFIG-SUM
DTH-SRC-RESET DTH-SRC-TASK-HEAD s" A-SUM " DTH-SRC+ DTH-SRC-END DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
s" arm" s" habu-lib" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
s" runtime_repetitions" 1 DTH-ROW-NEED-U
s" runtime_warmups" 0 DTH-ROW-NEED-U
s" runtime_status" s" ok" DTH-ROW-NEED-S
CLEANUP-RUN

s" stdlib" DAHT-CONFIG-REVERSE
DTH-SRC-RESET DTH-SRC-TASK-HEAD s" A-REVERSE! " DTH-SRC+ DTH-SRC-END DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
s" arm" s" habu-stdlib" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
s" runtime_repetitions" 1 DTH-ROW-NEED-U
s" runtime_warmups" 0 DTH-ROW-NEED-U
s" runtime_status" s" ok" DTH-ROW-NEED-S
CLEANUP-RUN

s" skeleton" DAHT-CONFIG-SUM
s" 0 len 0 ?do arr i cells + @ + loop" DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
s" arm" s" habu-skeleton" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
s" runtime_repetitions" 1 DTH-ROW-NEED-U
s" runtime_warmups" 0 DTH-ROW-NEED-U
s" runtime_status" s" ok" DTH-ROW-NEED-S
CLEANUP-RUN

s" lib" DAHT-CONFIG-SUM
DTH-SRC-RESET DTH-SRC-TASK-HEAD s" A-SUM 1 + " DTH-SRC+ DTH-SRC-END DAH-RUN-TEXT
LR-OUTCOME$ s" fail" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ 0 T=
s" arm" s" habu-lib" DTH-ROW-NEED-S
CLEANUP-RUN

s" lib" DAHT-CONFIG-SUM
DTH-SRC-RESET DTH-SRC-TASK-HEAD s" 42 " DTH-SRC+ DTH-SRC-END DAH-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" required array helper missing" DTH-ROW-NEED-EMBEDDED-TEXT
CLEANUP-RUN

s" lib" DAHT-CONFIG-SUM
DTH-SRC-RESET DTH-SRC-TASK-HEAD s" A-SUM dup " DTH-SRC+ DTH-SRC-END DAH-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" habu_repair_packet" DTH-ROW-NEED-EMBEDDED-TEXT
s" repair_class" s" remove_producer" DTH-ROW-NEED-EMBEDDED-FIELD-S
CLEANUP-RUN

T-REPORT
s" drive-array-habu-test: ok" type cr
