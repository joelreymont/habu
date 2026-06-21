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

: DAHT-SUM-LIB$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" A-SUM " DTH-SRC+
   DTH-SRC-END ;

: DAHT-SUM-PLUS1$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" A-SUM 1 + " DTH-SRC+
   DTH-SRC-END ;

: DAHT-SUM-DUP$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" A-SUM dup " DTH-SRC+
   DTH-SRC-END ;

: DAHT-SUM-CONSTANT$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" 42 " DTH-SRC+
   DTH-SRC-END ;

: DAHT-SUM-BODY$ ( -- ptr u8 n )
   s" 0 len 0 ?do arr i cells + @ + loop" ;

: DAHT-REVERSE-STDLIB$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" A-REVERSE! " DTH-SRC+
   DTH-SRC-END ;

: DAHT-ASSERT-PASS ( ptr u8 n -- ) {: arm:ptr armu :}
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   s" arm" arm armu DTH-ROW-NEED-S
   s" prompt_sha256" DTH-ROW-NEED-KEY
   s" final_bundle_sha256" DTH-ROW-NEED-KEY ;

: DAHT-ASSERT-REJECT ( ptr u8 n -- ) {: needle:ptr needleu :}
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   needle needleu DTH-ROW-NEED-EMBEDDED-TEXT ;

: DAHT-TEST-LIB-PASS ( -- )
   s" lib" DAHT-CONFIG-SUM
   DAHT-SUM-LIB$ DAH-RUN-TEXT
   s" habu-lib" DAHT-ASSERT-PASS
   CLEANUP-RUN ;

: DAHT-TEST-STDLIB-AA-PASS ( -- )
   s" stdlib" DAHT-CONFIG-REVERSE
   DAHT-REVERSE-STDLIB$ DAH-RUN-TEXT
   s" habu-stdlib" DAHT-ASSERT-PASS
   CLEANUP-RUN ;

: DAHT-TEST-SKELETON-PASS ( -- )
   s" skeleton" DAHT-CONFIG-SUM
   DAHT-SUM-BODY$ DAH-RUN-TEXT
   s" habu-skeleton" DAHT-ASSERT-PASS
   CLEANUP-RUN ;

: DAHT-TEST-CERTIFIED-FAIL ( -- )
   s" lib" DAHT-CONFIG-SUM
   DAHT-SUM-PLUS1$ DAH-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ 0 T=
   s" arm" s" habu-lib" DTH-ROW-NEED-S
   CLEANUP-RUN ;

: DAHT-TEST-MISSING-HELPER-REJECT ( -- )
   s" lib" DAHT-CONFIG-SUM
   DAHT-SUM-CONSTANT$ DAH-RUN-TEXT
   s" required array helper missing" DAHT-ASSERT-REJECT
   CLEANUP-RUN ;

: DAHT-TEST-REPAIR-PACKET ( -- )
   s" lib" DAHT-CONFIG-SUM
   DAHT-SUM-DUP$ DAH-RUN-TEXT
   s" habu_repair_packet" DAHT-ASSERT-REJECT
   s" repair_class" s" remove_producer" DTH-ROW-NEED-EMBEDDED-FIELD-S
   CLEANUP-RUN ;

: DAHT-MAIN ( -- )
   T-RESET
   DAHT-TEST-LIB-PASS
   DAHT-TEST-STDLIB-AA-PASS
   DAHT-TEST-SKELETON-PASS
   DAHT-TEST-CERTIFIED-FAIL
   DAHT-TEST-MISSING-HELPER-REJECT
   DAHT-TEST-REPAIR-PACKET
   T-REPORT
   s" drive-array-habu-test: ok" type cr ;

DAHT-MAIN
