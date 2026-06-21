\ drive-forth-test.f - focused tests for native Habu Forth driver.

: DFHT-CONFIG-SQUARE ( ptr u8 n ptr u8 n -- )
   {: feedback:ptr feedbacku arm:ptr armu :}
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   1 DS-ID !
   s" SQUARE" DS-NAME!
   s" i64 -- i64" DS-SIG!
   s" arithmetic" DS-CATEGORY!
   s" 7 -> 49; -3 -> 9" DS-TESTS!
   s" Return n squared." DS-SPEC!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   1 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS !
   feedback feedbacku DFH-FEEDBACK!
   arm armu DFH-ARM! ;

: DFHT-ASSERT-ROW-COMMON ( ptr u8 n -- ) {: arm:ptr armu :}
   s" arm" arm armu DTH-ROW-NEED-S
   s" prompt_sha256" DTH-ROW-NEED-KEY
   s" raw_response_sha256" DTH-ROW-NEED-KEY
   s" extracted_candidate_sha256" DTH-ROW-NEED-KEY
   s" final_bundle_sha256" DTH-ROW-NEED-KEY ;

: DFHT-ASSERT-PASS ( ptr u8 n -- ) {: arm:ptr armu :}
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-ROUNDS @ 1 T=
   arm armu DFHT-ASSERT-ROW-COMMON ;

: DFHT-ASSERT-FAIL ( ptr u8 n -- ) {: arm:ptr armu :}
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ 0 T=
   arm armu DFHT-ASSERT-ROW-COMMON
   LR-ROW$ s" llm test failed" CONTAINS? TTRUE ;

: DFHT-ASSERT-REJECT ( ptr u8 n -- ) {: arm:ptr armu :}
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   arm armu DFHT-ASSERT-ROW-COMMON ;

: DFHT-TEST-PASS ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * ;" DFH-RUN-TEXT
   s" habu-forth" DFHT-ASSERT-PASS
   LR-ROW$ s" ok" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DFHT-TEST-FAIL-RAW ( -- )
   s" raw" s" habu-forth-raw" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * 1 + ;" DFH-RUN-TEXT
   s" habu-forth-raw" DFHT-ASSERT-FAIL
   CLEANUP-RUN ;

: DFHT-TEST-CHECKER-REJECT ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * dup ;" DFH-RUN-TEXT
   s" habu-forth" DFHT-ASSERT-REJECT
   LR-DIAG-COUNT @ 0 > TTRUE
   LR-ROW$ s" habu_repair_packet" CONTAINS? TTRUE
   LR-ROW$ s" remove_producer" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DFHT-TEST-FORBIDDEN-BLIND ( -- )
   s" blind" s" habu-forth-blind" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) trust dup * ;" DFH-RUN-TEXT
   s" habu-forth-blind" DFHT-ASSERT-REJECT
   LR-TRUST-USES @ 1 T=
   LR-ROW$ s" forbidden unchecked boundary" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DFHT-MAIN ( -- )
   T-RESET
   DFHT-TEST-PASS
   DFHT-TEST-FAIL-RAW
   DFHT-TEST-CHECKER-REJECT
   DFHT-TEST-FORBIDDEN-BLIND
   T-REPORT
   s" drive-forth-test: ok" type cr ;

DFHT-MAIN
