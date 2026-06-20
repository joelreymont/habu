\ drive-stdlib-test.f - focused tests for native stdlib stack driver.

: DST-CONFIG-DATE ( -- )
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   62 DS-ID !
   s" DATE-PARSE-OK?" DS-NAME!
   s" -- bool" DS-SIG!
   s" date" DS-CATEGORY!
   s" empty -> -1" DS-TESTS!
   s" Using PARSE-YMD, parse 2026-06-16 and verify it is Unix epoch day 20620." DS-SPEC!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   7 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS ! ;

: DST-GOOD$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" 2026-06-16" DTH-SRC-S"
   s"  PARSE-YMD swap 20620 = and " DTH-SRC+
   DTH-SRC-END ;

: DST-CONSTANT$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" -1 " DTH-SRC+
   DTH-SRC-END ;

: DST-ASSERT-PASS ( -- )
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   s" arm" s" habu-stdlib" DTH-ROW-NEED-S
   s" prompt_sha256" DTH-ROW-NEED-KEY
   s" final_bundle_sha256" DTH-ROW-NEED-KEY ;

: DST-ASSERT-REJECT ( -- )
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   s" required stdlib word missing" DTH-ROW-HAS ;

: DST-TEST-PASS ( -- )
   DST-CONFIG-DATE
   DST-GOOD$ DS-RUN-TEXT
   DST-ASSERT-PASS
   CLEANUP-RUN ;

: DST-TEST-CONSTANT-REJECT ( -- )
   DST-CONFIG-DATE
   DST-CONSTANT$ DS-RUN-TEXT
   DST-ASSERT-REJECT
   CLEANUP-RUN ;

: DST-MAIN ( -- )
   T-RESET
   DST-TEST-PASS
   DST-TEST-CONSTANT-REJECT
   T-REPORT
   s" drive-stdlib-test: ok" type cr ;

DST-MAIN
