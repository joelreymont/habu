\ drive-stdlib-test.f - focused tests for native stdlib stack driver.

: DST-MODELS$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" ;

: DST-CONFIG-DATE ( -- )
   DST-MODELS$ MR-REGISTRY!
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

: DST-SRC-RESET ( -- )
   SB-RESET ;

: DST-SRC+ ( ptr u8 n -- )
   SB-APPEND ;

: DST-SRC-C ( n -- )
   SB-APPEND-C ;

: DST-SRC-DQ ( -- )
   JW-DQ DST-SRC-C ;

: DST-SRC-SP ( -- )
   JW-SP DST-SRC-C ;

: DST-SRC-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" DST-SRC+
   DST-SRC-DQ
   DST-SRC-SP
   a u DST-SRC+
   DST-SRC-DQ ;

: DST-SRC-TASK-HEAD ( -- )
   s" : " DST-SRC+
   DS-NAME$ DST-SRC+
   s"  ( " DST-SRC+
   DS-SIG$ DST-SRC+
   s"  ) " DST-SRC+ ;

: DST-SRC-END ( -- ptr u8 n )
   s" ;" DST-SRC+
   SB$ ;

: DST-GOOD$ ( -- ptr u8 n )
   DST-SRC-RESET
   DST-SRC-TASK-HEAD
   s" 2026-06-16" DST-SRC-S"
   s"  PARSE-YMD swap 20620 = and " DST-SRC+
   DST-SRC-END ;

: DST-CONSTANT$ ( -- ptr u8 n )
   DST-SRC-RESET
   DST-SRC-TASK-HEAD
   s" -1 " DST-SRC+
   DST-SRC-END ;

: DST-ROW-HAS ( ptr u8 n -- )
   LR-ROW$ 2swap CONTAINS? TTRUE ;

: DST-ROW-HAS-JSON ( -- )
   JW$ DST-ROW-HAS ;

: DST-ROW-HAS-KEY ( ptr u8 n -- )
   JW-RESET
   JW-KEY
   DST-ROW-HAS-JSON ;

: DST-ROW-HAS-S ( ptr u8 n ptr u8 n -- )
   JW-RESET
   JW-FIELD-S
   DST-ROW-HAS-JSON ;

: DST-ASSERT-PASS ( -- )
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   s" arm" s" habu-stdlib" DST-ROW-HAS-S
   s" prompt_sha256" DST-ROW-HAS-KEY
   s" final_bundle_sha256" DST-ROW-HAS-KEY ;

: DST-ASSERT-REJECT ( -- )
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   s" required stdlib word missing" DST-ROW-HAS ;

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
