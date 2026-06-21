\ drive-forth-test.f - focused tests for native Habu Forth driver.

1100 constant DFHT-LARGE-ROWS

variable DFHT-HERE
variable DFHT-SRC-P
variable DFHT-SRC-CAP-U
variable DFHT-SRC-U

TRUSTED: DFHT-SRC-BUF ( -- ptr u8 )
   DFHT-SRC-P @ ;

: DFHT-SRC-CAP ( -- n )
   DFHT-SRC-CAP-U @ ;

: DFHT-SRC$ ( -- ptr u8 n )
   DFHT-SRC-BUF DFHT-SRC-U @ ;

: DFHT-STORE-SRC-SPAN ( ptr u8 n -- )
   DFHT-SRC-CAP-U ! DFHT-SRC-P ! ;

: DFHT-SRC-ROOM ( n -- ) {: add :}
   add 0 < if E-DS-CAPACITY throw then
   add DFHT-SRC-CAP DFHT-SRC-U @ - > if E-DS-CAPACITY throw then ;

: DFHT-SRC+ ( ptr u8 n -- ) {: a:ptr u :}
   u DFHT-SRC-ROOM
   a DFHT-SRC-BUF DFHT-SRC-U @ + u BYTE-COPY
   DFHT-SRC-U @ u + DFHT-SRC-U ! ;

: DFHT-SRC-LN ( ptr u8 n -- )
   DFHT-SRC+
   1 DFHT-SRC-ROOM
   10 DFHT-SRC-BUF DFHT-SRC-U @ + c!
   DFHT-SRC-U @ 1+ DFHT-SRC-U ! ;

: DFHT-TASK-HEADER$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature" ;

: DFHT-FORTH-ROW$ ( -- ptr u8 n )
   s" 1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-" ;

: DFHT-BUILD-LARGE-TASKS ( -- )
   0 DFHT-SRC-U !
   DFHT-TASK-HEADER$ DFHT-SRC-LN
   DFHT-LARGE-ROWS 0 ?do DFHT-FORTH-ROW$ DFHT-SRC-LN loop ;

: DFHT-TEST-LARGE-TASK-COPY ( -- )
   here data-base - DFHT-HERE !
   2 MEM-ALLOC-64K-BUFFERS DFHT-STORE-SRC-SPAN
   DFHT-BUILD-LARGE-TASKS
   DFHT-SRC$ DFH-COPY-TASKS
   DFH-TASKS$ nip MEM-64K > TTRUE
   here data-base - DFHT-HERE @ T= ;

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

: DFHT-ASSERT-REPAIRED-PASS ( ptr u8 n -- ) {: arm:ptr armu :}
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-FIRST-PASS @ 0 T=
   LR-FIRST-TESTS @ 0 T=
   LR-TESTS-PASSED @ -1 T=
   LR-ROUNDS @ 2 T=
   LR-REPAIR-ITERATIONS @ 1 T=
   LR-CHECKER-ITERATIONS @ 2 T=
   LR-DIAG-COUNT @ 1 T=
   LR-ROW$ s" habu_repair_packet" CONTAINS? TTRUE
   arm armu DFHT-ASSERT-ROW-COMMON ;

: DFHT-PROMPT-HAS ( ptr u8 n -- )
   DS-PROMPT$ 2swap CONTAINS? TTRUE ;

: DFHT-PROMPT-NOT ( ptr u8 n -- )
   DS-PROMPT$ 2swap CONTAINS? 0= TTRUE ;

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

: DFHT-TEST-REPAIR-FEEDBACK ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * dup ;" DFH-RUN-TEXT
   DFH-ADD-FEEDBACK
   s" Use this repair packet" DFHT-PROMPT-HAS
   s" Raw checker diagnostics" DFHT-PROMPT-NOT
   CLEANUP-RUN ;

: DFHT-TEST-RAW-FEEDBACK ( -- )
   s" raw" s" habu-forth-raw" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * dup ;" DFH-RUN-TEXT
   DFH-ADD-FEEDBACK
   s" Raw checker diagnostics" DFHT-PROMPT-HAS
   s" Use this repair packet" DFHT-PROMPT-NOT
   CLEANUP-RUN ;

: DFHT-TEST-BLIND-FEEDBACK ( -- )
   s" blind" s" habu-forth-blind" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * dup ;" DFH-RUN-TEXT
   DFH-ADD-FEEDBACK
   s" Your attempt did not certify" DFHT-PROMPT-HAS
   s" Use this repair packet" DFHT-PROMPT-NOT
   s" Raw checker diagnostics" DFHT-PROMPT-NOT
   CLEANUP-RUN ;

: DFHT-TEST-FORBIDDEN-BLIND ( -- )
   s" blind" s" habu-forth-blind" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) trust dup * ;" DFH-RUN-TEXT
   s" habu-forth-blind" DFHT-ASSERT-REJECT
   LR-TRUST-USES @ 1 T=
   LR-ROW$ s" forbidden unchecked boundary" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DFHT-WRITE-RAW ( ptr u8 n -- )
   DS-RAW-PATH$ 2swap WRITE-ALL ;

: DFHT-RUN-TWO-TEXTS ( ptr u8 n ptr u8 n -- )
   {: bad:ptr badu good:ptr goodu :}
   DFH-PREPARE
   DFH-STATE-RESET
   DFH-NEXT-ROUND
   bad badu DFHT-WRITE-RAW
   bad badu DFH-EVALUATE-TEXT
   DFH-ADD-FEEDBACK
   DFH-NEXT-ROUND
   good goodu DFHT-WRITE-RAW
   good goodu DFH-EVALUATE-TEXT ;

: DFHT-TEST-REPAIRED-PASS ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   s" : SQUARE ( i64 -- i64 ) dup * dup ;"
   s" : SQUARE ( i64 -- i64 ) dup * ;"
   DFHT-RUN-TWO-TEXTS
   s" habu-forth" DFHT-ASSERT-REPAIRED-PASS
   CLEANUP-RUN ;

: DFHT-NONZERO$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" boom" DTH-SRC-S"
   s"  7 die " DTH-SRC+
   DTH-SRC-END ;

: DFHT-TEST-NONZERO-CHILD ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   DFHT-NONZERO$ DFH-RUN-TEXT
   s" habu-forth" DFHT-ASSERT-FAIL
   CLEANUP-RUN ;

: DFHT-TEST-TIMEOUT-CHILD ( -- )
   s" repair" s" habu-forth" DFHT-CONFIG-SQUARE
   1000 DS-HB-TIMEOUT-U !
   s" : SQUARE ( i64 -- i64 ) begin again ;" DFH-RUN-TEXT
   LR-OUTCOME$ s" timeout" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ 0 T=
   LR-ROUNDS @ 1 T=
   s" habu-forth" DFHT-ASSERT-ROW-COMMON
   CLEANUP-RUN ;

: DFHT-MAIN ( -- )
   T-RESET
   DFHT-TEST-LARGE-TASK-COPY
   DFHT-TEST-PASS
   DFHT-TEST-FAIL-RAW
   DFHT-TEST-CHECKER-REJECT
   DFHT-TEST-REPAIR-FEEDBACK
   DFHT-TEST-RAW-FEEDBACK
   DFHT-TEST-BLIND-FEEDBACK
   DFHT-TEST-FORBIDDEN-BLIND
   DFHT-TEST-REPAIRED-PASS
   DFHT-TEST-NONZERO-CHILD
   DFHT-TEST-TIMEOUT-CHILD
   T-REPORT
   s" drive-forth-test: ok" type cr ;

DFHT-MAIN
