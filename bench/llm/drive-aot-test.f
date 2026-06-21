\ drive-aot-test.f - focused tests for native stripped-AOT driver.

T-RESET

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- i64" DS-SIG!
s" aot-safe" DS-CATEGORY!
s" stdout 42" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
67 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
67 DS-ID !
s" AOT-MAIN-ARITH" DS-NAME!
s" Build a stripped-AOT checked MAIN that computes 6 7 * and prints 42." DS-SPEC!
s" : MAIN ( -- ) 6 7 * . ;" DA-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-aot" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- i64" DS-SIG!
s" aot-safe" DS-CATEGORY!
s" stdout 50" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
68 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
68 DS-ID !
s" AOT-MAIN-STRING" DS-NAME!
s" Build a stripped-AOT checked MAIN that uses a string literal length to print 50." DS-SPEC!
DTH-SRC-RESET
s" : MAIN ( -- ) " DTH-SRC+
s" xx" DTH-SRC-S"
s"  nip 48 + . ;" DTH-SRC+
SB$ DA-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-aot" DTH-ROW-NEED-S
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- error" DS-SIG!
s" aot-unsupported" DS-CATEGORY!
s" E-AOT-UNSUPPORTED token here" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
69 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
69 DS-ID !
s" AOT-UNSAFE-HERE" DS-NAME!
s" Reject a stripped-AOT checked MAIN that uses here." DS-SPEC!
s" : MAIN ( -- ) here drop ;" DA-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" E-AOT-UNSUPPORTED" DTH-ROW-HAS
s" here" DTH-ROW-HAS
s" aot_unsupported" DTH-ROW-HAS
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- error" DS-SIG!
s" aot-unsupported" DS-CATEGORY!
s" E-AOT-UNSUPPORTED token allot" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
70 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
70 DS-ID !
s" AOT-UNSAFE-ALLOT" DS-NAME!
s" Reject a stripped-AOT checked MAIN that uses allot." DS-SPEC!
s" : MAIN ( -- ) 1 allot ;" DA-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" E-AOT-UNSUPPORTED" DTH-ROW-HAS
s" allot" DTH-ROW-HAS
s" aot_unsupported" DTH-ROW-HAS
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- error" DS-SIG!
s" aot-unsupported" DS-CATEGORY!
s" E-AOT-UNSUPPORTED token here" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
69 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
69 DS-ID !
s" AOT-UNSAFE-HERE" DS-NAME!
s" Reject a stripped-AOT checked MAIN that uses here." DS-SPEC!
s" : MAIN ( -- ) 42 . ;" DA-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" required AOT token missing" DTH-ROW-HAS
s" aot_rejection" DTH-ROW-HAS
CLEANUP-RUN

T-REPORT
s" drive-aot-test: ok" type cr
