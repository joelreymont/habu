\ drive-array-habu-test.f - focused tests for native Habu array driver.

T-RESET

s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" MR-REGISTRY!
s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
46 DS-ID ! s" ARR-SUM" DS-NAME! s" ptr a n -- i64" DS-SIG! s" Sum the array." DS-SPEC!
s" as" DAH-CONV! s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" lib" DAH-ARM!
s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM ;" DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
LR-ROW$ s" habu-lib" CONTAINS? TTRUE
LR-ROW$ s" prompt_sha256" CONTAINS? TTRUE
LR-ROW$ s" final_bundle_sha256" CONTAINS? TTRUE
LR-ROW$ s" runtime_repetitions" CONTAINS? TTRUE
LR-ROW$ s" runtime_warmups" CONTAINS? TTRUE
LR-ROW$ s" ok" CONTAINS? TTRUE
CLEANUP-RUN

s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
51 DS-ID ! s" REVERSE" DS-NAME! s" ptr a n --" DS-SIG! s" Reverse the array in place." DS-SPEC!
s" aa" DAH-CONV! s" [1 2 3] -> [3 2 1]; [7] -> [7]" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" stdlib" DAH-ARM!
s" : REVERSE ( ptr a n -- ) >LEN A-REVERSE! ;" DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
LR-ROW$ s" habu-stdlib" CONTAINS? TTRUE
LR-ROW$ s" prompt_sha256" CONTAINS? TTRUE
LR-ROW$ s" final_bundle_sha256" CONTAINS? TTRUE
LR-ROW$ s" runtime_repetitions" CONTAINS? TTRUE
LR-ROW$ s" runtime_warmups" CONTAINS? TTRUE
LR-ROW$ s" ok" CONTAINS? TTRUE
CLEANUP-RUN

s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
46 DS-ID ! s" ARR-SUM" DS-NAME! s" ptr a n -- i64" DS-SIG! s" Sum the array." DS-SPEC!
s" as" DAH-CONV! s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" skeleton" DAH-ARM!
s" 0 len 0 ?do arr i cells + @ + loop" DAH-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
LR-RUNTIME-MS @ 0 >= TTRUE
LR-ROW$ s" habu-skeleton" CONTAINS? TTRUE
LR-ROW$ s" prompt_sha256" CONTAINS? TTRUE
LR-ROW$ s" final_bundle_sha256" CONTAINS? TTRUE
LR-ROW$ s" runtime_repetitions" CONTAINS? TTRUE
LR-ROW$ s" runtime_warmups" CONTAINS? TTRUE
LR-ROW$ s" ok" CONTAINS? TTRUE
CLEANUP-RUN

s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
46 DS-ID ! s" ARR-SUM" DS-NAME! s" ptr a n -- i64" DS-SIG! s" Sum the array." DS-SPEC!
s" as" DAH-CONV! s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" lib" DAH-ARM!
s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM 1 + ;" DAH-RUN-TEXT
LR-OUTCOME$ s" fail" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ 0 T=
LR-ROW$ s" habu-lib" CONTAINS? TTRUE
CLEANUP-RUN

s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
46 DS-ID ! s" ARR-SUM" DS-NAME! s" ptr a n -- i64" DS-SIG! s" Sum the array." DS-SPEC!
s" as" DAH-CONV! s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" lib" DAH-ARM!
s" : ARR-SUM ( ptr a n -- i64 ) 42 ;" DAH-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
LR-ROW$ s" required array helper missing" CONTAINS? TTRUE
CLEANUP-RUN

s" fixture" MR-REQUIRE s" arrays" DS-CATEGORY!
s" test-seed" DS-SEED! 1 DS-TRIAL ! 8 DS-TASK-ORDER ! 2 DS-K ! 1 DS-MAX-REPAIRS !
46 DS-ID ! s" ARR-SUM" DS-NAME! s" ptr a n -- i64" DS-SIG! s" Sum the array." DS-SPEC!
s" as" DAH-CONV! s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DAH-VECTORS! DAH-VECTORS$ DS-TESTS!
s" lib" DAH-ARM!
s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM dup ;" DAH-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
LR-ROW$ s" habu_repair_packet" CONTAINS? TTRUE
LR-ROW$ s" remove_producer" CONTAINS? TTRUE
CLEANUP-RUN

T-REPORT
s" drive-array-habu-test: ok" type cr
