\ drive-rust-test.f - focused tests for native Rust benchmark driver.

: DRT-CONFIG ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu vectors:ptr vectorsu :}
   DFG-RUST!
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   7 DS-TASK-ORDER !
   2 DS-K !
   2 DS-MAX-REPAIRS !
   46 DS-ID !
   s" RUST-ARRAY" DS-NAME!
   s" fn f(a: &[i64])" DS-SIG!
   s" Implement the requested array function." DS-SPEC!
   conv convu DFG-CONV!
   vectors vectorsu DS-TESTS!
   5000 DFG-TIMEOUT-U !
   5000 DFG-COMPILE-TIMEOUT-U ! ;

: DRT-AS ( -- )
   s" as" s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DRT-CONFIG ;

: DRT-AA ( -- )
   s" aa" s" [1 2 3] -> [3 2 1]; [7] -> [7]" DRT-CONFIG ;

: DRT-PASS-AS ( -- )
   DRT-AS
   s" fn f(a: &[i64]) -> i64 { a.iter().sum() }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   LR-ROW$ s" rust" CONTAINS? TTRUE
   LR-ROW$ s" runtime_status" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DRT-PASS-AA ( -- )
   DRT-AA
   s" fn f(a: &[i64]) -> Vec<i64> { let mut v = a.to_vec(); v.reverse(); v }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   CLEANUP-RUN ;

: DRT-FAIL ( -- )
   DRT-AS
   s" fn f(a: &[i64]) -> i64 { 42 }" DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DRT-COMPILE-FAIL ( -- )
   DRT-AS
   s" fn f(a: &[i64]) -> i64 {" DFG-RUN-TEXT
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-DIAG-COUNT @ 1 T=
   CLEANUP-RUN ;

: DRT-TIMEOUT ( -- )
   DRT-AS
   100 DFG-TIMEOUT-U !
   5000 DFG-COMPILE-TIMEOUT-U !
   s" fn f(a: &[i64]) -> i64 { loop {} }" DFG-RUN-TEXT
   LR-OUTCOME$ s" timeout" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DRT-REPAIR-PASS ( -- )
   DRT-AS
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   DFG-NEXT-ROUND
   s" fn f(a: &[i64]) -> i64 {" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" reject" T$=
   DFG-ADD-FEEDBACK
   DFG-NEXT-ROUND
   s" fn f(a: &[i64]) -> i64 { a.iter().sum() }" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-ROUNDS @ 2 T=
   LR-REPAIR-ITERATIONS @ 1 T=
   LR-FIRST-PASS @ 0 T=
   LR-TESTS-PASSED @ -1 T=
   CLEANUP-RUN ;

: DRT-MAIN ( -- )
   T-RESET
   DRT-PASS-AS
   DRT-PASS-AA
   DRT-FAIL
   DRT-COMPILE-FAIL
   DRT-TIMEOUT
   DRT-REPAIR-PASS
   T-REPORT
   s" drive-rust-test: ok" type cr ;

DRT-MAIN
