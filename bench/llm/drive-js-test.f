\ drive-js-test.f - focused tests for native JavaScript benchmark driver.

: DJT-CONFIG ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu vectors:ptr vectorsu :}
   DFG-JS!
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   4 DS-TASK-ORDER !
   2 DS-K !
   2 DS-MAX-REPAIRS !
   46 DS-ID !
   s" JS-ARRAY" DS-NAME!
   s" function f(a)" DS-SIG!
   s" Implement the requested array function." DS-SPEC!
   conv convu DFG-CONV!
   vectors vectorsu DS-TESTS!
   5000 DFG-TIMEOUT-U ! ;

: DJT-AS ( -- )
   s" as" s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DJT-CONFIG ;

: DJT-AA ( -- )
   s" aa" s" [1 2 3] -> [3 2 1]; [7] -> [7]" DJT-CONFIG ;

: DJT-PASS-AS ( -- )
   DJT-AS
   s" function f(a){ return a.reduce((s,x)=>s+x,0); }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   LR-ROW$ s" js" CONTAINS? TTRUE
   LR-ROW$ s" runtime_status" CONTAINS? TTRUE
   LR-ROW$ s" prompt_sha256" CONTAINS? TTRUE
   LR-ROW$ s" final_bundle_sha256" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DJT-PASS-AA ( -- )
   DJT-AA
   s" function f(a){ return a.slice().reverse(); }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   CLEANUP-RUN ;

: DJT-FAIL ( -- )
   DJT-AS
   s" function f(a){ return 42; }" DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DJT-SYNTAX-FAIL ( -- )
   DJT-AS
   s" function f(a){ " DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   CLEANUP-RUN ;

: DJT-TIMEOUT ( -- )
   DJT-AS
   100 DFG-TIMEOUT-U !
   s" function f(a){ while(true){} }" DFG-RUN-TEXT
   LR-OUTCOME$ s" timeout" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DJT-REPAIR-PASS ( -- )
   DJT-AS
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   DFG-NEXT-ROUND
   s" function f(a){ return 42; }" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" fail" T$=
   DFG-ADD-FEEDBACK
   DFG-NEXT-ROUND
   s" function f(a){ return a.reduce((s,x)=>s+x,0); }" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-ROUNDS @ 2 T=
   LR-REPAIR-ITERATIONS @ 1 T=
   LR-FIRST-PASS @ 0 T=
   LR-TESTS-PASSED @ -1 T=
   CLEANUP-RUN ;

: DJT-MAIN ( -- )
   T-RESET
   DJT-PASS-AS
   DJT-PASS-AA
   DJT-FAIL
   DJT-SYNTAX-FAIL
   DJT-TIMEOUT
   DJT-REPAIR-PASS
   T-REPORT
   s" drive-js-test: ok" type cr ;

DJT-MAIN
