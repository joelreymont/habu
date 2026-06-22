\ drive-ts-test.f - focused tests for native TypeScript benchmark driver.

: DTT-CONFIG ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu vectors:ptr vectorsu :}
   DFG-TS!
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   5 DS-TASK-ORDER !
   2 DS-K !
   2 DS-MAX-REPAIRS !
   46 DS-ID !
   s" TS-ARRAY" DS-NAME!
   s" function f(a: number[])" DS-SIG!
   s" Implement the requested array function." DS-SPEC!
   conv convu DFG-CONV!
   vectors vectorsu DS-TESTS!
   5000 DFG-TIMEOUT-U ! ;

: DTT-AS ( -- )
   s" as" s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DTT-CONFIG ;

: DTT-AA ( -- )
   s" aa" s" [1 2 3] -> [3 2 1]; [7] -> [7]" DTT-CONFIG ;

: DTT-PASS-AS ( -- )
   DTT-AS
   s" function f(a: number[]): number { return a.reduce((s,x)=>s+x,0); }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   LR-ROW$ s" ts" CONTAINS? TTRUE
   LR-ROW$ s" runtime_status" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DTT-PASS-AA ( -- )
   DTT-AA
   s" function f(a: number[]): number[] { return a.slice().reverse(); }" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   CLEANUP-RUN ;

: DTT-FAIL ( -- )
   DTT-AS
   s" function f(a: number[]): number { return 42; }" DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DTT-COMPILE-FAIL ( -- )
   DTT-AS
   s" function f(a: number[]): number { " DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   CLEANUP-RUN ;

: DTT-TIMEOUT ( -- )
   DTT-AS
   100 DFG-TIMEOUT-U !
   s" function f(a: number[]): number { while(true){} }" DFG-RUN-TEXT
   LR-OUTCOME$ s" timeout" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DTT-REPAIR-PASS ( -- )
   DTT-AS
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   DFG-NEXT-ROUND
   s" function f(a: number[]): number { return 42; }" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" fail" T$=
   DFG-ADD-FEEDBACK
   DFG-NEXT-ROUND
   s" function f(a: number[]): number { return a.reduce((s,x)=>s+x,0); }" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-ROUNDS @ 2 T=
   LR-REPAIR-ITERATIONS @ 1 T=
   LR-FIRST-PASS @ 0 T=
   LR-TESTS-PASSED @ -1 T=
   CLEANUP-RUN ;

: DTT-MAIN ( -- )
   T-RESET
   DTT-PASS-AS
   DTT-PASS-AA
   DTT-FAIL
   DTT-COMPILE-FAIL
   DTT-TIMEOUT
   DTT-REPAIR-PASS
   T-REPORT
   s" drive-ts-test: ok" type cr ;

DTT-MAIN
