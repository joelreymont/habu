\ drive-python-test.f - focused tests for native Python benchmark driver.

: DPT-CONFIG ( ptr u8 n ptr u8 n -- ) {: conv:ptr convu vectors:ptr vectorsu :}
   DFG-PY!
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   6 DS-TASK-ORDER !
   2 DS-K !
   2 DS-MAX-REPAIRS !
   46 DS-ID !
   s" PY-ARRAY" DS-NAME!
   s" def f(a)" DS-SIG!
   s" Implement the requested array function." DS-SPEC!
   conv convu DFG-CONV!
   vectors vectorsu DS-TESTS!
   5000 DFG-TIMEOUT-U ! ;

: DPT-AS ( -- )
   s" as" s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" DPT-CONFIG ;

: DPT-AA ( -- )
   s" aa" s" [1 2 3] -> [3 2 1]; [7] -> [7]" DPT-CONFIG ;

: DPT-PASS-AS ( -- )
   DPT-AS
   s" def f(a): return sum(a)" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   LR-ROW$ s" python" CONTAINS? TTRUE
   LR-ROW$ s" runtime_status" CONTAINS? TTRUE
   CLEANUP-RUN ;

: DPT-PASS-AA ( -- )
   DPT-AA
   s" def f(a): return list(reversed(a))" DFG-RUN-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-TESTS-PASSED @ -1 T=
   LR-RUNTIME-MS @ 0 >= TTRUE
   CLEANUP-RUN ;

: DPT-FAIL ( -- )
   DPT-AS
   s" def f(a): return 42" DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DPT-SYNTAX-FAIL ( -- )
   DPT-AS
   s" def f(a):" DFG-RUN-TEXT
   LR-OUTCOME$ s" fail" T$=
   CLEANUP-RUN ;

: DPT-TIMEOUT-SRC$ ( -- ptr u8 n )
   DS-MSG-RESET
   s" def f(a):" DS-MSG-LN
   s"     while True:" DS-MSG-LN
   s"         pass" DS-MSG-LN
   DS-MSG$ ;

: DPT-TIMEOUT ( -- )
   DPT-AS
   100 DFG-TIMEOUT-U !
   DPT-TIMEOUT-SRC$ DFG-RUN-TEXT
   LR-OUTCOME$ s" timeout" T$=
   LR-TESTS-PASSED @ 0 T=
   CLEANUP-RUN ;

: DPT-REPAIR-PASS ( -- )
   DPT-AS
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   DFG-NEXT-ROUND
   s" def f(a): return 42" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" fail" T$=
   DFG-ADD-FEEDBACK
   DFG-NEXT-ROUND
   s" def f(a): return sum(a)" DFG-EVALUATE-TEXT
   LR-OUTCOME$ s" pass" T$=
   LR-ROUNDS @ 2 T=
   LR-REPAIR-ITERATIONS @ 1 T=
   LR-FIRST-PASS @ 0 T=
   LR-TESTS-PASSED @ -1 T=
   CLEANUP-RUN ;

: DPT-MAIN ( -- )
   T-RESET
   DPT-PASS-AS
   DPT-PASS-AA
   DPT-FAIL
   DPT-SYNTAX-FAIL
   DPT-TIMEOUT
   DPT-REPAIR-PASS
   T-REPORT
   s" drive-python-test: ok" type cr ;

DPT-MAIN
