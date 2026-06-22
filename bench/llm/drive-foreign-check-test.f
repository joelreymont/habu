\ drive-foreign-check-test.f - source-list fixture for foreign driver core.
\
\ Load after bench/llm/drive-foreign-lib.f.

: DFC-MODELS$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" ;

: DFC-CONFIG ( -- )
   DFG-JS!
   DFC-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" check-seed" DS-SEED!
   1 DS-TRIAL !
   1 DS-TASK-ORDER !
   1 DS-K !
   1 DS-MAX-REPAIRS !
   46 DS-ID !
   s" JS-ARRAY" DS-NAME!
   s" function f(a)" DS-SIG!
   s" Implement the requested array function." DS-SPEC!
   s" as" DFG-CONV!
   s" [3 1 4] -> 8; [5] -> 5" DS-TESTS!
   5000 DFG-TIMEOUT-U ! ;

: DFC-SMOKE ( -- )
   DFC-CONFIG
   s" function f(a){ return a.reduce((s,x)=>s+x,0); }" DFG-RUN-TEXT
   LR-ROW$ 2drop
   CLEANUP-RUN ;
