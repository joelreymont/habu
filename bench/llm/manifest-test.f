\ manifest-test.f - focused tests for bench/llm/manifest.f.

: BM-EXPECT-BAD-TASK-HEADER ( -- )
   s" bad	header" BM-REQUIRE-TASK-HEADER ;

: BM-EXPECT-BAD-FIELD ( -- )
   s" 1	ONLY-TWO" BM-T-NAME BM-TASK-FIELD$ 2drop ;

: BM-TASK-ROW$ ( -- ptr u8 n )
   s" 62	DATE-PARSE-OK?	(-- bool)	date	empty -> -1	stdlib	stack	Using PARSE-YMD, parse 2026-06-16 and verify it is Unix epoch day 20620.	-	date,time,stdlib,v2,parse-ymd	-	-" ;

: BM-MODEL-ROW$ ( -- ptr u8 n )
   s" fixture	Fixture Model	/tmp/model.sh	{prompt}	raw		5" ;

: BM-MANIFEST-TEST-MAIN ( -- )
   T-RESET
   BM-TASK-HEADER$ BM-TASK-HEADER? TTRUE
   BM-MODEL-HEADER$ BM-MODEL-HEADER? TTRUE
   ['] BM-EXPECT-BAD-TASK-HEADER E-BM-SCHEMA TTHROWS
   ['] BM-EXPECT-BAD-FIELD E-BM-SCHEMA TTHROWS
   BM-TASK-ROW$ BM-FIELD-COUNT BM-TASK-FIELDS T=
   BM-TASK-ROW$ BM-T-ID BM-TASK-FIELD$ s" 62" T$=
   BM-TASK-ROW$ BM-T-NAME BM-TASK-FIELD$ s" DATE-PARSE-OK?" T$=
   BM-TASK-ROW$ BM-T-HARNESS BM-TASK-FIELD$ s" stdlib" T$=
   BM-TASK-ROW$ BM-T-CONV BM-TASK-FIELD$ s" stack" T$=
   BM-TASK-ROW$ BM-TASK-SIG$ s" -- bool" T$=
   BM-TASK-ROW$ s" 1,62,99" BM-TASK-SELECTED? TTRUE
   BM-TASK-ROW$ s" 1,99" BM-TASK-SELECTED? TFALSE
   BM-TASK-ROW$ s" " BM-TASK-SELECTED? TTRUE
   s" " BM-BLANK-OR-COMMENT? TTRUE
   s"   # comment" BM-BLANK-OR-COMMENT? TTRUE
   BM-MODEL-ROW$ BM-FIELD-COUNT BM-MODEL-FIELDS T=
   BM-MODEL-ROW$ BM-M-ID BM-MODEL-FIELD$ s" fixture" T$=
   BM-MODEL-ROW$ BM-M-TOKEN-FIELDS BM-MODEL-FIELD$ s" " T$=
   T-REPORT
   s" manifest-test: ok" type cr ;

BM-MANIFEST-TEST-MAIN
