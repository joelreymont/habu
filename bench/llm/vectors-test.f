\ vectors-test.f - focused tests for bench/llm/vectors.f.

: BV-AS-VECTORS$ ( -- ptr u8 n )
   s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5" ;

: BV-AA-VECTORS$ ( -- ptr u8 n )
   s" [1 2 3] -> [3 2 1]; [7] -> [7]" ;

: BV-STACK-VECTORS$ ( -- ptr u8 n )
   s" 1 2 -> 3; empty -> 0; 1 2 -> 2 1" ;

: BV-BAD-ARROW$ ( -- ptr u8 n )
   s" [1 2] => 3" ;

: BV-BAD-ARRAY$ ( -- ptr u8 n )
   s" [1 nope] -> 3" ;

: BV-BAD-SCALAR$ ( -- ptr u8 n )
   s" [1 2] -> nope" ;

: BV-BAD-MISMATCH$ ( -- ptr u8 n )
   s" [1 2] -> [1]" ;

: BV-EXPECT-BAD-ARROW ( -- )
   s" as" s" ARR-SUM" BV-BAD-ARROW$ BV-HABU-TESTS 2drop ;

: BV-EXPECT-BAD-ARRAY ( -- )
   s" as" s" ARR-SUM" BV-BAD-ARRAY$ BV-HABU-TESTS 2drop ;

: BV-EXPECT-BAD-SCALAR ( -- )
   s" as" s" ARR-SUM" BV-BAD-SCALAR$ BV-HABU-TESTS 2drop ;

: BV-EXPECT-BAD-MISMATCH ( -- )
   s" aa" s" REV" BV-BAD-MISMATCH$ BV-HABU-TESTS 2drop ;

: BV-EXPECT-BAD-CONV ( -- )
   s" bad" s" REV" BV-AA-VECTORS$ BV-HABU-TESTS 2drop ;

: BV-VECTORS-TEST-MAIN ( -- )
   T-RESET
   BV-AS-VECTORS$ BV-CASE-COUNT 3 T=
   BV-AA-VECTORS$ BV-CASE-COUNT 2 T=
   s" as" s" ARR-SUM" BV-AS-VECTORS$ BV-HABU-TESTS
   s" here 3 , 1 , 4 , AP !  AP @ 3 ARR-SUM 8 G=
here 5 , AP !  AP @ 1 ARR-SUM 5 G=
here -2 , -3 , AP !  AP @ 2 ARR-SUM -5 G=
" T$=
   s" aa" s" REV" BV-AA-VECTORS$ BV-HABU-TESTS
   s" here 1 , 2 , 3 , AP !  AP @ 3 REV AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 2 G= AP @ 2 cells + @ 1 G=
here 7 , AP !  AP @ 1 REV AP @ 0 cells + @ 7 G=
" T$=
   s" stack" s" PLUS" BV-STACK-VECTORS$ BV-HABU-TESTS
   s" T{ 1 2 PLUS -> 3 }T
T{  PLUS -> 0 }T
T{ 1 2 PLUS -> 2 1 }T
" T$=
   ['] BV-EXPECT-BAD-ARROW E-BM-SCHEMA TTHROWS
   ['] BV-EXPECT-BAD-ARRAY E-BM-FIELD TTHROWS
   ['] BV-EXPECT-BAD-SCALAR E-BM-FIELD TTHROWS
   ['] BV-EXPECT-BAD-MISMATCH E-BM-SCHEMA TTHROWS
   ['] BV-EXPECT-BAD-CONV E-BM-FIELD TTHROWS
   T-REPORT
   s" vectors-test: ok" type cr ;

BV-VECTORS-TEST-MAIN
