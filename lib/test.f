\ test.f - checked stdlib test assertions.

1 constant T-EX-FAIL

variable T-CASE#
variable T-FAIL#
variable T-EXPECTED#

: T-RESET ( -- )
   0 T-CASE# !
   0 T-FAIL# ! ;

: T-CASES ( -- n )
   T-CASE# @ ;

: T-FAILURES ( -- n )
   T-FAIL# @ ;

: T-NEXT ( -- )
   T-CASE# @ 1+ T-CASE# ! ;

: T-FAIL ( -- )
   [char] F emit T-CASE# @ .
   T-FAIL# @ 1+ T-FAIL# ! ;

: T-ASSERT ( bool -- )
   T-NEXT
   0= IF T-FAIL THEN ;

: T= ( n n -- ) {: got want :}
   got want = T-ASSERT ;

: T<> ( n n -- ) {: got want :}
   got want <> T-ASSERT ;

: TTRUE ( bool -- )
   T-ASSERT ;

: TFALSE ( bool -- )
   0= T-ASSERT ;

: T-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: T$= ( ptr u8 n ptr u8 n -- )
   T-STR= T-ASSERT ;

: T$<> ( ptr u8 n ptr u8 n -- )
   T-STR= 0= T-ASSERT ;

: T-REPORT ( -- )
   T-FAIL# @ 0= IF s" test: ok" type cr exit THEN
   T-FAIL# @ . s" test: failures" type cr
   s" test: failures" T-EX-FAIL die ;

\ Trusted boundary: catch is a runtime primitive with no checker model.
TRUSTED: TTHROWS-RAW ( a n -- )
   T-EXPECTED# !
   catch T-EXPECTED# @ = T-ASSERT ;

: TTHROWS ( a n -- )
   TTHROWS-RAW ;
