\ assert.f - checked stdlib test assertions.

1 constant T-EX-FAIL

variable T-CASE#
variable T-FAIL#
variable T-EXPECTED#

256 constant T-LABEL-CAP
create T-LABEL-BUF T-LABEL-CAP allot
variable T-LABEL-U

: T-RESET ( -- )
   0 T-CASE# !
   0 T-FAIL# !
   0 T-LABEL-U ! ;

: T-CASES ( -- n )
   T-CASE# @ ;

: T-FAILURES ( -- n )
   T-FAIL# @ ;

: T-NEXT ( -- )
   T-CASE# @ 1+ T-CASE# ! ;

: T-LABEL-CLEAR ( -- )
   0 T-LABEL-U ! ;

: T-LABEL$ ( -- ptr u8 n )
   T-LABEL-BUF T-LABEL-U @ ;

: T-LABEL ( ptr u8 n -- ) {: a:ptr u:n :}
   u T-LABEL-CAP > if s" test: label too long" T-EX-FAIL die then
   0 begin dup u < while
      dup a + c@  over T-LABEL-BUF + c!
      1+
   repeat drop
   u T-LABEL-U ! ;

: T-LABEL. ( -- )
   T-LABEL-U @ 0 > if s" case: " type T-LABEL$ type cr then ;

: T-FAIL+ ( -- )
   T-FAIL# @ 1+ T-FAIL# ! ;

: T-FAIL ( -- )
   [char] F emit T-CASE# @ .
   T-LABEL.
   T-FAIL+ ;

: T-ASSERT-DETAIL ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   T-FAIL
   s" assert: " type msg msgu type cr ;

: T-ASSERT ( bool -- )
   T-NEXT
   0= if s" expected true got false" T-ASSERT-DETAIL then
   T-LABEL-CLEAR ;

: T= ( n n -- ) {: got:n want:n :}
   T-NEXT
   got want <> if
      T-FAIL
      s" assert: expected " type want .
      s" got " type got .
   then
   T-LABEL-CLEAR ;

: T<> ( n n -- ) {: got:n want:n :}
   T-NEXT
   got want = if
      T-FAIL
      s" assert: expected not " type want .
      s" got " type got .
   then
   T-LABEL-CLEAR ;

: TTRUE ( bool -- )
   T-ASSERT ;

: TFALSE ( bool -- )
   T-NEXT
   if s" expected false got true" T-ASSERT-DETAIL then
   T-LABEL-CLEAR ;

: T-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: T$= ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   T-NEXT
   a u b v T-STR= 0= if
      T-FAIL
      s" assert: expected string:" type cr
      b v type cr
      s" got string:" type cr
      a u type cr
   then
   T-LABEL-CLEAR ;

: T$<> ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   T-NEXT
   a u b v T-STR= if
      T-FAIL
      s" assert: expected different string:" type cr
      a u type cr
   then
   T-LABEL-CLEAR ;

: T-REPORT ( -- )
   T-FAIL# @ 0= if s" test: ok" type cr exit then
   T-FAIL# @ . s" test: failures" type cr
   s" test: failures" T-EX-FAIL die ;

: TTHROWSQ ( [ -- ] n -- )
   T-EXPECTED# !
   catch T-EXPECTED# @ = T-ASSERT ;

\ Trusted boundary: top-level tests cannot push `[: ;]` quotations.
TRUSTED: TTHROWS-RAW ( a n -- )
   T-EXPECTED# !
   catch T-EXPECTED# @ = T-ASSERT ;

: TTHROWS ( a n -- )
   TTHROWS-RAW ;
