\ layout-buffer.f - generative typed ADT storage and provenance regressions.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" expected " type want . s" got " type got . cr
   then ;

SUMTYPE lb-res 2
   VARIANT ok a ;VARIANT
   VARIANT err b ;VARIANT
;SUMTYPE

SUMTYPE lb-other 1
   VARIANT some a ;VARIANT
;SUMTYPE

DEFLINEAR lb-linear
SUMTYPE lb-owned 0
   VARIANT hold lb-linear ;VARIANT
;SUMTYPE

LAYOUT-BUFFER LB-BUF lb-res<n,n> 3

TRUSTED: LB-UN ( lb-res<n,n> -- n n ) ;

: LB-GET ( n -- n n )
   LB-BUF @ LB-UN ;

: LB-PUT ( n n -- ) {: value:n index:n :}
   value LB--RES:OK index LB-BUF ! ;

: LB-LOW ( -- )
   -1 LB-BUF drop ;

: LB-HIGH ( -- )
   3 LB-BUF drop ;

: LB-LOW-RC ( -- n )
   [: LB-LOW ;] catch ;

: LB-HIGH-RC ( -- n )
   [: LB-HIGH ;] catch ;

variable LB-RAW
create LB-RAW-WIDE 2 cells allot

s" LB-CAST ( ptr a -- ptr lb-res<n,n> )" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-VAR-P ( -- ptr lb-res<n,n> ) LB-RAW" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-CREATE-P ( -- ptr lb-res<n,n> ) LB-RAW-WIDE" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-CELL+ ( ptr lb-res<n,n> -- ptr lb-res<n,n> ) cell+" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-BYTE ( ptr lb-res<n,n> -- ptr u8 )" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-WRONG-STORE ( lb-other<n> ptr lb-res<n,n> -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" LB-WRONG-FETCH ( ptr lb-res<n,n> -- lb-other<n> ) @" CHECK-QUIET-CANDIDATE! 0 T=

0 LB-GET 0 T= 0 T=
7 0 LB-PUT  8 1 LB-PUT  9 2 LB-PUT
0 LB-GET 0 T= 7 T=
1 LB-GET 0 T= 8 T=
2 LB-GET 0 T= 9 T=

LB-LOW-RC E-LAYOUT-BOUNDS T=
LB-HIGH-RC E-LAYOUT-BOUNDS T=

variable LB-EVAL-A
variable LB-EVAL-U

: LB-EVAL-RUN ( -- )
   LB-EVAL-A @ LB-EVAL-U @ INCLUDE-EVALUATE ;

: LB-EVAL ( ptr u8 n -- n )
   LB-EVAL-U ! LB-EVAL-A !
   [: LB-EVAL-RUN ;] catch ;

s" LAYOUT-BUFFER LB-ZERO lb-res<n,n> 0" LB-EVAL E-LAYOUT-BUFFER T=
s" LAYOUT-BUFFER LB-NEG lb-res<n,n> -1" LB-EVAL E-LAYOUT-BUFFER T=
s" LAYOUT-BUFFER LB-OVER lb-res<n,n> 9223372036854775808" LB-EVAL E-LAYOUT-BUFFER T=
s" LAYOUT-BUFFER LB-OPEN lb-res<a,a> 1" LB-EVAL E-LAYOUT-BUFFER T=
s" LAYOUT-BUFFER LB-LINEAR lb-owned 1" LB-EVAL E-LAYOUT-BUFFER T=

PTR-VARIABLE LB-DP

: LB-DUPLICATE ( -- )
   s" LAYOUT-BUFFER LB-BUF lb-res<n,n> 1" INCLUDE-EVALUATE ;

: LB-DUP-RC ( -- n )
   [: LB-DUPLICATE ;] catch ;

here LB-DP 0 ptr-field !
LB-DUP-RC $4E T=
here LB-DP 0 ptr-field @ = -1 T=

: REPORT ( -- )
   #FAIL @ 0= if s" ok" type cr exit then
   #FAIL @ . s" layout-buffer failures" 1 die ;

REPORT
