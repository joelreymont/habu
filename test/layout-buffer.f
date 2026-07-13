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

3 LAYOUT-BUFFER LB-BUF lb-res<n,n>

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
s" LB-DATA-P ( -- ptr lb-res<n,n> ) data-base" CHECK-QUIET-CANDIDATE! 0 T=
s" LBUF-PEND!" 0 search-wl 0= -1 T=
s" LBUF-PEND-CLEAR" 0 search-wl 0= -1 T=
s" LBUF-PEND-MATCH?" 0 search-wl 0= -1 T=
s" LBUF-PEND-A" 0 search-wl 0= -1 T=
s" LBUF-PEND-U" 0 search-wl 0= -1 T=
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

s" 0 LAYOUT-BUFFER LB-ZERO lb-res<n,n>" LB-EVAL E-LAYOUT-BUFFER T=
s" -1 LAYOUT-BUFFER LB-NEG lb-res<n,n>" LB-EVAL E-LAYOUT-BUFFER T=
s" $7FFFFFFFFFFFFFFF LAYOUT-BUFFER LB-EXTENT lb-res<n,n>" LB-EVAL E-LAYOUT-BUFFER T=
s" 1 LAYOUT-BUFFER LB-OPEN lb-res<a,a>" LB-EVAL E-LAYOUT-BUFFER T=
s" 1 LAYOUT-BUFFER LB-LINEAR lb-owned" LB-EVAL E-LAYOUT-BUFFER T=

PTR-VARIABLE LB-DP

: LB-DUPLICATE ( -- )
   s" 1 LAYOUT-BUFFER LB-BUF lb-res<n,n>" INCLUDE-EVALUATE ;

: LB-DUP-RC ( -- n )
   [: LB-DUPLICATE ;] catch ;

here LB-DP 0 ptr-field !
LB-DUP-RC $4E T=
here LB-DP 0 ptr-field @ = -1 T=

here LB-DP 0 ptr-field !
s" 1 LAYOUT-BUFFER A:B:C lb-res<n,n>" LB-EVAL E-LAYOUT-BUFFER T=

here LB-DP 0 ptr-field !
s" 1 LAYOUT-BUFFER TFAM:BAD lb-res<n,n>" LB-EVAL E-LAYOUT-BUFFER T=

\ ---- nominal scalars (arity-0 TK-CELL): LAYOUT-BUFFER is the introduction ----
TYPEFAMILY lbtk 0
TYPEFAMILY lbtk2 0

4 LAYOUT-BUFFER LBTK-AT lbtk

TRUSTED: N>LBTK ( n -- lbtk ) ;
TRUSTED: LBTK>N ( lbtk -- n ) ;

: LBTK-GET ( n -- n )  LBTK-AT @ LBTK>N ;
: LBTK-PUT ( n n -- ) {: v:n i:n :}  v N>LBTK i LBTK-AT ! ;
: LBTK-LOW ( -- )  -1 LBTK-AT drop ;
: LBTK-HIGH ( -- )  4 LBTK-AT drop ;
: LBTK-LOW-RC ( -- n )  [: LBTK-LOW ;] catch ;
: LBTK-HIGH-RC ( -- n )  [: LBTK-HIGH ;] catch ;

\ zero image reads as family id 0 (every raw cell is a valid nominal id)
0 LBTK-GET 0 T=
3 LBTK-GET 0 T=
\ typed round-trip through the generated accessor
7 0 LBTK-PUT  9 3 LBTK-PUT
0 LBTK-GET 7 T=
3 LBTK-GET 9 T=
\ bounds stay owned by the generated accessor
LBTK-LOW-RC E-LAYOUT-BOUNDS T=
LBTK-HIGH-RC E-LAYOUT-BOUNDS T=

\ checker seam: typed round-trip certifies; raw and foreign-family reject
s" LBTK-STORE ( lbtk ptr lbtk -- ) !" CHECK-QUIET-CANDIDATE! -1 T=
s" LBTK-FETCH ( ptr lbtk -- lbtk ) @" CHECK-QUIET-CANDIDATE! -1 T=
s" LBTK-RAW-STORE ( n ptr lbtk -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-RAW-FETCH ( ptr lbtk -- n ) @" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-WRONG-STORE ( lbtk2 ptr lbtk -- ) !" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-WRONG-FETCH ( ptr lbtk -- lbtk2 ) @" CHECK-QUIET-CANDIDATE! 0 T=
\ pointee governance: no raw pointer acquires nominal identity (P1/P2 rejects)
variable LBTK-RAW-CELL
s" LBTK-VAR-P ( -- ptr lbtk ) LBTK-RAW-CELL" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-VAR-P2 ( -- ptr lbtk2 ) LBTK-RAW-CELL" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-DATA-P ( -- ptr lbtk ) data-base" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-CAST ( ptr a -- ptr lbtk )" CHECK-QUIET-CANDIDATE! 0 T=
s" LBTK-CELL+ ( ptr lbtk -- ptr lbtk ) cell+" CHECK-QUIET-CANDIDATE! 0 T=
\ value laundering through the typed cell rejects (typed-storage ST2 pin)
s" LBTK-LAUNDER ( n -- lbtk ) 0 LBTK-AT ! 0 LBTK-AT @" CHECK-QUIET-CANDIDATE! 0 T=

\ rejected nominal-scalar declarations roll back allocation and define nothing
\ (LAYOUT-BUFFER never touches the TFAM registry; dictionary + data space are
\ the mutable state, pinned here like the layout-family duplicate case above)
: LBTK-ZERO-DECL ( -- )  s" 0 LAYOUT-BUFFER LBTK-ZERO lbtk" INCLUDE-EVALUATE ;
: LBTK-ZERO-RC ( -- n )  [: LBTK-ZERO-DECL ;] catch ;
: LBTK-DUP-DECL ( -- )  s" 1 LAYOUT-BUFFER LBTK-AT lbtk" INCLUDE-EVALUATE ;
: LBTK-DUP-RC ( -- n )  [: LBTK-DUP-DECL ;] catch ;
: LBTK-ZERO-ABSENT? ( -- bool )  s" LBTK-ZERO" 0 search-wl 0= ;
here LB-DP 0 ptr-field !
LBTK-ZERO-RC E-LAYOUT-BUFFER T=
here LB-DP 0 ptr-field @ = -1 T=
LBTK-ZERO-ABSENT? -1 T=
here LB-DP 0 ptr-field !
LBTK-DUP-RC $4E T=
here LB-DP 0 ptr-field @ = -1 T=

\ ---- PRODUCT positive: MAKE -> typed store -> fetch -> UNMAKE round-trip ----
\ (mirrors the type-decl-suite TDS2/TDP executed round-trips; the product
\ coverage above is reject-only, this pins the accepting memory path.)
ENUM lbhue red green blue ;ENUM

PRODUCT lbrec 0
  FIELD hue lbhue
  FIELD cnt n
;PRODUCT

3 LAYOUT-BUFFER LBP-BUF lbrec

: LBP-HUE-CODE ( lbhue -- n )
   MATCH lbhue
     red OF 0 ENDOF
     green OF 1 ENDOF
     blue OF 2 ENDOF
   ;MATCH ;
: LBP-PUT ( lbrec n -- ) LBP-BUF ! ;
: LBP-HUE@ ( n -- n ) LBP-BUF @ LBREC:UNMAKE drop LBP-HUE-CODE ;
: LBP-CNT@ ( n -- n ) LBP-BUF @ LBREC:UNMAKE nip ;

\ single-slot round-trip: every field survives typed product memory.
: LBP-ONE! ( -- ) LBHUE:GREEN 42 LBREC:MAKE 0 LBP-PUT ;
LBP-ONE!
0 LBP-HUE@ 1 T=
0 LBP-CNT@ 42 T=

\ multi-element positive: distinct values at three indices stay independent,
\ and a compiled walk over every slot asserts the stored values (the depth
\ suite pins no-throw only; this pins the values).
: LBP-FILL ( -- )
   LBHUE:RED 10 LBREC:MAKE 0 LBP-PUT
   LBHUE:GREEN 20 LBREC:MAKE 1 LBP-PUT
   LBHUE:BLUE 30 LBREC:MAKE 2 LBP-PUT ;
: LBP-WALK-SUM ( -- n )   \ fold hue-code*100 + cnt across all slots
   0 3 0 ?do
      i LBP-HUE@ 100 * i LBP-CNT@ + +
   loop ;
LBP-FILL
0 LBP-HUE@ 0 T=   0 LBP-CNT@ 10 T=
1 LBP-HUE@ 1 T=   1 LBP-CNT@ 20 T=
2 LBP-HUE@ 2 T=   2 LBP-CNT@ 30 T=
LBP-WALK-SUM 360 T=

: REPORT ( -- )
   #FAIL @ 0= if s" ok" type cr exit then
   #FAIL @ . s" layout-buffer failures" 1 die ;

REPORT
