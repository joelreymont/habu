\ array-test.f - focused tests for checked stdlib array helpers.
\ Run: bin/hb --load lib/array-test.f

require lib/errors.f
require lib/array.f
require lib/test.f
require test/checker-assert.f

create AT-EMPTY
create AT-ONE 42 ,
create AT-NORMAL 3 , -1 , 4 , 4 , 2 ,
create AT-WORK 5 cells allot

: AT-WORK1 ( n -- )
   AT-WORK ! ;

: AT-WORK4 ( n n n n -- )
   AT-WORK 3 cells + !
   AT-WORK 2 cells + !
   AT-WORK cell+ !
   AT-WORK ! ;

: AT-WORK5 ( n n n n n -- )
   AT-WORK 4 cells + !
   AT-WORK 3 cells + !
   AT-WORK 2 cells + !
   AT-WORK cell+ !
   AT-WORK ! ;

: AT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: AT-CHECK-INDEX ( n n -- ) {: len ix :}
   len A-LEN ix A-IDX A-CHECK-INDEX ;

: AT-CHECK-RANGE ( n n n -- ) {: len start cnt :}
   len A-LEN start A-IDX cnt A-COUNT A-CHECK-RANGE ;

: AT-CHECK-NONEMPTY ( n -- )
   A-LEN A-CHECK-NONEMPTY ;

: AT-CHECK-WHOLE ( n -- )
   A-LEN A-CHECK-WHOLE ;

: AT-A@ ( ptr a n n -- a ) {: arr:ptr len ix :}
   arr len A-LEN ix A-IDX A@ ;

: AT-A! ( a ptr a n n -- ) {: value arr:ptr len ix :}
   value arr len A-LEN ix A-IDX A! ;

: AT-A+! ( n ptr a n n -- ) {: delta arr:ptr len ix :}
   delta arr len A-LEN ix A-IDX A+! ;

: AT-A-SWAP ( ptr a n n n -- ) {: arr:ptr len ix jx :}
   arr len A-LEN ix A-IDX jx A-IDX A-SWAP ;

: AT-LAST-INDEX ( n -- n )
   A-LEN LAST-INDEX IDX>N ;

: AT-MIRROR-INDEX ( n n -- n ) {: len ix :}
   len A-LEN ix A-IDX MIRROR-INDEX IDX>N ;

: AT-A-SUM ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-SUM ;

: AT-A-MIN ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-MIN ;

: AT-A-MAX ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-MAX ;

: AT-A-COUNT-EVEN ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-COUNT-EVEN COUNT>N ;

: AT-A-ARGMAX ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-ARGMAX IDX>N ;

: AT-A-MAX-INDEX ( ptr n n -- n ) {: arr:ptr len :}
   arr len A-LEN A-MAX-INDEX IDX>N ;

: AT-A-REVERSE-RANGE! ( ptr a n n n -- ) {: arr:ptr len start cnt :}
   arr len A-LEN start A-IDX cnt A-COUNT A-REVERSE-RANGE! ;

: AT-A-REVERSE! ( ptr a n -- ) {: arr:ptr len :}
   arr len A-LEN A-REVERSE! ;

: AT-A-PREFIX-SUM! ( ptr n n -- ) {: arr:ptr len :}
   arr len A-LEN A-PREFIX-SUM! ;

: AT-A-RUNMAX! ( ptr n n -- ) {: arr:ptr len :}
   arr len A-LEN A-RUNMAX! ;

: AT-A-FILL! ( a ptr a n -- ) {: value arr:ptr len :}
   value arr len A-LEN A-FILL! ;

: AT-A-MAP! ( ptr a n [ a -- a ] -- ) {: arr:ptr len q :}
   arr len A-LEN q A-MAP! ;

: AT-A-MAPI! ( ptr a n [ idx a -- a ] -- ) {: arr:ptr len q :}
   arr len A-LEN q A-MAPI! ;

: AT-A-FOLD ( ptr a n b [ b a -- b ] -- b ) {: arr:ptr len acc q :}
   arr len A-LEN acc q A-FOLD ;

: AT-A-FOLDI ( ptr a n b [ b idx a -- b ] -- b ) {: arr:ptr len acc q :}
   arr len A-LEN acc q A-FOLDI ;

: AT-A-SCAN! ( ptr n n n [ n n -- n ] -- ) {: arr:ptr len acc q :}
   arr len A-LEN acc q A-SCAN! ;

: AT-A-SCAN1! ( ptr n n [ n n -- n ] -- ) {: arr:ptr len q :}
   arr len A-LEN q A-SCAN1! ;

: AT-A-FIND-INDEX ( ptr a n [ a -- bool ] -- n ) {: arr:ptr len q :}
   arr len A-LEN q A-FIND-INDEX MATCH option none OF -1 ENDOF some OF IDX>N ENDOF ;MATCH ;

: AT-A-FIND-INDEXI ( ptr a n [ idx a -- bool ] -- n ) {: arr:ptr len q :}
   arr len A-LEN q A-FIND-INDEXI MATCH option none OF -1 ENDOF some OF IDX>N ENDOF ;MATCH ;

\ Direct option<idx> assertions for the migrated finders (found -> some, absent -> none).
\ typed-local-lint: allow-bare-local - q keeps the predicate quotation effect from the stack signature.
: AT-A-FIND-SOME ( ptr a n [ a -- bool ] n -- ) {: arr:ptr len q want:n :}
   arr len A-LEN q A-FIND-INDEX MATCH option
     none OF 0 0= 0= ENDOF                          \ none -> false (unexpected)
     some OF IDX>N want = ENDOF                       \ some(idx) -> idx == want
   ;MATCH T-ASSERT ;
\ typed-local-lint: allow-bare-local - q keeps the predicate quotation effect from the stack signature.
: AT-A-FIND-NONE ( ptr a n [ a -- bool ] -- ) {: arr:ptr len q :}
   arr len A-LEN q A-FIND-INDEX MATCH option
     none OF 0 0= ENDOF                             \ none -> true
     some OF drop 0 0= 0= ENDOF                       \ some -> false (unexpected)
   ;MATCH T-ASSERT ;

: AT-WORK5@ ( -- n n n n n )
   AT-WORK 5 0 AT-A@
   AT-WORK 5 1 AT-A@
   AT-WORK 5 2 AT-A@
   AT-WORK 5 3 AT-A@
   AT-WORK 5 4 AT-A@ ;

: AT-CHECK-INDEX-VALID ( -- )
   5 0 AT-CHECK-INDEX
   5 4 AT-CHECK-INDEX ;

: AT-CHECK-INDEX-NEG ( -- )
   5 -1 AT-CHECK-INDEX ;

: AT-CHECK-INDEX-HIGH ( -- )
   5 5 AT-CHECK-INDEX ;

: AT-CHECK-INDEX-EMPTY ( -- )
   0 0 AT-CHECK-INDEX ;

: AT-CHECK-RANGE-VALID ( -- )
   5 0 0 AT-CHECK-RANGE
   5 5 0 AT-CHECK-RANGE
   5 1 3 AT-CHECK-RANGE ;

: AT-CHECK-RANGE-NEG-START ( -- )
   5 -1 1 AT-CHECK-RANGE ;

: AT-CHECK-RANGE-NEG-COUNT ( -- )
   5 1 -1 AT-CHECK-RANGE ;

: AT-CHECK-RANGE-HIGH-START ( -- )
   5 6 0 AT-CHECK-RANGE ;

: AT-CHECK-RANGE-OVERRUN ( -- )
   5 3 3 AT-CHECK-RANGE ;

: AT-CHECK-NONEMPTY-VALID ( -- )
   1 AT-CHECK-NONEMPTY ;

: AT-CHECK-NONEMPTY-EMPTY ( -- )
   0 AT-CHECK-NONEMPTY ;

: AT-CHECK-NONEMPTY-NEG ( -- )
   -1 AT-CHECK-NONEMPTY ;

: AT-CHECK-WHOLE-VALID ( -- )
   0 AT-CHECK-WHOLE
   5 AT-CHECK-WHOLE ;

: AT-CHECK-WHOLE-NEG ( -- )
   -1 AT-CHECK-WHOLE ;

: AT-LEN-NEG ( -- )
   -1 A-LEN drop ;

: AT-IDX-NEG ( -- )
   -1 A-IDX drop ;

: AT-COUNT-NEG ( -- )
   -1 A-COUNT drop ;

: AT-AFETCH-HIGH ( -- )
   AT-NORMAL 5 5 AT-A@ drop ;

: AT-ASTORE-HIGH ( -- )
   9 AT-WORK 5 5 AT-A! ;

: AT-APLUS-HIGH ( -- )
   1 AT-WORK 5 5 AT-A+! ;

: AT-ASWAP-HIGH ( -- )
   AT-WORK 5 0 5 AT-A-SWAP ;

: AT-ASWAP-NEG ( -- )
   AT-WORK 5 -1 0 AT-A-SWAP ;

: AT-LAST-EMPTY ( -- )
   0 AT-LAST-INDEX drop ;

: AT-MIRROR-HIGH ( -- )
   5 5 AT-MIRROR-INDEX drop ;

: AT-MIN-EMPTY ( -- )
   AT-EMPTY 0 AT-A-MIN drop ;

: AT-MAX-EMPTY ( -- )
   AT-EMPTY 0 AT-A-MAX drop ;

: AT-ARGMAX-EMPTY ( -- )
   AT-EMPTY 0 AT-A-ARGMAX drop ;

: AT-AMAP-NEG ( -- )
   AT-WORK -1 [: 1+ ;] AT-A-MAP! ;

: AT-AMAPI-NEG ( -- )
   AT-WORK -1 [: swap IDX>N + ;] AT-A-MAPI! ;

: AT-AFOLD-NEG ( -- )
   AT-WORK -1 0 [: + ;] AT-A-FOLD drop ;

: AT-AFOLDI-NEG ( -- )
   AT-WORK -1 0 [: swap IDX>N + + ;] AT-A-FOLDI drop ;

: AT-ASCAN-NEG ( -- )
   AT-WORK -1 0 [: + ;] AT-A-SCAN! ;

: AT-ASCAN1-NEG ( -- )
   AT-WORK -1 [: + ;] AT-A-SCAN1! ;

: AT-AREVERSE-RANGE-OVERRUN ( -- )
   AT-WORK 5 3 3 AT-A-REVERSE-RANGE! ;

: AT-AFIND-NEG ( -- )
   AT-WORK -1 [: 0= ;] AT-A-FIND-INDEX drop ;

: AT-AFINDI-NEG ( -- )
   AT-WORK -1 [: drop IDX>N 0= ;] AT-A-FIND-INDEXI drop ;

: AT-AMAX-INDEX-EMPTY ( -- )
   AT-EMPTY 0 AT-A-MAX-INDEX drop ;

: AT-TEST-CHECKS ( -- )
   [: AT-CHECK-INDEX-VALID ;] catch 0 T=
   [: AT-CHECK-INDEX-NEG ;] catch E-A-BOUNDS T=
   [: AT-CHECK-INDEX-HIGH ;] catch E-A-BOUNDS T=
   [: AT-CHECK-INDEX-EMPTY ;] catch E-A-BOUNDS T=
   [: AT-CHECK-RANGE-VALID ;] catch 0 T=
   [: AT-CHECK-RANGE-NEG-START ;] catch E-A-BOUNDS T=
   [: AT-CHECK-RANGE-NEG-COUNT ;] catch E-A-BOUNDS T=
   [: AT-CHECK-RANGE-HIGH-START ;] catch E-A-BOUNDS T=
   [: AT-CHECK-RANGE-OVERRUN ;] catch E-A-BOUNDS T=
   [: AT-CHECK-NONEMPTY-VALID ;] catch 0 T=
   [: AT-CHECK-NONEMPTY-EMPTY ;] catch E-A-EMPTY T=
   [: AT-CHECK-NONEMPTY-NEG ;] catch E-A-BOUNDS T=
   [: AT-CHECK-WHOLE-VALID ;] catch 0 T=
   [: AT-CHECK-WHOLE-NEG ;] catch E-A-BOUNDS T=
   [: AT-LEN-NEG ;] catch E-A-BOUNDS T=
   [: AT-IDX-NEG ;] catch E-A-BOUNDS T=
   [: AT-COUNT-NEG ;] catch E-A-BOUNDS T=
   s" BAD-A-FETCH ( ptr a len count -- a ) A@" AT-CHECK-REJECTS
   s" BAD-A-RANGE ( len idx len -- ) A-CHECK-RANGE" AT-CHECK-REJECTS ;

: AT-TEST-ACCESS ( -- )
   AT-ONE 1 0 AT-A@ 42 T=
   AT-NORMAL 5 0 AT-A@ 3 T=
   AT-NORMAL 5 2 AT-A@ 4 T=
   [: AT-AFETCH-HIGH ;] catch E-A-BOUNDS T=
   0 AT-WORK1
   99 AT-WORK 1 0 AT-A!
   AT-WORK 1 0 AT-A@ 99 T=
   [: AT-ASTORE-HIGH ;] catch E-A-BOUNDS T= ;

: AT-TEST-PROMOTED-HELPERS ( -- )
   1 2 3 4 AT-WORK4
   5 AT-WORK 4 1 AT-A+!
   AT-WORK 4 1 AT-A@ 7 T=
   [: AT-APLUS-HIGH ;] catch E-A-BOUNDS T=
   AT-WORK 4 0 3 AT-A-SWAP
   AT-WORK 4 0 AT-A@ 4 T=
   AT-WORK 4 3 AT-A@ 1 T=
   [: AT-ASWAP-HIGH ;] catch E-A-BOUNDS T=
   [: AT-ASWAP-NEG ;] catch E-A-BOUNDS T=
   5 AT-LAST-INDEX 4 T=
   [: AT-LAST-EMPTY ;] catch E-A-EMPTY T=
   5 1 AT-MIRROR-INDEX 3 T=
   [: AT-MIRROR-HIGH ;] catch E-A-BOUNDS T=
   4 EVEN? TTRUE
   5 EVEN? TFALSE
   -2 EVEN? TTRUE
   -3 EVEN? TFALSE ;

: AT-TEST-SCALARS ( -- )
   AT-EMPTY 0 AT-A-SUM 0 T=
   AT-ONE 1 AT-A-SUM 42 T=
   AT-NORMAL 5 AT-A-SUM 12 T=
   [: AT-MIN-EMPTY ;] catch E-A-EMPTY T=
   AT-ONE 1 AT-A-MIN 42 T=
   AT-NORMAL 5 AT-A-MIN -1 T=
   [: AT-MAX-EMPTY ;] catch E-A-EMPTY T=
   AT-ONE 1 AT-A-MAX 42 T=
   AT-NORMAL 5 AT-A-MAX 4 T=
   AT-EMPTY 0 AT-A-COUNT-EVEN 0 T=
   AT-ONE 1 AT-A-COUNT-EVEN 1 T=
   AT-NORMAL 5 AT-A-COUNT-EVEN 3 T=
   [: AT-ARGMAX-EMPTY ;] catch E-A-EMPTY T=
   AT-ONE 1 AT-A-ARGMAX 0 T=
   AT-NORMAL 5 AT-A-ARGMAX 2 T= ;

: AT-TEST-REVERSE ( -- )
   88 AT-WORK1
   AT-WORK 0 AT-A-REVERSE!
   88 AT-WORK1
   AT-WORK 1 AT-A-REVERSE!
   AT-WORK 1 0 AT-A@ 88 T=
   1 2 3 4 AT-WORK4
   AT-WORK 4 AT-A-REVERSE!
   AT-WORK 4 0 AT-A@ 4 T=
   AT-WORK 4 1 AT-A@ 3 T=
   AT-WORK 4 2 AT-A@ 2 T=
   AT-WORK 4 3 AT-A@ 1 T= ;

: AT-TEST-PREFIX ( -- )
   77 AT-WORK1
   AT-WORK 0 AT-A-PREFIX-SUM!
   77 AT-WORK1
   AT-WORK 1 AT-A-PREFIX-SUM!
   AT-WORK 1 0 AT-A@ 77 T=
   3 -1 4 1 5 AT-WORK5
   AT-WORK 5 AT-A-PREFIX-SUM!
   AT-WORK 5 0 AT-A@ 3 T=
   AT-WORK 5 1 AT-A@ 2 T=
   AT-WORK 5 2 AT-A@ 6 T=
   AT-WORK 5 3 AT-A@ 7 T=
   AT-WORK 5 4 AT-A@ 12 T= ;

: AT-TEST-RUNMAX ( -- )
   66 AT-WORK1
   AT-WORK 0 AT-A-RUNMAX!
   66 AT-WORK1
   AT-WORK 1 AT-A-RUNMAX!
   AT-WORK 1 0 AT-A@ 66 T=
   2 7 1 9 3 AT-WORK5
   AT-WORK 5 AT-A-RUNMAX!
   AT-WORK 5 0 AT-A@ 2 T=
   AT-WORK 5 1 AT-A@ 7 T=
   AT-WORK 5 2 AT-A@ 7 T=
   AT-WORK 5 3 AT-A@ 9 T=
   AT-WORK 5 4 AT-A@ 9 T= ;

: AT-TEST-FILL ( -- )
   123 AT-WORK 0 AT-A-FILL!
   -5 AT-WORK 1 AT-A-FILL!
   AT-WORK 1 0 AT-A@ -5 T=
   -6 AT-WORK 5 AT-A-FILL!
   AT-WORK 5 0 AT-A@ -6 T=
   AT-WORK 5 1 AT-A@ -6 T=
   AT-WORK 5 2 AT-A@ -6 T=
   AT-WORK 5 3 AT-A@ -6 T=
   AT-WORK 5 4 AT-A@ -6 T= ;

: AT-TEST-MAP-COMBINATORS ( -- )
   33 AT-WORK1
   AT-WORK 0 [: 1+ ;] AT-A-MAP!
   AT-WORK 1 0 AT-A@ 33 T=
   7 AT-WORK1
   AT-WORK 1 [: 2 * ;] AT-A-MAP!
   AT-WORK 1 0 AT-A@ 14 T=
   1 -2 3 0 5 AT-WORK5
   AT-WORK 5 [: dup * ;] AT-A-MAP!
   [: AT-WORK5@ ;] [: 1 4 9 0 25 ;] SNAP=
   [: AT-AMAP-NEG ;] catch E-A-BOUNDS T=
   44 AT-WORK1
   AT-WORK 0 [: swap IDX>N + ;] AT-A-MAPI!
   AT-WORK 1 0 AT-A@ 44 T=
   7 AT-WORK1
   AT-WORK 1 [: swap IDX>N + ;] AT-A-MAPI!
   AT-WORK 1 0 AT-A@ 7 T=
   10 10 10 10 10 AT-WORK5
   AT-WORK 5 [: swap IDX>N + ;] AT-A-MAPI!
   [: AT-WORK5@ ;] [: 10 11 12 13 14 ;] SNAP=
   [: AT-AMAPI-NEG ;] catch E-A-BOUNDS T= ;

: AT-TEST-FOLD-COMBINATORS ( -- )
   AT-EMPTY 0 100 [: + ;] AT-A-FOLD 100 T=
   AT-ONE 1 0 [: + ;] AT-A-FOLD 42 T=
   AT-NORMAL 5 0 [: + ;] AT-A-FOLD 12 T=
   [: AT-AFOLD-NEG ;] catch E-A-BOUNDS T=
   AT-EMPTY 0 100 [: swap IDX>N + + ;] AT-A-FOLDI 100 T=
   AT-ONE 1 0 [: swap IDX>N + + ;] AT-A-FOLDI 42 T=
   AT-NORMAL 5 0 [: swap IDX>N * + ;] AT-A-FOLDI 27 T=
   [: AT-AFOLDI-NEG ;] catch E-A-BOUNDS T= ;

: AT-TEST-SCAN-COMBINATORS ( -- )
   90 AT-WORK1
   AT-WORK 0 0 [: + ;] AT-A-SCAN!
   AT-WORK 1 0 AT-A@ 90 T=
   4 AT-WORK1
   AT-WORK 1 10 [: + ;] AT-A-SCAN!
   AT-WORK 1 0 AT-A@ 14 T=
   3 1 4 1 5 AT-WORK5
   AT-WORK 5 0 [: + ;] AT-A-SCAN!
   [: AT-WORK5@ ;] [: 3 4 8 9 14 ;] SNAP=
   [: AT-ASCAN-NEG ;] catch E-A-BOUNDS T=
   91 AT-WORK1
   AT-WORK 0 [: + ;] AT-A-SCAN1!
   AT-WORK 1 0 AT-A@ 91 T=
   4 AT-WORK1
   AT-WORK 1 [: + ;] AT-A-SCAN1!
   AT-WORK 1 0 AT-A@ 4 T=
   3 1 4 1 5 AT-WORK5
   AT-WORK 5 [: + ;] AT-A-SCAN1!
   [: AT-WORK5@ ;] [: 3 4 8 9 14 ;] SNAP=
   [: AT-ASCAN1-NEG ;] catch E-A-BOUNDS T= ;

: AT-TEST-INDEX-COMBINATORS ( -- )
   1 2 3 4 5 AT-WORK5
   AT-WORK 5 0 0 AT-A-REVERSE-RANGE!
   [: AT-WORK 5 0 AT-A@ AT-WORK 5 4 AT-A@ ;] [: 1 5 ;] SNAP=
   AT-WORK 5 2 1 AT-A-REVERSE-RANGE!
   [: AT-WORK5@ ;] [: 1 2 3 4 5 ;] SNAP=
   AT-WORK 5 1 3 AT-A-REVERSE-RANGE!
   [: AT-WORK5@ ;] [: 1 4 3 2 5 ;] SNAP=
   [: AT-AREVERSE-RANGE-OVERRUN ;] catch E-A-BOUNDS T=
   AT-EMPTY 0 [: 4 = ;] AT-A-FIND-INDEX -1 T=
   AT-ONE 1 [: 42 = ;] AT-A-FIND-INDEX 0 T=
   AT-NORMAL 5 [: 4 = ;] AT-A-FIND-INDEX 2 T=
   AT-NORMAL 5 [: 9 = ;] AT-A-FIND-INDEX -1 T=
   [: AT-AFIND-NEG ;] catch E-A-BOUNDS T=
   AT-EMPTY 0 [: drop IDX>N 0 = ;] AT-A-FIND-INDEXI -1 T=
   AT-ONE 1 [: drop IDX>N 0 = ;] AT-A-FIND-INDEXI 0 T=
   AT-NORMAL 5 [: drop IDX>N 3 = ;] AT-A-FIND-INDEXI 3 T=
   [: AT-AFINDI-NEG ;] catch E-A-BOUNDS T=
   \ migrated option<idx> API: found -> SOME(idx), absent -> NONE
   AT-NORMAL 5 [: 4 = ;] 2 AT-A-FIND-SOME
   AT-ONE 1 [: 42 = ;] 0 AT-A-FIND-SOME
   AT-NORMAL 5 [: 9 = ;] AT-A-FIND-NONE
   AT-EMPTY 0 [: 4 = ;] AT-A-FIND-NONE
   [: AT-AMAX-INDEX-EMPTY ;] catch E-A-EMPTY T=
   AT-ONE 1 AT-A-MAX-INDEX 0 T=
   AT-NORMAL 5 AT-A-MAX-INDEX 2 T=
   1 5 5 2 AT-WORK4
   AT-WORK 4 AT-A-MAX-INDEX 1 T= ;

: AT-MAIN ( -- )
   AT-TEST-CHECKS
   AT-TEST-ACCESS
   AT-TEST-PROMOTED-HELPERS
   AT-TEST-SCALARS
   AT-TEST-REVERSE
   AT-TEST-PREFIX
   AT-TEST-RUNMAX
   AT-TEST-FILL
   AT-TEST-MAP-COMBINATORS
   AT-TEST-FOLD-COMBINATORS
   AT-TEST-SCAN-COMBINATORS
   AT-TEST-INDEX-COMBINATORS
   T-REPORT ;

T-RESET
AT-MAIN
