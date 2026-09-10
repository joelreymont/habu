\ Existing nominal pointers instantiate checked generic inputs without minting identities.
require lib/test.f
require test/checker-assert.f

package NOMINAL-POINTER-TEST

NEWTYPE identity 0
NEWTYPE other 0
CAST: >IDENTITY ( n -- identity )
CAST: IDENTITY>N ( identity -- n )
4 LAYOUT-BUFFER SLOT identity
create RAW-CELL 1 cells allot

: SECOND@ ( ptr identity -- identity ) cell+ @ ;
: INDEX@ ( ptr identity n -- identity ) {: base index:n :}
   base index cells + @ ;
: REVERSE@ ( n ptr identity -- identity ) + @ ;
: PREVIOUS@ ( ptr identity -- identity ) 1 cells - @ ;
: DISTANCE ( ptr identity ptr identity -- n ) - ;
: BYTE-ROUNDTRIP ( ptr identity -- identity ) char+ 1- @ ;
: UNIT-ROUNDTRIP ( ptr identity -- identity ) 1+ 1- @ ;

: PASS ( ptr a -- ptr a ) ;
: VIEW ( ptr a -- ptr u8 ) BYTE-VIEW ;
: GENERIC@ ( ptr a -- a ) @ ;
: GENERIC! ( a ptr a -- ) ! ;
: PASSED@ ( ptr identity -- identity ) PASS GENERIC@ ;
: VIEWED-BYTE ( ptr identity -- u8 ) VIEW c@ ;
: STRUCT-BYTE ( ptr identity -- u8 ) 0 STRUCT-BYTE+ c@ ;

: REJECT ( ptr u8 n -- )
   2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   17 >IDENTITY 0 SLOT !
   29 >IDENTITY 1 SLOT !
   43 >IDENTITY 2 SLOT !
   0 SLOT SECOND@ IDENTITY>N 29 T=
   0 SLOT 2 INDEX@ IDENTITY>N 43 T=
   2 cells 0 SLOT REVERSE@ IDENTITY>N 43 T=
   2 SLOT PREVIOUS@ IDENTITY>N 29 T=
   2 SLOT 0 SLOT DISTANCE 2 cells T=
   0 SLOT BYTE-ROUNDTRIP IDENTITY>N 17 T=
   0 SLOT UNIT-ROUNDTRIP IDENTITY>N 17 T=
   0 SLOT PASSED@ IDENTITY>N 17 T=
   0 SLOT VIEWED-BYTE 17 T=
   0 SLOT STRUCT-BYTE 17 T=
   71 >IDENTITY 3 SLOT GENERIC!
   3 SLOT GENERIC@ IDENTITY>N 71 T=
   s" NP-GOOD-STEP ( ptr identity -- identity ) cell+ @"
   CHECK-QUIET-CANDIDATE! -1 T=
   s" NP-WRONG-STEP ( ptr identity -- ptr other ) cell+" REJECT
   s" NP-WRONG-INDEX ( ptr identity n -- other ) cells + @" REJECT
   s" NP-WRONG-DIFF ( ptr identity ptr other -- n ) -" REJECT
   s" NP-WRONG-DIFF-REV ( ptr other ptr identity -- n ) -" REJECT
   s" NP-PTR-PLUS-PTR ( ptr identity ptr identity -- ptr identity ) +" REJECT
   s" NP-N-MINUS-PTR ( n ptr identity -- ptr identity ) -" REJECT
   s" NP-RAW-STEP ( ptr a -- ptr identity ) cell+" REJECT
   s" NP-RAW-DIFF ( ptr a ptr identity -- n ) -" REJECT
   s" NP-RAW-DIFF-REV ( ptr identity ptr a -- n ) -" REJECT
   s" NP-RAW-ADDRESS ( -- ptr identity ) RAW-CELL" REJECT
   s" NP-RAW-FETCH ( -- identity ) RAW-CELL GENERIC@" REJECT
   s" NP-RAW-STORE ( identity -- ) RAW-CELL GENERIC!" REJECT
   s" NP-IMPLICIT-IDENTITY ( ptr a -- ptr identity ) PASS" REJECT
   s" NP-BYTE-CELL ( ptr u8 -- n ) cell+ @" REJECT
   T-REPORT ;

RUN
;package
