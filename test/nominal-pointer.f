split-str
\ Existing nominal pointers instantiate checked generic inputs without minting identities.
require src/lib/test.f

package NOMINAL-POINTER-TEST

NEWTYPE identity
NEWTYPE other
CAST: >IDENTITY ( n -- identity )
CAST: IDENTITY>N ( identity -- n )
4 LAYOUT-BUFFER SLOT identity
create RAW-CELL 1 cells allot

: SECOND@ ( ^identity -- identity ) cell+ @ ;
: INDEX@ ( ^identity n -- identity ) {: base index:n :}
   base index cells + @ ;
: REVERSE@ ( n ^identity -- identity ) + @ ;
: PREVIOUS@ ( ^identity -- identity ) 1 cells - @ ;
: DISTANCE ( ^identity ^identity -- n ) - ;
: BYTE-ROUNDTRIP ( ^identity -- identity ) char+ 1- @ ;
: UNIT-ROUNDTRIP ( ^identity -- identity ) 1+ 1- @ ;

: PASS ( ^a -- ^a ) ;
: GENERIC@ ( ^a -- a ) @ ;
: GENERIC! ( a ^a -- ) ! ;
: PASSED@ ( ^identity -- identity ) PASS GENERIC@ ;
: VIEWED-BYTE ( ^identity -- u8 ) BYTE-VIEW c@ ;
: STRUCT-BYTE ( ^identity -- u8 ) 0 STRUCT-BYTE+ c@ ;

: REJECT ( ^u8 n -- )
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
   s" NP-GOOD-STEP ( ^identity -- identity ) cell+ @"
   CHECK-QUIET-CANDIDATE! -1 T=
   s" NP-WRONG-STEP ( ^identity -- ^other ) cell+" REJECT
   s" NP-WRONG-INDEX ( ^identity n -- other ) cells + @" REJECT
   s" NP-WRONG-DIFF ( ^identity ^other -- n ) -" REJECT
   s" NP-WRONG-DIFF-REV ( ^other ^identity -- n ) -" REJECT
   s" NP-PTR-PLUS-PTR ( ^identity ^identity -- ^identity ) +" REJECT
   s" NP-N-MINUS-PTR ( n ^identity -- ^identity ) -" REJECT
   s" NP-RAW-STEP ( ^a -- ^identity ) cell+" REJECT
   s" NP-RAW-DIFF ( ^a ^identity -- n ) -" REJECT
   s" NP-RAW-DIFF-REV ( ^identity ^a -- n ) -" REJECT
   s" NP-RAW-ADDRESS ( -- ^identity ) RAW-CELL" REJECT
   s" NP-RAW-FETCH ( -- identity ) RAW-CELL GENERIC@" REJECT
   s" NP-RAW-STORE ( identity -- ) RAW-CELL GENERIC!" REJECT
   s" NP-IMPLICIT-IDENTITY ( ^a -- ^identity ) PASS" REJECT
   s" NP-BYTE-CELL ( ^u8 -- n ) cell+ @" REJECT
   T-REPORT ;

RUN
;package
