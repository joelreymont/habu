\ Nominal pointer arithmetic through the native load and execution path.
require lib/test.f
require test/checker-assert.f

package NOMINAL-POINTER-TEST

NEWTYPE identity 0
NEWTYPE other 0
CAST: >IDENTITY ( n -- identity )
CAST: IDENTITY>N ( identity -- n )
4 LAYOUT-BUFFER SLOT identity

: SECOND@ ( ptr identity -- identity ) cell+ @ ;
: INDEX@ ( ptr identity n -- identity ) {: base index:n :}
   base index cells + @ ;
: REVERSE@ ( n ptr identity -- identity ) + @ ;
: PREVIOUS@ ( ptr identity -- identity ) 1 cells - @ ;
: DISTANCE ( ptr identity ptr identity -- n ) - ;
: BYTE-ROUNDTRIP ( ptr identity -- identity ) char+ 1- @ ;
: UNIT-ROUNDTRIP ( ptr identity -- identity ) 1+ 1- @ ;

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
   s" NP-BYTE-CELL ( ptr u8 -- n ) cell+ @" REJECT
   T-REPORT ;

RUN
;package
