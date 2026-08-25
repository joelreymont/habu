\ native-generated-constructor.f - generated SUM/ENUM bodies use construct.
\
\ A payload constructor is the smallest shape whose raw runtime cells differ
\ from its one logical output bundle. The declaration drives the production
\ generator and native compiler at genuine top level, where RESULT:OK-shaped
\ definitions can collide with an older bare binding.

require src/compiler/native/compiler.f

s" ENUM nctorresult 0 VARIANT ok FIELD value n ;VARIANT VARIANT err FIELD error n ;VARIANT ;ENUM"
INCLUDE-EVALUATE

package NCTOR-WIDE-TEST
public

STRUCTURE nctorstructure 0
   FIELD x n
   FIELD y n
;STRUCTURE

PRODUCT nctorproduct 0
   FIELD x n
   FIELD y n
;PRODUCT

private

: NCTOR-STRUCTURE-ROUNDTRIP ( n n -- n n )
   NCTOR--WIDE--TEST-NCTORSTRUCTURE:MAKE
   NCTOR--WIDE--TEST-NCTORSTRUCTURE:UNMAKE ;

: NCTOR-PRODUCT-ROUNDTRIP ( n n -- n n )
   NCTOR--WIDE--TEST-NCTORPRODUCT:MAKE
   NCTOR--WIDE--TEST-NCTORPRODUCT:UNMAKE ;

: NCTOR-WIDE-ROUNDTRIPS? ( -- bool )
   3 4 NCTOR-STRUCTURE-ROUNDTRIP 4 = swap 3 = and
   5 6 NCTOR-PRODUCT-ROUNDTRIP 6 = swap 5 = and
   and ;

: NCTOR-REPORT ( -- )
   NCTOR-WIDE-ROUNDTRIPS? 0= if
      s" native-generated-constructor: wide round-trip failed" 1 die
   then
   s" test: ok" type cr ;

NCTOR-REPORT

;package
