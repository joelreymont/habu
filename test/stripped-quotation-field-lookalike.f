\ A scalar holding a live code address remains forbidden by stripped AOT's
\ DATA scan, even after an earlier quotation mark is cleared for this slot.
package STRIPPED-QUOT-LOOKALIKE

STRUCTURE hook 0 FIELD handler [ n -- n ] ;STRUCTURE
SUMTYPE choice 0
   VARIANT scalar n ;VARIANT
   VARIANT present hook ;VARIANT
;SUMTYPE
STRUCTURE envelope 0 FIELD item choice ;STRUCTURE
TYPED-VARIABLE SCALAR-ROW envelope

: INIT ( -- )
   [: 7 + ;] HOOK-MAKE construct choice present ENVELOPE-MAKE SCALAR-ROW !
   cp@ 8 - construct choice scalar ENVELOPE-MAKE SCALAR-ROW ! ;

INIT
;package

: MAIN ( -- ) s" unreachable" type cr ;
