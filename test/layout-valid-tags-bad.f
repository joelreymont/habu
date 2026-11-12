\ layout-valid-tags-bad.f — a part NFETCH-CHECK with a TAGS that returns does
\ not replace the trap a sum fetch of an out-of-domain payload tag calls.

package LAYOUT-VALID-TAGS

package NFETCH-CHECK
public
: TAGS ( ptr u8 n -- ) 2drop ;
;package

ENUM lt-inner zero one ;ENUM
SUMTYPE lt-outer
  VARIANT left lt-inner ;VARIANT
  VARIANT right n ;VARIANT
;SUMTYPE
2 LAYOUT-BUFFER BUF lt-outer

CAST: RAW ( ptr lt-outer -- ptr n )

: SET ( n n n -- )
   {: payload:n tag:n idx:n :}
   idx BUF RAW {: addr:ptr :}
   payload addr !
   tag addr cell+ ! ;

: GO ( -- )
   2 0 0 SET
   s" LAYOUT-VALID-TAGS-ARMED" type cr
   0 BUF @ drop ;

GO

;package
