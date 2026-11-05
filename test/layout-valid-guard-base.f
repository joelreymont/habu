\ layout-valid-guard-base.f — a nested sum and a raw view of a checked buffer of it, for the layout guard fixtures.

package LAYOUT-VALID-GUARD

public

ENUM lvg-inner zero one ;ENUM

SUMTYPE lvg-outer
  VARIANT left lvg-inner ;VARIANT
  VARIANT right n ;VARIANT
;SUMTYPE

private

2 LAYOUT-BUFFER BUF lvg-outer

\ The forged rows need a raw cell view of the checked buffer.
CAST: RAW ( ptr lvg-outer -- ptr n )

: SET ( n n n -- )
   {: payload:n tag:n idx:n :}
   idx BUF RAW {: addr:ptr :}
   payload addr !
   tag addr cell+ ! ;

;package
