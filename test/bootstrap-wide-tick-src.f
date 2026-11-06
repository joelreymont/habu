\ bootstrap-wide-tick-src.f - stage0 rejects ticking a wide-effect word.
\ This is an input fixture, not a standalone test.

SUMTYPE bwt<a>
  VARIANT value a ;VARIANT
;SUMTYPE

: BWT-WIDE ( -- bwt<n> ) 7 BWT:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
' BWT-WIDE drop
