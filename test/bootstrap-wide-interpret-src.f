\ bootstrap-wide-interpret-src.f - stage0 rejects top-level wide execution.
\ This is an input fixture, not a standalone test.

SUMTYPE bwi<a>
  VARIANT value a ;VARIANT
;SUMTYPE

: BWI-WIDE ( -- bwi<n> ) 7 BWI:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
BWI-WIDE
