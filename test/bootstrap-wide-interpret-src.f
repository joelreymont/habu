\ bootstrap-wide-interpret-src.f - stage0 rejects top-level wide execution.

\ The isolated stage0 fixture has no protected-WID registry; seal-absence.f
\ owns that registry-absence proof. This hook only permits the declaration.
: BWI-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
' BWI-STAGE0-PROT-NOP TDECL-PROT-WID-XT !

SUMTYPE bwi 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWI-WIDE ( -- bwi<n> ) 7 BWI:VALUE ;

BWI-WIDE
