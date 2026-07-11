\ bootstrap-wide-tick-src.f - stage0 rejects ticking a wide-effect word.

\ The isolated stage0 fixture has no protected-WID registry; seal-absence.f
\ owns that registry-absence proof. This hook only permits the declaration.
: BWT-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
' BWT-STAGE0-PROT-NOP TDECL-PROT-WID-XT !

SUMTYPE bwt 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWT-WIDE ( -- bwt<n> ) 7 BWT:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
' BWT-WIDE drop
