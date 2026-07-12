\ bootstrap-wide-interpret-src.f - stage0 rejects top-level wide execution.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. The no-op hook keeps the fixture focused on top-level wide execution.
: BWI-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
' BWI-STAGE0-PROT-NOP TDECL-PROT-WID-XT !

SUMTYPE bwi 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWI-WIDE ( -- bwi<n> ) 7 BWI:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
BWI-WIDE
