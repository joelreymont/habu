\ bootstrap-wide-tick-src.f - stage0 rejects ticking a wide-effect word.
\ schedule-lint: allow-unscheduled - habu-rehome-or-retire-65f56d69 owns the
\ decision. This stage0 fixture's wired siblings have rows in
\ test/candidate-validation.f or tools/package-diff-lint-core.f; this one has none.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. The no-op hook keeps the fixture focused on wide-effect ticking.
: BWT-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
: BWT-PROT-INSTALL ( -- ) [: BWT-STAGE0-PROT-NOP ;] is TDECL-PROT-WID-XT ;
BWT-PROT-INSTALL
-1 TDECL-PROT-WID-ARMED !

SUMTYPE bwt 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWT-WIDE ( -- bwt<n> ) 7 BWT:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
' BWT-WIDE drop
