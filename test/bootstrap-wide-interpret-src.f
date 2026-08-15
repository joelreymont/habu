\ bootstrap-wide-interpret-src.f - stage0 rejects top-level wide execution.
\ schedule-lint: allow-unscheduled - habu-rehome-or-retire-65f56d69 owns the
\ decision. This stage0 fixture's wired siblings have rows in
\ test/candidate-validation.f or tools/package-diff-lint-core.f; this one has none.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. The no-op hook keeps the fixture focused on top-level wide execution.
: BWI-STAGE0-PROT-NOP ( ptr u8 n -- ) 2drop ;
: BWI-PROT-INSTALL ( -- ) [: BWI-STAGE0-PROT-NOP ;] is TDECL-PROT-WID-XT ;
BWI-PROT-INSTALL
-1 TDECL-PROT-WID-ARMED !

SUMTYPE bwi 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWI-WIDE ( -- bwi<n> ) 7 BWI:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
BWI-WIDE
