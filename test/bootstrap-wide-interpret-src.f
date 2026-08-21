\ bootstrap-wide-interpret-src.f - stage0 rejects top-level wide execution.
\ schedule-lint: allow-unscheduled - habu-rehome-or-retire-65f56d69 owns the
\ decision. This stage0 fixture's wired siblings have rows in
\ test/candidate-validation.f or tools/package-diff-lint-core.f; this one has none.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. The no-op hook keeps the fixture focused on top-level wide execution.
using TYPE-DECL

package BWI-PROT
private
: STAGE0-NOP ( ptr u8 n -- ) 2drop ;
: INSTALL ( -- ) [: STAGE0-NOP ;] is TYPE-DECL:TDECL-PROT-WID-XT ;
INSTALL
;package

-1 TDECL-PROT-WID-ARMED !
;using

SUMTYPE bwi 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWI-WIDE ( -- bwi<n> ) 7 BWI:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
BWI-WIDE
