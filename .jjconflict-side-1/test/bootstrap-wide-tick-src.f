\ bootstrap-wide-tick-src.f - stage0 rejects ticking a wide-effect word.
\ This is an input fixture, not a standalone test.

\ This isolated fixture does not load xref.f's constructor-package registration
\ bridge. The no-op hook keeps the fixture focused on wide-effect ticking.
using TYPE-DECL

package BWT-PROT
private
: STAGE0-NOP ( ptr u8 n -- ) 2drop ;
: INSTALL ( -- ) [: STAGE0-NOP ;] is TYPE-DECL:TDECL-PROT-WID-XT ;
INSTALL
;package

-1 TDECL-PROT-WID-ARMED !
;using

SUMTYPE bwt 1
  VARIANT value a ;VARIANT
;SUMTYPE

: BWT-WIDE ( -- bwt<n> ) 7 BWT:VALUE ;

s" BOOTSTRAP-WIDE-ARMED" type cr
' BWT-WIDE drop
