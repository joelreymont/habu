\ prot-wid-probe.f - read-only view of the engine's protected-WID bitmap.

package PROT-WID-PROBE

public

\ The engine's own answer, pins included (src/habu/xref.f XREF-WID-PROTECTED?).
: MEMBER? ( n -- bool )
   XREF-WID-PROTECTED? ;

: COUNT ( -- n )
   0 PROT-WID-MAX 0 ?do i MEMBER? IF 1 + THEN loop ;

;package
