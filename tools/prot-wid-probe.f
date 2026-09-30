\ prot-wid-probe.f - read-only view of the engine's protected-WID bitmap.

package PROT-WID-PROBE

public

\ Is wordlist `wid` protected? The engine's own answer, pins included: the two
\ engine-reserved OWNER-API wordlists are protected by rule rather than by a bit,
\ so that every boot path (cold init, AOT restore, snapshot restore) protects
\ them identically and unforgeably (src/habu/xref.f XREF-WID-PROTECTED?).
: MEMBER? ( n -- bool )
   XREF-WID-PROTECTED? ;

\ How many wordlists are protected right now.
: COUNT ( -- n )
   0 PROT-WID-MAX 0 ?do i MEMBER? IF 1 + THEN loop ;

;package
