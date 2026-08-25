\ prot-wid-probe.f - read-only view of the engine's protected-WID bitmap.

package PROT-WID-PROBE

private

: WORD@ ( n -- n )                       \ the band word holding bit `wid`
   6 rshift 8 * data-base PROT-BITS-OFF + + @ ;

public

\ Is wordlist `wid` protected? This answers the same question the engine's PROT-WID?
\ answers, pins included: the two engine-reserved OWNER-API wordlists are protected
\ by rule rather than by a bit, so that every boot path (cold init, AOT restore,
\ snapshot restore) protects them identically and unforgeably. A wid outside the
\ bitmap's bound has no bit and can never have been protected, because prot-wid-add
\ refuses to protect one.
: MEMBER? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or IF 0 0= EXIT THEN
   wid 0 < wid PROT-WID-MAX < 0= or IF 0 0= 0= EXIT THEN
   wid WORD@  wid 63 and rshift  1 and 0= 0= ;

\ How many wordlists are protected right now.
: COUNT ( -- n )
   0 PROT-WID-MAX 0 ?do i MEMBER? IF 1 + THEN loop ;

;package
