\ prot-wid-probe.f - read-only view of the engine's protected-WID bitmap.
\
\ The band at PROT-BITS-OFF holds one bit per wordlist id: bit w is set exactly when
\ wordlist w is protected (a sealed system package or a generated constructor
\ package). The engine reaches it through the PROT-WID? routine, which source cannot
\ call; this is the read-only Habu-side view of the same bits, for capacity guards
\ and for tests that need to assert WHICH wordlist got protected rather than merely
\ how many did.
\
\ It reads the band and never writes it - the band is inside the PROT-GUARD range, so
\ a write from source fails closed at the sink anyway. ROOM mirrors the prot-wid-room
\ primitive: how many more wordlists may still be allocated and protected. That is
\ the quantity a declaration consumes, and the one whose exhaustion used to surface
\ as an uncaught 7169 against an unrelated enum.

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

\ Wordlist ids handed out so far, and the ids still available to allocate-and-protect.
: WIDS ( -- n ) data-base WIDN-CELL + @ ;
: ROOM ( -- n ) prot-wid-room ;

\ The bound itself, so a caller can report headroom as a fraction rather than a bare
\ number that means nothing without it.
: CAP ( -- n ) PROT-WID-MAX ;

;package
