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
\
\ REPORT and REQUIRE-ROOM are the capacity guard a long-running suite ends with (dot
\ habu-guard-maki-suite-070e2221). maki/test.f calls REQUIRE-ROOM after its whole
\ inventory has run, in the ONE process that ran it, so the number it reports is the
\ headroom the entire suite consumed - not a per-file figure. That is exactly the
\ measurement whose absence let the registry fill silently until an unrelated enum
\ took the blame.

require lib/errors.f
require lib/string.f
require lib/fmt.f

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

\ The floor a suite must still be above when it finishes. Half the id space, stated
\ as a fraction of the bound rather than as a measured number: a measured floor would
\ have to be re-pinned by every lane that adds a public family, which is how the old
\ 256-row ceiling stayed invisible until it was hit. Half leaves a 2x margin, so a
\ run that doubles its wordlist appetite names itself here - with the exact numbers -
\ one whole doubling before anything is refused.
: ROOM-FLOOR ( -- n ) CAP 2 / ;

\ One line of evidence per run: what the registry holds, what the run consumed, and
\ what is left, against the bound. Printed whether or not the floor holds, so a run
\ that is merely approaching the floor still leaves a trend in its log.
: REPORT ( -- )
   s" protected-wordlist registry: " type COUNT FMT:.U
   s"  protected, " type WIDS FMT:.U s"  wordlist ids used, " type
   ROOM FMT:.U s"  of " type CAP FMT:.U s"  left (floor " type ROOM-FLOOR FMT:.U s" )" type cr ;

\ Fail closed under the floor, naming the registry. A caller reaching this has NOT
\ hit the ceiling - it has crossed the warning line with a full doubling still in
\ hand, which is the whole point of the guard. Public so a caller that wants to
\ catch the guard can name the code rather than match a number.
-7210 constant E-PROT-ROOM   \ protected-wordlist headroom fell below the floor

: REQUIRE-ROOM ( -- )
   REPORT
   ROOM ROOM-FLOOR >= if exit then
   s" prot-wid-probe: protected-wordlist headroom below the floor" type cr
   E-PROT-ROOM throw ;

;package
