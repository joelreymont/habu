\ owner-wid-state.f - read-only proof of the isolated cold owner image.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-FORGED

require lib/test.f

T-RESET

package OWNER-WID-STATE

: PREFLIGHT ( n n n -- bool )
   owner-wid-preflight? ;

: PUBLIC? ( n -- bool )
   owner-wid-public? ;

: PRIVATE? ( n -- bool )
   owner-wid-private? ;

: MEMBER? ( n -- bool )
   owner-wid? ;

: CHECK ( -- )
   data-base OWNER-WID-N-CELL + @ 1 T=
   data-base OWNER-WID-OFF + @ {: pair:n :}
   pair $FFFFFFFF and {: pub:n :}
   pair 32 rshift {: pri:n :}
   pub 0 > TTRUE
   pri 0 > TTRUE
   pub pri <> TTRUE
   data-base WIDN-CELL + @ pub > TTRUE
   data-base WIDN-CELL + @ pri > TTRUE
   pub PUBLIC? TTRUE
   pub PRIVATE? TFALSE
   pri PRIVATE? TTRUE
   pri PUBLIC? TFALSE
   pub MEMBER? TTRUE
   pri MEMBER? TTRUE
   pub pri OWNER-WID-MAX PREFLIGHT TFALSE
   OWNER-WID-COLD-TEST:PUBLIC-PROOF 314159 T=
   s" OWNER-WID-COLD-TEST:PRIVATE-PROOF" 0 search-wl 0 T=
   s" owner-wid-add" 0 search-wl 0 T=
   s" owner-wid-test-add" 0 search-wl 0 T= ;

CHECK

;package

T-REPORT
s" owner-wid-state-test: ok" type cr
