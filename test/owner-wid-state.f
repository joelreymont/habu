\ owner-wid-state.f - read-only proof of the isolated cold owner image.

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

data-base OWNER-WID-N-CELL + @ OWNER-WID-MAX T=
data-base OWNER-WID-OFF + @ $200000001000 T=
data-base OWNER-WID-OFF OWNER-WID-ROW 255 * + + @ $20FF000010FF T=
$1000 PUBLIC? TTRUE
$1000 PRIVATE? TFALSE
$2000 PRIVATE? TTRUE
$2000 PUBLIC? TFALSE
$10FF PUBLIC? TTRUE
$10FF PRIVATE? TFALSE
$20FF PRIVATE? TTRUE
$20FF PUBLIC? TFALSE
$10FF MEMBER? TTRUE
$20FF MEMBER? TTRUE
$1100 $2100 OWNER-WID-MAX PREFLIGHT TFALSE
s" owner-wid-add" 0 search-wl 0 T=
s" owner-wid-test-add" 0 search-wl 0 T=

;package

T-REPORT
s" owner-wid-state-test: ok" type cr
