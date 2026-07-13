\ owner-wid-call.f - hostile checked call must not resolve the cold mutator.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-FORGED

package OWNER-WID-CALL

: FORGE ( n n -- bool )
   owner-wid-add ;

;package
