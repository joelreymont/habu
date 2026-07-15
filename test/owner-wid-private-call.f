\ owner-wid-private-call.f - persisted private package word stays hidden.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-FORGED

package OWNER-WID-PRIVATE-CALL

: FORGE ( -- n )
   OWNER-WID-COLD-TEST:PRIVATE-PROOF ;

;package
