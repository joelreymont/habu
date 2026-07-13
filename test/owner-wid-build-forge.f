\ owner-wid-build-forge.f - package reopen cannot recover source injection.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-FORGED

require test/owner-wid-image.f

package BUILD-EXT

: FORGE ( ptr u8 n -- )
   SET ;

;package
