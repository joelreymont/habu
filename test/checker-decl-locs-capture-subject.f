\ checker-decl-locs-capture-subject.f - the two halves of
\ test/checker-decl-locs-capture.f: KEEP runs in the unsealed engine that then
\ saves an image, RESTORED in that image.

require lib/test.f
require src/habu/verify-source.f
require test/checker-decl-locs-lib.f

package NAVL

public

\ Arm, replay a declaration that takes the location, and return still armed:
\ the producer saves its image next, with the location armed and a row in the
\ table.
: KEEP ( -- )
   T-RESET
   VISIT AT-START AT-END ARM
   s" : NAVC-KEPT ( -- n ) 1 ;" VERIFY:SOURCE-BUF-IN-SCOPE
   s" NAVC-KEPT" LOCATED
   T-REPORT ;

\ In the image the kept row survives and its location does not; a declaration
\ replayed unarmed takes none, so no arm survived either; and an armed one
\ takes its location, in tables grown afresh after the capture discarded them.
: RESTORED ( -- )
   T-RESET
   s" NAVC-KEPT" UNLOCATED
   s" : NAVC-AFTER ( -- n ) 2 ;" VERIFY:SOURCE-BUF-IN-SCOPE
   s" NAVC-AFTER" UNLOCATED
   s" : NAVC-ARMED ( -- n ) 3 ;" ARMED-REPLAY
   s" NAVC-ARMED" LOCATED
   T-REPORT ;

;package
