\ checker-decl-locs-capture-subject.f - the two halves of
\ test/checker-decl-locs-capture.f: KEEP runs in the unsealed engine that then
\ saves an image, RESTORED in that image.

require lib/test.f
require src/habu/verify-source.f
require test/checker-decl-locs-lib.f

package NAVL

private

\ The kept row and its symbol. A replayed name binds only in its replay's
\ scope (ARMED-REPLAY), which the image does not hold, so KEEP reads both
\ there and the image asks the symbol for its row.
variable KEPT-SYM
variable KEPT-ROW

: KEPT-ROWS ( -- )
   s" NAVC-KEPT" LOCATED
   s" NAVC-KEPT" CHECKER-FIND-ACTIVE-SYM KEPT-SYM !
   s" NAVC-KEPT" ROW KEPT-ROW ! ;

: AFTER-ROWS ( -- ) s" NAVC-AFTER" UNLOCATED ;
: ARMED-ROWS ( -- ) s" NAVC-ARMED" LOCATED ;

public

\ Arm, replay a declaration that takes the location, and return still armed:
\ the producer saves its image next, with the location armed and a row in the
\ table.
: KEEP ( -- )
   T-RESET
   VISIT AT-START AT-END ARM
   s" : NAVC-KEPT ( -- n ) 1 ;" [: KEPT-ROWS ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE
   T-REPORT ;

\ In the image the kept row survives, still its symbol's newest, and its
\ location does not; a declaration replayed unarmed takes none, so no arm
\ survived either; and an armed one takes its location, in tables grown afresh
\ after the capture discarded them.
: RESTORED ( -- )
   T-RESET
   s" NAVC-KEPT" T-LABEL
   KEPT-SYM @ USIG-NEWEST {: rec1:n :}
   rec1 KEPT-ROW @ T=
   rec1 ROW-UNLOCATED
   s" : NAVC-AFTER ( -- n ) 2 ;" [: AFTER-ROWS ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE
   s" : NAVC-ARMED ( -- n ) 3 ;" [: ARMED-ROWS ;] ARMED-REPLAY
   T-REPORT ;

;package
