\ program-diagnostic-quiet-count.f - a multi-error load counts only the refusals
\ it renders, for test/program-diagnostics-test.f QUIET-COUNT-ONCE. A quiet scan
\ (CHECK-QUIET-CANDIDATE!, a `TRUSTED:` body's) adds nothing; a refused colon
\ definition prints its diagnostic and adds 1.
package PROGRAM-DIAG-QUIET-COUNT
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: FOUND ( ptr u8 n -- n ) get-current search-wl 0<> if 1 else 0 then ;
;package

PROGRAM-DIAG-QUIET-COUNT:START
s" QC ( -- ) drop" CHECK-QUIET-CANDIDATE! .
s" TRUSTED: PV2 ( a b -- b a ) ;" evaluate
s" PV2" PROGRAM-DIAG-QUIET-COUNT:FOUND .
s" : BADC ( -- ) drop ;" evaluate
PROGRAM-DIAG-QUIET-COUNT:FINISH .
s" ok" type cr
