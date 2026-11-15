\ checker-owner.f - the checker this compiler asks.

require src/lib/prelude.f
require src/core/checker.f
require src/core/check-walk.f

package CHECKER-OWNER

variable BIND-UNJUDGED

public

\ ---- the does> split, the last scan's report and the compile window ----------

: RESET-REPORT ( -- )
   CHECKER-REPORT-RESET ;

: BIND-REGIME! ( n -- ) BIND-UNJUDGED ! ;

\ A TRUSTED: parent's clause is scanned unjudged, as its body was.
: DOES-CHECK ( ^u8 n ^u8 n -- n )
   BIND-UNJUDGED @ 0<> if CHECK-DOES-UNJUDGED! exit then
   CHECK-DOES! ;

EXPORT DOES-REPORT

\ Opens the compile window with the code a callback's checker write is refused
\ with, or shuts it with 0: between them only the compiler's callbacks run, so
\ a refused definition's cut takes exactly its rows.
: WRITE-WINDOW ( n -- )
   CHECKER-WRITE-WINDOW! ;

\ ---- the finalized per-call-site facts the scan recorded ----------------

: CALL-CELLS ( n -- n n )
   CWIN-CELLS ;

: MATCH-PAYLOAD ( n -- n n )
   CWIN-MATCH-PAYLOAD ;

;package
