\ A failed declaration's row serves only its own diagnostic run. Open a payload
\ window, fail one checked definition under multi-error recovery, then capture
\ the window: the capture refuses that row (`checker: a failed declaration's row
\ is not portable`, exit 76), so no import restores it as a seeded row.
s" src/lib/prelude.f" provided
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/aot-arm.f
\ src/habu/xref.f calls CODE-SPAN:CHECK without requiring code-span.f (the cold
\ prefix loads it first), and aot-capture.f reaches xref.f through
\ address-cells.f before its own require, so this window loads it here.
require src/habu/code-span.f
require src/habu/aot-capture.f

package GRAPH-RECOVERY
using AOT-ARM
using AOT-CAPTURE
public

: EQ ( n n -- )
   2dup <> IF s" graph mismatch got/expected: " type swap . . 79 throw THEN 2drop ;

: MODE? ( ptr u8 n -- bool )
   s" HABU_PAYLOAD_TEST_MODE" GETENV CORE-STR= ;

: ARM ( -- ) CHECKER-SCOPE-START WINDOW-OPEN ;
: CORRUPT-SOURCE ( -- )
   s" recovery" MODE? IF
      MULTI-ERR-BEGIN
      s" : PAYLOAD-RECOVERY ( n -- n ) drop ;" evaluate-closed
      MULTI-ERR-END 1 EQ
      s" graph corruption applied: recovery" type cr
   THEN ;
: INSTALL ( -- )
   WINDOW-CLOSE
   R0 @ D0 @ PRELUDE-MARK
   PAYLOAD-CAPTURE
   WINDOW$ CAPTURE ;

ARM
;package

\ Outside GRAPH-RECOVERY, whose wordlist predates the window: the capture takes
\ a record only from the global wordlist or one the window created
\ (aot-capture.f ACAP-WID-IN?).
GRAPH-RECOVERY:CORRUPT-SOURCE

package GRAPH-RECOVERY
INSTALL
;package
