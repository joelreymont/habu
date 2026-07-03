\ golden-test.f - focused tests for the byte-exact golden-file mechanism.
\ Run: bin/hb --load test/golden.f lib/test.f test/golden-test.f

require test/golden.f
require lib/test.f

package GT-TEST

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-golden" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a u GOLD:ROOT!
   GOLD:REDACT-CLEAR
   0 GOLD:UPDATE! ;

\ Update mode writes the golden and reports pass; compare mode then matches.
: UPDATE-THEN-MATCH ( -- )
   -1 GOLD:UPDATE!
   s" alpha diagnostic" s" case-a.json" GOLD:CHECK TTRUE
   0 GOLD:UPDATE!
   s" alpha diagnostic" s" case-a.json" GOLD:CHECK TTRUE ;

\ A changed byte fails byte-exact compare.
: DRIFT-FAILS ( -- )
   0 GOLD:UPDATE!
   s" alpha diagnostiX" s" case-a.json" GOLD:CHECK TFALSE ;

\ A missing golden fails closed (forces --update-golden first).
: MISSING-FAILS ( -- )
   0 GOLD:UPDATE!
   s" whatever" s" no-such.json" GOLD:CHECK TFALSE ;

\ The redact prefix is normalized to <root>, so a run-varying temp path in the
\ captured text still matches a stable golden.
: REDACT-STABLE ( -- )
   -1 GOLD:UPDATE!
   s" /tmp/habu-111" GOLD:REDACT!
   s" file: /tmp/habu-111/foo.f line 1" s" case-b.json" GOLD:CHECK TTRUE
   0 GOLD:UPDATE!
   s" /tmp/habu-222" GOLD:REDACT!
   s" file: /tmp/habu-222/foo.f line 1" s" case-b.json" GOLD:CHECK TTRUE ;

: CLEANUP ( -- )
   0 GOLD:UPDATE!
   GOLD:REDACT-CLEAR
   CLEANUP-RUN ;

: MAIN ( -- )
   T-RESET
   SETUP
   UPDATE-THEN-MATCH
   DRIFT-FAILS
   MISSING-FAILS
   REDACT-STABLE
   CLEANUP
   T-REPORT ;

MAIN

end-package
