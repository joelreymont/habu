\ run-rerun-failed-test.f - focused tests for --rerun-failed red-phase persist.
\ Run: bin/hb --load test/run-lib.f lib/test.f test/run-rerun-failed-test.f

require test/run-lib.f
require lib/test.f

package RRF

6 constant PH-DEBUG
9 constant PH-FIXTURES

: T-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-rerun" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a u TEST-RUN:PERSIST!
   TEST-RUN:PERSIST$ MAKE-DIRS ;

\ TEST-RUN:PHASE-LABEL is the pool label, so a red label maps back to its phase index.
: LABEL-ROUNDTRIP ( -- )
   PH-DEBUG >IDX TEST-RUN:PHASE-LABEL TEST-RUN:LABEL>IDX TTRUE
   IDX>N PH-DEBUG T= ;

: LINE-CONTENT ( -- )
   TEST-RUN:RED-FILE-RESET
   PH-DEBUG >IDX TEST-RUN:RED-LINE+
   TEST-RUN:RED-FILE$ s" 6" STARTS-WITH? TTRUE
   TEST-RUN:RED-FILE$ s" bin/hb" T-CONTAINS
   TEST-RUN:RED-FILE$ s" --load" T-CONTAINS
   TEST-RUN:RED-FILE$ s" test/gate-debug.f" T-CONTAINS ;

: WRITE-RED-FILE ( -- )
   TEST-RUN:RED-FILE-RESET
   PH-DEBUG >IDX TEST-RUN:RED-LINE+
   PH-FIXTURES >IDX TEST-RUN:RED-LINE+
   TEST-RUN:RED-LIST$ TEST-RUN:RED-FILE$ WRITE-ALL ;

: ROUNDTRIP ( -- )
   WRITE-RED-FILE
   TEST-RUN:RERUN-LOAD
   TEST-RUN:RERUN-N 2 T=
   PH-DEBUG >IDX TEST-RUN:RR-MARKED? TTRUE
   PH-FIXTURES >IDX TEST-RUN:RR-MARKED? TTRUE
   5 >IDX TEST-RUN:RR-MARKED? TFALSE
   7 >IDX TEST-RUN:RR-MARKED? TFALSE ;

\ Duplicate lines must not double-count.
: DEDUP ( -- )
   TEST-RUN:RR-CLEAR
   s" 6" TEST-RUN:RERUN-LINE
   s" 6" TEST-RUN:RERUN-LINE
   TEST-RUN:RERUN-N 1 T= ;

: SKIP-GUARD ( -- )
   ROUNDTRIP
   -1 TEST-RUN:RERUN!
   PH-DEBUG >IDX TEST-RUN:RERUN-SKIP? TFALSE
   5 >IDX TEST-RUN:RERUN-SKIP? TTRUE
   0 TEST-RUN:RERUN!
   5 >IDX TEST-RUN:RERUN-SKIP? TFALSE ;

: CLEANUP ( -- )
   0 TEST-RUN:RERUN!
   CLEANUP-RUN ;

: MAIN ( -- )
   T-RESET
   SETUP
   LABEL-ROUNDTRIP
   LINE-CONTENT
   ROUNDTRIP
   DEDUP
   SKIP-GUARD
   CLEANUP
   T-REPORT ;

MAIN

;package
