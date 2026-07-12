\ run-rerun-failed-test.f - focused tests for --rerun-failed red-phase persist.
\ Run: bin/hb --load test/run-lib.f lib/test.f test/run-rerun-failed-test.f

require test/run-lib.f
require lib/test.f

package RRF

6 constant PH-DEBUG
9 constant PH-FIXTURES

: PERSIST! ( ptr u8 n -- ) {: a:ptr u:n :}
   a TR-PERSIST-BUF u BYTE-COPY
   u TR-PERSIST-U ! ;

: RED$ ( -- ptr u8 n )
   TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N ;

: T-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-rerun" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a u PERSIST!
   TR-PERSIST$ MAKE-DIRS ;

\ TR-PHASE-LABEL is the pool label, so a red label maps back to its phase index.
: LABEL-ROUNDTRIP ( -- )
   PH-DEBUG >IDX TR-PHASE-LABEL TR-LABEL>IDX TTRUE
   IDX>N PH-DEBUG T= ;

: LINE-CONTENT ( -- )
   TR-RED-FILE-U BUF-RESET
   PH-DEBUG >IDX TR-RED-LINE+
   RED$ s" 6" STARTS-WITH? TTRUE
   RED$ s" bin/hb" T-CONTAINS
   RED$ s" --load" T-CONTAINS
   RED$ s" test/gate-debug.f" T-CONTAINS ;

: WRITE-RED-FILE ( -- )
   TR-RED-FILE-U BUF-RESET
   PH-DEBUG >IDX TR-RED-LINE+
   PH-FIXTURES >IDX TR-RED-LINE+
   TR-RED-LIST$ TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N WRITE-ALL ;

: ROUNDTRIP ( -- )
   WRITE-RED-FILE
   TR-RERUN-LOAD
   TR-RERUN-N @ 2 T=
   PH-DEBUG >IDX TR-RR-MARKED? TTRUE
   PH-FIXTURES >IDX TR-RR-MARKED? TTRUE
   5 >IDX TR-RR-MARKED? TFALSE
   7 >IDX TR-RR-MARKED? TFALSE ;

\ Duplicate lines must not double-count.
: DEDUP ( -- )
   TR-RR-CLEAR
   s" 6" TR-RERUN-LINE
   s" 6" TR-RERUN-LINE
   TR-RERUN-N @ 1 T= ;

: SKIP-GUARD ( -- )
   ROUNDTRIP
   -1 TR-RERUN !
   PH-DEBUG >IDX TR-RERUN-SKIP? TFALSE
   5 >IDX TR-RERUN-SKIP? TTRUE
   0 TR-RERUN !
   5 >IDX TR-RERUN-SKIP? TFALSE ;

: CLEANUP ( -- )
   0 TR-RERUN !
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
