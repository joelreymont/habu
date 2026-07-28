\ run-rerun-failed-test.f - focused tests for --rerun-failed red-phase persist.
\ Run: bin/hb --load test/run-lib.f lib/test.f test/run-rerun-failed-test.f

require test/run-lib.f
require lib/test.f

package TEST

private

6 constant PH-DEBUG
9 constant PH-FIXTURES

: T-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: RERUN-SETUP ( -- )
   CLEANUP-RESET
   s" habu-rerun" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a u PERSIST!
   PERSIST$ MAKE-DIRS ;

\ PHASE-LABEL is the pool label, so a red label maps back to its phase index.
: LABEL-ROUNDTRIP ( -- )
   PH-DEBUG >IDX PHASE-LABEL LABEL>IDX TTRUE
   IDX>N PH-DEBUG T= ;

: LINE-CONTENT ( -- )
   TR-RED-FILE-U STR:BUF-RESET
   PH-DEBUG >IDX RED-LINE+
   TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N s" 6" STARTS-WITH? TTRUE
   TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N s" bin/hb" T-CONTAINS
   TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N s" --load" T-CONTAINS
   TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N s" test/gate-debug.f" T-CONTAINS ;

: WRITE-RED-FILE ( -- )
   TR-RED-FILE-U STR:BUF-RESET
   PH-DEBUG >IDX RED-LINE+
   PH-FIXTURES >IDX RED-LINE+
   RED-LIST$ TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N WRITE-ALL ;

: ROUNDTRIP ( -- )
   WRITE-RED-FILE
   RERUN-LOAD
   TR-RERUN-N @ 2 T=
   PH-DEBUG >IDX RR-MARKED? TTRUE
   PH-FIXTURES >IDX RR-MARKED? TTRUE
   5 >IDX RR-MARKED? TFALSE
   7 >IDX RR-MARKED? TFALSE ;

\ Duplicate lines must not double-count.
: DEDUP ( -- )
   RR-CLEAR
   s" 6" RERUN-LINE
   s" 6" RERUN-LINE
   TR-RERUN-N @ 1 T= ;

: SKIP-GUARD ( -- )
   ROUNDTRIP
   -1 TR-RERUN !
   PH-DEBUG >IDX RERUN-SKIP? TFALSE
   5 >IDX RERUN-SKIP? TTRUE
   0 TR-RERUN !
   5 >IDX RERUN-SKIP? TFALSE ;

\ PERSIST! copies a caller's bytes into a fixed FS-PATH-CAP region, so the
\ length is what stands between a caller and the data next to that buffer. The
\ refusal is proven through the REAL entry - this file reopens TEST, so the
\ word called here is the production one, not a copy - and at the exact edge:
\ FS-PATH-CAP is accepted and FS-PATH-CAP+1 throws. A truncating PERSIST! would
\ pass the first case and silently fail the second.
create OVER-PATH FS-PATH-CAP 1 + allot

: OVER-PATH-FILL ( -- )
   FS-PATH-CAP 1 + 0 ?do  $61 OVER-PATH i + c!  loop ;

: PATH-LENGTH-GUARD ( -- )
   OVER-PATH-FILL
   s" a path one byte past the buffer is refused, not copied" T-LABEL
   [: OVER-PATH FS-PATH-CAP 1 + PERSIST! ;] E-TR-PATH-LEN TTHROWSQ
   s" a path of exactly the buffer size is accepted" T-LABEL
   [: OVER-PATH FS-PATH-CAP PERSIST! ;] catch 0 T= ;

: CLEANUP ( -- )
   0 TR-RERUN !
   CLEANUP-RUN ;

: MAIN ( -- )
   T-RESET
   RERUN-SETUP
   LABEL-ROUNDTRIP
   LINE-CONTENT
   ROUNDTRIP
   DEDUP
   SKIP-GUARD
   PATH-LENGTH-GUARD
   CLEANUP
   T-REPORT ;

MAIN

;package
