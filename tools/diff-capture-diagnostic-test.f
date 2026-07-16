\ diff-capture-diagnostic-test.f - structured report fixtures.

require lib/errors.f
require lib/test.f
require lib/memory.f
require tools/json.f
require tools/diff-capture-diagnostic.f

package DIFF-CMD
public

: TEST-DIAG-FAULT ( ptr u8 n -- ) {: bytes:ptr bytesu:n :}
   true COMMAND-READY !
   DIFF--CAPTURE-COMMAND--PHASE:SNAPSHOT LAST-PHASE!
   DIFF--CAPTURE-COMMAND--OUTCOME:FAULT LAST-OUTCOME!
   s" jj" LAST-EXE-U ! LAST-EXE-A !
   0 ARG-N !
   0 LAST-RC-N !
   E-PROC-SPAWN LAST-CODE-N !
   0 LAST-OUT-CODE-N !
   0 LAST-ERR-CODE-N !
   bytes LAST-OUT-A ! bytesu LAST-OUT-U !
   bytes LAST-ERR-A ! bytesu LAST-ERR-U ! ;

;package

package DIFF-TXN
public

: TEST-DIAG-COMBINED ( -- )
   true REPORT-READY !
   E-FS-OPEN E-FS-DIR SET-CAPTURE-RESULT ;

;package

package DIFF-DIAG-TEST
private

create BYTES $FF c, $80 c, $22 c, $5C c, $0A c,
5 constant BYTES-U

: MAIN ( -- )
   T-RESET
   BYTES BYTES-U DIFF-CMD:TEST-DIAG-FAULT
   DIFF-TXN:TEST-DIAG-COMBINED
   DIFF-DIAG:REPORT$ JSON-PARSE {: root:n :}
   root JSON-KIND J-OBJ T=
   root s" capture_outcome" JSON-GET JSON-STRING$ s" combined-failed" T$=
   root s" primary_code" JSON-GET JSON-NUMBER$ s" -2102" T$=
   root s" cleanup_code" JSON-GET JSON-NUMBER$ s" -2103" T$=
   root s" command_present" JSON-GET JSON-BOOL@ TTRUE
   root s" phase" JSON-GET JSON-STRING$ s" snapshot" T$=
   root s" outcome" JSON-GET JSON-STRING$ s" fault" T$=
   root s" code" JSON-GET JSON-NUMBER$ s" -2500" T$=
   root s" stdout_encoding" JSON-GET JSON-STRING$ s" hex" T$=
   root s" stdout" JSON-GET JSON-STRING$ s" ff80225c0a" T$=
   root s" stderr" JSON-GET JSON-STRING$ s" ff80225c0a" T$=
   T-REPORT
   s" diff-capture-diagnostic-test: ok" type cr ;

MAIN

;package
