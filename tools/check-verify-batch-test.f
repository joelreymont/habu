\ End-to-end parity of framed, isolated reference checks with independent
\ verifier children. Run: bin/hb --load tools/check-verify-batch-test.f
require src/lib/test.f
require src/lib/fs.f
require tools/check-verify-core.f

package CHECK-VERIFY-BATCH-TEST

60000 constant GUARD-MS
5 constant CASES
DYNAMIC-BUFFER EXPECTED u8
DYNAMIC-BUFFER RECORDS n
DYNAMIC-BUFFER VERDICTS n
4096 BUFFER: OVER-SRC
variable EXPECTED-U
variable RECORD-N
variable RECORD-READ
variable OVER-U

: KIND ( CHECK:verdict -- n )
   MATCH CHECK:verdict
      verified OF 0 ENDOF
      refused OF 1 ENDOF
      engine-provided OF 2 ENDOF
      held OF 3 ENDOF
      incomplete OF drop 4 ENDOF
      deferred OF 5 ENDOF
   ;MATCH ;

: SRC$ ( n -- ptr u8 n )
   dup 0 = if drop s\" : BATCH-A ( n -- n ) 2 * ;\n" exit then
   dup 1 = if drop s\" package BATCH-SHADOW private : * ( n n -- n ) + ; public : USE ( n n -- n ) * ; ;package\n" exit then
   dup 2 = if drop s\" : BAD ( -- n ) NO-SUCH-WORD ;\n" exit then
   dup 3 = if drop s\" require test/fixtures/check-batch-dep.f\n: USE ( n -- n ) CHECK-BATCH-DEP:OPEN-ONLY ;\n" exit then
   drop s\" ignored\n" ;

: PATH$ ( n -- ptr u8 n )
   dup 0 = if drop s" test/check-batch-a.f" exit then
   dup 1 = if drop s" test/check-batch-shadow.f" exit then
   dup 2 = if drop s" test/check-batch-bad.f" exit then
   dup 3 = if drop s" test/check-batch-open.f" exit then
   drop s" src/habu/prims.f" ;

: OPEN-DEP ( [ ptr u8 n ptr u8 n -- ] -- )
   {: add :}
   s" test/fixtures/check-batch-dep.f" SOURCE-ROOT:CANONICAL drop
   s\" package CHECK-BATCH-DEP public : OPEN-ONLY ( n -- n ) 2 * ; ;package\n"
   add execute ;

: SINGLE ( n -- CHECK:verdict )
   {: k:n :}
   k SRC$ k PATH$ s" *"
   k 3 = if [: OPEN-DEP ;] else [: drop ;] then
   GUARD-MS >MS CHECK:VERIFY-BYTES-OF ;

: ADD ( n -- )
   {: k:n :}
   k SRC$ k PATH$
   k 3 = if [: OPEN-DEP ;] else [: drop ;] then
   CHECK:VERIFY-BATCH-ADD ;

: SAVE-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   RECORD-N @ 1+ 2 * RECORDS-RESERVE
   EXPECTED-U @ RECORD-N @ 2 * RECORDS !
   u RECORD-N @ 2 * 1+ RECORDS !
   u 0<> if
      EXPECTED-U @ u + EXPECTED-RESERVE
      a EXPECTED-U @ EXPECTED u BYTE-COPY
   then
   EXPECTED-U @ u + EXPECTED-U !
   1 RECORD-N +! ;

: SAVE ( n -- )
   {: k:n :}
   k SINGLE KIND k VERDICTS !
   CHECK:VERIFY-OUT$ SAVE-SPAN
   CHECK:VERIFY-FILES$ SAVE-SPAN
   CHECK:VERIFY-DEFS$ SAVE-SPAN
   CHECK:VERIFY-USES$ SAVE-SPAN
   CHECK:VERIFY-LOADS$ SAVE-SPAN ;

: COMPARE-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   RECORD-READ @ 2 * {: at:n :}
   a u at RECORDS @ EXPECTED at 1+ RECORDS @ T$=
   1 RECORD-READ +! ;

: COMPARE ( n -- )
   {: k:n :}
   k CHECK:VERIFY-BATCH-RESULT KIND k VERDICTS @ T=
   CHECK:VERIFY-OUT$ COMPARE-SPAN
   CHECK:VERIFY-FILES$ COMPARE-SPAN
   CHECK:VERIFY-DEFS$ COMPARE-SPAN
   CHECK:VERIFY-USES$ COMPARE-SPAN
   CHECK:VERIFY-LOADS$ COMPARE-SPAN ;

\ A later child failure cannot turn its earlier marker into a clean exit.
\ The LSP retries all three subjects independently after this verdict.
: UNCLEAN ( -- )
   SB-RESET
   CK-USE-MAX 1+ 0 ?do s\" using SOURCE-ROOT\n" SB-APPEND loop
   SB$ dup OVER-U ! OVER-SRC swap BYTE-COPY
   s" *" CHECK:VERIFY-BATCH-BEGIN
   0 SRC$ s" test/check-batch-first.f" [: drop ;] CHECK:VERIFY-BATCH-ADD
   OVER-SRC OVER-U @ s" test/check-batch-over.f"
   [: drop ;] CHECK:VERIFY-BATCH-ADD
   0 SRC$ s" test/check-batch-last.f" [: drop ;] CHECK:VERIFY-BATCH-ADD
   GUARD-MS >MS CHECK:VERIFY-BATCH-RUN
   3 0 ?do
      s" batch after crash: incomplete" T-LABEL
      i CHECK:VERIFY-BATCH-RESULT KIND 4 T=
      s" batch after crash: retry" T-LABEL
      CHECK:VERIFY-BATCH-COMPLETE? 0= TTRUE
   loop ;

public
: MAIN ( -- )
   T-RESET
   CASES VERDICTS-RESERVE
   1 EXPECTED-RESERVE
   0 EXPECTED-U ! 0 RECORD-N ! 0 RECORD-READ !
   CASES 0 ?do i SAVE loop
   s" *" CHECK:VERIFY-BATCH-BEGIN
   CASES 0 ?do i ADD loop
   GUARD-MS >MS CHECK:VERIFY-BATCH-RUN
   CASES CHECK:VERIFY-BATCH-N T=
   CASES 0 ?do i COMPARE loop
   UNCLEAN
   T-FAILURES 0<> if s" check-verify-batch-test failed" 1 die then
   s" check-verify-batch-test: parity OK" type cr ;
;package

CHECK-VERIFY-BATCH-TEST:MAIN
