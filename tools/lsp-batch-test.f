\ The primitive References end-to-end cases through the real LSP server.
\ Run: bin/hb --load tools/lsp-batch-test.f
require tools/lsp-test-lib.f

LSP-TEST:TEST-BATCH

\ A missed disk subject grows LSP-REFERENCES's movable pool after BEGIN. The
\ checker must retain the original tail, not the pool's former address.
package CHECK
public
: BATCH-TEST-TAIL$ ( -- ptr u8 n )  VFY-OF-A @ VFY-OF-U @ ;
;package

package LSP-REFERENCES
32000 BUFFER: BATCH-TEST-PAD
public
: TEST-BATCH-TAIL ( -- )
   T-RESET
   FRESH
   s" *" POOL+ NAME-AT ! 1 NAME-U !
   s" test/fixtures/check-batch-dep.f" SOURCE-ROOT:CANONICAL drop SUBJECT+
   s" test/missing-batch-tail.f" SOURCE-ROOT:CANONICAL drop SUBJECT+
   BATCH-TEST-PAD 32000 POOL+ drop
   NAME$ drop {: old:ptr :}
   NAME$ LSP-CHECK:USES-BATCH-BEGIN
   0 BATCH-N !
   0 BATCH-SUBJECT
   1 BATCH-SUBJECT
   s" batch-tail: pool moved" T-LABEL NAME$ drop old <> TTRUE
   s" batch-tail: retained tail" T-LABEL NAME$ CHECK:BATCH-TEST-TAIL$ T$=
   LSP-CHECK:USES-BATCH-RUN
   s" batch-tail: earlier frame" T-LABEL
   0 [: ;] LSP-CHECK:USES-BATCH-RESULT TTRUE
   T-REPORT ;
;package

LSP-REFERENCES:TEST-BATCH-TAIL
