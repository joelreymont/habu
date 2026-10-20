\ Internal batch regressions; protocol cases run in tools/lsp-test.f.
\ Run: bin/hb --load tools/lsp-batch-test.f
require src/lib/string.f
require tools/lsp-test-lib.f

LSP-TEST:TEST-INIT

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
   0 1 PREPARE-WAVE drop
   BATCH-TEST-PAD 32000 POOL+ drop
   s" batch-tail: pool moved" T-LABEL NAME$ drop old <> TTRUE
   s" batch-tail: retained tail" T-LABEL NAME$ CHECK:BATCH-TEST-TAIL$ T$=
   1 RUN-WAVE
   0 LSP-CHECK:USES-BATCH-WAVE-LOAD
   s" batch-tail: earlier frame" T-LABEL
   0 -1 [: ;] LSP-CHECK:USES-BATCH-RESULT TTRUE
   T-REPORT ;
;package

LSP-REFERENCES:TEST-BATCH-TAIL

\ Capture three real checks, then change b.f before applying them. A's check
\ read b.f and names it as changed; b.f's captured frame must still be consumed
\ so c.f receives only its own bindings.
package LSP-REFERENCES

create BATCH-CURSOR-A FS-PATH-CAP allot
create BATCH-CURSOR-B FS-PATH-CAP allot
create BATCH-CURSOR-C FS-PATH-CAP allot
variable BATCH-CURSOR-A-U
variable BATCH-CURSOR-B-U
variable BATCH-CURSOR-C-U

: BATCH-PRIM-ROW ( -- n )
   PRIM-SPEC:COUNT 0 ?do
      i PRIM-SPEC:NAME$ s" *" STR= if i unloop exit then
   loop
   E-TBL-BOUNDS throw ;

public

: TEST-WAVE-CURSOR ( -- )
   T-RESET
   CLEANUP-RESET
   s" hb-ref-wave-cursor" HB-TMP-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-TREE+
   root rootu s" a.f" BATCH-CURSOR-A JOIN-PATH BATCH-CURSOR-A-U !
   root rootu s" b.f" BATCH-CURSOR-B JOIN-PATH BATCH-CURSOR-B-U !
   root rootu s" c.f" BATCH-CURSOR-C JOIN-PATH BATCH-CURSOR-C-U !
   BATCH-CURSOR-A BATCH-CURSOR-A-U @
   s\" require b.f\n: A ( -- n ) 2 3 * ;\n" WRITE-ALL
   BATCH-CURSOR-B BATCH-CURSOR-B-U @
   s\" : B ( -- n ) 3 4 * ;\n" WRITE-ALL
   BATCH-CURSOR-C BATCH-CURSOR-C-U @
   s\" : C ( -- n ) 4 5 * ;\n" WRITE-ALL
   FRESH
   BATCH-PRIM-ROW FROM-PRIMITIVE
   BATCH-CURSOR-A BATCH-CURSOR-A-U @ SUBJECT+
   BATCH-CURSOR-B BATCH-CURSOR-B-U @ SUBJECT+
   BATCH-CURSOR-C BATCH-CURSOR-C-U @ SUBJECT+
   0 1 PREPARE-WAVE RUN-WAVE
   BATCH-CURSOR-B BATCH-CURSOR-B-U @
   s\" : B ( -- n ) 9 8 * ;\n" WRITE-ALL
   1 APPLY-WAVE
   s" named frame was consumed" T-LABEL BATCH-FALLBACK @ 0= TTRUE
   s" final frame belongs to c.f" T-LABEL
   CHECK:VERIFY-USES$ BATCH-CURSOR-C BATCH-CURSOR-C-U @ CONTAINS? TTRUE
   CHECK:VERIFY-USES$ BATCH-CURSOR-B BATCH-CURSOR-B-U @ CONTAINS? TFALSE
   s" changed b.f was named" T-LABEL
   BATCH-CURSOR-B BATCH-CURSOR-B-U @ NAMED? TTRUE
   s" later c.f kept its own use" T-LABEL
   LOC-N @ 2 T=
   0 L-FILE LOC@ FILE$ BATCH-CURSOR-A BATCH-CURSOR-A-U @ T$=
   1 L-FILE LOC@ FILE$ BATCH-CURSOR-C BATCH-CURSOR-C-U @ T$=
   CLEANUP-RUN
   T-REPORT ;

;package

LSP-REFERENCES:TEST-WAVE-CURSOR
