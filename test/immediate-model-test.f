\ immediate-model-test.f - immediates are compile-time effects, not body steps.
\ Dot habu-checker-fitting-arity-70dc94e4 (docs/typed-top-level.md section 5
\ sub-dot 1, probe p5): an IMMEDIATE word whose declared arity fits certifies
\ inside a checked body, but the engine executes it at COMPILE time on the
\ interpret stack, so the published certificate describes an EMPTY runtime
\ body. Checked callers then unify against effects the runtime never delivers.
\ The checker must reject an immediate word as a checked body step unless its
\ compile-time expansion is explicitly modeled by PARSE-IMM/REPLAY-IMM.
\ Run: bin/hb --load test/immediate-model-test.f

require lib/test.f
require test/checker-assert.f

package IMT
private

T-RESET

: REJECTS ( ptr u8 n -- )
   2dup T-LABEL
   CHECK-QUIET-CANDIDATE! 0 T= ;

TRUSTED: PASSES ( ptr u8 n -- )
   2dup T-LABEL
   CHECK! -1 T= ;

\ --- p5 reproducer: fitting-arity immediate called in a checked body --------
: IM2 ( n -- n n ) dup ; immediate

\ negative regression: the p5 shape must REJECT. The runtime body of IMT-USER
\ would be empty while its certificate claims ( n -- n n ).
s" USER ( n -- n n ) IM2" REJECTS

\ a fitting no-op immediate must also reject without an audited expansion row:
\ the checker cannot prove the compile-time expansion is empty.
: NOP ( -- ) ; immediate
s" USER2 ( -- ) NOP" REJECTS

\ The stack-string loaders are runtime effects, not parsing immediates. Their
\ checked helpers remain valid; only actual execution performs source loading.
s" INCLUDED-USER ( ptr u8 n -- ) included" PASSES
s" REQUIRED-USER ( ptr u8 n -- ) required" PASSES
s" PROVIDED-USER ( ptr u8 n -- ) provided" PASSES

\ A declared fixed expansion is the explicit checked compile-time boundary.
: MODELED ( -- ) ; immediate
s" MODELED" 0 PARSE-IMM
: MODELED-USER ( -- ) MODELED ;

\ A modeled immediate's declared signature constrains its compile-time
\ execution only. Its checked body occurrence contributes no runtime effect.
: IM ( n -- n n ) dup ; immediate
s" IM" 0 PARSE-IMM
s" BAD ( n -- n n ) IM" REJECTS
s" USE ( n -- n ) IM" PASSES

T-REPORT

;package
