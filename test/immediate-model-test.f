\ immediate-model-test.f - immediates are compile-time effects, not body steps.
\ Dot habu-checker-fitting-arity-70dc94e4 (docs/typed-top-level.md section 5
\ sub-dot 1, probe p5): an IMMEDIATE word whose declared arity fits certifies
\ inside a checked body, but the engine executes it at COMPILE time on the
\ interpret stack, so the published certificate describes an EMPTY runtime
\ body. Checked callers then unify against effects the runtime never delivers.
\ The checker must reject an immediate word as a checked body step unless its
\ compile-time expansion is explicitly modeled (trust-immediate).
\ Run: bin/hb --load test/immediate-model-test.f

require lib/test.f
require test/checker-assert.f

T-RESET

: IMT-REJECTS ( ptr u8 n -- )
   2dup T-LABEL
   CHECK-QUIET-CANDIDATE! 0 T= ;

TRUSTED: IMT-PASSES ( ptr u8 n -- )
   2dup T-LABEL
   CHECK! -1 T= ;

\ --- p5 reproducer: fitting-arity immediate called in a checked body --------
: IMT-IM2 ( n -- n n ) dup ; immediate

\ negative regression: the p5 shape must REJECT. The runtime body of IMT-USER
\ would be empty while its certificate claims ( n -- n n ).
s" IMT-USER ( n -- n n ) IMT-IM2" IMT-REJECTS

\ a fitting no-op immediate must also reject without an audited expansion row:
\ the checker cannot prove the compile-time expansion is empty.
: IMT-NOP ( -- ) ; immediate
s" IMT-U2 ( -- ) IMT-NOP" IMT-REJECTS

T-REPORT
