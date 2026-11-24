\ target-run.f - line 8 runs a host word that calls target-tail.f's target word;
\ target-named.f's build wrote that file's object, so this build links it, and a
\ Wasm build refuses FO-TAIL by name there.

require test/fixtures/file-objects/target-tail.f

: FO-CALLS ( -- ) FO-TAIL ;
FO-CALLS

: MAIN ( -- ) ;
