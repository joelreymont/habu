\ target-run.f - line 9 runs a host word that calls a target word: a Wasm build
\ refuses FO-TARGET by name there.

target
: FO-TARGET ( -- ) ;
host

: FO-CALLS ( -- ) FO-TARGET ;
FO-CALLS

: MAIN ( -- ) ;
