\ target-named.f - target words a build names and never runs, one holding a
\ quotation and one a does> definer; FO-AFTER, defined and run after a file that
\ ends in a target section, is a host word. A Wasm build exits 0.

require test/fixtures/file-objects/target-tail.f
: FO-AFTER ( -- n ) 7 ;
FO-AFTER drop

target
: FO-TARGET ( -- ) ;
: FO-QUOTED ( -- ) [: FO-TARGET ;] execute ;
: FO-MAKER ( n -- ) create , does> ( -- n ) @ ;
host

: FO-CALLS ( -- ) FO-TARGET FO-QUOTED ;
' FO-TARGET drop

: MAIN ( -- ) FO-CALLS ;
