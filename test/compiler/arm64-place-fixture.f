\ arm64-place-fixture.f - the words test/compiler/arm64-place.f loads through the
\ typed interpret loop at tier 0 while an arm64 shadow is open, as private words
\ of its part, which is open as they load. PL-RUN's shadow emission holds a call,
\ a tail call, a data address and a code address chain; PL-TICK reaches PL-TOCK
\ only through a code address chain. PL-FIVE, made by the does> definer PL-MAKE,
\ holds a does> slot; PL-MAKE's own body calls PL-INIT.

: PL-LEAF ( n -- n ) 3 + ;
variable PL-V
: PL-RUN ( n -- n ) PL-V @ +  PL-LEAF  ['] PL-LEAF execute  PL-LEAF ;
: PL-TOCK ( n -- n ) 7 + ;
: PL-TICK ( n -- n ) ['] PL-TOCK execute ;
: PL-INIT ( n -- n ) 1 + ;
: PL-MAKE ( n -- ) PL-INIT create , does> ( -- n ) @ 1+ ;
4 PL-MAKE PL-FIVE
