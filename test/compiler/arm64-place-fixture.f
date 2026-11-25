\ arm64-place-fixture.f - the words test/compiler/arm64-place.f and
\ test/compiler/arm64-place-unsafe-bad.f include in a stripped build's body
\ before they write a program. PL-RUN's shadow emission holds a call, a tail
\ call, a data address and a code address chain; PL-TICK reaches PL-TOCK only
\ through a code address chain. PL-FIVE, made by the does> definer PL-MAKE,
\ holds a does> slot; PL-MAKE's own body calls PL-INIT. PL-LEAF and PL-INIT read
\ PL-V, so a mention calls them rather than compiling their steps in place.

variable PL-V
: PL-LEAF ( n -- n ) PL-V @ + 2 + ;
: PL-RUN ( n -- n ) PL-V @ +  PL-LEAF  ['] PL-LEAF execute  PL-LEAF ;
: PL-TOCK ( n -- n ) 7 + ;
: PL-TICK ( n -- n ) ['] PL-TOCK execute ;
: PL-INIT ( n -- n ) PL-V @ + 1+ ;
: PL-MAKE ( n -- ) PL-INIT create , does> ( -- n ) @ 1+ ;
4 PL-MAKE PL-FIVE
