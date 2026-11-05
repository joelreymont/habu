\ arm64-place-fixture.f - the words test/compiler/arm64-place.f loads through the
\ typed interpret loop at tier 0 while an arm64 shadow is open, as private words
\ of its part, which is open as they load. PL-RUN's shadow emission holds a call,
\ a tail call, a data address and a code address chain; PL-FIVE, made by the
\ does> definer PL-MAKE after it, holds a does> slot.

: PL-LEAF ( n -- n ) 3 + ;
variable PL-V
: PL-RUN ( n -- n ) PL-V @ +  PL-LEAF  ['] PL-LEAF execute  PL-LEAF ;
: PL-MAKE ( n -- ) create , does> ( -- n ) @ 1+ ;
5 PL-MAKE PL-FIVE
