\ A second does> inside an open structure is refused for the structure, as
\ native J-DOES checks the structure at the keyword and has no second-does> gate.
: MK ( n -- ) create , does> ( -- n ) @ dup 0 > if does> ( -- n ) @ then ;
5 MK X X . cr
