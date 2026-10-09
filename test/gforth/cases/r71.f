\ does> with a structure open is refused at the token, on the checked and the
\ trusted path, as native J-DOES refuses it (C-CF-NONE).
: MK ( n -- ) dup 0 > if create , does> ( -- n ) @ then ;
5 MK X X . cr
