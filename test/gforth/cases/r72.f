\ The trusted path refuses does> with a structure open before Gforth sees it.
trusted: MK ( n -- ) dup 0 > if create , does> ( -- n ) @ then ;
5 MK X X . cr
