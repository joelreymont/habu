\ enums.f - checked enum defining words.

: ENUM ( n -- n )
   dup create , 1 + does> ( -- n ) @ ;

: ENUM4 ( n -- n )
   dup create , 4 + does> ( -- n ) @ ;
