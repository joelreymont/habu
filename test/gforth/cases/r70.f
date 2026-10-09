\ does> is a keyword in any case, on the checked and the trusted path, as native
\ folds every word it reads.
: MK ( n -- ) create , DOES> ( -- n ) @ ;
trusted: TK ( n -- ) create , Does> ( -- n ) @ 1 + ;
5 MK X  X . cr
7 TK Y  Y . cr
