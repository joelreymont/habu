\ structures.f - checked structure and field defining words.

$8 constant CELL
$4C constant STRUCT-RC

variable STRUCT-ACTIVE

: STRUCT-BYTE+ ( ptr a n -- ptr u8 )
   + ;

: STRUCT-REQUIRE-CLOSED ( -- )
   STRUCT-ACTIVE @ if s" structure: nested begin" STRUCT-RC die then ;

: STRUCT-REQUIRE-OPEN ( -- )
   STRUCT-ACTIVE @ 0= if s" structure: no active structure" STRUCT-RC die then ;

: BEGIN-STRUCTURE ( -- ptr a n )
   STRUCT-REQUIRE-CLOSED
   -1 STRUCT-ACTIVE !
   create here 0 , 0 does> ( -- n ) @ ;

: +FIELD ( ptr a n n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   create over , + does> ( ptr a -- ptr a ) @ + ;

: PTR-FIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup CELL mod 0 <> if s" structure: pointer field alignment" STRUCT-RC die then
   create dup CELL / , CELL + does> ( ptr a -- ptr ptr a ) @ ptr-field ;

: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

: CFIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   create dup , 1 + does> ( ptr a -- ptr u8 ) @ STRUCT-BYTE+ ;

: END-STRUCTURE ( ptr a n -- )
   STRUCT-REQUIRE-OPEN
   0 STRUCT-ACTIVE !
   swap ! ;
