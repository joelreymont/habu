\ structures.f - checked structure and field defining words.

$4C constant STRUCT-RC

variable STRUCT-ACTIVE

TRUSTED: STRUCT-BYTE+ ( ptr a n -- ptr u8 )
   + ;

: STRUCT-REQUIRE-CLOSED ( -- )
   STRUCT-ACTIVE @ if s" structure: nested begin" STRUCT-RC die then ;

: STRUCT-REQUIRE-OPEN ( -- )
   STRUCT-ACTIVE @ 0= if s" structure: no active structure" STRUCT-RC die then ;

: BEGIN-STRUCTURE ( -- ptr n n )
   STRUCT-REQUIRE-CLOSED
   -1 STRUCT-ACTIVE !
   create here 0 , 0 does> ( -- n ) @ ;

: +FIELD ( ptr a n n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   create over , + does> ( ptr a -- ptr a ) @ + ;

: PTR-FIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup CELL mod 0 <> if s" structure: pointer field alignment" STRUCT-RC die then
   \ `ptr ptr b`, not `ptr ptr a`: the pointer a field HOLDS is independent of the
   \ record's own element type, so a cell record can store a byte pointer. This
   \ matches the `ptr-field` primitive, which is ( ptr a n -- ptr ptr b ).
   create dup CELL / , CELL + does> ( ptr a -- ptr ptr b ) @ ptr-field ;

: CFIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   create dup , 1 + does> ( ptr a -- ptr u8 ) @ STRUCT-BYTE+ ;

: END-STRUCTURE ( ptr n n -- )
   STRUCT-REQUIRE-OPEN
   0 STRUCT-ACTIVE !
   swap ! ;
