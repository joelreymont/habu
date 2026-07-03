\ enums.f - checked legacy numeric enum counter definers.
\ ENUM+/ENUM4+ thread a running counter: each defines the next name as the
\ current value and returns value+1 / value+4. The bare ENUM token is reserved
\ for the block-style ENUM ... END-ENUM type family (PLAN.md item 14).

: ENUM+ ( n -- n )
   dup create , 1 + does> ( -- n ) @ ;

: ENUM4+ ( n -- n )
   dup create , 4 + does> ( -- n ) @ ;
