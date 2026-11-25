\ enums.f - numeric enum counter definers. The bare ENUM token belongs to the
\ block-style ENUM ... ;ENUM type family (src/core/enum-decl.f).

: ENUM+ ( n -- n )
   dup create , 1 + does> ( -- n ) @ ;

: ENUM4+ ( n -- n )
   dup create , 4 + does> ( -- n ) @ ;
