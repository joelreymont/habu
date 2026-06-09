\ arena.fs — per-check heap (cells). Holds stack/effect/ptr/quot nodes.
\ Reset at the start of each definition check; never freed individually.

create ARENA  ARENA-SIZE cells allot
variable AP                          \ next free cell index

: ARENA-RESET  ( -- )   0 AP ! ;

\ Reserve n cells, return the base index. THROWs E-ARENA on overflow.
: ARENA-ALLOT  ( n -- idx )
   AP @ swap over +                  ( base new )
   dup ARENA-SIZE > if E-ARENA throw then
   AP !                              ( base )
   ;

: ARENA@  ( idx -- x )   cells ARENA + @ ;
: ARENA!  ( x idx -- )   cells ARENA + ! ;
