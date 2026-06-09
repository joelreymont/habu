\ t-arena.fs — arena alloc/reset/store/overflow. Assumes config+arena loaded.

\ fresh arena hands out increasing indices from 0
T{ ARENA-RESET 3 ARENA-ALLOT -> 0 }T
T{ 2 ARENA-ALLOT -> 3 }T
T{ AP @ -> 5 }T
\ store/read a cell at an allotted index
T{ ARENA-RESET 1 ARENA-ALLOT  ( idx ) dup 99 swap ARENA!  ARENA@ -> 99 }T
\ reset reclaims everything
T{ ARENA-RESET AP @ -> 0 }T
\ overflow throws E-ARENA, no truncation
T{ ARENA-RESET ARENA-SIZE 1+ ' ARENA-ALLOT catch nip -> E-ARENA }T
\ exact-fit does not throw
T{ ARENA-RESET ARENA-SIZE ARENA-ALLOT -> 0 }T
