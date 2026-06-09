\ t-types.fs — type terms + type-var store. Assumes config+arena+types loaded.

\ con/var round-trip
T{ TC-I64 MK-CON TERM>TAG -> T-CON }T
T{ TC-I64 MK-CON TERM>PAYLOAD -> TC-I64 }T
T{ TC-I64 MK-CON TYCON? -> true }T
T{ TC-I64 MK-CON TYVAR? -> false }T
T{ 5 MK-VAR TERM>TAG -> T-VAR }T
T{ 5 MK-VAR TERM>PAYLOAD -> 5 }T
T{ 0 MK-VAR TYVAR? -> true }T

\ ptr round-trip through the arena
T{ ARENA-RESET TC-U8 MK-CON MK-PTR TERM>TAG -> T-PTR }T
T{ ARENA-RESET TC-U8 MK-CON MK-PTR PTR>INNER -> TC-U8 MK-CON }T

\ quot tag round-trip (eff is just an index here)
T{ 7 MK-QUOT TERM>TAG -> T-QUOT }T
T{ 7 MK-QUOT QUOT>EFF -> 7 }T

\ TV store + RESOLVE-TYPE
T{ TV-RESET 3 MK-VAR RESOLVE-TYPE -> 3 MK-VAR }T                       \ unbound -> self
T{ TV-RESET TC-BOOL MK-CON 3 TV! 3 MK-VAR RESOLVE-TYPE -> TC-BOOL MK-CON }T
T{ TV-RESET 1 MK-VAR 0 TV! TC-I64 MK-CON 1 TV! 0 MK-VAR RESOLVE-TYPE -> TC-I64 MK-CON }T  \ chain
T{ TC-I64 MK-CON RESOLVE-TYPE -> TC-I64 MK-CON }T

\ TV-ALLOC clears its block and advances the high-water mark
T{ TV-RESET 4 TV-ALLOC -> 0 }T
T{ TV-NEXT @ -> 4 }T
T{ 0 TV@ -> UNBOUND }T
T{ 3 TV@ -> UNBOUND }T
T{ 2 TV-ALLOC -> 4 }T
T{ TV-NEXT @ -> 6 }T
T{ TV-RESET MAX-TV 1+ ' TV-ALLOC catch nip -> E-TOOMANYVARS }T
