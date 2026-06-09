\ t-rows.fs — stack terms + row-var store. Assumes config+arena+types+rows.

\ row var round-trip; a row-term cell is never 0 (UNBOUND) even for id 0
T{ 0 MK-ROW SROW? -> true }T
T{ 0 MK-ROW 0<> -> true }T
T{ 4 MK-ROW TERM>PAYLOAD -> 4 }T
T{ 4 MK-ROW SPUSH? -> false }T

\ push round-trip:  ( R0 , i64 )  -> rest=R0, top=i64
T{ ARENA-RESET  0 MK-ROW  TC-I64 MK-CON  MK-PUSH  SPUSH? -> true }T
T{ ARENA-RESET  0 MK-ROW  TC-I64 MK-CON  MK-PUSH  STACK-REST -> 0 MK-ROW }T
T{ ARENA-RESET  0 MK-ROW  TC-I64 MK-CON  MK-PUSH  STACK-TOP  -> TC-I64 MK-CON }T

\ two-deep push: ( R0 , bool , i64 ), top = i64, second = bool
T{ ARENA-RESET  0 MK-ROW  TC-BOOL MK-CON MK-PUSH  TC-I64 MK-CON MK-PUSH
   dup STACK-TOP  swap STACK-REST STACK-TOP  -> TC-I64 MK-CON TC-BOOL MK-CON }T

\ RV store + RESOLVE-ROW
T{ RV-RESET 2 MK-ROW RESOLVE-ROW -> 2 MK-ROW }T                 \ unbound -> self
T{ RV-RESET  5 MK-ROW 2 RV!  2 MK-ROW RESOLVE-ROW -> 5 MK-ROW }T \ bound -> target
T{ ARENA-RESET RV-RESET                                          \ row resolves into a push
   0 MK-ROW TC-I64 MK-CON MK-PUSH 1 RV!
   1 MK-ROW RESOLVE-ROW SPUSH? -> true }T

\ RV-ALLOC clears + advances; overflow throws
T{ RV-RESET 3 RV-ALLOC -> 0 }T
T{ RV-NEXT @ -> 3 }T
T{ 0 RV@ -> UNBOUND }T
T{ RV-RESET MAX-RV 1+ ' RV-ALLOC catch nip -> E-TOOMANYVARS }T
