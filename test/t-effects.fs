\ t-effects.fs — effect node round-trip. Assumes config+arena+types+rows+effects-repr.
\ Build an effect ( R0 i64 -- R0 bool ) with untouched return rows R1==R1.
T{ ARENA-RESET
   0 MK-ROW TC-I64 MK-CON MK-PUSH      \ din  = R0 , i64
   0 MK-ROW TC-BOOL MK-CON MK-PUSH     \ dout = R0 , bool
   1 MK-ROW                            \ rin  = R1
   1 MK-ROW                            \ rout = R1
   MK-EFFECT                           ( e )
   dup EFF>DIN  STACK-TOP              \ -> i64
   swap EFF>DOUT STACK-TOP             \ -> bool
   -> TC-I64 MK-CON TC-BOOL MK-CON }T
T{ ARENA-RESET
   5 MK-ROW 6 MK-ROW 7 MK-ROW 8 MK-ROW MK-EFFECT
   dup EFF>RIN swap EFF>ROUT  -> 7 MK-ROW 8 MK-ROW }T
