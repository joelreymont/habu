\ vsjit.fs — runtime abstract value stack for the `:` compiler, transcribed from
\ src/cg/vsjit.fs for the engine-builder port (lockstep; goldens enforce parity).
\ Tag 1 = constant (no code yet), tag 0 = live register; Lvspill materializes all
\ entries as [x19] pushes bottom-up and empties the VS. State in DATA header cells.
\ Load after prof.fs, before engine2.fs.
variable Lvspill   variable Lvlitpush   variable Lvpushc
$200 constant VSP-CELL
$208 constant VRFREE-CELL
$210 constant VTAG-OFF
$250 constant VVAL-OFF
32   constant VSMAX
$7F  constant VRALL
$F9000260 constant W-PUSHR
: emit-vlitpush
   Lvlitpush @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
: emit-vspill
   Lvspill @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL {: vl vd vcon vnext :}
   SP SP 16 SUBI,  30 SP 0 STR,
   5 0 MOVZ,
   vl LBL,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE vd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 1 CMPI,  C-EQ vcon BCOND,
         8 W-PUSHR LIT64,  9 8 11 ORR,  Lcemit @ BL,
         9 W-PUSH1 LIT64,  Lcemit @ BL,  vnext B,
      vcon LBL,  Lvlitpush @ BL,
   vnext LBL,  5 5 1 ADDI,  vl B,
   vd LBL,
   6 0 MOVZ,  6 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 DATA VRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
: emit-vpushc
   Lvpushc @ LBL,
   NEWLBL {: room :}
   SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT room BCOND,
      Lvspill @ BL,  6 0 MOVZ,
   room LBL,
   11 SP 8 LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 1 MOVZ,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
: emit-vsjit  emit-vlitpush  emit-vspill  emit-vpushc ;
