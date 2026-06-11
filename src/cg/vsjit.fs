\ vsjit.fs — the runtime ABSTRACT VALUE STACK for the engine's `:` compiler (the
\ vs.fs register-allocator model, re-expressed as JIT-compiler routines). While a
\ word compiles, the top of the data stack is tracked as VS entries — tag 1 = a
\ known CONSTANT (no code emitted yet), tag 0 = live in a REGISTER (x9..x15) — and
\ Lvspill materializes all of them as real [x19] pushes (bottom-up) whenever a
\ consumer needs the plain memory-stack convention (a call, control flow, `;`).
\ After a spill the VS is simply EMPTY: no third "mem" tag — spilled values are
\ ordinary stack cells. State lives in DATA header cells, reset at `:`.

require asm.fs

variable Lvspill   variable Lvlitpush   variable Lvpushc
$200 constant VSP-CELL          \ VS depth
$208 constant VRFREE-CELL       \ free-register bitmask, bit r-9 for x9..x15
$210 constant VTAG-OFF          \ 32 tag bytes   (1=con, 0=reg)
$250 constant VVAL-OFF          \ 32 value cells (constant or register number)
32   constant VSMAX
$7F  constant VRALL
$F9000260 constant W-PUSHR      \ str xR,[x19]  (or with R)

\ Lvlitpush ( x11=val ) : emit movz/movk x9,val + push — the c-lit sequence as a
\ BL-able routine (the dispatch's inline c-lit becomes a call to this).
: emit-vlitpush ( -- )
   Lvlitpush @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Lvspill ( -- ) : emit pushes for every VS entry bottom-up, then VS = empty and
\ all registers free. The one bridge from register-tracked to plain memory stack.
: emit-vspill ( -- )
   Lvspill @ LBL,
   NEWLBL {: vl :}  NEWLBL {: vd :}  NEWLBL {: vcon :}  NEWLBL {: vnext :}
   SP SP 16 SUBI,  30 SP 0 STR,
   5 0 MOVZ,                                                \ k
   vl LBL,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE vd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,        \ tag[k]
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,   \ val[k]
      7 1 CMPI,  C-EQ vcon BCOND,
         8 W-PUSHR LIT64,  9 8 11 ORR,  Lcemit @ BL,        \ str xR,[x19]
         9 W-PUSH1 LIT64,  Lcemit @ BL,  vnext B,           \ add x19,#8
      vcon LBL,  Lvlitpush @ BL,
   vnext LBL,  5 5 1 ADDI,  vl B,
   vd LBL,
   6 0 MOVZ,  6 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 DATA VRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Lvpushc ( x11=val ) : record a constant on the VS (no code); spill first if full.
: emit-vpushc ( -- )
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

: emit-vsjit ( -- )  emit-vlitpush  emit-vspill  emit-vpushc ;
