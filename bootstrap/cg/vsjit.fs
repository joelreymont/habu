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
variable Lvtop2c   variable Lvfoldput
variable Lvralloc  variable Lvmovk  variable Lvforcek  variable Lvbinprep  variable Lvpushr
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
   14 16 MOVZ,  Lvmovk @ BL,                            \ movz/movk x16,val (x16: never pooled)
   9 $F9000270 LIT64,  Lcemit @ BL,                     \ str x16,[x19]
   9 W-PUSH1 LIT64,  Lcemit @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Lvspill ( -- ) : emit pushes for every VS entry bottom-up, then VS = empty and
\ all registers free. The one bridge from register-tracked to plain memory stack.
: emit-vspill ( -- )
   Lvspill @ LBL,
   NEWLBL {: vl :}  NEWLBL {: vd :}  NEWLBL {: vcon :}  NEWLBL {: vnext :}
   SP SP 16 SUBI,  30 SP 0 STR,
   5 0 MOVZ,  5 SP 8 STR,                                   \ k (in the frame: the
   vl LBL,                                                  \ helper calls clobber x5)
      5 SP 8 LDR,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE vd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,        \ tag[k]
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,   \ val[k]
      7 1 CMPI,  C-EQ vcon BCOND,
         8 W-PUSHR LIT64,  9 8 11 ORR,  Lcemit @ BL,        \ str xR,[x19]
         9 W-PUSH1 LIT64,  Lcemit @ BL,  vnext B,           \ add x19,#8
      vcon LBL,  Lvlitpush @ BL,
   vnext LBL,  5 SP 8 LDR,  5 5 1 ADDI,  5 SP 8 STR,  vl B,
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


\ Lvtop2c ( -- x13=ok x11=a x12=b ) : are the top two VS entries constants? (no pop)
: emit-vtop2c ( -- )
   Lvtop2c @ LBL,
   NEWLBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   13 1 MOVZ,
   no LBL,  RET, ;
\ Lvfoldput ( x11=result ) : val[VSP-2] = result (still con), VSP--
: emit-vfoldput ( -- )
   Lvfoldput @ LBL,
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  RET, ;
variable Lkwplus  variable Lkwminus  variable Lkwstar
variable Lkwand2  variable Lkwor2   variable Lkwxor2
variable FESK
\ fold-entry: if the token is this operator AND the top two VS entries are
\ constants, fold at JIT time (no code) and continue the main loop; else fall
\ through to the generic dispatch (which spills + calls the prim).
: fold-entry {: lmainlbl kwvar kwlen fxt :}
   NEWLBL FESK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK @ CBZ,
   Lvtop2c @ BL,  13 FESK @ CBZ,
   fxt execute
   Lvfoldput @ BL,
   lmainlbl B,
   FESK @ LBL, ;
: f+ 11 11 12 ADD, ;   : f- 11 11 12 SUB, ;   : f* 11 11 12 MUL, ;
: fand 11 11 12 AND, ;  : for2 11 11 12 ORR, ;  : fxor2 11 11 12 EOR, ;
: emit-foldkw
   Lkwplus @ LBL,  s" +" BYTES,    Lkwminus @ LBL,  s" -" BYTES,
   Lkwstar @ LBL,  s" *" BYTES,    Lkwand2 @ LBL,   s" and" BYTES,
   Lkwor2 @ LBL,   s" or" BYTES,   Lkwxor2 @ LBL,   s" xor" BYTES, ;
variable Lvtop1c
\ Lvtop1c ( -- x13=ok x11=top ) : is the top VS entry a constant? (no pop)
: emit-vtop1c
   Lvtop1c @ LBL,
   NEWLBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 1 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   13 1 MOVZ,
   no LBL,  RET, ;
variable Lkwdup2  variable Lkwdrop2  variable Lkwswap2  variable Lkwover2  variable Lkwnip2
\ shuf entries: VS-resident stack ops on constant entries (no code emitted at all —
\ a dropped literal vanishes; swap/over/nip are relabels). Non-con falls through.
: shuf1-entry {: lmainlbl kwvar kwlen sxt :}
   NEWLBL FESK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK @ CBZ,
   Lvtop1c @ BL,  13 FESK @ CBZ,
   sxt execute
   lmainlbl B,
   FESK @ LBL, ;
: shuf2-entry {: lmainlbl kwvar kwlen sxt :}
   NEWLBL FESK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK @ CBZ,
   Lvtop2c @ BL,  13 FESK @ CBZ,
   sxt execute
   lmainlbl B,
   FESK @ LBL, ;
: sdup   Lvpushc @ BL, ;                                  \ x11 = top from Lvtop1c
: sdrop  6 DATA VSP-CELL LDR,  6 6 1 SUBI,  6 DATA VSP-CELL STR, ;
: sswap                                                   \ x11=a x12=b -> store b a
   6 DATA VSP-CELL LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   5 6 2 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 STR, ;
: sover  Lvpushc @ BL, ;                                  \ x11 = a (deep) from Lvtop2c
: snip   11 12 0 ADDI,  Lvfoldput @ BL, ;                 \ keep top, drop deep
: emit-shufkw
   Lkwdup2 @ LBL,   s" dup" BYTES,    Lkwdrop2 @ LBL,  s" drop" BYTES,
   Lkwswap2 @ LBL,  s" swap" BYTES,   Lkwover2 @ LBL,  s" over" BYTES,
   Lkwnip2 @ LBL,   s" nip" BYTES, ;
\ Lvralloc ( -- x14=reg | 0 ) : grab a free register from the x9..x15 bitmask
: emit-vralloc
   Lvralloc @ LBL,
   NEWLBL NEWLBL NEWLBL {: rl rgot rno :}
   6 DATA VRFREE-CELL LDR,  5 0 MOVZ,
   rl LBL,
      5 7 CMPI,  C-GE rno BCOND,
      7 6 5 LSRV,  7 7 $1000 ANDI,  7 rgot CBNZ,
      5 5 1 ADDI,  rl B,
   rno LBL,  14 0 MOVZ,  RET,
   rgot LBL,
      7 1 MOVZ,  7 7 5 LSLV,  6 6 7 EOR,  6 DATA VRFREE-CELL STR,
      14 5 9 ADDI,  RET, ;
\ Lvmovk ( x11=val x14=rd ) : emit movz/movk chain targeting rd (no push)
: emit-vmovk
   Lvmovk @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 $D2800000 LIT64,  8 8 14 ORR,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00000 LIT64,  8 8 14 ORR,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00000 LIT64,  8 8 14 ORR,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2E00000 LIT64,  8 8 14 ORR,  9 8 7 ORR,  Lcemit @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
\ Lvforcek ( x5=k -- x14=reg | 0 ) : force VS entry k into a register, in place.
\ Atomic: an allocation failure mutates nothing.
: emit-vforcek
   Lvforcek @ LBL,
   NEWLBL NEWLBL {: fr fd :}
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 fr CBZ,                                            \ tag 0 = already a register
   11 SP 16 STR,
   Lvralloc @ BL,  14 fd CBZ,                           \ no reg -> 0 (nothing mutated)
   11 SP 16 LDR,  14 SP 24 STR,
   Lvmovk @ BL,                                         \ emit movz/movk rd,val
   14 SP 24 LDR,  5 SP 8 LDR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   fd B,
   fr LBL,  14 11 0 ADDI,
   fd LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
\ Lvbinprep ( -- x13=mode ) : 0 fall-through; 1 fold (x11=a x12=b, VS untouched);
\ 2 registers ready (x14=rd result slot, x15=rm; rm freed; VSP already --).
: emit-vbinprep
   Lvbinprep @ LBL,
   NEWLBL NEWLBL NEWLBL {: bno bfold b2 :}
   SP SP 32 SUBI,  30 SP 0 STR,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT bno BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDRB,
   7 7 8 AND,  7 1 CMPI,  C-EQ bfold BCOND,
   \ register path: force deep then top
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  Lvforcek @ BL,  14 bno CBZ,
   14 SP 8 STR,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  Lvforcek @ BL,  14 bno CBZ,
   15 14 0 ADDI,  14 SP 8 LDR,
   \ free rm's bit; VSP--  (entry[VSP-2] already = reg rd via force-in-place)
   7 15 9 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
   6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
   6 DATA VSP-CELL LDR,  6 6 1 SUBI,  6 DATA VSP-CELL STR,
   13 2 MOVZ,  b2 B,
   bfold LBL,
   6 DATA VSP-CELL LDR,
   5 6 2 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   13 1 MOVZ,  b2 B,
   bno LBL,  13 0 MOVZ,
   b2 LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
\ Lvpushr ( x14=reg ) : push a register entry (spill-on-full keeps x14 claimed)
: emit-vpushr
   Lvpushr @ LBL,
   NEWLBL {: pr :}
   SP SP 16 SUBI,  30 SP 0 STR,  14 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT pr BCOND,
      Lvspill @ BL,
      14 SP 8 LDR,  7 14 9 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
      6 DATA VRFREE-CELL LDR,  6 6 8 EOR,  6 DATA VRFREE-CELL STR,   \ re-claim x14
   pr LBL,
   14 SP 8 LDR,  6 DATA VSP-CELL LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
variable FESK2
\ vop-entry: fold when both con, register op when forceable, else fall through
: vop-entry {: lmainlbl kwvar kwlen foldxt emitxt :}
   NEWLBL FESK !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK @ CBZ,
   Lvbinprep @ BL,
   13 FESK @ CBZ,
   13 1 CMPI,  C-NE FESK2 @ BCOND,
      foldxt execute
      Lvfoldput @ BL,
      lmainlbl B,
   FESK2 @ LBL,
      emitxt execute
      9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  Lcemit @ BL,
      lmainlbl B,
   FESK @ LBL, ;
: e+   8 $8B000000 LIT64, ;   : e-   8 $CB000000 LIT64, ;
: e*   8 $9B007C00 LIT64, ;   : eand 8 $8A000000 LIT64, ;
: eor2 8 $AA000000 LIT64, ;   : exor 8 $CA000000 LIT64, ;
variable Lkweq2  variable Lkwne2  variable Lkwlt2  variable Lkwgt2  variable Lkwle2  variable Lkwge2
\ comparison entry: fold -> dispatch computes the flag; registers -> emit
\ cmp rd,rm ; cset rd,cond ; sub rd,xzr,rd  (Forth flag 0/-1)
: vcmp-entry {: lmainlbl kwvar kwlen cond :}
   NEWLBL FESK !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK @ CBZ,
   Lvbinprep @ BL,
   13 FESK @ CBZ,
   13 1 CMPI,  C-NE FESK2 @ BCOND,
      11 12 CMP,  11 cond CSET,  11 SP 11 SUB,
      Lvfoldput @ BL,
      lmainlbl B,
   FESK2 @ LBL,
      8 $EB00001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  Lcemit @ BL,
      8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  Lcemit @ BL,
      8 $CB0003E0 LIT64,  9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  Lcemit @ BL,
      lmainlbl B,
   FESK @ LBL, ;
: emit-cmpkw
   Lkweq2 @ LBL,  s" =" BYTES,    Lkwne2 @ LBL,  s" <>" BYTES,
   Lkwlt2 @ LBL,  s" <" BYTES,    Lkwgt2 @ LBL,  s" >" BYTES,
   Lkwle2 @ LBL,  s" <=" BYTES,   Lkwge2 @ LBL,  s" >=" BYTES, ;
: emit-vsjit ( -- )  emit-vlitpush  emit-vspill  emit-vpushc  emit-vtop2c  emit-vfoldput  emit-vtop1c
   emit-vralloc  emit-vmovk  emit-vforcek  emit-vbinprep  emit-vpushr ;
