\ jit.fs — runtime abstract value stack for the `:` compiler, transcribed from
\ bootstrap/cg/jit.fs for the engine-builder port (lockstep; goldens enforce parity).
\ Tag 1 = constant (no code yet), tag 0 = live register; LVSPILL materializes all
\ entries as [x19] pushes bottom-up and empties the VS. State in DATA header cells.
\ Load after prof.fs, before habu2.f.
variable LVSPILL   variable LVLITPUSH   variable LVPUSHC
variable LVTOP2C   variable LVFOLDPUT
variable LVMOVK  variable LVFORCEK  variable LVBINPREP  variable LVPUSHR
$200 constant VSP-CELL
$210 constant VTAG-OFF
$250 constant VVAL-OFF
32   constant VSMAX
$F9000260 constant W-PUSHR

: EMIT-VLITPUSH
   LVLITPUSH @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   14 16 MOVZ,  LVMOVK @ BL,                            \ movz/movk x16,val (x16: never pooled)
   9 $F9000270 LIT64,  LCEMIT @ BL,                     \ str x16,[x19]
   9 W-PUSH1 LIT64,  LCEMIT @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-VSPILL
   LVSPILL @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL {: vl vd vcon vnext :}
   SP SP 16 SUBI,  30 SP 0 STR,
   5 0 MOVZ,  5 SP 8 STR,                                   \ k (in the frame: the
   vl LBL,                                                  \ helper calls clobber x5)
      5 SP 8 LDR,
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE vd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,        \ tag[k]
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,   \ val[k]
      7 1 CMPI,  C-EQ vcon BCOND,
         8 W-PUSHR LIT64,  9 8 11 ORR,  LCEMIT @ BL,        \ str xR,[x19]
         9 W-PUSH1 LIT64,  LCEMIT @ BL,  vnext B,           \ add x19,#8
      vcon LBL,  LVLITPUSH @ BL,
   vnext LBL,  5 SP 8 LDR,  5 5 1 ADDI,  5 SP 8 STR,  vl B,
   vd LBL,
   6 0 MOVZ,  6 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 DATA VRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-VPUSHC
   LVPUSHC @ LBL,
   NEWLBL {: room :}
   SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT room BCOND,
      LVSPILL @ BL,  6 0 MOVZ,
   room LBL,
   11 SP 8 LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 1 MOVZ,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVTOP2C ( -- x13=ok x11=a x12=b ) : are the top two VS entries constants? (no pop)
: EMIT-VTOP2C
   LVTOP2C @ LBL,
   NEWLBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 1 CMPI,  C-NE no BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   5 6 1 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   13 1 MOVZ,
   no LBL,  RET, ;

\ LVFOLDPUT ( x11=result ) : val[VSP-2] = result (still con), VSP--
: EMIT-VFOLDPUT
   LVFOLDPUT @ LBL,
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 STR,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  RET, ;
variable LKWPLUS  variable LKWMINUS  variable LKWSTAR
variable LKWAND2  variable LKWOR2   variable LKWXOR2
variable FESK

\ fold-entry: if the token is this operator AND the top two VS entries are
\ constants, fold at JIT time (no code) and continue the main loop; else fall
\ through to the generic dispatch (which spills + calls the prim).
: FOLD-ENTRY {: lmainlbl kwvar kwlen fxt :}
   NEWLBL FESK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 FESK @ CBZ,
   LVTOP2C @ BL,  13 FESK @ CBZ,
   fxt execute
   LVFOLDPUT @ BL,
   lmainlbl B,
   FESK @ LBL, ;
s" fold-entry" s" n n n n --" TRUST

: VF+ 11 11 12 ADD, ;   \ fold helpers — NOT f+/f-/f*: those are the FLOAT prims

: VF- 11 11 12 SUB, ;

: VF* 11 11 12 MUL, ;

: FAND 11 11 12 AND, ;

: FOR2 11 11 12 ORR, ;

: FXOR2 11 11 12 EOR, ;

: EMIT-FOLDKW
   LKWPLUS @ LBL,  s" +" BYTES,    LKWMINUS @ LBL,  s" -" BYTES,
   LKWSTAR @ LBL,  s" *" BYTES,    LKWAND2 @ LBL,   s" and" BYTES,
   LKWOR2 @ LBL,   s" or" BYTES,   LKWXOR2 @ LBL,   s" xor" BYTES, ;
variable LKWDUP2  variable LKWDROP2  variable LKWSWAP2  variable LKWOVER2  variable LKWNIP2

: EMIT-SHUFKW
   LKWDUP2 @ LBL,   s" dup" BYTES,    LKWDROP2 @ LBL,  s" drop" BYTES,
   LKWSWAP2 @ LBL,  s" swap" BYTES,   LKWOVER2 @ LBL,  s" over" BYTES,
   LKWNIP2 @ LBL,   s" nip" BYTES, ;

\ LVMOVK ( x11=val x14=rd ) : emit a MINIMAL movz/movn + movk chain targeting rd —
\ movn form when $FFFF chunks dominate; chunks the base op already set are skipped.
\ x5=k x6=val x7=nz/started x8=nf/chunk x9=instr x10=form x12=$FFFF (Lcemit saves all).
: EMIT-VMOVK
   LVMOVK @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL
   {: cl cd ml mk mn mset mnext md mz1 mout :}
   SP SP 16 SUBI,  30 SP 0 STR,
   6 11 0 ADDI,  12 $FFFF MOVZ,
   7 0 MOVZ,  8 0 MOVZ,  5 0 MOVZ,
   cl LBL,
      5 4 CMPI,  C-GE cd BCOND,
      9 5 4 LSLI,  10 6 9 LSRV,  10 10 12 AND,
      10 0 CMPI,  9 1 CSET,  7 7 9 ADD,
      10 12 CMP,  9 1 CSET,  8 8 9 ADD,
      5 5 1 ADDI,  cl B,
   cd LBL,
   8 7 CMP,  10 11 CSET,
   7 0 MOVZ,  5 0 MOVZ,
   ml LBL,
      5 4 CMPI,  C-GE md BCOND,
      9 5 4 LSLI,  8 6 9 LSRV,  8 8 12 AND,
      9 12 10 MUL,  8 9 CMP,  C-EQ mnext BCOND,
      7 mk CBNZ,
      10 mn CBNZ,
         9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR,
         8 $D2800000 LIT64,  9 9 8 ORR,  LCEMIT @ BL,  mset B,
      mn LBL,
         8 8 12 EOR,
         9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR,
         8 $92800000 LIT64,  9 9 8 ORR,  LCEMIT @ BL,
      mset LBL,  7 1 MOVZ,  mnext B,
      mk LBL,
         9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR,
         8 $F2800000 LIT64,  9 9 8 ORR,  LCEMIT @ BL,
   mnext LBL,  5 5 1 ADDI,  ml B,
   md LBL,
   7 mout CBNZ,
   10 mz1 CBNZ,
      8 $D2800000 LIT64,  9 8 14 ORR,  LCEMIT @ BL,  mout B,
   mz1 LBL,
      8 $92800000 LIT64,  9 8 14 ORR,  LCEMIT @ BL,
   mout LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVFORCEK ( x5=k -- x14=reg | 0 ) : force VS entry k into a register, in place.
\ Atomic: an allocation failure mutates nothing.
: EMIT-VFORCEK
   LVFORCEK @ LBL,
   NEWLBL NEWLBL {: fr fd :}
   SP SP 32 SUBI,  30 SP 0 STR,  5 SP 8 STR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 fr CBZ,                                            \ tag 0 = already a register
   11 SP 16 STR,
   LVRALLOC @ BL,  14 fd CBZ,                           \ no reg -> 0 (nothing mutated)
   11 SP 16 LDR,  14 SP 24 STR,
   LVMOVK @ BL,                                         \ emit movz/movk rd,val
   14 SP 24 LDR,  5 SP 8 LDR,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   fd B,
   fr LBL,  14 11 0 ADDI,
   fd LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ LVBINPREP ( -- x13=mode ) : 0 fall-through; 1 fold (x11=a x12=b, VS untouched);
\ 2 registers ready (x14=rd result slot, x15=rm; rm freed; VSP already --).
: EMIT-VBINPREP
   LVBINPREP @ LBL,
   NEWLBL NEWLBL NEWLBL {: bno bfold b2 :}
   SP SP 32 SUBI,  30 SP 0 STR,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT bno BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDRB,
   7 7 8 AND,  7 1 CMPI,  C-EQ bfold BCOND,
   \ register path: force deep then top
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LVFORCEK @ BL,  14 bno CBZ,
   14 SP 8 STR,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LVFORCEK @ BL,  14 bno CBZ,
   15 14 0 ADDI,  14 SP 8 LDR,
   \ free rm's bit; VSP--  (entry[VSP-2] already = reg rd via force-in-place)
   7 15 0 ADDI,  LVBIT @ BL,
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

\ LVPUSHR ( x14=reg ) : push a register entry (spill-on-full keeps x14 claimed)
: EMIT-VPUSHR
   LVPUSHR @ LBL,
   NEWLBL {: pr :}
   SP SP 16 SUBI,  30 SP 0 STR,  14 SP 8 STR,
   6 DATA VSP-CELL LDR,  6 VSMAX CMPI,  C-LT pr BCOND,
      LVSPILL @ BL,
      14 SP 8 LDR,  7 14 0 ADDI,  LVBIT @ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 EOR,  6 DATA VRFREE-CELL STR,   \ re-claim x14
   pr LBL,
   14 SP 8 LDR,  6 DATA VSP-CELL LDR,
   7 6 VTAG-OFF ADDI,  7 DATA 7 ADD,  8 0 MOVZ,  8 7 0 STRB,
   8 6 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 STR,
   6 6 1 ADDI,  6 DATA VSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
variable FESK2

\ vop-entry: fold when both con, register op when forceable, else fall through
: VOP-ENTRY {: lmainlbl kwvar kwlen foldxt emitxt :}
   NEWLBL FESK !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 FESK @ CBZ,
   LVBINPREP @ BL,
   13 FESK @ CBZ,
   13 1 CMPI,  C-NE FESK2 @ BCOND,
      foldxt execute
      LVFOLDPUT @ BL,
      lmainlbl B,
   FESK2 @ LBL,
      emitxt execute
      9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      lmainlbl B,
   FESK @ LBL, ;
s" vop-entry" s" n n n n n --" TRUST

: E+   8 $8B000000 LIT64, ;

: E-   8 $CB000000 LIT64, ;

: E*   8 $9B007C00 LIT64, ;

: EAND 8 $8A000000 LIT64, ;

: EOR2 8 $AA000000 LIT64, ;

: EXOR 8 $CA000000 LIT64, ;
variable LKWEQ2  variable LKWNE2  variable LKWLT2  variable LKWGT2  variable LKWLE2  variable LKWGE2

\ comparison entry: fold -> dispatch computes the flag; registers -> emit
\ cmp rd,rm ; cset rd,cond ; sub rd,xzr,rd  (Forth flag 0/-1)
: VCMP-ENTRY {: lmainlbl kwvar kwlen cond :}
   NEWLBL FESK !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 FESK @ CBZ,
   LVBINPREP @ BL,
   13 FESK @ CBZ,
   13 1 CMPI,  C-NE FESK2 @ BCOND,
      11 12 CMP,  11 cond CSET,  11 SP 11 SUB,
      LVFOLDPUT @ BL,
      lmainlbl B,
   FESK2 @ LBL,
      8 $EB00001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  LCEMIT @ BL,
      8 $CB0003E0 LIT64,  9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      lmainlbl B,
   FESK @ LBL, ;

: EMIT-CMPKW
   LKWEQ2 @ LBL,  s" =" BYTES,    LKWNE2 @ LBL,  s" <>" BYTES,
   LKWLT2 @ LBL,  s" <" BYTES,    LKWGT2 @ LBL,  s" >" BYTES,
   LKWLE2 @ LBL,  s" <=" BYTES,   LKWGE2 @ LBL,  s" >=" BYTES, ;
variable LVDROP  variable LVSWAPX  variable LVNIPX  variable LVCOPY
$AA0003E0 constant W-MOVRR        \ orr rd,xzr,rs (| rd | rs<<16)

\ LVDROP ( -- x13=ok ) : drop ANY top entry (reg -> free, con -> forget); no code
: EMIT-VDROP
   LVDROP @ LBL,
   NEWLBL NEWLBL {: no fr :}
   SP SP 16 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 no CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 0 ADDI,  LVBIT @ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      6 DATA VSP-CELL LDR,
   fr LBL,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  13 1 MOVZ,
   no LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVSWAPX ( -- x13=ok ) : swap ANY top two entries (pure relabel; no code)
: EMIT-VSWAPX
   LVSWAPX @ LBL,
   NEWLBL {: no :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,
   5 6 2 SUBI,  8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,
   9 7 0 LDRB,  10 8 0 LDRB,  10 7 0 STRB,  9 8 0 STRB,
   5 6 1 SUBI,  7 5 3 LSLI,  7 7 VVAL-OFF ADDI,  7 DATA 7 ADD,
   5 6 2 SUBI,  8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,
   9 7 0 LDR,  10 8 0 LDR,  10 7 0 STR,  9 8 0 STR,
   13 1 MOVZ,
   no LBL,  RET, ;

\ LVNIPX ( -- x13=ok ) : remove the DEEP entry (free if reg), keep top; no code
: EMIT-VNIPX
   LVNIPX @ LBL,
   NEWLBL NEWLBL {: no fr :}
   SP SP 16 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 0 ADDI,  LVBIT @ BL,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      6 DATA VSP-CELL LDR,
   fr LBL,
   5 6 1 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  9 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  10 8 0 LDR,
   5 6 2 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  9 7 0 STRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  10 8 0 STR,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  13 1 MOVZ,
   no LBL,  30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVCOPY ( x5=k -- x13=ok ) : push a copy of entry k (con free; reg = one mov)
: EMIT-VCOPY
   LVCOPY @ LBL,
   NEWLBL NEWLBL NEWLBL {: no isreg done :}
   SP SP 32 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 isreg CBZ,
      LVPUSHC @ BL,  13 1 MOVZ,  done B,
   isreg LBL,
      11 SP 8 STR,  LVRALLOC @ BL,  14 no CBZ,  11 SP 8 LDR,
      8 W-MOVRR LIT64,  9 8 14 ORR,  7 11 16 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
      LVPUSHR @ BL,
      13 1 MOVZ,  done B,
   no LBL,
   done LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
variable LVSNAP  variable LVRECON
$358 constant SNAPSP-CELL       \ BEGIN snapshot stack depth
$360 constant SNAPSTK-OFF       \ 28 x (k, p0, p1) BEGIN frames, 24 B each (to $600)

\ LVSNAP ( -- ) : BEGIN. VSP<=13: force every VS entry into a register (movz
\ chains for cons emitted HERE, before the loop top) and push (k, packed regs —
\ a byte per slot, bottom-up) on the snapshot stack. Deep VS or a failed
\ force: spill-all and push (0,0) — that loop runs memory-resident as before.
: EMIT-VSNAP
   LVSNAP @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: fl fd fail spush pl pd :}
   NEWLBL {: plo :}  NEWLBL {: pnx :}  NEWLBL {: snok :}
   SP SP 16 SUBI,  30 SP 0 STR,
   6 DATA SNAPSP-CELL LDR,  6 28 CMPI,  C-LT snok BCOND,
      0 75 MOVZ,  NR-EXIT SYS,              \ BEGIN nesting past the frame area
   snok LBL,
   6 DATA VSP-CELL LDR,  6 14 CMPI,  C-GE fail BCOND,   \ two cells pack 13 (the pool)
   5 0 MOVZ,
   fl LBL,                                  \ force entry x5 (Lvforcek saves x5)
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE fd BCOND,
      LVFORCEK @ BL,  14 fail CBZ,
      5 5 1 ADDI,  fl B,
   fd LBL,                                  \ pack: x12 = slots 0-7, x10 = slots 8+
   12 0 MOVZ,  10 0 MOVZ,  11 0 MOVZ,
   pl LBL,
      6 DATA VSP-CELL LDR,  11 6 CMP,  C-GE pd BCOND,
      8 11 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  7 8 0 LDR,
      8 11 7 ANDI,  8 8 3 LSLI,  7 7 8 LSLV,
      11 8 CMPI,  C-GE plo BCOND,
         12 12 7 ORR,  pnx B,
      plo LBL,  10 10 7 ORR,
      pnx LBL,  11 11 1 ADDI,  pl B,
   pd LBL,
   6 DATA VSP-CELL LDR,  13 6 0 ADDI,  spush B,
   fail LBL,
      LVSPILL @ BL,  13 0 MOVZ,  12 0 MOVZ,  10 0 MOVZ,
   spush LBL,                               \ frame = (k, p0, p1); 24 B stride
   6 DATA SNAPSP-CELL LDR,
   7 6 4 LSLI,  8 6 3 LSLI,  7 7 8 ADD,  7 7 SNAPSTK-OFF ADDI,  7 DATA 7 ADD,
   13 7 0 STR,  12 7 8 STR,  10 7 16 STR,
   6 6 1 ADDI,  6 DATA SNAPSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LVRECON ( -- ) : back edge (UNTIL/AGAIN/REPEAT). Pop the snapshot; if the VS
\ is exactly it (k register entries, same registers, bottom-up) emit nothing.
\ Otherwise spill-all then emit k pops into the snapshot registers (top-down)
\ and set the VS to exactly the snapshot. Loop-carried values stay in their
\ BEGIN registers across iterations either way.
: EMIT-VRECON
   LVRECON @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: cl cd rel rl rln :}
   NEWLBL {: chi :}  NEWLBL {: cnx :}  NEWLBL {: rhi :}  NEWLBL {: rnx :}
   SP SP 32 SUBI,  30 SP 0 STR,
   6 DATA SNAPSP-CELL LDR,  6 6 1 SUBI,  6 DATA SNAPSP-CELL STR,
   7 6 4 LSLI,  8 6 3 LSLI,  7 7 8 ADD,  7 7 SNAPSTK-OFF ADDI,  7 DATA 7 ADD,
   13 7 0 LDR,  12 7 8 LDR,  14 7 16 LDR,             \ x13=k x12=p0 x14=p1 (x10 = LVBIT scratch)
   6 DATA VSP-CELL LDR,  6 13 CMP,  C-NE rel BCOND,   \ depth differs -> reload
   5 0 MOVZ,
   cl LBL,
      5 13 CMP,  C-GE cd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 rel CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDR,
      6 5 7 ANDI,  6 6 3 LSLI,
      5 8 CMPI,  C-GE chi BCOND,
         7 12 6 LSRV,  cnx B,
      chi LBL,  7 14 6 LSRV,
      cnx LBL,  7 7 $FF ANDI,
      8 7 CMP,  C-NE rel BCOND,
      5 5 1 ADDI,  cl B,
   cd LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET,                 \ exact: emit nothing
   rel LBL,
   13 SP 8 STR,  12 SP 16 STR,  14 SP 24 STR,
   LVSPILL @ BL,
   13 SP 8 LDR,  12 SP 16 LDR,  14 SP 24 LDR,
   11 0 MOVZ,  5 13 0 ADDI,                           \ x11=claimed bits, x5=i
   rl LBL,
      5 rln CBZ,
      5 5 1 SUBI,
      6 5 7 ANDI,  6 6 3 LSLI,
      5 8 CMPI,  C-GE rhi BCOND,
         7 12 6 LSRV,  rnx B,
      rhi LBL,  7 14 6 LSRV,
      rnx LBL,  7 7 $FF ANDI,                          \ x7 = L[i]
      9 $D1002273 LIT64,  LCEMIT @ BL,                \ sub x19,#8
      8 $F9400260 LIT64,  9 8 7 ORR,  LCEMIT @ BL,    \ ldr L[i],[x19]
      8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  6 0 MOVZ,  6 8 0 STRB,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  7 8 0 STR,
      LVBIT @ BL,  11 11 8 ORR,
      rl B,
   rln LBL,
   13 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 6 11 EOR,  6 DATA VRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
variable FESK3

\ vshuf-entry: reg-aware stack ops — relabels and register moves, no memory traffic
: VSHUF-ENTRY {: lmainlbl kwvar kwlen min sxt :}
   NEWLBL FESK3 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 FESK3 @ CBZ,
   6 DATA VSP-CELL LDR,  6 min CMPI,  C-LT FESK3 @ BCOND,
   sxt execute
   13 FESK3 @ CBZ,
   lmainlbl B,
   FESK3 @ LBL, ;
s" vshuf-entry" s" n n n n n --" TRUST

: XDUP   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LVCOPY @ BL, ;

: XOVER  6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LVCOPY @ BL, ;

: XDROP  LVDROP @ BL, ;

: XSWAP  LVSWAPX @ BL, ;

: XNIP   LVNIPX @ BL, ;
variable LKWINC  variable LKWDEC  variable LKWZEQ  variable LKWZLT
variable LKWNEG2  variable LKWINV2
variable FESK4

\ vun-entry: unary op on the VS top — con folds at JIT time (no code); reg gets
\ an in-place op (rd = rs, entry unchanged); empty VS falls through to the prim.
: VUN-ENTRY {: lmainlbl kwvar kwlen foldxt emitxt :}
   NEWLBL FESK4 !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 FESK4 @ CBZ,
   6 DATA VSP-CELL LDR,  6 FESK4 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 FESK2 @ CBZ,
      foldxt execute
      11 8 0 STR,
      lmainlbl B,
   FESK2 @ LBL,
      14 11 0 ADDI,
      emitxt execute
      lmainlbl B,
   FESK4 @ LBL, ;
s" vun-entry" s" n n n n n --" TRUST

: FU1+  11 11 1 ADDI, ;

: FU1-  11 11 1 SUBI, ;

: FUNEG 11 SP 11 SUB, ;

: FUINV 7 0 MOVN,  11 11 7 EOR, ;

: FU0=  11 0 CMPI,  11 0 CSET,  11 SP 11 SUB, ;

: FU0<  11 0 CMPI,  11 11 CSET,  11 SP 11 SUB, ;

: EU2R  9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  LCEMIT @ BL, ;   \ base | rd | rm<<16

: EU2N  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  LCEMIT @ BL, ;    \ base | rd | rn<<5

: EU1+  8 $91000400 LIT64,  EU2N ;

: EU1-  8 $D1000400 LIT64,  EU2N ;

: EUNEG 8 $CB0003E0 LIT64,  EU2R ;

: EUINV 8 $AA2003E0 LIT64,  EU2R ;

: EUCMP0  8 $F100001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  LCEMIT @ BL, ;

: EUCSET {: cond :}  8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  LCEMIT @ BL, ;

: EU0=  EUCMP0  0 EUCSET  EUNEG ;

: EU0<  EUCMP0  11 EUCSET  EUNEG ;

: EMIT-UNKW
   LKWINC @ LBL,   s" 1+" BYTES,      LKWDEC @ LBL,   s" 1-" BYTES,
   LKWZEQ @ LBL,   s" 0=" BYTES,      LKWZLT @ LBL,   s" 0<" BYTES,
   LKWNEG2 @ LBL,  s" negate" BYTES,  LKWINV2 @ LBL,  s" invert" BYTES, ;

: EMIT-JIT  EMIT-VLITPUSH  EMIT-VSPILL  EMIT-VPUSHC  EMIT-VTOP2C  EMIT-VFOLDPUT
   EMIT-VRALLOC  EMIT-VBIT  EMIT-VRINIT  EMIT-VMOVK  EMIT-VFORCEK  EMIT-VBINPREP  EMIT-VPUSHR
   EMIT-VDROP  EMIT-VSWAPX  EMIT-VNIPX  EMIT-VCOPY  EMIT-VSNAP  EMIT-VRECON ;
