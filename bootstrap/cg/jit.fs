\ jit.fs — the runtime ABSTRACT VALUE STACK for the engine's `:` compiler (the
\ vs.fs register-allocator model, re-expressed as JIT-compiler routines). While a
\ word compiles, the top of the data stack is tracked as VS entries — tag 1 = a
\ known CONSTANT (no code emitted yet), tag 0 = live in a REGISTER (x9..x15) — and
\ Lvspill materializes all of them as real [x19] pushes (bottom-up) whenever a
\ consumer needs the plain memory-stack convention (a call, control flow, `;`).
\ After a spill the VS is simply EMPTY: no third "mem" tag — spilled values are
\ ordinary stack cells. State lives in DATA header cells, reset at `:`.

require asm.fs
require regalloc.fs

variable Lvspill   variable Lvlitpush   variable Lvpushc
variable Lvtop2c   variable Lvfoldput
variable Lvmovk  variable Lvforcek  variable Lvbinprep  variable Lvpushr
$200 constant VSP-CELL          \ VS depth
$210 constant VTAG-OFF          \ 32 tag bytes   (1=con, 0=reg)
$250 constant VVAL-OFF          \ 32 value cells (constant or register number)
32   constant VSMAX
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

: vf+ 11 11 12 ADD, ;   \ fold helpers — NOT f+/f-/f*: those are the FLOAT prims

: vf- 11 11 12 SUB, ;

: vf* 11 11 12 MUL, ;

: fand 11 11 12 AND, ;

: for2 11 11 12 ORR, ;

: fxor2 11 11 12 EOR, ;

: emit-foldkw
   Lkwplus @ LBL,  s" +" BYTES,    Lkwminus @ LBL,  s" -" BYTES,
   Lkwstar @ LBL,  s" *" BYTES,    Lkwand2 @ LBL,   s" and" BYTES,
   Lkwor2 @ LBL,   s" or" BYTES,   Lkwxor2 @ LBL,   s" xor" BYTES, ;
variable Lkwdup2  variable Lkwdrop2  variable Lkwswap2  variable Lkwover2  variable Lkwnip2

: emit-shufkw
   Lkwdup2 @ LBL,   s" dup" BYTES,    Lkwdrop2 @ LBL,  s" drop" BYTES,
   Lkwswap2 @ LBL,  s" swap" BYTES,   Lkwover2 @ LBL,  s" over" BYTES,
   Lkwnip2 @ LBL,   s" nip" BYTES, ;

\ Lvmovk ( x11=val x14=rd ) : emit a MINIMAL movz/movn + movk chain targeting rd —
\ movn form when $FFFF chunks dominate; chunks the base op already set are skipped.
\ x5=k x6=val x7=nz/started x8=nf/chunk x9=instr x10=form x12=$FFFF (Lcemit saves all).
: emit-vmovk
   Lvmovk @ LBL,
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
         8 $D2800000 LIT64,  9 9 8 ORR,  Lcemit @ BL,  mset B,
      mn LBL,
         8 8 12 EOR,
         9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR,
         8 $92800000 LIT64,  9 9 8 ORR,  Lcemit @ BL,
      mset LBL,  7 1 MOVZ,  mnext B,
      mk LBL,
         9 5 21 LSLI,  8 8 5 LSLI,  9 9 8 ORR,  9 9 14 ORR,
         8 $F2800000 LIT64,  9 9 8 ORR,  Lcemit @ BL,
   mnext LBL,  5 5 1 ADDI,  ml B,
   md LBL,
   7 mout CBNZ,
   10 mz1 CBNZ,
      8 $D2800000 LIT64,  9 8 14 ORR,  Lcemit @ BL,  mout B,
   mz1 LBL,
      8 $92800000 LIT64,  9 8 14 ORR,  Lcemit @ BL,
   mout LBL,
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

: e+   8 $8B000000 LIT64, ;

: e-   8 $CB000000 LIT64, ;

: e*   8 $9B007C00 LIT64, ;

: eand 8 $8A000000 LIT64, ;

: eor2 8 $AA000000 LIT64, ;

: exor 8 $CA000000 LIT64, ;
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
variable Lvdrop  variable Lvswapx  variable Lvnipx  variable Lvcopy
$AA0003E0 constant W-MOVRR        \ orr rd,xzr,rs (| rd | rs<<16)

\ Lvdrop ( -- x13=ok ) : drop ANY top entry (reg -> free, con -> forget); no code
: emit-vdrop
   Lvdrop @ LBL,
   NEWLBL NEWLBL {: no fr :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 no CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 9 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
      6 DATA VRFREE-CELL LDR,  6 6 8 ORR,  6 DATA VRFREE-CELL STR,
      6 DATA VSP-CELL LDR,
   fr LBL,
   6 6 1 SUBI,  6 DATA VSP-CELL STR,  13 1 MOVZ,
   no LBL,  RET, ;

\ Lvswapx ( -- x13=ok ) : swap ANY top two entries (pure relabel; no code)
: emit-vswapx
   Lvswapx @ LBL,
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

\ Lvnipx ( -- x13=ok ) : remove the DEEP entry (free if reg), keep top; no code
: emit-vnipx
   Lvnipx @ LBL,
   NEWLBL NEWLBL {: no fr :}
   13 0 MOVZ,
   6 DATA VSP-CELL LDR,  6 2 CMPI,  C-LT no BCOND,
   5 6 2 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 fr CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
      7 11 9 SUBI,  8 1 MOVZ,  8 8 7 LSLV,
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
   no LBL,  RET, ;

\ Lvcopy ( x5=k -- x13=ok ) : push a copy of entry k (con free; reg = one mov)
: emit-vcopy
   Lvcopy @ LBL,
   NEWLBL NEWLBL NEWLBL {: no isreg done :}
   SP SP 32 SUBI,  30 SP 0 STR,
   13 0 MOVZ,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 isreg CBZ,
      Lvpushc @ BL,  13 1 MOVZ,  done B,
   isreg LBL,
      11 SP 8 STR,  Lvralloc @ BL,  14 no CBZ,  11 SP 8 LDR,
      8 W-MOVRR LIT64,  9 8 14 ORR,  7 11 16 LSLI,  9 9 7 ORR,  Lcemit @ BL,
      Lvpushr @ BL,
      13 1 MOVZ,  done B,
   no LBL,
   done LBL,  30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
variable Lvsnap  variable Lvrecon
$358 constant SNAPSP-CELL       \ BEGIN snapshot stack depth
$360 constant SNAPSTK-OFF       \ 32 x (k, packed-regs) BEGIN nesting frames

\ Lvsnap ( -- ) : BEGIN. VSP<=7: force every VS entry into a register (movz
\ chains for cons emitted HERE, before the loop top) and push (k, packed regs —
\ a nibble per slot, bottom-up) on the snapshot stack. Deep VS or a failed
\ force: spill-all and push (0,0) — that loop runs memory-resident as before.
: emit-vsnap
   Lvsnap @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: fl fd fail spush pl pd :}
   SP SP 16 SUBI,  30 SP 0 STR,
   6 DATA VSP-CELL LDR,  6 8 CMPI,  C-GE fail BCOND,
   5 0 MOVZ,
   fl LBL,                                  \ force entry x5 (Lvforcek saves x5)
      6 DATA VSP-CELL LDR,  5 6 CMP,  C-GE fd BCOND,
      Lvforcek @ BL,  14 fail CBZ,
      5 5 1 ADDI,  fl B,
   fd LBL,                                  \ pack regs: x12 |= val[i] << 4i
   12 0 MOVZ,  11 0 MOVZ,
   pl LBL,
      6 DATA VSP-CELL LDR,  11 6 CMP,  C-GE pd BCOND,
      8 11 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  7 8 0 LDR,
      8 11 2 LSLI,  7 7 8 LSLV,  12 12 7 ORR,
      11 11 1 ADDI,  pl B,
   pd LBL,
   6 DATA VSP-CELL LDR,  13 6 0 ADDI,  spush B,
   fail LBL,
      Lvspill @ BL,  13 0 MOVZ,  12 0 MOVZ,
   spush LBL,                               \ snap[SNAPSP] = (k, packed); SNAPSP++
   6 DATA SNAPSP-CELL LDR,
   7 6 4 LSLI,  7 7 SNAPSTK-OFF ADDI,  7 DATA 7 ADD,
   13 7 0 STR,  12 7 8 STR,
   6 6 1 ADDI,  6 DATA SNAPSP-CELL STR,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ Lvrecon ( -- ) : back edge (UNTIL/AGAIN/REPEAT). Pop the snapshot; if the VS
\ is exactly it (k register entries, same registers, bottom-up) emit nothing.
\ Otherwise spill-all then emit k pops into the snapshot registers (top-down)
\ and set the VS to exactly the snapshot. Loop-carried values stay in their
\ BEGIN registers across iterations either way.
: emit-vrecon
   Lvrecon @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: cl cd rel rl rln :}
   SP SP 32 SUBI,  30 SP 0 STR,
   6 DATA SNAPSP-CELL LDR,  6 6 1 SUBI,  6 DATA SNAPSP-CELL STR,
   7 6 4 LSLI,  7 7 SNAPSTK-OFF ADDI,  7 DATA 7 ADD,
   13 7 0 LDR,  12 7 8 LDR,                           \ x13=k x12=packed
   6 DATA VSP-CELL LDR,  6 13 CMP,  C-NE rel BCOND,   \ depth differs -> reload
   5 0 MOVZ,
   cl LBL,
      5 13 CMP,  C-GE cd BCOND,
      7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,  7 rel CBNZ,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  8 8 0 LDR,
      6 5 2 LSLI,  7 12 6 LSRV,  7 7 $F ANDI,
      8 7 CMP,  C-NE rel BCOND,
      5 5 1 ADDI,  cl B,
   cd LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET,                 \ exact: emit nothing
   rel LBL,
   13 SP 8 STR,  12 SP 16 STR,
   Lvspill @ BL,
   13 SP 8 LDR,  12 SP 16 LDR,
   11 0 MOVZ,  5 13 0 ADDI,                           \ x11=claimed bits, x5=i
   rl LBL,
      5 rln CBZ,
      5 5 1 SUBI,
      6 5 2 LSLI,  7 12 6 LSRV,  7 7 $F ANDI,         \ x7 = L[i]
      9 $D1002273 LIT64,  Lcemit @ BL,                \ sub x19,#8
      8 $F9400260 LIT64,  9 8 7 ORR,  Lcemit @ BL,    \ ldr L[i],[x19]
      8 5 VTAG-OFF ADDI,  8 DATA 8 ADD,  6 0 MOVZ,  6 8 0 STRB,
      8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  7 8 0 STR,
      7 7 9 SUBI,  6 1 MOVZ,  6 6 7 LSLV,  11 11 6 ORR,
      rl B,
   rln LBL,
   13 DATA VSP-CELL STR,
   6 VRALL MOVZ,  6 6 11 EOR,  6 DATA VRFREE-CELL STR,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;
variable FESK3

\ vshuf-entry: reg-aware stack ops — relabels and register moves, no memory traffic
: vshuf-entry {: lmainlbl kwvar kwlen min sxt :}
   NEWLBL FESK3 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 FESK3 @ CBZ,
   6 DATA VSP-CELL LDR,  6 min CMPI,  C-LT FESK3 @ BCOND,
   sxt execute
   13 FESK3 @ CBZ,
   lmainlbl B,
   FESK3 @ LBL, ;

: xdup   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  Lvcopy @ BL, ;

: xover  6 DATA VSP-CELL LDR,  5 6 2 SUBI,  Lvcopy @ BL, ;

: xdrop  Lvdrop @ BL, ;

: xswap  Lvswapx @ BL, ;

: xnip   Lvnipx @ BL, ;
variable Lkwinc  variable Lkwdec  variable Lkwzeq  variable Lkwzlt
variable Lkwneg2  variable Lkwinv2
variable FESK4

\ vun-entry: unary op on the VS top — con folds at JIT time (no code); reg gets
\ an in-place op (rd = rs, entry unchanged); empty VS falls through to the prim.
: vun-entry {: lmainlbl kwvar kwlen foldxt emitxt :}
   NEWLBL FESK4 !  NEWLBL FESK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
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

: fu1+  11 11 1 ADDI, ;

: fu1-  11 11 1 SUBI, ;

: funeg 11 SP 11 SUB, ;

: fuinv 7 0 MOVN,  11 11 7 EOR, ;

: fu0=  11 0 CMPI,  11 0 CSET,  11 SP 11 SUB, ;

: fu0<  11 0 CMPI,  11 11 CSET,  11 SP 11 SUB, ;

: eu2r  9 8 14 ORR,  7 14 16 LSLI,  9 9 7 ORR,  Lcemit @ BL, ;   \ base | rd | rm<<16

: eu2n  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  Lcemit @ BL, ;    \ base | rd | rn<<5

: eu1+  8 $91000400 LIT64,  eu2n ;

: eu1-  8 $D1000400 LIT64,  eu2n ;

: euneg 8 $CB0003E0 LIT64,  eu2r ;

: euinv 8 $AA2003E0 LIT64,  eu2r ;

: eucmp0  8 $F100001F LIT64,  7 14 5 LSLI,  9 8 7 ORR,  Lcemit @ BL, ;

: eucset {: cond :}  8 $9A9F07E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  Lcemit @ BL, ;

: eu0=  eucmp0  0 eucset  euneg ;

: eu0<  eucmp0  11 eucset  euneg ;

: emit-unkw
   Lkwinc @ LBL,   s" 1+" BYTES,      Lkwdec @ LBL,   s" 1-" BYTES,
   Lkwzeq @ LBL,   s" 0=" BYTES,      Lkwzlt @ LBL,   s" 0<" BYTES,
   Lkwneg2 @ LBL,  s" negate" BYTES,  Lkwinv2 @ LBL,  s" invert" BYTES, ;

: emit-jit ( -- )  emit-vlitpush  emit-vspill  emit-vpushc  emit-vtop2c  emit-vfoldput
   emit-vralloc  emit-vmovk  emit-vforcek  emit-vbinprep  emit-vpushr
   emit-vdrop  emit-vswapx  emit-vnipx  emit-vcopy  emit-vsnap  emit-vrecon ;
