\ engine2.fs — engine-builder port, part 2 (from src/cg/forth.fs): the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and EMIT-FORTH. Needs engine.fs (part 1). emit-main is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission ORDER is exactly src/cg's, so the output is byte-identical.
\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: c-lit
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL, ;
\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
$28 constant INL-MAX
: c-call
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: lcall lcopy lscan lsbody lnopro linl ldone :}
   9 11 0 LDRW,  8 $D10043FF LIT64,  9 8 CMP,  C-NE lnopro BCOND,
      12 INL-MAX 16 + CMPI,  C-GT lcall BCOND,
      13 11 8 ADDI,  14 11 12 ADD,  14 14 8 SUBI,  lscan B,
   lnopro LBL,
      12 INL-MAX CMPI,  C-GT lcall BCOND,
      13 11 0 ADDI,  14 11 12 ADD,
   lscan LBL,
      15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,
      8 $FC000000 LIT64,  10 9 8 AND,  8 $94000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $FFFFFC1F LIT64,  10 9 8 AND,
         8 $D63F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
         8 $D61F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $D65F03C0 LIT64,  9 8 CMP,  C-EQ lcall BCOND,
      8 $1F000000 LIT64,  10 9 8 AND,  8 $10000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      lsbody B,
   lcopy LBL,
      15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  Lcemit @ BL,  linl B,
   lcall LBL,
      5 $FFFF MOVZ,
      7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      9 $D63F0200 LIT64,  Lcemit @ BL,
   ldone LBL, ;
\ ---- source setup: baked Lsrc or stdin ----
: emit-source
   NEWLBL NEWLBL {: rl rd :}              \ locals BEFORE the IF (frame footgun)
   STDIN? @ IF
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      16 197 MOVZ,  $80 SVC,
      11 0 0 ADDI,  9 0 0 ADDI,
      rl LBL,
         0 0 MOVZ,  1 9 0 ADDI,
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
         2 rd CBZ,
         16 3 MOVZ,  $80 SVC,
         0 rd CBZ,
         9 9 0 ADD,  rl B,
      rd LBL,
      INP 11 0 ADDI,  INE 9 0 ADDI,
   ELSE
      INP Lsrc @ ADR,  INE Lsrc @ ADR,  5 SRCN @ LIT64,  INE INE 5 ADD,
   THEN ;
\ ---- control-flow JIT helpers ----
: emit-cf-helpers
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: pisb pdone kno kyes kchk knf :}
   Lcfpush @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   Lcfpop @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 LDR,  RET,
   Lpat @ LBL,
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      13 pisb CBZ,
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   Lkwcmp @ LBL,
      TKL 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 TKA 2 ADD,  4 4 0 LDRB,
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET, ;
: emit-loc-find
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: ll lmiss lhit lcmp lnext :}
   Lloc-find @ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,
      12 11 0 LDR,  12 TKL CMP,  C-NE lnext BCOND,
      13 0 MOVZ,
      lcmp LBL,  13 TKL CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB,
         15 TKA 13 ADD,  15 15 0 LDRB,
         14 15 CMP,  C-NE lnext BCOND,
         13 13 1 ADDI,  lcmp B,
      lhit LBL,  0 10 0 ADDI,  RET,
      lnext LBL,  10 10 1 ADDI,  ll B,
   lmiss LBL,  0 0 MOVN,  RET, ;
\ keyword bytes (lower-case / literal) at known labels
create SQ-KW  115 c, 34 c,
create TICK-KW   39 c,
create BTICK-KW  91 c, 39 c, 93 c,
create LBRACE-KW 123 c, 58 c,
create ENDLOC-KW 58 c, 125 c,
: emit-kwdata
   Lkwif @ LBL,     s" if"     BYTES,    Lkwthen @ LBL,   s" then"   BYTES,
   Lkwelse @ LBL,   s" else"   BYTES,    Lkwbegin @ LBL,  s" begin"  BYTES,
   Lkwuntil @ LBL,  s" until"  BYTES,    Lkwagain @ LBL,  s" again"  BYTES,
   Lkwwhile @ LBL,  s" while"  BYTES,    Lkwrepeat @ LBL, s" repeat" BYTES,
   Lkwcreate @ LBL, s" create" BYTES,    Lkwvar @ LBL,    s" variable" BYTES,
   Lkwsq @ LBL,     SQ-KW 2 BYTES,
   Lkwtick @ LBL,   TICK-KW 1 BYTES,    Lkwbtick @ LBL,  BTICK-KW 3 BYTES,
   Lkwlbrace @ LBL, LBRACE-KW 2 BYTES,  Lkwendloc @ LBL, ENDLOC-KW 2 BYTES,
   Lkwconst @ LBL,  s" constant" BYTES,
   Lkwdo @ LBL,  s" do" BYTES,    Lkwloop @ LBL,  s" loop" BYTES,    Lkwi @ LBL,  s" i" BYTES, ;
\ ---- compile-time keyword handlers (append JIT-emitter code at BUILD time) ----
: c-emitw {: w :}  9 w LIT64,  Lcemit @ BL, ;
: c-popflag  $D1002273 c-emitw  $F9400269 c-emitw ;
: c-pushcp   9 CP 0 ADDI,  Lcfpush @ BL, ;
: c-bback {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: j-if    c-popflag  c-pushcp  $B4000009 c-emitw ;
: j-then  Lcfpop @ BL,  Lpat @ BL, ;
: j-else  Lcfpop @ BL,  14 9 0 ADDI,  c-pushcp  $14000000 c-emitw  9 14 0 ADDI,  Lpat @ BL, ;
: j-begin c-pushcp ;
: j-again Lcfpop @ BL,  $14000000 $3FFFFFF c-bback ;
: j-until Lcfpop @ BL,  14 9 0 ADDI,  c-popflag
   10 14 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000009 LIT64,  9 9 10 ORR,  Lcemit @ BL, ;
: j-while c-popflag  c-pushcp  $B4000009 c-emitw ;
: j-repeat Lcfpop @ BL,  14 9 0 ADDI,  Lcfpop @ BL,  $14000000 $3FFFFFF c-bback
   9 14 0 ADDI,  Lpat @ BL, ;
: j-do
   3506446963 c-emitw  4181721705 c-emitw  3506446963 c-emitw  4181721706 c-emitw
   4181780107 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4177527177 c-emitw  4177528202 c-emitw  2432697707 c-emitw  4177585803 c-emitw
   c-pushcp ;
: j-loop
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4181722506 c-emitw  2432697641 c-emitw  4177527177 c-emitw  3943301439 c-emitw
   Lcfpop @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  Lcemit @ BL,
   4181780107 c-emitw  3506439531 c-emitw  4177585803 c-emitw ;
: j-i
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4177527401 c-emitw  2432705139 c-emitw ;
\ ---- interpret-mode defining words ----
: c-create
   NEWLBL NEWLBL {: ncp ncpd :}
   2 3 MOVZ,  Lprot @ BL,
   Ltok @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,
   14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   ncp LBL,  12 ncpd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncp B,
   ncpd LBL,
   11 DATA 0 LDR,
   c-lit
   9 W-RET LIT64,  Lcemit @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,
   2 5 MOVZ,  Lprot @ BL,  Lflush @ BL, ;
: c-variable  c-create
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DATA 0 STR, ;
: c-constant
   NEWLBL NEWLBL {: kcp kcd :}
   15 g-pop
   2 3 MOVZ,  Lprot @ BL,  Ltok @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   kcp LBL,  12 kcd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  kcp B,
   kcd LBL,
   11 15 0 ADDI,  c-lit
   9 W-RET LIT64,  Lcemit @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  2 5 MOVZ,  Lprot @ BL,  Lflush @ BL, ;
: c-tick
   NEWLBL {: tk :}
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 tk CBZ,  11 g-push  tk LBL, ;
: c-btick
   NEWLBL {: bk :}
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 bk CBZ,  c-lit  bk LBL, ;
: c-lbrace
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: cfok havef nl nd nstore noti ncp ncd pl pd :}
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 cfok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  16 4 MOVZ,  $80 SVC,
      0 75 MOVZ,  16 1 MOVZ,  $80 SVC,
   cfok LBL,
   12 DATA LOCF-CELL LDR,  12 havef CBNZ,
      9 $D10203FF LIT64,  Lcemit @ BL,
      9 128 MOVZ,  9 DATA LOCF-CELL STR,
   havef LBL,
   6 DATA LOCN-CELL LDR,
   nl LBL,
      Ltok @ BL,  0 nd CBZ,
      0 Lkwendloc @ ADR,  1 2 MOVZ,  Lkwcmp @ BL,  0 nstore CBZ,  nd B,
      nstore LBL,
      TKL 1 CMPI,  C-NE noti BCOND,
      13 TKA 0 LDRB,  14 $20 MOVZ,  13 13 14 ORR,  13 105 CMPI,  C-NE noti BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  16 4 MOVZ,  $80 SVC,
         0 75 MOVZ,  16 1 MOVZ,  $80 SVC,
      noti LBL,
      11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,
      TKL 11 0 STR,
      12 11 8 ADDI,  13 TKA 0 ADDI,  14 TKL 0 ADDI,
      ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
      ncd LBL,
      11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR,
      nl B,
   nd LBL,
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,
      9 $D1002273 LIT64,  Lcemit @ BL,
      9 $F9400269 LIT64,  Lcemit @ BL,
      9 $F90003E9 LIT64,  14 13 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
      13 13 1 SUBI,  pl B,
   pd LBL, ;
: c-sdq
   NEWLBL NEWLBL NEWLBL NEWLBL {: sl sd cl cd :}
   INP INP 1 ADDI,  13 INP 0 ADDI,
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,
   15 CP 0 ADDI,  9 $14000000 LIT64,  Lcemit @ BL,
   12 CP 0 ADDI,
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  Lpat @ BL,
   11 12 0 ADDI,  c-lit
   11 15 0 ADDI,  c-lit ;
variable CFSK
: cf-entry {: lmainlbl kwvar kwlen hxt :}
   NEWLBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 CFSK @ CBZ,
   Lvspill @ BL,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
\ ---- MAIN, split into emission-ordered phases sharing label variables ----
variable Lmain  variable Lexit  variable Lcompile  variable Lundef
: em-startup
   NEWLBL NEWLBL {: scopy scdone :}
   Lanchor @ LBL,
   RBASE Lanchor @ ADR,
   SP SP 2048 SUBI,  XDS SP 0 ADDI,
   0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   16 197 MOVZ,  $80 SVC,
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 Lncount @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 Ldict @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      5 9 16 LDR,  5 10 16 STR,
      5 9 24 LDR,  5 10 24 STR,  5 9 32 LDR,  5 10 32 STR,
      5 9 40 LDR,  5 10 40 STR,
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL,
   0 0 MOVZ,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   16 197 MOVZ,  $80 SVC,
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   5 DATA-START MOVZ,  7 DATA 5 ADD,  7 DATA DP-CELL STR,
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 1 MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,
   g-install-crash
   emit-source
   PEND 0 MOVZ, ;
: em-comment
   NEWLBL NEWLBL NEWLBL {: notcom skln skpar :}
   Lmain @ LBL,
      Ltok @ BL,  0 Lexit @ CBZ,
      TKL 1 CMPI,  C-NE notcom BCOND,
      9 TKA 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,
      9 40 CMPI,  C-NE notcom BCOND,
      skpar LBL,  INP INE CMP,  C-GE Lmain @ BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 41 CMPI,  C-NE skpar BCOND,  Lmain @ B,
      skln LBL,   INP INE CMP,  C-GE Lmain @ BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 10 CMPI,  C-NE skln BCOND,  Lmain @ B,
      notcom LBL,
      PEND Lcompile @ CBNZ, ;
: em-interpret
   NEWLBL NEWLBL NEWLBL NEWLBL {: lnotcolon ncopy ncd lnotnum :}
   TKL 1 CMPI,  C-NE lnotcolon BCOND,
   9 TKA 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,
      2 3 MOVZ,  Lprot @ BL,
      Ltok @ BL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      PEND 9 0 ADDI,
      CP 9 0 STR,  TKL 9 16 STR,
      14 DATA CUR-CELL LDR,  14 9 40 STR,
      10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
      ncopy LBL,  12 ncd CBZ,
         13 11 0 LDRB,  13 10 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncopy B,
      ncd LBL,
      5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
      12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
      12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
      12 0 MOVZ,  12 DATA VSP-CELL STR,
      12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
      9 $D10043FF LIT64,  Lcemit @ BL,
      9 $F90003FE LIT64,  Lcemit @ BL,
      Lmain @ B,
   lnotcolon LBL,
   Lmain @ Lkwcreate 6 ['] c-create   cf-entry
   Lmain @ Lkwvar    8 ['] c-variable cf-entry
   Lmain @ Lkwconst  8 ['] c-constant cf-entry
   Lmain @ Lkwtick   1 ['] c-tick     cf-entry
   9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,
   12 lnotnum CBZ,  11 g-push  Lmain @ B,
   lnotnum LBL,
   9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 Lundef @ CBZ,
   11 BLR,  Lmain @ B, ;
: em-compile
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: lnotsemi notd nohook rejected bcap bcp bcd notloc lcnotnum :}
   Lcompile @ LBL,
      TKL 1 CMPI,  C-NE lnotsemi BCOND,
      9 TKA 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
         Lvspill @ BL,
         12 DATA LOCF-CELL LDR,  12 notd CBZ,
            9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
         notd LBL,
         9 $F94003FE LIT64,  Lcemit @ BL,
         9 $910043FF LIT64,  Lcemit @ BL,
         9 W-RET LIT64,  Lcemit @ BL,
         9 PEND 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 PEND 8 STR,
         2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,
         9 DATA HOOK-CELL LDR,  9 nohook CBZ,
            10 DATA BODYBUF-OFF ADDI,  10 g-push
            10 DATA BODYLEN-CELL LDR,  10 g-push
            SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
            10 g-pop  10 rejected CBZ,
         nohook LBL,
            NDICT NDICT 1 ADDI,
         rejected LBL,
         PEND 0 MOVZ,
         Lmain @ B,
      lnotsemi LBL,
      14 DATA BODYLEN-CELL LDR,
      5 BODYBUF-CAP MOVZ,  14 5 CMP,  C-LT bcap BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  16 4 MOVZ,  $80 SVC,
         0 71 MOVZ,  16 1 MOVZ,  $80 SVC,
      bcap LBL,
         15 DATA BODYBUF-OFF ADDI,  15 15 14 ADD,
         11 TKA 0 ADDI,  12 TKL 0 ADDI,
         bcp LBL,  12 bcd CBZ,  13 11 0 LDRB,  13 15 0 STRB,
            15 15 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  bcp B,
         bcd LBL,  13 32 MOVZ,  13 15 0 STRB,
         14 14 TKL ADD,  14 14 1 ADDI,  14 DATA BODYLEN-CELL STR,
      Lmain @ Lkwif     2 ['] j-if     cf-entry
      Lmain @ Lkwthen   4 ['] j-then   cf-entry
      Lmain @ Lkwelse   4 ['] j-else   cf-entry
      Lmain @ Lkwbegin  5 ['] j-begin  cf-entry
      Lmain @ Lkwuntil  5 ['] j-until  cf-entry
      Lmain @ Lkwagain  5 ['] j-again  cf-entry
      Lmain @ Lkwwhile  5 ['] j-while  cf-entry
      Lmain @ Lkwrepeat 6 ['] j-repeat cf-entry
      Lmain @ Lkwsq     2 ['] c-sdq    cf-entry
      Lmain @ Lkwbtick  3 ['] c-btick  cf-entry
      Lmain @ Lkwdo     2 ['] j-do     cf-entry
      Lmain @ Lkwloop   4 ['] j-loop   cf-entry
      Lmain @ Lkwi      1 ['] j-i      cf-entry
      Lmain @ Lkwlbrace 2 ['] c-lbrace cf-entry
      Lloc-find @ BL,  0 0 CMPI,  C-LT notloc BCOND,
         Lvspill @ BL,
         9 $F94003E9 LIT64,  14 0 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
         9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL,
         Lmain @ B,
      notloc LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,
      12 lcnotnum CBZ,  Lvpushc @ BL,  Lmain @ B,
      lcnotnum LBL,
      Lmain @ Lkwplus  1 ['] f+    fold-entry
      Lmain @ Lkwminus 1 ['] f-    fold-entry
      Lmain @ Lkwstar  1 ['] f*    fold-entry
      Lmain @ Lkwand2  3 ['] fand  fold-entry
      Lmain @ Lkwor2   2 ['] for2  fold-entry
      Lmain @ Lkwxor2  3 ['] fxor2 fold-entry
      Lvspill @ BL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
      13 Lundef @ CBZ,
      c-call  Lmain @ B,
   Lundef @ LBL,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  16 4 MOVZ,  $80 SVC,
      0 70 MOVZ,  16 1 MOVZ,  $80 SVC,
   Lexit @ LBL,
      0 0 MOVZ,  16 1 MOVZ,  $80 SVC, ;
: emit-main
   NEWLBL Lmain !  NEWLBL Lexit !  NEWLBL Lcompile !  NEWLBL Lundef !
   em-startup  em-comment  em-interpret  em-compile ;
variable SRCA
: EMIT-FORTH {: a u :}
   u SRCN !  a SRCA !
   ASM-INIT  0 #PL !  0 PNP !
   NEWLBL Lanchor !  NEWLBL Lfind !  NEWLBL Lnum !  NEWLBL Ldict !  NEWLBL Lsrc !
   NEWLBL Lcemit !  NEWLBL Ltok !  NEWLBL Lprot !  NEWLBL Lflush !  NEWLBL Lncount !
   NEWLBL Lcfpush !  NEWLBL Lcfpop !  NEWLBL Lpat !  NEWLBL Lkwcmp !
   NEWLBL Lkwif !  NEWLBL Lkwthen !  NEWLBL Lkwelse !  NEWLBL Lkwbegin !
   NEWLBL Lkwuntil !  NEWLBL Lkwagain !  NEWLBL Lkwwhile !  NEWLBL Lkwrepeat !
   NEWLBL Lkwcreate !  NEWLBL Lkwvar !  NEWLBL Lkwsq !
   NEWLBL Lkwtick !  NEWLBL Lkwbtick !
   NEWLBL Lkwlbrace !  NEWLBL Lkwendloc !  NEWLBL Lloc-find !  NEWLBL Lkwconst !
   NEWLBL Lkwdo !  NEWLBL Lkwloop !  NEWLBL Lkwi !
   NEWLBL Lcrashh !  NEWLBL Lhex !  NEWLBL Lhdr !
   NEWLBL Lprofh !  NEWLBL Lprofdump !
   NEWLBL Lvspill !  NEWLBL Lvlitpush !  NEWLBL Lvpushc !
   NEWLBL Lvtop2c !  NEWLBL Lvfoldput !
   NEWLBL Lkwplus !  NEWLBL Lkwminus !  NEWLBL Lkwstar !
   NEWLBL Lkwand2 !  NEWLBL Lkwor2 !  NEWLBL Lkwxor2 !
   emit-main
   emit-prims  emit-prof-prims  emit-cemit  emit-tok  emit-prot  emit-flush  emit-find  emit-num
   emit-cf-helpers  emit-loc-find  emit-kwdata  emit-foldkw  emit-crash-handler  emit-hex
   emit-profdump  emit-prof  emit-vsjit
   emit-dict
   Lsrc @ LBL,  SRCA @ SRCN @ BYTES, ;
