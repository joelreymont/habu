\ habu1.f — the ENGINE BUILDER ported to the subset (from bootstrap/cg/forth.fs):
\ emits the standalone native Forth's primitives, helper routines, and seed
\ dictionary. Golden word-for-word vs habu in test/t-sh-habu1.fs. Needs asm.fs +
\ icode.fs + mnem.fs + rt.fs (g-push/g-pop/g-print9) + crash.fs + macho.fs.
\ Part 1: prims + tok/find/num/prot/flush/cemit + dict. The interpreter main
\ loop, keyword JIT and EMIT-FORTH follow in part 2 (habu2.f).
20 constant RBASE   21 constant INP    22 constant INE   23 constant TKA   24 constant TKL
25 constant PEND    26 constant DBASE  27 constant NDICT  28 constant CP
$100000 constant REGION
$10000  constant DICT-SIZE
48      constant DREC
$F000   constant CFSTK-OFF
$200000 constant DATA-SIZE
$100000 constant IBUFSZ
20 constant DATA
0   constant DP-CELL    8  constant HND-CELL
16  constant LOCN-CELL   24 constant LOCF-CELL    32 constant LOCNAMES
24  constant LOC-REC
$1A0 constant CUR-CELL
$1A8 constant WIDN-CELL
$1B0 constant HOOK-CELL
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
$1D0 constant S0-CELL
$1D8 constant SSCR-CELL
$600 constant LOOP-STK-OFF
$800 constant BODYBUF-OFF
8000 constant BODYBUF-CAP
$2800 constant DATA-START
variable STDIN?   0 STDIN? !
\ runtime instruction-word constants the JIT compiler stamps out
$D65F03C0 constant W-RET
$F9000269 constant W-PUSH0
$91002273 constant W-PUSH1
$D2800009 constant W-MOVZ0
$F2A00009 constant W-MOVK1
$F2C00009 constant W-MOVK2
$F2E00009 constant W-MOVK3
\ --- primitive registry (build-side, for the seed dictionary) ---
create PLBL 64 cells allot   create PEL 64 cells allot
create PLEN 64 cells allot   create PNAM 64 cells allot
create PNPOOL 1024 allot   variable PNP   variable #PL
variable RPD
: reg-prim {: na nu lbl elbl :}
   lbl  #PL @ cells PLBL + !
   elbl #PL @ cells PEL  + !
   nu   #PL @ cells PLEN + !
   PNPOOL PNP @ + RPD !  RPD @ #PL @ cells PNAM + !
   0 BEGIN dup nu < WHILE  dup na + c@  over RPD @ + c!  1 + REPEAT drop
   PNP @ nu + PNP !  #PL @ 1 + #PL ! ;
variable FPL  variable FPE
: FPRIM {: na nu xt :}
   NEWLBL FPL !  NEWLBL FPE !
   na nu FPL @ FPE @ reg-prim
   FPL @ LBL,  SP SP 16 SUBI,  30 SP 0 STR,
   xt execute  30 SP 0 LDR,  SP SP 16 ADDI,  RET,  FPE @ LBL, ;
: FPRIM-L {: na nu xt :}               \ LEAF prim: no BL/BLR in body -> no x30 frame
   NEWLBL FPL !  NEWLBL FPE !
   na nu FPL @ FPE @ reg-prim
   FPL @ LBL,  xt execute  RET,  FPE @ LBL, ;
\ shared label ids (forward refs)
variable Lanchor  variable Lfind  variable Lnum  variable Ldict  variable Lsrc  variable SRCN
variable Lcemit   variable Ltok   variable Lprot  variable Lflush variable Lncount
variable Lcfpush  variable Lcfpop  variable Lpat   variable Lkwcmp
variable Lkwif    variable Lkwthen variable Lkwelse variable Lkwbegin
variable Lkwuntil variable Lkwagain variable Lkwwhile variable Lkwrepeat
variable Lkwcreate variable Lkwvar variable Lkwsq variable Lkwtick variable Lkwbtick
variable Lkwlbrace variable Lkwendloc variable Lloc-find variable Lkwconst
variable Lkwdo variable Lkwloop variable Lkwi
9 constant A   10 constant B   11 constant C
\ ---- primitive bodies (operate on the x19 data stack) ----
: b+   B g-pop  A g-pop  A A B ADD,  A g-push ;
: b-   B g-pop  A g-pop  A A B SUB,  A g-push ;
: b*   B g-pop  A g-pop  A A B MUL,  A g-push ;
: bdup  A g-pop  A g-push  A g-push ;
: bdrop XDS XDS 8 SUBI, ;
: bswap A g-pop  B g-pop  A g-push  B g-push ;
: bdot  A g-pop  g-print9 ;
: b.s
   NEWLBL NEWLBL {: sl sd :}
   9 DATA S0-CELL LDR,  9 DATA SSCR-CELL STR,
   sl LBL,
      9 DATA SSCR-CELL LDR,  9 XDS CMP,  C-GE sd BCOND,
      9 9 0 LDR,  g-print9
      9 DATA SSCR-CELL LDR,  9 9 8 ADDI,  9 DATA SSCR-CELL STR,
      sl B,
   sd LBL, ;
: (cmp) {: cond :}  B g-pop  A g-pop  A B CMP,  A cond CSET,  A SP A SUB,  A g-push ;
: b=  C-EQ (cmp) ;   : b<> C-NE (cmp) ;   : b<  C-LT (cmp) ;   : b>  C-GT (cmp) ;
: b<= C-LE (cmp) ;   : b>= C-GE (cmp) ;
: b0= A g-pop  A 0 CMPI,  A C-EQ CSET,  A SP A SUB,  A g-push ;
: b0< A g-pop  A 0 CMPI,  A C-LT CSET,  A SP A SUB,  A g-push ;
: b1+ A g-pop  A A 1 ADDI,  A g-push ;
: b1- A g-pop  A A 1 SUBI,  A g-push ;
: band B g-pop A g-pop  A A B AND, A g-push ;
: bor  B g-pop A g-pop  A A B ORR, A g-push ;
: bxor B g-pop A g-pop  A A B EOR, A g-push ;
: binv A g-pop  B 0 MOVN,  A A B EOR,  A g-push ;
: bneg A g-pop  A SP A SUB,  A g-push ;
: blsh B g-pop A g-pop  A A B LSLV, A g-push ;
: brsh B g-pop A g-pop  A A B LSRV, A g-push ;
: bdiv B g-pop A g-pop  A A B SDIV, A g-push ;
: bmod B g-pop A g-pop  C A B SDIV,  C C B MUL,  A A C SUB,  A g-push ;
: bnip  A g-pop  XDS XDS 8 SUBI,  A g-push ;
: bover B g-pop A g-pop  A g-push B g-push A g-push ;
: btuck B g-pop A g-pop  B g-push A g-push B g-push ;
: brot  C g-pop B g-pop A g-pop  B g-push C g-push A g-push ;
: bmrot C g-pop B g-pop A g-pop  C g-push A g-push B g-push ;
: b2dup B g-pop A g-pop  A g-push B g-push A g-push B g-push ;
: b2drop XDS XDS 16 SUBI, ;
: bfetch  A g-pop  A A 0 LDR,  A g-push ;
: bstore  B g-pop A g-pop  A B 0 STR, ;
: bcfetch A g-pop  A A 0 LDRB, A g-push ;
: bcstore B g-pop A g-pop  A B 0 STRB, ;
: bcells  A g-pop  A A 3 LSLI, A g-push ;
: bhere   7 DATA 0 LDR,  7 g-push ;
: ballot  A g-pop  7 DATA 0 LDR,  7 7 A ADD,  7 DATA 0 STR, ;
: bcomma  A g-pop  7 DATA 0 LDR,  A 7 0 STR,  7 7 8 ADDI,  7 DATA 0 STR, ;
: bccomma A g-pop  7 DATA 0 LDR,  A 7 0 STRB, 7 7 1 ADDI,  7 DATA 0 STR, ;
: btype   2 g-pop  1 g-pop  0 1 MOVZ,  16 4 MOVZ,  $80 SVC, ;
: bdie    7 g-pop  2 g-pop  1 g-pop  0 2 MOVZ,  16 4 MOVZ,  $80 SVC,
          0 7 0 ADDI,  16 1 MOVZ,  $80 SVC, ;
: bopen   2 g-pop  1 g-pop  0 g-pop  16 5 MOVZ,  $80 SVC,  0 g-push ;
: bwrite  2 g-pop  1 g-pop  0 g-pop  16 4 MOVZ,  $80 SVC,  0 g-push ;
: bread   2 g-pop  1 g-pop  0 g-pop  16 3 MOVZ,  $80 SVC,  0 g-push ;
: bclose  0 g-pop  16 6 MOVZ,  $80 SVC, ;
: brbase  9 DATA RBASE-CELL LDR,  9 g-push ;
: bexec   A g-pop  SP SP 16 SUBI,  30 SP 0 STR,  A BLR,  30 SP 0 LDR,  SP SP 16 ADDI, ;
: bcatch
   NEWLBL NEWLBL {: lres lpush :}
   A g-pop
   SP SP 48 SUBI,
   30 SP 32 STR,
   11 DATA 8 LDR,  11 SP 0 STR,
   19 SP 8 STR,
   13 SP 48 ADDI,  13 SP 16 STR,
   12 lres ADR,  12 SP 24 STR,
   14 SP 0 ADDI,  14 DATA 8 STR,
   9 BLR,
   11 SP 0 LDR,  11 DATA 8 STR,
   30 SP 32 LDR,  SP SP 48 ADDI,
   9 0 MOVZ,  lpush B,
   lres LBL,
   lpush LBL,  9 g-push ;
: bthrow
   NEWLBL {: lnoh :}
   A g-pop
   11 DATA 8 LDR,
   11 lnoh CBZ,
   19 11 8 LDR,
   10 11 0 LDR,  10 DATA 8 STR,
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   lnoh LBL,  0 9 0 ADDI,  16 1 MOVZ,  $80 SVC, ;
: bwordlist  9 DATA WIDN-CELL LDR,  9 g-push  9 9 1 ADDI,  9 DATA WIDN-CELL STR, ;
: bgetcur    9 DATA CUR-CELL LDR,  9 g-push ;
: bsetcur    A g-pop  A DATA CUR-CELL STR, ;
: bsetcheck  A g-pop  A DATA HOOK-CELL STR, ;
: bswl
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: wl wend wnext wcmp wmatch wf1 wf2 :}
   2 g-pop  1 g-pop  0 g-pop
   3 $20 MOVZ,  5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  11 0 MOVZ,
   wl LBL,  6 wend CBZ,
      9 5 40 LDR,  9 2 CMP,  C-NE wnext BCOND,
      9 5 16 LDR,  9 1 CMP,  C-NE wnext BCOND,
      7 0 MOVZ,
      wcmp LBL,  7 1 CMP,  C-GE wmatch BCOND,
         9 5 24 ADDI,  9 9 7 ADD,  9 9 0 LDRB,
         9 $41 CMPI,  C-LT wf1 BCOND,  9 $5A CMPI,  C-GT wf1 BCOND,  9 9 3 ORR,
         wf1 LBL,
         10 0 7 ADD,  10 10 0 LDRB,
         10 $41 CMPI,  C-LT wf2 BCOND,  10 $5A CMPI,  C-GT wf2 BCOND,  10 10 3 ORR,
         wf2 LBL,
         9 10 CMP,  C-NE wnext BCOND,
         7 7 1 ADDI,  wcmp B,
      wmatch LBL,  11 5 0 LDR,  wnext B,
      wnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  wl B,
   wend LBL,  11 g-push ;
: emit-prims
   s" +"    ['] b+    FPRIM-L   s" -"    ['] b-    FPRIM-L   s" *"    ['] b*    FPRIM-L
   s" dup"  ['] bdup  FPRIM-L   s" drop" ['] bdrop FPRIM-L   s" swap" ['] bswap FPRIM-L
   s" ."    ['] bdot  FPRIM-L   s" .s"   ['] b.s   FPRIM-L
   s" ="    ['] b=    FPRIM-L   s" <>"   ['] b<>   FPRIM-L   s" <"    ['] b<    FPRIM-L
   s" >"    ['] b>    FPRIM-L   s" <="   ['] b<=   FPRIM-L   s" >="   ['] b>=   FPRIM-L
   s" 0="   ['] b0=   FPRIM-L   s" 0<"   ['] b0<   FPRIM-L
   s" 1+"   ['] b1+   FPRIM-L   s" 1-"   ['] b1-   FPRIM-L
   s" and"  ['] band  FPRIM-L   s" or"   ['] bor   FPRIM-L   s" xor"  ['] bxor  FPRIM-L
   s" invert" ['] binv FPRIM-L  s" negate" ['] bneg FPRIM-L
   s" lshift" ['] blsh FPRIM-L  s" rshift" ['] brsh FPRIM-L
   s" /"    ['] bdiv  FPRIM-L   s" mod"  ['] bmod  FPRIM-L
   s" nip"  ['] bnip  FPRIM-L   s" over" ['] bover FPRIM-L   s" tuck" ['] btuck FPRIM-L
   s" rot"  ['] brot  FPRIM-L   s" -rot" ['] bmrot FPRIM-L
   s" 2dup" ['] b2dup FPRIM-L   s" 2drop" ['] b2drop FPRIM-L
   s" @"    ['] bfetch FPRIM-L   s" !"    ['] bstore FPRIM-L
   s" c@"   ['] bcfetch FPRIM-L  s" c!"   ['] bcstore FPRIM-L
   s" cells" ['] bcells FPRIM-L
   s" here" ['] bhere  FPRIM-L   s" allot" ['] ballot FPRIM-L
   s" ,"    ['] bcomma FPRIM-L   s" c,"   ['] bccomma FPRIM-L
   s" type" ['] btype  FPRIM-L   s" execute" ['] bexec FPRIM
   s" die"  ['] bdie   FPRIM-L
   s" open" ['] bopen FPRIM-L   s" write" ['] bwrite FPRIM-L   s" read" ['] bread FPRIM-L
   s" close" ['] bclose FPRIM-L
   s" rbase" ['] brbase FPRIM-L
   s" catch" ['] bcatch FPRIM   s" throw" ['] bthrow FPRIM-L
   s" wordlist" ['] bwordlist FPRIM-L   s" get-current" ['] bgetcur FPRIM-L
   s" set-current" ['] bsetcur FPRIM-L  s" search-wl" ['] bswl FPRIM-L
   s" set-check" ['] bsetcheck FPRIM-L ;
: emit-cemit
   Lcemit @ LBL,  9 28 0 STRW,  28 28 4 ADDI,  RET, ;
: emit-tok
   Ltok @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: tskip thas tscan tgot tnone :}
   tskip LBL,
      INP INE CMP,  C-GE tnone BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-HI thas BCOND,
      INP INP 1 ADDI,  tskip B,
   thas LBL,  TKA INP 0 ADDI,
   tscan LBL,
      INP INE CMP,  C-GE tgot BCOND,
      9 INP 0 LDRB,  9 32 CMPI,  C-LS tgot BCOND,
      INP INP 1 ADDI,  tscan B,
   tgot LBL,  TKL INP TKA SUB,  0 1 MOVZ,  RET,
   tnone LBL,  0 0 MOVZ,  RET, ;
: emit-prot
   Lprot @ LBL,
   0 DBASE 0 ADDI,  1 REGION LIT64,  16 74 MOVZ,  $80 SVC,  RET, ;
: emit-flush
   Lflush @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL {: fdl fdd fil fid :}
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,
   10 9 0 ADDI,
   fdl LBL,  10 CP CMP,  C-GE fdd BCOND,  10 DCCVAU,  10 10 64 ADDI,  fdl B,
   fdd LBL,  DSB-ISH,
   10 9 0 ADDI,
   fil LBL,  10 CP CMP,  C-GE fid BCOND,  10 ICIVAU,  10 10 64 ADDI,  fil B,
   fid LBL,  DSB-ISH,  ISB,  RET, ;
: emit-find
   Lfind @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: floop fdone fnext fcmp fmatch :}
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  13 0 MOVZ,
   floop LBL,
      6 fdone CBZ,
      14 5 16 LDR,  14 10 CMP,  C-NE fnext BCOND,
      7 0 MOVZ,
      fcmp LBL,
         7 10 CMP,  C-GE fmatch BCOND,
         15 5 24 ADDI,  15 15 7 ADD,  15 15 0 LDRB,
         3 15 $41 SUBI,  3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
         4 9 7 ADD,     4 4 0 LDRB,
         3 4 $41 SUBI,   3 26 CMPI,  3 C-CC CSET,  3 3 5 LSLI,  4 4 3 ORR,
         15 4 CMP,  C-NE fnext BCOND,
         7 7 1 ADDI,  fcmp B,
      fmatch LBL,
         11 5 0 LDR,  12 5 8 LDR,  13 1 MOVZ,  fnext B,
      fnext LBL,  5 5 DREC ADDI,  6 6 1 SUBI,  floop B,
   fdone LBL,  RET, ;
: emit-num
   Lnum @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: ldone ndoll nohex lloop lok gotd nd nuc :}
   11 0 MOVZ,  13 1 MOVZ,  14 0 MOVZ,  12 0 MOVZ,  6 10 MOVZ,
   10 ldone CBZ,
   15 9 0 LDRB,  15 45 CMPI,  C-NE ndoll BCOND,
      13 0 MOVN,  14 1 MOVZ,
   ndoll LBL,
   14 10 CMP,  C-GE ldone BCOND,
   5 9 14 ADD,  15 5 0 LDRB,  15 36 CMPI,  C-NE nohex BCOND,
      6 16 MOVZ,  14 14 1 ADDI,
   nohex LBL,
   14 10 CMP,  C-GE ldone BCOND,
   lloop LBL,
   14 10 CMP,  C-GE lok BCOND,
   5 9 14 ADD,  15 5 0 LDRB,
   15 48 CMPI,  C-LT ldone BCOND,
   15 57 CMPI,  C-GT nd BCOND,
      7 15 48 SUBI,  gotd B,
   nd LBL,
   6 16 CMPI,  C-NE ldone BCOND,
   15 97 CMPI,  C-LT nuc BCOND,  15 102 CMPI,  C-GT ldone BCOND,
      7 15 87 SUBI,  gotd B,
   nuc LBL,
   15 65 CMPI,  C-LT ldone BCOND,  15 70 CMPI,  C-GT ldone BCOND,
      7 15 55 SUBI,
   gotd LBL,
   11 11 6 MUL,  11 11 7 ADD,
   14 14 1 ADDI,  lloop B,
   lok LBL,  11 11 13 MUL,  12 1 MOVZ,
   ldone LBL,  RET, ;
: emit-dict
   Lncount @ LBL,  #PL @ DCQ,
   Ldict @ LBL,
   0 BEGIN dup #PL @ < WHILE
      dup cells PLBL + @ DLBL,
      dup cells PEL  + @ DLBL,
      dup cells PLEN + @ DCQ,
      dup cells PNAM + @  over cells PLEN + @  BYTES,
      16  over cells PLEN + @  3 + -4 and  -  dup 0 > IF PNPOOL swap BYTES, ELSE drop THEN
      0 DCQ,
      1 + REPEAT drop ;
