\ prof.fs — the in-binary sampling profiler for the native engine. `n prof-on`
\ = SIGALRM + 1 ms timer; ticks map the interrupted pc to its dict word and count;
\ at the limit: dump "name count" + exit(99). prof-report dumps on demand.
\ Load after habu1.f (uses DATA/DBASE/NDICT/DREC/A/FPRIM-L), before habu2.f.
variable LPROFH   variable LPROFDUMP
$1E0 constant PROF-TOT
$1E8 constant PROF-LIM
$1F8 constant PROF-OTHER      \ samples outside any dict word (main loop, helpers)
DATA-SIZE $10000 - constant PROF-CNT
14  constant SIGALRM
$10000004 constant LINUX-SA-PROF-FLAGS
$0042 constant MACOS-SA-PROF-FLAGS

: EMIT-PROFDUMP ( -- )
   LPROFDUMP LABEL@ LBL,
   LBL LBL LBL LBL LBL {: dl dn dd dret pinl :}
   5 DBASE 0 ADDI,  6 0 MOVZ,
   dl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE dd BCOND,
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,  17 7 0 LDR,
      17 dn CBZ,
      0 1 MOVZ,  1 5 24 ADDI,  2 5 16 LDR,
      9 2 DNAME-EXT ANDI,  9 pinl CBZ,
         1 5 24 LDR,
      pinl LBL,  2 2 4 LSLI,  2 2 4 LSRI,  NR-WRITE SYS,
      SP SP 16 SUBI,  12 32 MOVZ,  12 SP 0 STRB,
      0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dn LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  dl B,
   dd LBL,  17 DATA PROF-OTHER LDR,  17 dret CBZ,        \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dret LBL,  RET, ;

: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 4 MCTX-OFF LDR, ;
s" c-prof-mctx>r21" s" --" TRUST

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;
s" c-prof-pc>r9" s" --" TRUST

: EMIT-PROF ( -- )
   LPROFH LABEL@ LBL,
   LBL LBL LBL LBL LBL {: pl pnext pdone prep psig :}
   C-PROF-MCTX>R21  C-PROF-PC>R9
   10 DATA PROF-TOT LDR,  10 10 1 ADDI,  10 DATA PROF-TOT STR,
   11 DATA PROF-LIM LDR,  10 11 CMP,  C-GE prep BCOND,
   5 DBASE 0 ADDI,  6 0 MOVZ,
   pl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE pdone BCOND,
      12 5 0 LDR,  12 9 12 SUB,
      13 5 8 LDR,  12 13 CMP,  C-CS pnext BCOND,
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,
      12 7 0 LDR,  12 12 1 ADDI,  12 7 0 STR,
      psig B,
   pnext LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  pl B,
   pdone LBL,
   12 DATA PROF-OTHER LDR,  12 12 1 ADDI,  12 DATA PROF-OTHER STR,
   psig LBL,
   0 4 0 ADDI,  NR-SIGRETURN SYS,
   prep LBL,  LPROFDUMP LABEL@ BL,  0 99 MOVZ,  NR-EXIT SYS, ;

: C-PROF-SIGACTION-FRAME ( -- )
   SP SP 32 SUBI,
   9 LPROFH LABEL@ ADR,  9 SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-PROF-FLAGS LIT64,  10 SP 8 STR,
      10 0 MOVZ,  10 SP 16 STR,  10 SP 24 STR,
      exit
   THEN
   9 SP 8 STR,
   10 MACOS-SA-PROF-FLAGS MOVZ,  10 10 32 LSLI,  10 SP 16 STR, ;
s" c-prof-sigaction-frame" s" --" TRUST

: C-PROF-SIGACTION ( -- )
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;
s" c-prof-sigaction" s" --" TRUST

: C-PROF-SIGACTION-DONE ( -- )
   SP SP 32 ADDI, ;
s" c-prof-sigaction-done" s" --" TRUST

: C-PROF-TIMER-FRAME ( -- )
   SP SP 32 SUBI,
   9 0 MOVZ,   9 SP 0 STR,  10 1000 MOVZ,  10 SP 8 STR,
   9 SP 16 STR,  10 SP 24 STR, ;
s" c-prof-timer-frame" s" --" TRUST

: C-PROF-TIMER ( -- )
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS, ;
s" c-prof-timer" s" --" TRUST

: C-PROF-TIMER-DONE ( -- )
   SP SP 32 ADDI, ;
s" c-prof-timer-done" s" --" TRUST

: BPROF-ON ( -- )
   LBL LBL {: zl zd :}
   A G-POP  A DATA PROF-LIM STR,
   9 0 MOVZ,  9 DATA PROF-TOT STR,  9 DATA PROF-OTHER STR,
   7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 7 0 STR,  7 7 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE ;

: BPROF-REPORT ( -- )  SP SP 16 SUBI,  30 SP 0 STR,  LPROFDUMP LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L ;
s" emit-prof-prims" s" --" TRUST
