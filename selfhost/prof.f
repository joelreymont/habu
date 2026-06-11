\ prof.fs — the in-binary sampling profiler, transcribed from src/cg/prof.fs for
\ the engine-builder port (lockstep; the engine goldens enforce parity). `n prof-on`
\ = SIGALRM + 1 ms timer; ticks map the interrupted pc to its dict word and count;
\ at the limit: dump "name count" + exit(99). prof-report dumps on demand.
\ Load after habu1.f (uses DATA/DBASE/NDICT/DREC/A/FPRIM-L), before habu2.f.
variable Lprofh   variable Lprofdump
$1E0 constant PROF-TOT
$1E8 constant PROF-LIM
$1F8 constant PROF-OTHER      \ samples outside any dict word (main loop, helpers)
$1F0000 constant PROF-CNT
14  constant SIGALRM
83  constant NR-SETITIMER
184 constant NR-SIGRETURN
$0042 constant SA-PROF-FLAGS
: emit-profdump
   Lprofdump @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL {: dl dn dd dret :}
   5 DBASE 0 ADDI,  6 0 MOVZ,
   dl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE dd BCOND,
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,  17 7 0 LDR,
      17 dn CBZ,
      0 1 MOVZ,  1 5 24 ADDI,  2 5 16 LDR,  16 4 MOVZ,  $80 SVC,
      SP SP 16 SUBI,  12 32 MOVZ,  12 SP 0 STRB,
      0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  16 4 MOVZ,  $80 SVC,
      SP SP 16 ADDI,
      9 17 0 ADDI,  g-print9
   dn LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  dl B,
   dd LBL,  17 DATA PROF-OTHER LDR,  17 dret CBZ,        \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  16 4 MOVZ,  $80 SVC,
      SP SP 16 ADDI,
      9 17 0 ADDI,  g-print9
   dret LBL,  RET, ;
: emit-prof
   Lprofh @ LBL,
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: pl pnext pdone prep psig :}
   21 4 MCTX-OFF LDR,  9 21 272 LDR,
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
   0 4 0 ADDI,  16 NR-SIGRETURN MOVZ,  $80 SVC,
   prep LBL,  Lprofdump @ BL,  0 99 MOVZ,  16 1 MOVZ,  $80 SVC, ;
: bprof-on
   NEWLBL NEWLBL {: zl zd :}
   A g-pop  A DATA PROF-LIM STR,
   9 0 MOVZ,  9 DATA PROF-TOT STR,  9 DATA PROF-OTHER STR,
   7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 7 0 STR,  7 7 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   SP SP 64 SUBI,
   9 Lprofh @ ADR,  9 SP 0 STR,  9 SP 8 STR,
   10 SA-PROF-FLAGS MOVZ,  10 10 32 LSLI,  10 SP 16 STR,
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  16 46 MOVZ,  $80 SVC,
   9 0 MOVZ,   9 SP 32 STR,  10 1000 MOVZ,  10 SP 40 STR,
   9 SP 48 STR,  10 SP 56 STR,
   0 0 MOVZ,  1 SP 32 ADDI,  2 0 MOVZ,  16 NR-SETITIMER MOVZ,  $80 SVC,
   SP SP 64 ADDI, ;
: bprof-report  SP SP 16 SUBI,  30 SP 0 STR,  Lprofdump @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;
: emit-prof-prims
   s" prof-on" ['] bprof-on FPRIM-L  s" prof-report" ['] bprof-report FPRIM-L ;
