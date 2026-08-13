\ prof.fs — the in-binary sampling profiler for the native engine. `n prof-on`
\ = SIGALRM + 1 ms timer; ticks map the interrupted pc to its dict word and count;
\ at the limit: dump "name count" + exit(99). prof-report dumps on demand.
\ Load after habu1.f (uses DATA/DBASE/NDICT/DREC/A/FPRIM-L), before habu2.f.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM

\ ---- package -----------------------------------------------------------------
\ One concern: the sampling profiler's emitted code. The five names habu2.f
\ reaches for are public and the rest is this file's own, which is what a package
\ is for - before this the whole file was global, and a change to any word in it
\ was refused by the package lint with nowhere to put the word
\ (dot habu-fold-a-named-052f4c4b's field carve is what needed one).
package PROF

public
variable LPROFH   variable LPROFDUMP
private
$1E0 constant PROF-TOT
$1E8 constant PROF-LIM
$1F8 constant PROF-OTHER      \ samples outside any dict word (main loop, helpers)
DATA-SIZE PROF-CNT-BYTES - constant PROF-CNT   \ band base: PROF-CNT-BYTES (= DICT-CAP cells, layout.f) below DATA top
14  constant SIGALRM
$10000004 constant LINUX-SA-PROF-FLAGS
$0042 constant MACOS-SA-PROF-FLAGS

public

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
      \ The record's flags cell carries the name length in its low bits and
      \ four fields above it: DKIND (50-51), DNAME-MIN-IN (52-59) and the
      \ IMM/EXT/WIDE/INT nibble (60-63). Clearing the top FOURTEEN is what
      \ leaves the length alone - src/habu/layout.f states the band. A clear
      \ of twelve leaves a definer's stamp in the count and hands `write` a
      \ length of 2^50 bytes, which writes nothing and loses the row's name.
      pinl LBL,  2 2 14 LSLI,  2 2 14 LSRI,  NR-WRITE SYS,
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

private

\ Profiler trust rows emit ARM64 signal-context, sigaction/timer-frame, syscall,
\ sampling, and primitive-publication code.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 4 MCTX-OFF LDR, ;
s" c-prof-mctx>r21" s" --" TRUST

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;
s" c-prof-pc>r9" s" --" TRUST

\ Attribute the interrupted pc FIRST (a dict word's counter or PROF-OTHER), THEN
\ bump PROF-TOT once and test the limit, so every delivered sample is counted:
\ sum(word counters) + PROF-OTHER == PROF-TOT exactly, including the sample that
\ reaches the limit. Below the limit we sigreturn; at it we dump + exit(99).
public

: EMIT-PROF ( -- )
   LPROFH LABEL@ LBL,
   LBL LBL LBL LBL LBL {: pl pnext pdone prep psig :}
   C-PROF-MCTX>R21  C-PROF-PC>R9
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
   10 DATA PROF-TOT LDR,  10 10 1 ADDI,  10 DATA PROF-TOT STR,
   11 DATA PROF-LIM LDR,  10 11 CMP,  C-GE prep BCOND,
   0 4 0 ADDI,  NR-SIGRETURN SYS,
   prep LBL,  LPROFDUMP LABEL@ BL,  0 99 MOVZ,  NR-EXIT-GROUP SYS, ;

private

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

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L ;
s" emit-prof-prims" s" --" TRUST

;using

;package
