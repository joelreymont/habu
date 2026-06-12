\ prof.fs — in-binary SAMPLING PROFILER for habu-built binaries, in Forth (like
\ crash.fs). `n prof-on` installs a SIGALRM handler + a 1 ms interval timer; each
\ tick maps the interrupted pc to the dict word containing it (addr <= pc <
\ addr+clen) and bumps that word's counter. After n samples it dumps "name count"
\ per hot word and exit(99) — so it doubles as a hang diagnoser. `prof-report`
\ dumps on demand. Counters live high in the data region; cells in the header.
\ sigreturn(#184) resumes the interrupted context (kernel restores all regs).

require asm.fs

variable Lprofh   variable Lprofdump
$1E0 constant PROF-TOT          \ samples taken
$1E8 constant PROF-LIM          \ sample limit (auto-dump + exit(99) when reached)
$1F8 constant PROF-OTHER      \ samples outside any dict word (main loop, helpers)
$1F0000 constant PROF-CNT       \ counters: one cell per dict slot (high in data region)
14  constant SIGALRM
83  constant NR-SETITIMER
184 constant NR-SIGRETURN
$0042 constant SA-PROF-FLAGS    \ SA_SIGINFO|SA_RESTART

\ Lprofdump ( -- ) : write "name count\n" (fd 1) for every dict word with samples.
\ Clobbers x0-x2,x9-x17; loop regs x5/x6 survive g-print9 (x9..x14) + syscalls.
: emit-profdump ( -- )
   Lprofdump @ LBL,
   NEWLBL {: dl :}  NEWLBL {: dn :}  NEWLBL {: dd :}  NEWLBL {: dret :}
   5 DBASE 0 ADDI,  6 0 MOVZ,                       \ rec, i
   dl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE dd BCOND,
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,  17 7 0 LDR,
      17 dn CBZ,                                    \ no samples -> next
      0 1 MOVZ,  1 5 24 ADDI,  2 5 16 LDR,  16 4 MOVZ,  $80 SVC,   \ write(1, name, len)
      SP SP 16 SUBI,  12 32 MOVZ,  12 SP 0 STRB,                    \ " "
      0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  16 4 MOVZ,  $80 SVC,
      SP SP 16 ADDI,
      9 17 0 ADDI,  g-print9                                        \ count + newline
   dn LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  dl B,
   dd LBL,  17 DATA PROF-OTHER LDR,  17 dret CBZ,        \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  16 4 MOVZ,  $80 SVC,
      SP SP 16 ADDI,
      9 17 0 ADDI,  g-print9
   dret LBL,  RET, ;

\ The SIGALRM handler, entered directly as sa_tramp: x1=infostyle, x4=ucontext.
\ Bump the owning word's counter; at the sample limit dump + exit(99); else
\ sigreturn(ucontext, infostyle) — the kernel restores the interrupted context,
\ so clobbering registers here is safe.
: emit-prof ( -- )
   Lprofh @ LBL,
   NEWLBL {: pl :}  NEWLBL {: pnext :}  NEWLBL {: pdone :}  NEWLBL {: prep :}  NEWLBL {: psig :}
   21 4 MCTX-OFF LDR,  9 21 272 LDR,                \ x9 = interrupted pc
   10 DATA PROF-TOT LDR,  10 10 1 ADDI,  10 DATA PROF-TOT STR,
   11 DATA PROF-LIM LDR,  10 11 CMP,  C-GE prep BCOND,
   5 DBASE 0 ADDI,  6 0 MOVZ,                       \ rec, i
   pl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE pdone BCOND,
      12 5 0 LDR,  12 9 12 SUB,                     \ x12 = pc - addr
      13 5 8 LDR,  12 13 CMP,  C-CS pnext BCOND,    \ not (pc-addr u< clen) -> next
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,
      12 7 0 LDR,  12 12 1 ADDI,  12 7 0 STR,       \ counter[i]++
      psig B,
   pnext LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  pl B,
   pdone LBL,                                       \ no owning word: count as (other)
   12 DATA PROF-OTHER LDR,  12 12 1 ADDI,  12 DATA PROF-OTHER STR,
   psig LBL,
   0 4 0 ADDI,  16 NR-SIGRETURN MOVZ,  $80 SVC,     \ sigreturn(ucontext, infostyle=x1)
   prep LBL,  Lprofdump @ BL,  0 99 MOVZ,  16 1 MOVZ,  $80 SVC, ;

\ prims. prof-on ( n -- ): zero counters, set limit, install handler + 1ms timer.
: bprof-on
   NEWLBL {: zl :}  NEWLBL {: zd :}
   A g-pop  A DATA PROF-LIM STR,
   9 0 MOVZ,  9 DATA PROF-TOT STR,  9 DATA PROF-OTHER STR,
   7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 NDICT 0 ADDI,         \ zero NDICT counters
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 7 0 STR,  7 7 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   SP SP 64 SUBI,
   9 Lprofh @ ADR,  9 SP 0 STR,  9 SP 8 STR,                  \ sa_handler, sa_tramp
   10 SA-PROF-FLAGS MOVZ,  10 10 32 LSLI,  10 SP 16 STR,      \ mask=0, flags
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  16 46 MOVZ,  $80 SVC,   \ sigaction
   9 0 MOVZ,   9 SP 32 STR,  10 1000 MOVZ,  10 SP 40 STR,     \ it_interval = 0s 1000us
   9 SP 48 STR,  10 SP 56 STR,                                \ it_value    = 0s 1000us
   0 0 MOVZ,  1 SP 32 ADDI,  2 0 MOVZ,  16 NR-SETITIMER MOVZ,  $80 SVC,
   SP SP 64 ADDI, ;

: bprof-report  SP SP 16 SUBI,  30 SP 0 STR,  Lprofdump @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ registered right after emit-prims (EMIT-FORTH) so the dict order is stable
: emit-prof-prims ( -- )
   s" prof-on" ['] bprof-on FPRIM-L  s" prof-report" ['] bprof-report FPRIM-L ;
