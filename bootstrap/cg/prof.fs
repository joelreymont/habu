\ prof.fs — in-binary SAMPLING PROFILER for habu-built binaries, in Forth (like
\ crash.fs). `n prof-on` installs a SIGALRM handler + a 1 ms interval timer; each
\ tick maps the interrupted pc to the dict word containing it (addr <= pc <
\ addr+clen) and bumps that word's counter. After n samples it dumps "name count"
\ per hot word and exit(99) — so it doubles as a hang diagnoser. `prof-report`
\ dumps on demand. Counters live high in the data region; cells in the header.
\ sigreturn(#184) resumes the interrupted context (kernel restores all regs).

require asm.fs
require sys.fs

variable LPROFH   variable LPROFDUMP
$1E0 constant PROF-TOT          \ samples taken
$1E8 constant PROF-LIM          \ sample limit (auto-dump + exit(99) when reached)
$1F8 constant PROF-OTHER      \ samples outside any dict word (main loop, helpers)
DATA-SIZE $10000 - constant PROF-CNT \ counters: one cell per dict slot (high in data region)
14  constant SIGALRM
48 constant PROF-MACOS-MCTX-OFF
176 constant PROF-LINUX-UC-MCTX-OFF
264 constant PROF-LINUX-MCTX-PC-OFF
272 constant PROF-MACOS-MCTX-PC-OFF
8 constant PROF-LINUX-SIGSET-SIZE
$10000004 constant LINUX-SA-PROF-FLAGS
$0042 constant MACOS-SA-PROF-FLAGS

\ LPROFDUMP ( -- ) : write "name count\n" (fd 1) for every dict word with samples.
\ Clobbers x0-x2,x9-x17; loop regs x5/x6 survive G-PRINT9 (x9..x14) + syscalls.
: EMIT-PROFDUMP ( -- )
   LPROFDUMP @ LBL,
   LBL {: dl :}  LBL {: dn :}  LBL {: dd :}  LBL {: dret :}  LBL {: pinl :}
   5 DBASE 0 ADDI,  6 0 MOVZ,                       \ rec, i
   dl LBL,
      7 NDICT 0 ADDI,  6 7 CMP,  C-GE dd BCOND,
      7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 6 3 LSLI,  7 7 8 ADD,  17 7 0 LDR,
      17 dn CBZ,                                    \ no samples -> next
      0 1 MOVZ,  1 5 24 ADDI,  2 5 16 LDR,                  \ write(1, name, len)
      9 2 DNAME-EXT ANDI,  9 pinl CBZ,
         1 5 24 LDR,
      pinl LBL,  2 2 4 LSLI,  2 2 4 LSRI,  NR-WRITE SYS,
      SP SP 16 SUBI,  12 32 MOVZ,  12 SP 0 STRB,                    \ " "
      0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9                                        \ count + newline
   dn LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  dl B,
   dd LBL,  17 DATA PROF-OTHER LDR,  17 dret CBZ,        \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dret LBL,  RET, ;

\ The SIGALRM handler is entered directly. Linux SA_SIGINFO passes
\ x0=sig,x1=siginfo,x2=ucontext; macOS sa_tramp passes x4=ucontext.
: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF
      21 2 PROF-LINUX-UC-MCTX-OFF ADDI,
   ELSE
      21 4 PROF-MACOS-MCTX-OFF LDR,
   THEN ;

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX? IF
      9 21 PROF-LINUX-MCTX-PC-OFF LDR,
   ELSE
      9 21 PROF-MACOS-MCTX-PC-OFF LDR,
   THEN ;

\ Bump the owning word's counter; at the sample limit dump + exit(99); else
\ sigreturn — the kernel restores the interrupted context, so clobbering
\ registers here is safe.
: EMIT-PROF ( -- )
   LPROFH @ LBL,
   LBL {: pl :}  LBL {: pnext :}  LBL {: pdone :}  LBL {: prep :}  LBL {: psig :}
   C-PROF-MCTX>R21  C-PROF-PC>R9                    \ x9 = interrupted pc
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
   0 4 0 ADDI,  NR-SIGRETURN SYS,     \ sigreturn(ucontext, infostyle=x1)
   prep LBL,  LPROFDUMP @ BL,  0 99 MOVZ,  NR-EXIT-GROUP SYS, ;

\ prims. prof-on ( n -- ): zero counters, set limit, install handler + 1ms timer.
: C-PROF-SIGACTION-FRAME ( -- )
   SP SP 32 SUBI,
   9 LPROFH @ ADR,  9 SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-PROF-FLAGS LIT64,  10 SP 8 STR,
      10 0 MOVZ,  10 SP 16 STR,  10 SP 24 STR,
   ELSE
      9 SP 8 STR,
      10 MACOS-SA-PROF-FLAGS MOVZ,  10 10 32 LSLI,  10 SP 16 STR,
   THEN ;

: C-PROF-SIGACTION ( -- )
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 PROF-LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

: C-PROF-SIGACTION-DONE ( -- )
   SP SP 32 ADDI, ;

: C-PROF-TIMER-FRAME ( -- )
   SP SP 32 SUBI,
   9 0 MOVZ,   9 SP 0 STR,  10 1000 MOVZ,  10 SP 8 STR,
   9 SP 16 STR,  10 SP 24 STR, ;

: C-PROF-TIMER ( -- )
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-SETITIMER SYS, ;

: C-PROF-TIMER-DONE ( -- )
   SP SP 32 ADDI, ;

: BPROF-ON
   LBL {: zl :}  LBL {: zd :}
   A G-POP  A DATA PROF-LIM STR,
   9 0 MOVZ,  9 DATA PROF-TOT STR,  9 DATA PROF-OTHER STR,
   7 PROF-CNT LIT64,  7 DATA 7 ADD,  8 NDICT 0 ADDI,         \ zero NDICT counters
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 7 0 STR,  7 7 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE ;

: BPROF-REPORT  SP SP 16 SUBI,  30 SP 0 STR,  LPROFDUMP @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ registered right after EMIT-PRIMS (EMIT-FORTH) so the dict order is stable
: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L ;
