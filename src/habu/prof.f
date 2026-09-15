\ prof.f — the in-binary sampling profiler for the native engine. `n prof-on`
\ = SIGALRM + 1 ms timer; ticks map the interrupted pc to its dict word and count;
\ at the limit: dump "name count" + exit(99). prof-report dumps on demand.
\ Load after habu1.f (uses DBASE/NDICT/DREC/A/FPRIM-L), before habu2.f.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
\
\ The tick lands in whatever the timer interrupted, and that is not always Habu
\ code: a foreign callee (libc's dlsym, libzip's inflate) reuses the callee-saved
\ registers the engine reserves for DATA (x20), DBASE (x26) and NDICT (x27), and
\ a handler that read them walked garbage (SIGSEGV in the handler, x0 = SIGALRM).
\ So the handler takes nothing from the live registers. Its state lives in the
\ profiler band at the top of the DATA region, reached through the fixed DATA-VA;
\ prof-on records the dictionary base there; the interrupted context's x20 and
\ x26 are read from the signal frame and compared with those two facts, and only
\ a context that holds both is walked, with its x27 clamped to DICT-CAP. Any
\ other context is a "(foreign)" sample. The handler runs on its own alternate
\ stack, so a tick with the machine stack deep never pushes the signal frame over
\ the data stack. The dump reads the same band, so it needs no register either.
using A64ASM

package PROF

public
variable LPROFH   variable LPROFDUMP
private
\ ---- the band: PROF-STATE-BYTES of state cells, then one counter per dict record
DATA-SIZE PROF-CNT-BYTES - constant PROF-BAND           \ band base, DATA-relative
DATA-VA VA>N PROF-BAND + constant PROF-BAND-VA          \ absolute: DATA is MAP_FIXED at DATA-VA
PROF-BAND-VA PROF-STATE-BYTES + constant PROF-CNT-VA    \ the counters
0  constant PROF-TOT        \ samples delivered
8  constant PROF-LIM        \ dump + exit(99) at this many
16 constant PROF-OTHER      \ Habu-code samples outside any dict word (main loop, helpers)
24 constant PROF-FOREIGN    \ samples in a context that is not Habu code
32 constant PROF-DBASE      \ the dictionary base, recorded by prof-on
40 constant PROF-STACK      \ the handler's alternate stack, mapped once per process
48 constant PROF-COUNT      \ the record count the dump walks
$10000 constant PROF-STACK-BYTES
14  constant SIGALRM
$18000004 constant LINUX-SA-PROF-FLAGS   \ SA_SIGINFO | SA_ONSTACK | SA_RESTART
$0043 constant MACOS-SA-PROF-FLAGS        \ SA_ONSTACK | SA_RESTART | SA_SIGINFO
42 constant PROFMMAPMSG-LEN               \ "hb: prof-on: cannot map the handler stack\n"
46 constant PROFSTKMSG-LEN                \ "hb: prof-on: cannot install the handler stack\n"

\ Profiler helpers emit ARM64 signal-context, sigaction/timer-frame, syscall,
\ sampling, and primitive-publication code.
: C-PROF-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 4 MCTX-OFF LDR, ;

: C-PROF-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;

\ rd = the interrupted context's x<n>, read from the signal frame (x21 = mcontext).
: C-PROF-CTX-X>R ( n n -- ) {: x:n rd:n :}
   HB-TARGET-LINUX? IF rd 21 LINUX-MCTX-X0-OFF x 8 * + LDR, exit THEN
   rd 21 SS-OFF x 8 * + LDR, ;

\ x13 = the record's code bytes from its raw length cell (src/habu/code-span.f):
\ bit 31 marks an exact span, otherwise the body precedes one final slot. Read
\ raw, that bit made the first record below the pc own every sample, and the
\ report's first line was DEFER-UNSET.
: C-PROF-SPAN-BYTES ( -- )
   14 13 31 LSRI,                         \ x14 = the exact-span bit
   13 13 CODE-SPAN:MASK ANDI,             \ x13 = body
   13 13 CODE-SPAN:INSN-BYTES ADDI,       \ a legacy body ends before its final slot
   14 14 2 LSLI,  13 13 14 SUB, ;         \ an exact span does not

public

\ LPROFDUMP ( x15 = record count ): one "name count" line per counted record,
\ then "(other) N" and "(foreign) N" when non-zero. Every input comes from the
\ band or from x15, so the handler may call it from a validated sample and
\ prof-report from Habu code.
: EMIT-PROFDUMP ( -- )
   LPROFDUMP LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL {: dl dn dd dfor dret pinl :}
   7 PROF-BAND-VA LIT64,  15 7 PROF-COUNT STR,
   6 0 MOVZ,
   dl LBL,
      7 PROF-BAND-VA LIT64,  15 7 PROF-COUNT LDR,  6 15 CMP,  C-GE dd BCOND,
      5 7 PROF-DBASE LDR,  8 DREC MOVZ,  8 6 8 MUL,  5 5 8 ADD,        \ x5 = record x6
      14 PROF-CNT-VA LIT64,  8 6 3 LSLI,  14 14 8 ADD,  17 14 0 LDR,   \ x17 = its count
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
   dn LBL,  6 6 1 ADDI,  dl B,
   dd LBL,  7 PROF-BAND-VA LIT64,  17 7 PROF-OTHER LDR,  17 dfor CBZ,     \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dfor LBL,  7 PROF-BAND-VA LIT64,  17 7 PROF-FOREIGN LDR,  17 dret CBZ,  \ "(foreign) N" if any
      SP SP 16 SUBI,  12 $6E676965726F6628 LIT64,  12 SP 0 STR,  12 $2029 MOVZ,  12 SP 8 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 10 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dret LBL,  RET, ;

\ Attribute the interrupted pc FIRST (a dict word's counter, PROF-OTHER, or
\ PROF-FOREIGN), THEN bump PROF-TOT once and test the limit, so every delivered
\ sample is counted: sum(word counters) + PROF-OTHER + PROF-FOREIGN == PROF-TOT
\ exactly, including the sample that reaches the limit. Below the limit we
\ sigreturn; at it we dump + exit(99), but only from a validated sample, whose
\ record count is trustworthy; a foreign sample at the limit sigreturns and the
\ next validated one dumps.
: EMIT-PROF ( -- )
   LPROFH LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL {: pl pnext pdone pforeign psig pexit :}
   C-PROF-MCTX>R21  C-PROF-PC>R9
   7 PROF-BAND-VA LIT64,
   20 10 C-PROF-CTX-X>R  11 DATA-VA VA>N LIT64,  10 11 CMP,  C-NE pforeign BCOND,   \ context x20 must be DATA
   26 10 C-PROF-CTX-X>R  11 7 PROF-DBASE LDR,  10 11 CMP,  C-NE pforeign BCOND,   \ context x26 must be the recorded DBASE
   27 15 C-PROF-CTX-X>R  12 DICT-CAP LIT64,  15 12 CMP,  15 15 12 C-LS CSEL,     \ x15 = min(context NDICT, DICT-CAP)
   17 1 MOVZ,                                        \ x17 = 1: a Habu context, its count may dump (x16 is the Darwin syscall number, never kept across a sys)
   5 11 0 ADDI,  6 0 MOVZ,
   pl LBL,
      6 15 CMP,  C-GE pdone BCOND,
      12 5 0 LDR,  12 9 12 SUB,
      13 5 8 LDR,  C-PROF-SPAN-BYTES  12 13 CMP,  C-CS pnext BCOND,
      14 PROF-CNT-VA LIT64,  8 6 3 LSLI,  14 14 8 ADD,
      12 14 0 LDR,  12 12 1 ADDI,  12 14 0 STR,
      psig B,
   pnext LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  pl B,
   pdone LBL,
   12 7 PROF-OTHER LDR,  12 12 1 ADDI,  12 7 PROF-OTHER STR,
   psig B,
   pforeign LBL,
   17 0 MOVZ,                                        \ not Habu code: no walk, no dump from here
   12 7 PROF-FOREIGN LDR,  12 12 1 ADDI,  12 7 PROF-FOREIGN STR,
   psig LBL,
   10 7 PROF-TOT LDR,  10 10 1 ADDI,  10 7 PROF-TOT STR,
   11 7 PROF-LIM LDR,  10 11 CMP,  C-LT pexit BCOND,
   17 pexit CBZ,                                     \ the limit reached on a foreign sample: the next Habu sample dumps
   LPROFDUMP LABEL@ BL,  0 99 MOVZ,  NR-EXIT-GROUP SYS,
   pexit LBL,  0 4 0 ADDI,  NR-SIGRETURN SYS, ;

private

\ The handler's alternate stack: mapped on the first prof-on of the process,
\ kept in the band, registered with sigaltstack before every sigaction so the
\ SA_ONSTACK handler never runs on the interrupted program's stack.
: C-PROF-ALTSTACK ( -- )
   LBL LBL LBL LBL LBL {: have mok pmsg sok smsg :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-STACK LDR,  9 have CBNZ,
      0 0 MOVZ,  1 PROF-STACK-BYTES LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      C-CC mok BCOND,
         1 pmsg ADR,  2 PROFMMAPMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      pmsg LBL,  S\" hb: prof-on: cannot map the handler stack\n" BYTES,   \ BYTES, pads the stream back to a word
      mok LBL,
      7 PROF-BAND-VA LIT64,  0 7 PROF-STACK STR,
   have LBL,
   7 PROF-BAND-VA LIT64,
   SP SP 32 SUBI,
   9 7 PROF-STACK LDR,  9 SP 0 STR,
   HB-TARGET-LINUX? IF
      10 0 MOVZ,  10 SP 8 STR,                       \ ss_flags (int) and its padding
      10 PROF-STACK-BYTES LIT64,  10 SP 16 STR,      \ ss_size
   ELSE
      10 PROF-STACK-BYTES LIT64,  10 SP 8 STR,       \ Darwin: ss_size second
      10 0 MOVZ,  10 SP 16 STR,                      \ ss_flags third
   THEN
   0 SP 0 ADDI,  1 0 MOVZ,  NR-SIGALTSTACK SYS,
   C-CC sok BCOND,                                   \ a refused alternate stack would leave the handler on the interrupted one: fail closed, named
      1 smsg ADR,  2 PROFSTKMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
      0 78 MOVZ,  NR-EXIT-GROUP SYS,
      smsg LBL,  S\" hb: prof-on: cannot install the handler stack\n" BYTES,
   sok LBL,
   SP SP 32 ADDI, ;

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

: C-PROF-SIGACTION ( -- )
   0 SIGALRM MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 LINUX-SIGSET-SIZE MOVZ, THEN
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

\ prof-on runs as Habu code, where DBASE and NDICT are live: it records the base
\ the handler will trust and clears one counter per current record.
: BPROF-ON ( -- )
   LBL LBL {: zl zd :}
   7 PROF-BAND-VA LIT64,
   A G-POP  A 7 PROF-LIM STR,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   DBASE 7 PROF-DBASE STR,
   14 PROF-CNT-VA LIT64,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   C-PROF-ALTSTACK
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE ;

: BPROF-REPORT ( -- )  SP SP 16 SUBI,  30 SP 0 STR,  15 NDICT 0 ADDI,  LPROFDUMP LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L ;

;using

;package
