\ prof.f — the in-binary sampling profiler for the native engine. `n prof-on`
\ = SIGALRM + 1 ms timer; ticks map the interrupted pc to its dict word and count;
\ at the limit (0 = none): dump "name count" + exit(99). prof-report dumps on
\ demand. prof-pc>rec answers the handler's attribution question from Habu code.
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
\ a context that holds both is attributed; any other context is a "(foreign)"
\ sample. The handler runs on its own alternate stack, so a tick with the
\ machine stack deep never pushes the signal frame over the data stack. The dump reads the same band, so it needs no register either.
using A64ASM

package PROF

public
variable LPROFH   variable LPROFDUMP   variable LPROFFIND
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
56 constant PROF-ARENA      \ the profiler arena, mapped once per process

\ ---- the arena: the pc index the handler searches -----------------------------
\ THE HANDLER NEVER WALKS THE DICTIONARY. It searches a pc-sorted live-range
\ index prof-on builds, so a tick costs log2(entries) compares instead of a scan
\ that grows with the dictionary (measured 240,021 instructions per tick at
\ ndict 15,835 before this index; the walk stopped at the FIRST record holding
\ the pc, so its cost was the hot word's record number, not a constant).
\
\ The band cell PROF-ARENA holds the arena base and every table below it sits at
\ a build-time offset from that base, so the handler reaches any of them with one
\ load and no bound of its own. The arena is mapped once per process, like the
\ alternate stack, and rebuilt in place by each prof-on.
\
\ WHAT THE INDEX ANSWERS, exactly, so a linear reference can state the same rule:
\ the entry with the greatest start <= pc, reported only when pc < that entry's
\ end. Entries are sorted by start with a STABLE merge, so records that share a
\ start (an alias and its original) keep dictionary order and the search lands on
\ the last of them. Records with no code of their own (a namespace row, a retired
\ row, a zero-length body) are not in the index at all.
64 constant ARN-HDR         \ arena header bytes, then the index
0  constant ARN-COUNT       \ index entries
8  constant ARN-LO          \ lowest code address in the index
16 constant ARN-HI          \ one past the highest
24 constant ARN-NDICT       \ the record count the index was built from
32 constant ARN-NEW         \ samples at or above ARN-HI: code compiled after the build
32 constant PROF-ENT        \ index entry bytes
0  constant ENT-START
8  constant ENT-END
16 constant ENT-IDX         \ the dictionary record index, which owns the counter
24 constant ENT-INCL        \ inclusive samples (the caller slice fills it)
DICT-CAP PROF-ENT * constant ARN-IDX-BYTES
ARN-HDR constant ARN-IDX                       \ the index itself
ARN-IDX ARN-IDX-BYTES + constant ARN-SCR       \ the merge sort's second half
ARN-SCR ARN-IDX-BYTES + constant ARN-BYTES
$10000 constant PROF-STACK-BYTES
14  constant SIGALRM
$18000004 constant LINUX-SA-PROF-FLAGS   \ SA_SIGINFO | SA_ONSTACK | SA_RESTART
$0043 constant MACOS-SA-PROF-FLAGS        \ SA_ONSTACK | SA_RESTART | SA_SIGINFO
42 constant PROFMMAPMSG-LEN               \ "hb: prof-on: cannot map the handler stack\n"
46 constant PROFSTKMSG-LEN                \ "hb: prof-on: cannot install the handler stack\n"
43 constant PROFARNMSG-LEN                \ "hb: prof-on: cannot map the profiler arena\n"
40 constant DICT-WL-OFF                   \ the record's wordlist cell (habu1.f BSWL reads the same 40)

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

\ LPROFFIND ( x9 = pc, x8 = arena -- x6 = index entry or 0 ): the whole of pc
\ attribution, shared by the signal handler and by `prof-pc>rec`, so the property
\ test exercises the code the handler runs and not a copy of it. Bounded: one
\ range test plus at most log2(DICT-CAP) = 16 compares. Clobbers x10-x13 and
\ obeys the primitive ABI (x19-x28 untouched), because prof-pc>rec calls it from
\ compiled Habu code.
: EMIT-PROFFIND ( -- )
   LPROFFIND LABEL@ LBL,
   LBL LBL LBL LBL LBL {: bl bhi bdone miss out :}
   10 8 ARN-COUNT LDR,  10 miss CBZ,
   11 8 ARN-LO LDR,  9 11 CMP,  C-CC miss BCOND,      \ below every span
   11 8 ARN-HI LDR,  9 11 CMP,  C-CS miss BCOND,      \ at or above every span
   11 0 MOVZ,                                         \ lo
   bl LBL,                                            \ rightmost entry with start <= pc
      11 10 CMP,  C-CS bdone BCOND,
      12 11 10 ADD,  12 12 1 LSRI,                    \ mid = (lo + hi) / 2
      13 12 5 LSLI,  13 8 13 ADD,  13 13 ARN-IDX ADDI,
      13 13 ENT-START LDR,
      13 9 CMP,  C-HI bhi BCOND,                      \ start > pc: mid is the new hi
      11 12 1 ADDI,  bl B,
   bhi LBL,  10 12 0 ADDI,  bl B,
   bdone LBL,
   11 miss CBZ,                                       \ every start is above the pc
   11 11 1 SUBI,  6 11 5 LSLI,  6 8 6 ADD,  6 6 ARN-IDX ADDI,
   13 6 ENT-END LDR,  9 13 CMP,  C-CC out BCOND,      \ the pc sits in a gap above it
   miss LBL,  6 0 MOVZ,
   out LBL,  RET, ;

\ LPROFDUMP ( x15 = record count ): one "name count" line per counted record,
\ then "(other) N", "(new) N" and "(foreign) N" when non-zero. Every input comes from the
\ band or from x15, so the handler may call it from a validated sample and
\ prof-report from Habu code.
: EMIT-PROFDUMP ( -- )
   LPROFDUMP LABEL@ LBL,
   LBL LBL LBL LBL LBL LBL LBL {: dl dn dd dnew dfor dret pinl :}
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
   dd LBL,  7 PROF-BAND-VA LIT64,  17 7 PROF-OTHER LDR,  17 dnew CBZ,     \ "(other) N" if any
      SP SP 16 SUBI,  12 $2029726568746F28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 8 MOVZ,  NR-WRITE SYS,
      SP SP 16 ADDI,
      9 17 0 ADDI,  G-PRINT9
   dnew LBL,  7 PROF-BAND-VA LIT64,  17 7 PROF-ARENA LDR,  17 dfor CBZ,   \ "(new) N" if any
      17 17 ARN-NEW LDR,  17 dfor CBZ,
      SP SP 16 SUBI,  12 $202977656E28 LIT64,  12 SP 0 STR,
      0 1 MOVZ,  1 SP 0 ADDI,  2 6 MOVZ,  NR-WRITE SYS,
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
   LBL LBL LBL LBL LBL LBL {: pnew pdone pforeign psig pexit pcount :}
   C-PROF-MCTX>R21  C-PROF-PC>R9
   7 PROF-BAND-VA LIT64,
   20 10 C-PROF-CTX-X>R  11 DATA-VA VA>N LIT64,  10 11 CMP,  C-NE pforeign BCOND,   \ context x20 must be DATA
   26 10 C-PROF-CTX-X>R  11 7 PROF-DBASE LDR,  10 11 CMP,  C-NE pforeign BCOND,   \ context x26 must be the recorded DBASE
   17 1 MOVZ,                                        \ x17 = 1: a Habu context, its count may dump (x16 is the Darwin syscall number, never kept across a sys)
   8 7 PROF-ARENA LDR,  8 pdone CBZ,                 \ no index: the sample is still counted
   SP SP 16 SUBI,  30 SP 0 STR,                      \ the alternate stack frames the search
   LPROFFIND LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   6 pnew CBZ,
   12 6 ENT-IDX LDR,                                 \ the record owning the pc: bump its counter
   14 PROF-CNT-VA LIT64,  13 12 3 LSLI,  14 14 13 ADD,
   12 14 0 LDR,  12 12 1 ADDI,  12 14 0 STR,
   psig B,
   pnew LBL,                                         \ Habu code the index does not name
   11 8 ARN-HI LDR,  9 11 CMP,  C-CC pdone BCOND,    \ below the index's high mark: engine helpers, main loop, a gap
   12 8 ARN-NEW LDR,  12 12 1 ADDI,  12 8 ARN-NEW STR,  \ at or above it: compiled after prof-on built the index
   psig B,
   pdone LBL,
   12 7 PROF-OTHER LDR,  12 12 1 ADDI,  12 7 PROF-OTHER STR,
   psig B,
   pforeign LBL,
   17 0 MOVZ,                                        \ not Habu code: no search, no dump from here
   12 7 PROF-FOREIGN LDR,  12 12 1 ADDI,  12 7 PROF-FOREIGN STR,
   psig LBL,
   10 7 PROF-TOT LDR,  10 10 1 ADDI,  10 7 PROF-TOT STR,
   11 7 PROF-LIM LDR,  11 pexit CBZ,                 \ limit 0: no auto-dump, sample to exit
   10 11 CMP,  C-LT pexit BCOND,
   17 pexit CBZ,                                     \ the limit reached on a foreign sample: the next Habu sample dumps
   8 7 PROF-ARENA LDR,  15 0 MOVZ,  8 pcount CBZ,    \ x15 = the record count the dump walks
   15 8 ARN-NDICT LDR,
   pcount LBL,
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

\ The arena, mapped on the first prof-on of the process and kept in the band.
\ A refused mapping is named and fatal for the same reason the handler stack is:
\ a profiler that silently kept the old dictionary walk would report numbers
\ nobody could trust.
: C-PROF-ARENA-MAP ( -- )
   LBL LBL LBL {: have mok amsg :}
   7 PROF-BAND-VA LIT64,  9 7 PROF-ARENA LDR,  9 have CBNZ,
      0 0 MOVZ,  1 ARN-BYTES LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      C-CC mok BCOND,
         1 amsg ADR,  2 PROFARNMSG-LEN MOVZ,  0 2 MOVZ,  NR-WRITE SYS,
         0 78 MOVZ,  NR-EXIT-GROUP SYS,
      amsg LBL,  S\" hb: prof-on: cannot map the profiler arena\n" BYTES,
      mok LBL,
      7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA STR,
   have LBL, ;

\ One index entry per live record with code of its own, in dictionary order.
\ x0 = arena; leaves x11 = the entry count and fills the header's lo/hi marks.
\ A record whose wordlist cell is DICT-WL:NAMESPACE (-1) or DICT-WL:RETIRED (-2)
\ owns no code, and neither does one whose code span is empty; the +2 test names
\ both of those wordlists at once.
: C-PROF-INDEX-BUILD ( -- )
   LBL LBL LBL LBL LBL {: bl bnext bdone blo bhi :}
   9 ARN-IDX LIT64,  9 0 9 ADD,
   11 0 MOVZ,  6 0 MOVZ,  5 DBASE 0 ADDI,
   1 0 MOVN,  2 0 MOVZ,                              \ lo = all ones, hi = 0
   bl LBL,
      6 NDICT CMP,  C-CS bdone BCOND,
      12 5 DICT-WL-OFF LDR,  12 12 2 ADDI,  12 2 CMPI,  C-CC bnext BCOND,
      13 5 8 LDR,  C-PROF-SPAN-BYTES  13 bnext CBZ,
      12 5 0 LDR,  12 bnext CBZ,
      12 9 ENT-START STR,
      14 12 13 ADD,  14 9 ENT-END STR,
      6 9 ENT-IDX STR,
      15 0 MOVZ,  15 9 ENT-INCL STR,
      9 9 PROF-ENT ADDI,  11 11 1 ADDI,
      12 1 CMP,  C-CS blo BCOND,  1 12 0 ADDI,
      blo LBL,  14 2 CMP,  C-LS bhi BCOND,  2 14 0 ADDI,
      bhi LBL,
   bnext LBL,  5 5 DREC ADDI,  6 6 1 ADDI,  bl B,
   bdone LBL,
   11 0 ARN-COUNT STR,  1 0 ARN-LO STR,  2 0 ARN-HI STR,
   NDICT 0 ARN-NDICT STR,
   15 0 MOVZ,  15 0 ARN-NEW STR, ;

\ The merge's two moves: one entry from the a side (x17) or the b side (x5) to
\ the output cursor (x6), each source advancing with it.
: C-PROF-ENT-COPY-A ( -- )
   3 17 0 LDR,   3 6 0 STR,
   3 17 8 LDR,   3 6 8 STR,
   3 17 16 LDR,  3 6 16 STR,
   3 17 24 LDR,  3 6 24 STR,
   17 17 PROF-ENT ADDI,  6 6 PROF-ENT ADDI, ;

: C-PROF-ENT-COPY-B ( -- )
   3 5 0 LDR,   3 6 0 STR,
   3 5 8 LDR,   3 6 8 STR,
   3 5 16 LDR,  3 6 16 STR,
   3 5 24 LDR,  3 6 24 STR,
   5 5 PROF-ENT ADDI,  6 6 PROF-ENT ADDI, ;

\ An odd number of passes leaves the sorted entries in the scratch half; bring
\ them home so the index always lives at one address the handler knows.
: C-PROF-SORT-LAND ( -- )
   LBL LBL {: cpl fin :}
   2 ARN-IDX LIT64,  2 0 2 ADD,
   9 2 CMP,  C-EQ fin BCOND,
   3 0 MOVZ,
   cpl LBL,
      3 14 CMP,  C-CS fin BCOND,
      4 9 3 ADD,   4 4 0 LDR,
      15 2 3 ADD,  4 15 0 STR,
      3 3 8 ADDI,  cpl B,
   fin LBL, ;

\ Bottom-up merge sort of x11 entries by start address, arena in x0, using the
\ second half of the arena as the other side of each pass. Stable, so records
\ that share a start stay in dictionary order and the search's "last start <= pc"
\ answer is the same one an exhaustive scan in record order reports. O(n log n)
\ once per prof-on rather than O(n) in every tick.
: C-PROF-SORT ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL {: pass block mrg ta tb arest brest bfin cb done skip :}
   11 1 CMPI,  C-LS skip BCOND,
   9 ARN-IDX LIT64,  9 0 9 ADD,
   10 ARN-SCR LIT64,  10 0 10 ADD,
   14 PROF-ENT MOVZ,  14 11 14 MUL,                  \ total bytes
   12 PROF-ENT MOVZ,                                 \ merge width, bytes
   pass LBL,
      12 14 CMP,  C-CS done BCOND,
      13 9 0 ADDI,  11 9 14 ADD,
      block LBL,
         13 11 CMP,  C-CS bfin BCOND,
         15 13 12 ADD,  15 11 CMP,  15 15 11 C-LS CSEL,     \ mid
         16 15 12 ADD,  16 11 CMP,  16 16 11 C-LS CSEL,     \ hi
         17 13 0 ADDI,  5 15 0 ADDI,
         6 13 9 SUB,  6 6 10 ADD,
         mrg LBL,
            17 15 CMP,  C-CS brest BCOND,
            5 16 CMP,  C-CS arest BCOND,
            3 17 ENT-START LDR,  4 5 ENT-START LDR,  3 4 CMP,  C-HI tb BCOND,
         ta LBL,  C-PROF-ENT-COPY-A  mrg B,
         tb LBL,  C-PROF-ENT-COPY-B  mrg B,
         arest LBL,  17 15 CMP,  C-CS cb BCOND,  C-PROF-ENT-COPY-A  arest B,
         brest LBL,  5 16 CMP,  C-CS cb BCOND,  C-PROF-ENT-COPY-B  brest B,
         cb LBL,  13 16 0 ADDI,  block B,
      bfin LBL,
      3 9 0 ADDI,  9 10 0 ADDI,  10 3 0 ADDI,        \ this pass's output is the next pass's input
      12 12 1 LSLI,  pass B,
   done LBL,
   C-PROF-SORT-LAND
   skip LBL, ;

\ prof-on runs as Habu code, where DBASE and NDICT are live: it records the base
\ the handler will trust, clears one counter per current record, and builds the
\ index the handler searches.
: BPROF-ON ( -- )
   LBL LBL {: zl zd :}
   7 PROF-BAND-VA LIT64,
   A G-POP  A 7 PROF-LIM STR,
   9 0 MOVZ,  9 7 PROF-TOT STR,  9 7 PROF-OTHER STR,  9 7 PROF-FOREIGN STR,
   DBASE 7 PROF-DBASE STR,
   14 PROF-CNT-VA LIT64,  8 NDICT 0 ADDI,
   zl LBL,  8 zd CBZ,  9 0 MOVZ,  9 14 0 STR,  14 14 8 ADDI,  8 8 1 SUBI,  zl B,
   zd LBL,
   C-PROF-ARENA-MAP
   7 PROF-BAND-VA LIT64,  0 7 PROF-ARENA LDR,
   C-PROF-INDEX-BUILD
   C-PROF-SORT
   C-PROF-ALTSTACK
   C-PROF-SIGACTION-FRAME
   C-PROF-SIGACTION
   C-PROF-SIGACTION-DONE
   C-PROF-TIMER-FRAME
   C-PROF-TIMER
   C-PROF-TIMER-DONE ;

: BPROF-REPORT ( -- )  SP SP 16 SUBI,  30 SP 0 STR,  15 NDICT 0 ADDI,  LPROFDUMP LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

\ prof-pc>rec ( pc -- n ): the record index the armed index gives that pc, or -1.
\ It runs the handler's own search, so a test that compares it with an exhaustive
\ scan of the live dictionary is testing the attribution the handler performs.
: BPROF-PCREC ( -- )
   LBL LBL {: miss out :}
   SP SP 16 SUBI,  30 SP 0 STR,
   9 G-POP
   7 PROF-BAND-VA LIT64,  8 7 PROF-ARENA LDR,  8 miss CBZ,
   LPROFFIND LABEL@ BL,
   6 miss CBZ,
   9 6 ENT-IDX LDR,  out B,
   miss LBL,  9 0 MOVN,
   out LBL,  9 G-PUSH
   30 SP 0 LDR,  SP SP 16 ADDI, ;

public

: EMIT-PROF-PRIMS ( -- )
   s" prof-on" ['] BPROF-ON FPRIM-L  s" prof-report" ['] BPROF-REPORT FPRIM-L
   s" prof-pc>rec" ['] BPROF-PCREC FPRIM-L ;

;using

;package
