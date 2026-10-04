\ x86-64-kernel-prof.f - the profiler rows of the x86-64 kernel
\ (src/habu/kernel-x64.f PROFILER,, src/habu/prof-x64.f) in the booted
\ harness, cross-built for an x86-64 peer. The samples case seeds two records
\ whose code is its own text: A, a `call B` alone, and B, a spin, with A's
\ `ret` in the gap between them, so the return address the walk finds is A's
\ end. Three spins are no record: one below A and two above B. The report
\ case reads prof-report, prof-json and prof-row on fd 1 through a pipe and
\ compares every byte with what this host's own rows print for the same
\ state, captured at load through a pipe dup2'd onto its fd 1: once with no
\ arena, and once for state S below over records the image seeds at the
\ host's own record indices, so the edge table keys the same pairs into the
\ same slots. The `indexed` count is each engine's live one: the image's
\ replaces the host's in what the image expects. Each image is one case, and
\ the peer that runs it must see its status:
\
\    hb-x64-kernel-prof           0  prof-on at the default rate; A calls B
\                                    with rsp 64 bytes above the DO/LOOP
\                                    stack's lower guard, where a signal frame
\                                    pushed on that stack dies of SIGSEGV
\                                    (139), and B spins until its counter and
\                                    A's inclusive count move, five scratch
\                                    registers it holds intact; the spin below
\                                    A, rsp 64 bytes under the stack's upper
\                                    guard, until PROF-OTHER moves; one above
\                                    B until ARN-DEFER moves, its newest slot
\                                    the spin's pc and return address; one with
\                                    rbp moved until PROF-FOREIGN moves; after
\                                    prof-off the identity of prof.f EMIT-PROF;
\                                    prof-pc>rec of B and of the gap, and the
\                                    edge (B, A) and B's inclusive count are
\                                    nonzero; prof-reset clears the band, the
\                                    counts, the edges and the stamps; prof-rate
\                                    250 then prof-on arm the timer at 250 us
\                                    and prof-off disarms it
\    hb-x64-kernel-prof-negative 21  the same, expecting B's spin to fail
\    hb-x64-kernel-prof-shared-stack
\                                 0  prof-rate maps the arena, RLIMIT_AS 0,
\                                    then prof-on/off works using the runtime
\                                    signal stack, without mapping another
\    hb-x64-kernel-prof-report    0  prof-report, prof-json and prof-row with
\                                    no arena; prof-on, prof-off, prof-reset
\                                    and state S, then prof-report, B's
\                                    prof-row and prof-json, each the host's
\                                    bytes; prof-on again, and PROF-LIM 1
\                                    while rbp is moved until PROF-FOREIGN
\                                    moves: a foreign sample at the limit
\                                    returns; a report while armed arms
\                                    ITIMER_REAL again at 1000 us
\    hb-x64-kernel-prof-limit     0  forks; the child, its fd 1 the pipe, runs
\                                    3 prof-on and spins, and the parent sees
\                                    it exit 99 with its fd 1 starting
\                                    `profiler samples 3 words `
\    hb-x64-kernel-prof-slow      0  prof-rate 1500000 and prof-on arm the
\                                    timer at 1 s and 500000 us; four seconds
\                                    by mono-ns take at least two samples
\    hb-x64-kernel-prof-resets    0  prof-rate 50, then 1000 turns of
\                                    prof-on, a delay of 0 to 50.4 us,
\                                    prof-reset, prof-off and the identity:
\                                    none finds it false
\    hb-x64-kernel-prof-rate-refused
\                                67  prof-rate -1 throws E-PROF-RATE, which
\                                    no handler catches
\    hb-x64-kernel-prof-rate-caught
\                                 0  prof-rate 250, then prof-rate -1 under
\                                    catch: the code is E-PROF-RATE, and
\                                    prof-on arms the timer at 250 us
\    hb-x64-kernel-prof-arm-refused
\                                78  -1 written into ARN-USEC past prof-rate,
\                                    then prof-on: `hb: prof-on: cannot arm
\                                    the interval timer` on fd 2
\
\ State S: B's pc deferred three times with A's return address kept beside it,
\ twice with C's and once with 0, and pc 1, which no record owns, once; A's
\ inclusive count 9 with no exclusive one, 4 in the per-record array and 5 in
\ its index entry; ARN-HI at C's start, as if C were compiled after prof-on;
\ PROF-OTHER, PROF-FOREIGN, ARN-SPILL, ARN-FRAMES and ARN-DROP nonzero; and
\ PROF-TOT their sum with the deferred samples. A report's sync folds A's
\ entry into the array and replays the deferred samples: B takes six
\ exclusive and inclusive samples, the edges (B, A), (B, C) and (B, none)
\ count 3, 2 and 1, C, which only the rebuild reaches, takes two inclusive
\ samples, and ARN-NEW the sample at pc 1. A and B are global, and C, whose
\ name is longer than DNAME-INL, is package X64K-PROF-Q's, so the reports
\ print an inline name, one behind DNAME-EXT and a qualifier.
\
\ A spin that waits for a tick is bounded, so a tick that never comes fails
\ its check instead of hanging the image. The host checks nothing but the
\ build; running the images is the peer's.
require src/habu/layout.f
require src/habu/stack-abi.f
require src/habu/prof-abi.f
require src/os/linux-x86-64/target-layout.f
require lib/string.f
require src/core/bytes.f
require src/habu/xref.f
require test/x86-64-boot-harness.f

\ The host's words the report case names, at the host's own record indices.
: prof-a ( -- n ) 1 ;
: prof-b ( -- n ) 2 ;

package X64K-PROF-Q
public
: CALLER-WITH-A-LONG-NAME ( -- n ) 3 ;
;package

package X64K-PROF
using X64ASM
using X64CODE
using X64RT
using PROF-ABI
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

\ The band at the top of the target's DATA, and the counters after its cells.
X64LAYOUT:DATA-VA VA>N X64LAYOUT:DATA-SIZE PROF-BAND-AT constant BAND
BAND PROF-STATE-BYTES + constant COUNTERS
0 constant A-REC                        \ the records' indices, in seeding order
1 constant B-REC
$40000000 constant SPIN-BOUND           \ turns a spin waits: about a second, a thousand default ticks
64 constant GUARD-GAP                   \ rsp's distance from a guard page in the two stack cases
250 constant RATE                       \ the prof-rate case's interval, microseconds
-1 constant POISON                      \ an answer no check expects
36 constant NR-GETITIMER
160 constant NR-SETRLIMIT
0 constant ITIMER-REAL
9 constant RLIMIT-AS
32 constant ITIMER-BYTES                \ struct itimerval: the interval, then the value
8 constant IT-USEC                      \ the interval's microseconds

\ ---- the report case's facts ---------------------------------------------------
1 constant STDOUT
0 constant F-DUPFD
3 constant FD-FLOOR                     \ F_DUPFD's lowest descriptor: past the standard three
6 constant TEXTS                        \ the captures the report case compares
$1000 constant TEXT-CAP                 \ a capture's room, and what the image reads into
8 constant RFD                          \ scratch (PUSH-SCRATCH,): the pipe's two ends,
16 constant WFD
24 constant SAVED                       \ fd 1 as the image found it
$40 constant GOT                        \ and what it read
1000 constant DEFAULT-RATE              \ prof-on's interval when prof-rate set none, microseconds
3 constant LIMIT-SAMPLES                \ the limit case's prof-on
1500000 constant SLOW-RATE              \ the slow case's interval: a second and a half, microseconds
4000000000 constant SLOW-PHASE-NS       \ its phase on the wall clock, which ticks at 1.5 s and 3 s
2 constant SLOW-TICKS                   \ the samples that phase takes at least
50 constant FAST-RATE                   \ the resets case's interval, microseconds
1000 constant RESET-TURNS               \ the resets that case runs
32 constant TURNS                       \ scratch: the turns left
40 constant SKEWED                      \ and those that found the identity false
64 constant PHASES                      \ the delays between a turn's arm and its reset
800 constant PHASE-STEP-NS              \ and their step: together they cross the interval
3 constant IMAGE-INDEXED                \ the report image indexes A, B and C: one digit
4 constant A64-INSN                     \ how far back the host's replay searches a kept cell

\ State S, the header's.
3 constant FROM-A                       \ B's deferred samples A called, in the slots from 0
2 constant FROM-C                       \ and those C called, in the slots after
FROM-A FROM-C + constant NONE-AT        \ B's sample whose kept cell is 0
NONE-AT 1+ constant PC1-AT              \ the sample at pc 1
PC1-AT 1+ constant DEFERRED
5 constant S-OTHER
4 constant S-FOREIGN
3 constant S-SPILL
11 constant S-FRAMES
2 constant S-DROP
4 constant S-A-ARRAY                    \ A's inclusive samples in the per-record array
5 constant S-A-ENTRY                    \ and in its index entry, which the sync folds
DEFERRED S-OTHER + S-FOREIGN + S-SPILL + constant S-TOT

\ The host's records, the package's wids and the code starts, which the
\ report image seeds and states at the same indices.
s" prof-a" XREF-FIND-INDEX XREF-REQUIRE-INDEX constant A-IX
s" prof-b" XREF-FIND-INDEX XREF-REQUIRE-INDEX constant B-IX
s" X64K-PROF-Q:CALLER-WITH-A-LONG-NAME" XREF-FIND-INDEX XREF-REQUIRE-INDEX constant C-IX
s" X64K-PROF-Q" XREF-NAMESPACE-WL XREF-FIND-WL-INDEX XREF-REQUIRE-INDEX constant Q-IX
Q-IX XREF-REC XREF-PKG-PUBLIC constant Q-PUBLIC
Q-IX XREF-REC XREF-PKG-PRIVATE constant Q-PRIVATE
C-IX XREF-REC XREF-WORDLIST constant C-WID
A-IX B-IX max C-IX max Q-IX max 1+ constant TOP-IX
A-IX XREF-REC XREF-START constant A-START
B-IX XREF-REC XREF-START constant B-START
C-IX XREF-REC XREF-START constant C-START

\ The host's band as an offset from data-base: the band closes the running
\ engine's DATA, whose last cell SNAP-RELOC:XTCELL-OFF-MAX states as the
\ engine was built. The bare DATA-SIZE is only the host global, which the
\ macOS layout shim stands another host's value in for.
0 SNAP-RELOC:XTCELL-OFF-MAX CELL + PROF-BAND-AT constant HOST-BAND

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: LOAD, ( r64 mem -- ) ASM-SINK ENC-MOV-RM ;
: STORE, ( r64 mem -- ) ASM-SINK ENC-MOV-MR ;
: COPY, ( r64 r64 -- ) ASM-SINK ENC-MOV-RR ;
: TEST, ( r64 -- ) dup ASM-SINK ENC-TEST-RR ;
: IMM, ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;
: ADDI, ( r64 n -- ) >IMM32 ASM-SINK ENC-ADD-RI32 ;
: SUBI, ( r64 n -- ) >IMM32 ASM-SINK ENC-SUB-RI32 ;

\ r = the arena's base, read from the band.
: ARENA, ( r64 -- ) {: r:r64 :}  r BAND IMM,  r r PROF-ARENA MEM-OFF LOAD, ;

\ Replace rax with POISON when the condition holds. It clobbers r11; a mov
\ keeps the flags the condition reads.
: POISON-IF, ( condition -- ) {: c:condition :}
   R11 POISON IMM,
   c RAX R11 ASM-SINK ENC-CMOVCC ;

\ Poison rax unless the register holds n.
: KEEP-IF-EQ, ( r64 n -- ) {: r:r64 v:n :}
   r v >IMM32 ASM-SINK ENC-CMP-RI32
   C-NE POISON-IF, ;

\ ---- the spins ---------------------------------------------------------------
\ Spin until the cell `off` bytes past r8 moves from the value it held on
\ entry, at most SPIN-BOUND turns: rax = 1 when it moved, 0 when the bound ran
\ out. It clobbers rcx and rdx.
: WAIT-MOVE, ( n -- ) {: off:n :}
   LBL LBL LBL {: turn:label moved:label out:label :}
   RDX R8 off MEM-OFF LOAD,
   RCX SPIN-BOUND IMM,
   turn LBL,
      RDX R8 off MEM-OFF ASM-SINK ENC-CMP-RM  C-NE moved JCC,
      RCX ASM-SINK ENC-DEC  C-NE turn JCC,
   RAX ZERO-REG,  out JMP,
   moved LBL,  RAX 1 IMM,
   out LBL, ;

\ The value a held register keeps across the ticks, one per register.
: SENTINEL ( r64 -- n ) R64>N $5E000000 + ;
: SEED, ( r64 -- ) {: r:r64 :}  r r SENTINEL >IMM32 ASM-SINK ENC-MOV-RI32 ;
: HELD, ( r64 -- ) {: r:r64 :}  r r SENTINEL KEEP-IF-EQ, ;

\ B's body: spin until B's counter and A's inclusive count, in the first index
\ entry, both leave 0; then rax = 1 while every held register keeps its
\ sentinel, r11 checked first since the checks borrow it. rax = 0 when the
\ bound ran out.
: B-SPIN, ( -- )
   LBL LBL LBL LBL {: turn:label next:label moved:label out:label :}
   R8 COUNTERS B-REC CELL * + IMM,
   R9 ARENA,  R9 ARN-IDX ENT-INCL + ADDI,
   RDX SEED,  RSI SEED,  RDI SEED,  R10 SEED,  R11 SEED,
   RCX SPIN-BOUND IMM,
   turn LBL,
      RAX R8 MEM-AT LOAD,  RAX TEST,  C-E next JCC,
      RAX R9 MEM-AT LOAD,  RAX TEST,  C-NE moved JCC,
   next LBL,
      RCX ASM-SINK ENC-DEC  C-NE turn JCC,
   RAX ZERO-REG,  out JMP,
   moved LBL,
   RAX 1 IMM,
   R11 HELD,  RDX HELD,  RSI HELD,  RDI HELD,  R10 HELD,
   out LBL, ;

\ A and B behind a jump, each span its first label to its second.
: RECORDS, ( -- label label label label )
   LBL LBL LBL LBL {: a:label a-end:label b:label b-end:label :}
   LBL {: past:label :}
   past JMP,
   a LBL,  b CALL,  a-end LBL,
   ASM-SINK ENC-RET
   b LBL,  B-SPIN,  ASM-SINK ENC-RET  b-end LBL,
   past LBL,
   a a-end b b-end ;

\ The spin above B: once ARN-DEFER moves, the newest slot must hold a pc in
\ this routine and the cell at the interrupted rsp, this routine's return
\ address, since it pushes nothing.
: HIGH, ( -- label )
   LBL LBL LBL {: start:label end:label past:label :}
   past JMP,
   start LBL,
   R8 ARENA,
   ARN-DEFER WAIT-MOVE,
   R9 R8 ARN-DEFER MEM-OFF LOAD,
   R9 R9 PROF-DEFER-ENT >IMM8 ASM-SINK ENC-IMUL-RRI8
   R9 R8 R9 1 ARN-DEF PROF-DEFER-ENT - MEM-IDX ASM-SINK ENC-LEA
   R10 R9 CELL MEM-OFF LOAD,
   R10 RSP MEM-AT ASM-SINK ENC-CMP-RM  C-NE POISON-IF,
   R10 R9 MEM-AT LOAD,
   RCX start MOVABS,  R10 RCX ASM-SINK ENC-CMP-RR  C-B POISON-IF,
   RCX end MOVABS,  R10 RCX ASM-SINK ENC-CMP-RR  C-AE POISON-IF,
   ASM-SINK ENC-RET
   end LBL,
   past LBL,
   start ;

\ The foreign spin: rbp is not DATA while it waits for PROF-FOREIGN.
: FOREIGN-SPIN, ( -- )
   R10 DATA-REG COPY,  DATA-REG ZERO-REG,
   R8 BAND IMM,  PROF-FOREIGN WAIT-MOVE,
   DATA-REG R10 COPY, ;

\ Call a routine with rsp n bytes past the DO/LOOP stack's base, put rsp back
\ from the scratch cell and push the routine's rax.
: STACK-CALL, ( label n -- ) {: at:label off:n :}
   0 X64HARNESS:PUSH-SCRATCH,  RCX R64>N G-POP  RSP RCX MEM-AT STORE,
   RSP DATA-REG STACK-ABI:LOOP-BASE-CELL MEM-OFF LOAD,
   RSP off CELL + ADDI,                       \ the call's push leaves the routine at n
   at CALL,
   0 G-PUSH
   0 X64HARNESS:PUSH-SCRATCH,  RCX R64>N G-POP  RSP RCX MEM-AT LOAD, ;

\ ---- the checks after the clock stops ---------------------------------------
\ rax = sum(counters) + ARN-NEW + ARN-DEFER + ARN-SPILL + PROF-OTHER +
\ PROF-FOREIGN - PROF-TOT, over the records r14 counts: 0 while the identity
\ holds. It clobbers rcx, rdx and r8.
: SKEW, ( -- )
   LBL LBL {: sum:label done:label :}
   RAX ZERO-REG,
   RDX COUNTERS IMM,  RCX ENGINE-GPR:X64-NDICT >R64 COPY,
   sum LBL,
      RCX TEST,  C-E done JCC,
      RAX RDX MEM-AT ASM-SINK ENC-ADD-RM
      RDX CELL ADDI,  RCX ASM-SINK ENC-DEC  sum JMP,
   done LBL,
   R8 ARENA,
   RAX R8 ARN-NEW MEM-OFF ASM-SINK ENC-ADD-RM
   RAX R8 ARN-DEFER MEM-OFF ASM-SINK ENC-ADD-RM
   RAX R8 ARN-SPILL MEM-OFF ASM-SINK ENC-ADD-RM
   R8 BAND IMM,
   RAX R8 PROF-OTHER MEM-OFF ASM-SINK ENC-ADD-RM
   RAX R8 PROF-FOREIGN MEM-OFF ASM-SINK ENC-ADD-RM
   RAX R8 PROF-TOT MEM-OFF ASM-SINK ENC-SUB-RM ;

: IDENTITY, ( -- )  SKEW,  0 G-PUSH  0 WANT ;

\ Poison rax unless the edge table holds the pair (B, A), keyed as EDGE keys
\ it, with a count, and B's own inclusive count, in the second index entry, is
\ nonzero: the walk's edge and the sample's count of itself. It clobbers rcx,
\ rdx and r8-r11.
: EDGE-SEEN, ( -- )
   LBL LBL LBL {: turn:label hit:label out:label :}
   R8 ARENA,
   RCX R8 ARN-CALL MEM-OFF ASM-SINK ENC-LEA
   RDX R8 ARN-INCL MEM-OFF ASM-SINK ENC-LEA
   R9 B-REC PROF-REC-BITS lshift A-REC or 1+ IMM,
   R10 ZERO-REG,
   turn LBL,
      RCX RDX ASM-SINK ENC-CMP-RR  C-AE out JCC,
      R9 RCX MEM-AT ASM-SINK ENC-CMP-RM  C-E hit JCC,
      RCX PROF-CALL-ENT ADDI,  turn JMP,
   hit LBL,  R10 RCX CELL MEM-OFF LOAD,
   out LBL,
   R10 TEST,  C-E POISON-IF,
   R10 R8 ARN-IDX PROF-ENT + ENT-INCL + MEM-OFF LOAD,
   R10 TEST,  C-E POISON-IF, ;

\ prof-pc>rec names B for B's first byte and nothing for A's end, the gap,
\ and the edge (B, A) and B's inclusive count are nonzero.
: PC-CHECK, ( label label -- ) {: b:label gap:label :}
   b 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW
   gap 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW
   RCX R64>N G-POP  RAX R64>N G-POP
   RCX -1 KEEP-IF-EQ,
   EDGE-SEEN,
   0 G-PUSH  B-REC WANT ;

\ After prof-reset, every count the spins moved reads 0: the band's cells, B's
\ counter, the arena's counts, A's inclusive count, and every cell of the edge
\ table, the inclusive array and the stamps, which lie back to back.
: RESET-CHECK, ( -- )
   LBL {: next:label :}
   RAX ZERO-REG,
   R8 BAND IMM,
   RAX R8 PROF-TOT MEM-OFF ASM-SINK ENC-OR-RM
   RAX R8 PROF-OTHER MEM-OFF ASM-SINK ENC-OR-RM
   RAX R8 PROF-FOREIGN MEM-OFF ASM-SINK ENC-OR-RM
   RAX R8 PROF-STATE-BYTES B-REC CELL * + MEM-OFF ASM-SINK ENC-OR-RM
   R8 ARENA,
   RAX R8 ARN-DEFER MEM-OFF ASM-SINK ENC-OR-RM
   RAX R8 ARN-FRAMES MEM-OFF ASM-SINK ENC-OR-RM
   RAX R8 ARN-IDX ENT-INCL + MEM-OFF ASM-SINK ENC-OR-RM
   RCX R8 ARN-CALL MEM-OFF ASM-SINK ENC-LEA
   RDX R8 ARN-DEF MEM-OFF ASM-SINK ENC-LEA
   next LBL,
      RAX RCX MEM-AT ASM-SINK ENC-OR-RM
      RCX CELL ADDI,  RCX RDX ASM-SINK ENC-CMP-RR  C-B next JCC,
   0 G-PUSH  0 WANT ;

\ getitimer(ITIMER_REAL) into the itimerval at rsp.
: TIMER?, ( -- )
   RDI ITIMER-REAL >IMM32 ASM-SINK ENC-MOV-RI32
   RSI RSP COPY,
   NR-GETITIMER SYS, ;

\ prof-on after prof-rate RATE: the timer's interval is RATE us and no
\ seconds; prof-off: every field of the timer reads 0.
: ARMED-CHECK, ( -- )
   0 N,  s" prof-on" ROW
   RSP ITIMER-BYTES SUBI,
   TIMER?,
   RAX RSP IT-USEC MEM-OFF LOAD,
   RCX RSP MEM-AT LOAD,  RCX 0 KEEP-IF-EQ,
   0 G-PUSH
   s" prof-off" ROW
   TIMER?,
   RDX RSP MEM-AT LOAD,
   ITIMER-BYTES CELL ?do  RDX RSP i MEM-OFF ASM-SINK ENC-OR-RM  CELL +loop
   RAX R64>N G-POP  RDX 0 KEEP-IF-EQ,
   RSP ITIMER-BYTES ADDI,
   0 G-PUSH  RATE WANT ;

: RATE-CHECK, ( -- )  RATE N,  s" prof-rate" ROW  ARMED-CHECK, ;

\ ---- the cases ---------------------------------------------------------------
\ The routines first, so the spin below A precedes A in the text and the code
\ after B, the case's own, lies above the index.
: SAMPLES-CASE ( -- )
   [: R8 BAND IMM,  PROF-OTHER WAIT-MOVE, ;] X64HARNESS:ROUTINE, {: low:label :}
   RECORDS, {: a:label a-end:label b:label b-end:label :}
   HIGH, {: high:label :}
   [: FOREIGN-SPIN, ;] X64HARNESS:ROUTINE, {: foreign:label :}
   s" prof-a" a a-end X64HARNESS:CODE-RECORD,
   s" prof-b" b b-end X64HARNESS:CODE-RECORD,
   0 N,  s" prof-on" ROW
   a GUARD-GAP CELL + STACK-CALL,  1 WANT
   low STACK-ABI:LOOP-BYTES GUARD-GAP - STACK-CALL,  1 WANT
   high CALL,  0 G-PUSH  1 WANT
   foreign CALL,  0 G-PUSH  1 WANT
   s" prof-off" ROW
   IDENTITY,
   b a-end PC-CHECK,
   s" prof-reset" ROW  RESET-CHECK,
   RATE-CHECK, ;

\ The arena comes from prof-rate before the limit. With no address space left
\ for mmap, prof-on can only succeed by using the thread's runtime altstack.
: SHARED-STACK-CASE ( -- )
   RATE N,  s" prof-rate" ROW
   RSP 2 CELL * SUBI,
   RAX ZERO-REG,  RAX RSP MEM-AT STORE,  RAX RSP CELL MEM-OFF STORE,
   RDI RLIMIT-AS >IMM32 ASM-SINK ENC-MOV-RI32
   RSI RSP COPY,
   NR-SETRLIMIT SYS,
   RSP 2 CELL * ADDI,
   0 N,  s" prof-on" ROW
   s" prof-off" ROW ;

\ ---- the host's own bytes --------------------------------------------------------
TEXTS TEXT-CAP * BUFFER: TEXT-BYTES
TEXTS TYPED-BUFFER TEXT-LEN n
variable GOT-N
variable AT-BYTE

\ Capture k: its bytes and their count.
: TEXT ( n -- ptr u8 n ) {: k:n :}  TEXT-BYTES k TEXT-CAP * +  k TEXT-LEN @ ;

\ Run the quotation with this host's fd 1 on a pipe, put fd 1 back, which
\ closes the pipe's last write end, and keep what the pipe holds as capture k.
: CAPTURE ( [ -- ] n -- ) {: k:n :}
   STDOUT F-DUPFD FD-FLOOR fcntl {: saved:n :}
   pipe {: r:n w:n rc:n :}
   saved 0 <  rc 0<>  or if s" x86-64-kernel-prof: no pipe to capture fd 1" 1 die then
   w STDOUT dup2 drop
   execute
   saved STDOUT dup2 drop  saved close  w close
   0 GOT-N !
   begin
      r  TEXT-BYTES k TEXT-CAP * + GOT-N @ +  TEXT-CAP GOT-N @ -  read  dup 0 >
   while
      GOT-N +!
   repeat drop
   r close
   GOT-N @ TEXT-CAP >= if s" x86-64-kernel-prof: a capture outgrew its room" 1 die then
   GOT-N @ k TEXT-LEN ! ;

: HOST-BAND@ ( n -- n ) {: off:n :}  data-base HOST-BAND + off + @ ;
: HOST-BAND! ( n n -- ) {: v:n off:n :}  v  data-base HOST-BAND + off +  ! ;

\ The one cast: the arena's address, which the band holds as a number.
CAST: >ARENA ( n -- ptr n )

: ARENA@ ( n -- n ) {: off:n :}  PROF-ARENA HOST-BAND@ >ARENA off + @ ;
: ARENA! ( n n -- ) {: v:n off:n :}  v  PROF-ARENA HOST-BAND@ >ARENA off +  ! ;

\ The arena offset of the index entry that names record ix.
variable ENTRY-AT
: HOST-ENTRY ( n -- n ) {: ix:n :}
   -1 ENTRY-AT !
   ARN-COUNT ARENA@ 0 ?do
      ARN-IDX i PROF-ENT * + ENT-IDX + ARENA@ ix = if  ARN-IDX i PROF-ENT * + ENTRY-AT !  then
   loop
   ENTRY-AT @ 0 < if s" x86-64-kernel-prof: the index names no prof-a" 1 die then
   ENTRY-AT @ ;

\ Deferred slot i's offset in the arena: its pc, then the cell kept beside it.
: SLOT ( n -- n ) PROF-DEFER-ENT * ARN-DEF + ;

: HOST-SLOT! ( n n n -- ) {: pc:n cell:n i:n :}
   pc i SLOT ARENA!  cell i SLOT CELL + ARENA! ;

\ State S in this host's band and arena. The cells kept beside B's pc lie an
\ instruction past A's and C's starts, so the replay's search one instruction
\ back lands on each start.
: HOST-STATE ( -- )
   S-TOT PROF-TOT HOST-BAND!  S-OTHER PROF-OTHER HOST-BAND!
   S-FOREIGN PROF-FOREIGN HOST-BAND!
   S-SPILL ARN-SPILL ARENA!  S-FRAMES ARN-FRAMES ARENA!  S-DROP ARN-DROP ARENA!
   S-A-ARRAY  ARN-INCL A-IX CELL * +  ARENA!
   S-A-ENTRY  A-IX HOST-ENTRY ENT-INCL +  ARENA!
   C-START ARN-HI ARENA!
   FROM-A 0 ?do  B-START  A-START A64-INSN +  i HOST-SLOT!  loop
   NONE-AT FROM-A ?do  B-START  C-START A64-INSN +  i HOST-SLOT!  loop
   B-START 0 NONE-AT HOST-SLOT!
   1 0 PC1-AT HOST-SLOT!
   DEFERRED ARN-DEFER ARENA! ;

\ Replace the digits after the key in capture k with the image's count.
: REINDEX ( ptr u8 n n -- ) {: key:ptr keyu:n k:n :}
   k TEXT {: a:ptr u:n :}
   0 AT-BYTE !
   begin
      AT-BYTE @ keyu + u <= if  a AT-BYTE @ + keyu key keyu STR= 0=  else  false  then
   while
      1 AT-BYTE +!
   repeat
   AT-BYTE @ keyu + u > if s" x86-64-kernel-prof: a capture names no indexed count" 1 die then
   AT-BYTE @ keyu + {: at:n :}
   at AT-BYTE !
   begin
      AT-BYTE @ u < if  a AT-BYTE @ + c@ STR-DIGIT?  else  false  then
   while
      1 AT-BYTE +!
   repeat
   AT-BYTE @ {: past:n :}
   IMAGE-INDEXED [char] 0 +  a at + c!
   a past +  a at 1+ +  u past -  BYTE-COPY
   u past - at 1+ +  k TEXT-LEN ! ;

\ The six captures, the image's indexed count in the two S reports. The JSON
\ rows follow the index's start order, which the image's spans keep.
: HOST-CAPTURES ( -- )
   A-START B-START >=  B-START C-START >=  or if
      s" x86-64-kernel-prof: prof-a, prof-b and C do not start in that order" 1 die
   then
   [: prof-report ;] 0 CAPTURE
   [: prof-json ;] 1 CAPTURE
   [: B-IX prof-row ;] 2 CAPTURE
   0 prof-on  prof-off  prof-reset
   HOST-STATE
   [: prof-report ;] 3 CAPTURE
   [: B-IX prof-row ;] 4 CAPTURE
   [: prof-json ;] 5 CAPTURE
   s"  indexed " 3 REINDEX
   s\" \"indexed\":" 5 REINDEX ;

\ ---- the report and limit cases --------------------------------------------------
: NDICT-REG ( -- r64 ) ENGINE-GPR:X64-NDICT >R64 ;
: DBASE-REG ( -- r64 ) ENGINE-GPR:X64-DBASE >R64 ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

\ ( x -- ) into the scratch cell at an offset, and ( -- x ) back out of it.
: KEEP, ( n -- ) AT,  1 G-POP  0 G-POP  RAX RCX MEM-AT STORE, ;
: RECALL, ( n -- ) AT,  0 G-POP  RAX RAX MEM-AT LOAD,  0 G-PUSH ;

\ Store n into the cell at an offset in record ix.
: RECORD-CELL!, ( n n n -- ) {: v:n ix:n off:n :}
   RAX v IMM,  RAX DBASE-REG ix DREC * off + MEM-OFF STORE, ;

\ A, B and C behind a jump, each span its first label to its second, in the
\ host's start order: A and C each a `call B` alone, whose return address is
\ its end, and B a `ret`. Nothing runs them; state S names their addresses.
: SPANS, ( -- label label label label label label )
   LBL LBL LBL LBL {: a:label a-end:label b:label b-end:label :}
   LBL LBL LBL {: c:label c-end:label past:label :}
   past JMP,
   a LBL,  b CALL,  a-end LBL,
   b LBL,  ASM-SINK ENC-RET  b-end LBL,
   c LBL,  b CALL,  c-end LBL,
   past LBL,
   a a-end b b-end c c-end ;

\ The records at the host's indices: A, B, the package row with the host's
\ two wids, and C in the host's wid for it; then r14 past the highest.
: HOST-RECORDS, ( label label label label label label -- )
   {: a:label a-end:label b:label b-end:label c:label c-end:label :}
   NDICT-REG A-IX IMM,  s" prof-a" a a-end X64HARNESS:CODE-RECORD,
   NDICT-REG B-IX IMM,  s" prof-b" b b-end X64HARNESS:CODE-RECORD,
   NDICT-REG Q-IX IMM,  s" X64K-PROF-Q" DICT-WL:NAMESPACE 0 X64HARNESS:RECORD,
   Q-PUBLIC Q-IX 0 RECORD-CELL!,  Q-PRIVATE Q-IX CELL RECORD-CELL!,
   NDICT-REG C-IX IMM,  s" CALLER-WITH-A-LONG-NAME" c c-end X64HARNESS:CODE-RECORD,
   C-WID C-IX DICT-WL-OFF RECORD-CELL!,
   NDICT-REG TOP-IX IMM, ;

\ Store n, or a label's address, into the cell at an offset past r8.
: R8-N!, ( n n -- ) {: v:n off:n :}  RAX v IMM,  RAX R8 off MEM-OFF STORE, ;
: R8-LABEL!, ( label n -- ) {: at:label off:n :}  RAX at MOVABS,  RAX R8 off MEM-OFF STORE, ;

\ State S in the image's band and arena. The cells kept beside B's pc are A's
\ and C's return addresses, which the replay searches one byte back. A's
\ entry is the index's first: its span is the lowest the image seeds.
: IMAGE-STATE, ( label label label label -- )
   {: b:label a-end:label c:label c-end:label :}
   R8 BAND IMM,
   S-TOT PROF-TOT R8-N!,  S-OTHER PROF-OTHER R8-N!,  S-FOREIGN PROF-FOREIGN R8-N!,
   R8 ARENA,
   S-SPILL ARN-SPILL R8-N!,  S-FRAMES ARN-FRAMES R8-N!,  S-DROP ARN-DROP R8-N!,
   S-A-ARRAY  ARN-INCL A-IX CELL * +  R8-N!,
   S-A-ENTRY  ARN-IDX ENT-INCL +  R8-N!,
   c ARN-HI R8-LABEL!,
   FROM-A 0 ?do  b i SLOT R8-LABEL!,  a-end i SLOT CELL + R8-LABEL!,  loop
   NONE-AT FROM-A ?do  b i SLOT R8-LABEL!,  c-end i SLOT CELL + R8-LABEL!,  loop
   b NONE-AT SLOT R8-LABEL!,  0 NONE-AT SLOT CELL + R8-N!,
   1 PC1-AT SLOT R8-N!,  0 PC1-AT SLOT CELL + R8-N!,
   DEFERRED ARN-DEFER R8-N!, ;

\ Run the rows the quotation calls with fd 1 on a fresh pipe, put fd 1 back
\ from SAVED, which closes the pipe's last write end, and push the two reads
\ of the pipe into GOT: all the rows wrote, then EOF's 0.
: PIPED, ( [ -- ] -- )
   s" pipe" ROW  0 G-POP  WFD KEEP,  RFD KEEP,
   WFD RECALL,  STDOUT N,  s" dup2" ROW  0 G-POP
   WFD RECALL,  s" close" ROW
   execute
   SAVED RECALL,  STDOUT N,  s" dup2" ROW  0 G-POP
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   RFD RECALL,  s" close" ROW ;

\ rax |= each of r9 bytes at rsi XORed with the one at rdi. It clobbers rcx
\ rdx rsi rdi r9.
: XOR-BYTES, ( -- )
   LBL LBL {: turn:label done:label :}
   turn LBL,
      R9 TEST,  C-E done JCC,
      RCX RSI MEM-AT ASM-SINK ENC-MOVZX-8-RM
      RDX RDI MEM-AT ASM-SINK ENC-MOVZX-8-RM
      RCX RDX ASM-SINK ENC-XOR-RR  RAX RCX ASM-SINK ENC-OR-RR
      RSI ASM-SINK ENC-INC  RDI ASM-SINK ENC-INC  R9 ASM-SINK ENC-DEC
      turn JMP,
   done LBL, ;

\ Check the two reads PIPED, pushed against capture k: the OR of the first
\ count XORed with k's length, the second count and every byte XORed with
\ k's is 0 when the rows wrote exactly the host's bytes.
: SAME, ( n -- ) {: k:n :}
   k TEXT X64HARNESS:PUSH-TEXT,
   GOT AT,
   RSI R64>N G-POP  R9 R64>N G-POP  RDI R64>N G-POP
   RDX R64>N G-POP  RAX R64>N G-POP
   RAX R9 ASM-SINK ENC-XOR-RR  RAX RDX ASM-SINK ENC-OR-RR
   XOR-BYTES,
   0 G-PUSH  0 WANT ;

\ The foreign spin at the limit: PROF-LIM 1 while rbp is not DATA, until
\ PROF-FOREIGN moves, so a tick reaches the limit on a foreign sample, which
\ must return; PROF-LIM 0 again before rbp comes back.
: FOREIGN-LIMIT, ( -- )
   R10 DATA-REG COPY,  DATA-REG ZERO-REG,
   R8 BAND IMM,
   R9 1 IMM,  R9 R8 PROF-LIM MEM-OFF STORE,
   PROF-FOREIGN WAIT-MOVE,
   R9 ZERO-REG,  R9 R8 PROF-LIM MEM-OFF STORE,
   DATA-REG R10 COPY, ;

\ After a report while armed, the timer's interval is prof-on's default and
\ no seconds.
: RESUMED, ( -- )
   RSP ITIMER-BYTES SUBI,
   TIMER?,
   RAX RSP IT-USEC MEM-OFF LOAD,
   RCX RSP MEM-AT LOAD,  RCX 0 KEEP-IF-EQ,
   RSP ITIMER-BYTES ADDI,
   0 G-PUSH  DEFAULT-RATE WANT ;

: REPORT-CASE ( -- )
   SPANS, {: a:label a-end:label b:label b-end:label c:label c-end:label :}
   a a-end b b-end c c-end HOST-RECORDS,
   STDOUT N,  F-DUPFD N,  FD-FLOOR N,  s" fcntl" ROW  SAVED KEEP,
   [: s" prof-report" ROW ;] PIPED,  0 SAME,
   [: s" prof-json" ROW ;] PIPED,  1 SAME,
   [: B-IX N,  s" prof-row" ROW ;] PIPED,  2 SAME,
   0 N,  s" prof-on" ROW  s" prof-off" ROW  s" prof-reset" ROW
   b a-end c c-end IMAGE-STATE,
   [: s" prof-report" ROW ;] PIPED,  3 SAME,
   [: B-IX N,  s" prof-row" ROW ;] PIPED,  4 SAME,
   [: s" prof-json" ROW ;] PIPED,  5 SAME,
   0 N,  s" prof-on" ROW
   FOREIGN-LIMIT,  0 G-PUSH  1 WANT
   [: s" prof-report" ROW ;] PIPED,  0 G-POP  0 G-POP
   RESUMED,
   s" prof-off" ROW ;

\ After fork: the child, fork's 0, takes the pipe's write end as fd 1, runs
\ LIMIT-SAMPLES prof-on and spins in Habu code, where the tick that reaches
\ the limit dumps and exits 99; a spin that outlasts the bound exits 0. The
\ parent goes on with the pid.
: LIMIT-CHILD, ( -- )
   LBL LBL {: turn:label parent:label :}
   0 G-POP  RAX TEST,  C-NE parent JCC,
   WFD RECALL,  STDOUT N,  s" dup2" ROW  0 G-POP
   LIMIT-SAMPLES N,  s" prof-on" ROW
   RCX SPIN-BOUND IMM,
   turn LBL,  RCX ASM-SINK ENC-DEC  C-NE turn JCC,
   RDI ZERO-REG,  NR-EXIT-GROUP SYS,
   parent LBL,
   0 G-PUSH ;

\ Check the count read into GOT and its bytes: 0 when GOT starts with the
\ text, which the count must reach.
: STARTS, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL {: enough:label :}
   a u X64HARNESS:PUSH-TEXT,
   GOT AT,
   RSI R64>N G-POP  R9 R64>N G-POP  RDI R64>N G-POP  RDX R64>N G-POP
   RAX ZERO-REG,
   RDX R9 ASM-SINK ENC-CMP-RR  C-GE enough JCC,  RAX 1 IMM,
   enough LBL,
   XOR-BYTES,
   0 G-PUSH  0 WANT ;

\ The limit's report is the text one, from its first line: LIMIT-SAMPLES
\ samples.
: LIMIT-CASE ( -- )
   s" pipe" ROW  0 G-POP  WFD KEEP,  RFD KEEP,
   s" fork" ROW  LIMIT-CHILD,
   WFD RECALL,  s" close" ROW
   s" wait-status" ROW  PROF-LIMIT-RC 8 lshift WANT
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   s" profiler samples 3 words " STARTS,
   RFD RECALL,  s" close" ROW ;

\ ---- the clock's bounds -----------------------------------------------------------
\ ( n -- ): spin until n more nanoseconds have passed by mono-ns.
: DELAY, ( -- )
   LBL {: turn:label :}
   s" mono-ns" ROW
   RCX R64>N G-POP  RAX R64>N G-POP  RAX RCX ASM-SINK ENC-ADD-RR  0 G-PUSH
   turn LBL,
      s" mono-ns" ROW
      RCX R64>N G-POP  RAX R64>N G-POP  0 G-PUSH
      RCX RAX ASM-SINK ENC-CMP-RR  C-L turn JCC,
   0 G-POP ;

\ prof-rate SLOW-RATE, then prof-on: the timer's interval is one whole second
\ and the rest in microseconds; then a phase of SLOW-PHASE-NS, timed by
\ mono-ns, takes at least SLOW-TICKS samples.
: SLOW-CASE ( -- )
   SLOW-RATE N,  s" prof-rate" ROW
   0 N,  s" prof-on" ROW
   RSP ITIMER-BYTES SUBI,
   TIMER?,
   RAX RSP IT-USEC MEM-OFF LOAD,
   RCX RSP MEM-AT LOAD,  RCX SLOW-RATE USEC-PER-SEC / KEEP-IF-EQ,
   RSP ITIMER-BYTES ADDI,
   0 G-PUSH  SLOW-RATE USEC-PER-SEC mod WANT
   SLOW-PHASE-NS N,  DELAY,
   s" prof-off" ROW
   R8 BAND IMM,  RCX R8 PROF-TOT MEM-OFF LOAD,
   RAX 1 IMM,  RCX SLOW-TICKS >IMM32 ASM-SINK ENC-CMP-RI32  C-L POISON-IF,
   0 G-PUSH  1 WANT ;

\ prof-rate FAST-RATE, then RESET-TURNS turns of prof-on, a delay, prof-reset,
\ prof-off and the identity: SKEWED counts the turns that found it false. A
\ reset straight after the arm would start a whole interval before the first
\ tick; the delay, PHASES steps of PHASE-STEP-NS, moves its start across the
\ interval.
\ prof-reset's own ticks would land in its record, which the kernel registers
\ near the end of a short dictionary, a few hundred stores into the band's
\ clear: about one turn in a thousand. An alias of its body at TOP-IX, which
\ the index prefers to the original as the last record sharing its start, is
\ the counter the band clears last, so any tick in the band's clear lands
\ between the two stores the race needs.
: RESETS-CASE ( -- )
   LBL LBL {: turn:label same:label :}
   NDICT-REG TOP-IX IMM,
   s" prof-reset-alias" s" prof-reset" X64KERNEL:ENTRY-LABEL s" prof-rate" X64KERNEL:ENTRY-LABEL
   X64HARNESS:CODE-RECORD,
   NDICT-REG TOP-IX 1+ IMM,
   FAST-RATE N,  s" prof-rate" ROW
   RESET-TURNS N,  TURNS KEEP,  0 N,  SKEWED KEEP,
   turn LBL,
      0 N,  s" prof-on" ROW
      TURNS AT,  RAX R64>N G-POP  RAX RAX MEM-AT LOAD,
      RAX PHASES 1- >IMM8 ASM-SINK ENC-AND-RI8
      RCX PHASE-STEP-NS IMM,  RAX RCX ASM-SINK ENC-IMUL-RR  0 G-PUSH
      DELAY,
      s" prof-reset" ROW  s" prof-off" ROW
      SKEW,
      RAX TEST,  C-E same JCC,
      SKEWED AT,  RCX R64>N G-POP  RDX RCX MEM-AT LOAD,  RDX ASM-SINK ENC-INC  RDX RCX MEM-AT STORE,
      same LBL,
      TURNS AT,  RCX R64>N G-POP  RDX RCX MEM-AT LOAD,  RDX ASM-SINK ENC-DEC  RDX RCX MEM-AT STORE,
      C-NE turn JCC,
   SKEWED RECALL,  0 WANT ;

\ prof-rate -1 with no handler: the throw exits UNCAUGHT-RC.
: RATE-REFUSED-CASE ( -- )
   -1 N,  s" prof-rate" ROW ;

\ prof-rate RATE, then -1 under catch: the code is E-PROF-RATE, and the rate
\ the next prof-on arms is still RATE.
: RATE-CAUGHT-CASE ( -- )
   [: -1 N,  s" prof-rate" ROW ;] X64HARNESS:ROUTINE, {: refused:label :}
   RATE N,  s" prof-rate" ROW
   RAX refused MOVABS,  0 G-PUSH  s" catch" ROW
   PROF-ABI:E-PROF-RATE WANT
   ARMED-CHECK, ;

\ -1 in ARN-USEC, which only a write past prof-rate can leave: prof-on's arm
\ is refused, named on fd 2, exit PROF-MAP-RC.
: ARM-REFUSED-CASE ( -- )
   RATE N,  s" prof-rate" ROW
   R8 ARENA,  RAX -1 IMM,  RAX R8 ARN-USEC MEM-OFF STORE,
   0 N,  s" prof-on" ROW ;

\ An image: the case, then the stack checks every case ends with.
: BUILD ( [ -- ] bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   HOST-CAPTURES
   X64HARNESS:INIT
   [: SAMPLES-CASE ;] false s" hb-x64-kernel-prof" TMP-PATH BUILD
   [: SAMPLES-CASE ;] true s" hb-x64-kernel-prof-negative" TMP-PATH BUILD
   [: SHARED-STACK-CASE ;] false s" hb-x64-kernel-prof-shared-stack" TMP-PATH BUILD
   [: REPORT-CASE ;] false s" hb-x64-kernel-prof-report" TMP-PATH BUILD
   [: LIMIT-CASE ;] false s" hb-x64-kernel-prof-limit" TMP-PATH BUILD
   [: SLOW-CASE ;] false s" hb-x64-kernel-prof-slow" TMP-PATH BUILD
   [: RESETS-CASE ;] false s" hb-x64-kernel-prof-resets" TMP-PATH BUILD
   [: RATE-REFUSED-CASE ;] false s" hb-x64-kernel-prof-rate-refused" TMP-PATH BUILD
   [: RATE-CAUGHT-CASE ;] false s" hb-x64-kernel-prof-rate-caught" TMP-PATH BUILD
   [: ARM-REFUSED-CASE ;] false s" hb-x64-kernel-prof-arm-refused" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ X64LAYOUT
;using
;using
;using
;using
;package

X64K-PROF:RUN
