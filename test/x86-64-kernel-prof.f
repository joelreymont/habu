\ x86-64-kernel-prof.f - the profiler rows of the x86-64 kernel
\ (src/habu/kernel-x64.f PROFILER,, src/habu/prof-x64.f) in the booted
\ harness, cross-built for an x86-64 peer. The case seeds two records whose
\ code is its own text: A, a `call B` alone, and B, a spin, with A's `ret` in
\ the gap between them, so the return address the walk finds is A's end. Three
\ spins are no record: one below A and two above B. Each image is one case,
\ and the peer that runs it must see its status:
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
\    hb-x64-kernel-prof-refused  78  prof-rate maps the arena, RLIMIT_AS 0,
\                                    then prof-on cannot map the handler
\                                    stack: `hb: prof-on: cannot map the
\                                    handler stack` on fd 2
\
\ A spin that waits for a tick is bounded, so a tick that never comes fails
\ its check instead of hanging the image. The host checks nothing but the
\ build; running the images is the peer's.
require src/habu/layout.f
require src/habu/stack-abi.f
require src/habu/prof-abi.f
require src/os/linux-x86-64/target-layout.f
require test/x86-64-boot-harness.f

package X64K-PROF
using X64ASM
using X64CODE
using X64RT
using PROF-ABI

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
\ sum(counters) + ARN-NEW + ARN-DEFER + ARN-SPILL + PROF-OTHER + PROF-FOREIGN
\ - PROF-TOT, over the records r14 counts.
: IDENTITY, ( -- )
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
   RAX R8 PROF-TOT MEM-OFF ASM-SINK ENC-SUB-RM
   0 G-PUSH  0 WANT ;

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

\ prof-rate RATE, then prof-on: the timer's interval is RATE us and no
\ seconds; prof-off: every field of the timer reads 0.
: RATE-CHECK, ( -- )
   RATE N,  s" prof-rate" ROW
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

\ The arena comes from prof-rate before the limit, so the first mapping
\ prof-on asks for is the handler's stack.
: REFUSED-CASE ( -- )
   RATE N,  s" prof-rate" ROW
   RSP 2 CELL * SUBI,
   RAX ZERO-REG,  RAX RSP MEM-AT STORE,  RAX RSP CELL MEM-OFF STORE,
   RDI RLIMIT-AS >IMM32 ASM-SINK ENC-MOV-RI32
   RSI RSP COPY,
   NR-SETRLIMIT SYS,
   RSP 2 CELL * ADDI,
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
   X64HARNESS:INIT
   [: SAMPLES-CASE ;] false s" hb-x64-kernel-prof" TMP-PATH BUILD
   [: SAMPLES-CASE ;] true s" hb-x64-kernel-prof-negative" TMP-PATH BUILD
   [: REFUSED-CASE ;] false s" hb-x64-kernel-prof-refused" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;using
;package

X64K-PROF:RUN
