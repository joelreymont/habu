\ x86-64-kernel-prof.f - the profiler rows of the x86-64 kernel
\ (src/habu/kernel-x64.f PROFILER,, src/habu/prof-x64.f) in the booted harness,
\ cross-built for an x86-64 peer. A spin waiting for a tick is bounded, so a
\ missing tick fails its check instead of hanging. Each case's word states what
\ it checks.
require src/core/exit-status.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/stack-abi.f
require src/habu/prof-abi.f
require src/os/linux/layout.f
require src/habu/xref.f
require test/x86-64-boot-harness.f
require src/lib/fd.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

\ The host's words the report case names, at the host's own record indices.
: prof-a ( -- n ) 1 ;
: prof-b ( -- n ) 2 ;

package X64K-PROF-Q
public
: CALLER-WITH-A-LONG-NAME ( -- n ) 3 ;
;package

package X64K-PROF

\ The band at the top of the target's DATA, and the counters after its cells.
LINUX-LAYOUT:DATA-VA VA>N LINUX-LAYOUT:DATA-SIZE PROF-ABI:PROF-BAND-AT constant BAND
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
32 constant ITIMER-BYTES               \ struct itimerval: the interval, then the value
8 constant IT-USEC                      \ the interval's microseconds

\ ---- the report case's facts ---------------------------------------------------
0 constant F-DUPFD
3 constant FD-FLOOR                     \ F_DUPFD's lowest descriptor: past the standard three
3 constant TEXTS                        \ the captures the report case compares
$1000 constant TEXT-CAP                 \ a capture's room, and what the image reads into
8 constant RFD                          \ scratch (PUSH-SCRATCH,): the pipe's two ends,
16 constant WFD
24 constant SAVED                       \ fd 1 as the image found it
48 constant CAP-LEN                     \ bytes in the latest image-side pipe capture
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
0 constant SIG-BLOCK
1 constant SIG-UNBLOCK
8 constant SIGSET-BYTES                 \ the kernel's sigset_t
50000000 constant WAIT-NS               \ the weigh case's wait with SIGALRM blocked
WAIT-NS DEFAULT-RATE 1000 * / constant WAIT-TICKS   \ the intervals it spans
50 constant CARRY-ROUNDS                \ the carry case's spins, each followed by a prof-row
600000 constant CARRY-SPIN-NS           \ and each spin's length, under the interval
20 constant CARRY-TICKS                 \ the samples their 30 ms take at least

\ State S: B's pc deferred three times with A's return address kept beside it,
\ twice with C's and once with 0, and pc 1, which no record owns, once, the
\ first of C's slots weighing HEAVY intervals and every other slot one; A's
\ inclusive count 9 with no exclusive one, 4 in the per-record array and 5 in
\ its index entry; ARN-HI at C's start, as if C were compiled after prof-on;
\ PROF-OTHER, PROF-FOREIGN, ARN-SPILL, ARN-FRAMES and ARN-DROP nonzero;
\ PROF-TOT their sum with the slots' weights; and ARN-FCALLED six, as if
\ each of B's samples were in a foreign callee of B. A report's sync folds A's
\ entry into the array and replays the deferred samples by weight: B takes
\ seven exclusive and inclusive samples, the edges (B, A), (B, C) and
\ (B, none) count 3, 3 and 1, C, which only the rebuild reaches, takes three
\ inclusive samples, and ARN-NEW the sample at pc 1. A and B are global, and C, whose
\ name is longer than DNAME-INL, is package X64K-PROF-Q's, so the reports
\ print an inline name, one behind DNAME-EXT and a qualifier.
3 constant FROM-A                       \ B's deferred samples A called, in the slots from 0
2 constant FROM-C                       \ and those C called, in the slots after
FROM-A FROM-C + constant NONE-AT        \ B's sample whose kept cell is 0
NONE-AT 1+ constant PC1-AT              \ the sample at pc 1
PC1-AT 1+ constant DEFERRED
2 constant HEAVY                        \ slot FROM-A's weight in intervals
: S-WEIGHT ( n -- n ) FROM-A = if HEAVY else 1 then ;
: S-WEIGHTS ( -- n ) 0  DEFERRED 0 ?do  i S-WEIGHT +  loop ;
5 constant S-OTHER
4 constant S-FOREIGN
3 constant S-SPILL
11 constant S-FRAMES
2 constant S-DROP
6 constant S-FCALLED
4 constant S-A-ARRAY                    \ A's inclusive samples in the per-record array
5 constant S-A-ENTRY                    \ and in its index entry, which the sync folds
S-WEIGHTS S-OTHER + S-FOREIGN + S-SPILL + constant S-TOT

: HOST-IX ( str n -- n )
   XREF-FIND-WL-INDEX dup 0 >= if exit then
   s" x86-64-kernel-prof: the host lacks a word the report names" 1 die ;

\ The host's records and the package's wids, which the report image seeds at
\ the same indices.
s" prof-a" BOOT:GLOBAL-WL HOST-IX constant A-IX
s" prof-b" BOOT:GLOBAL-WL HOST-IX constant B-IX
s" X64K-PROF-Q" BOOT:NAMESPACE-WL HOST-IX constant Q-IX
Q-IX XREF-REC XREF-PKG-PUBLIC constant Q-PUBLIC
s" CALLER-WITH-A-LONG-NAME" Q-PUBLIC HOST-IX constant C-IX
Q-IX XREF-REC XREF-PKG-PRIVATE constant Q-PRIVATE
C-IX XREF-REC XREF-WORDLIST constant C-WID
A-IX B-IX max C-IX max Q-IX max 1+ constant TOP-IX

: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: CP-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-CP X64ASM:>R64 ;
: ROW ( str -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: LOAD, ( X64ASM:r64 X64ASM:mem -- ) X64CODE:ASM-SINK X64ASM:ENC-MOV-RM ;
: STORE, ( X64ASM:r64 X64ASM:mem -- ) X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;
: COPY, ( X64ASM:r64 X64ASM:r64 -- ) X64CODE:ASM-SINK X64ASM:ENC-MOV-RR ;
: TEST, ( X64ASM:r64 -- ) dup X64CODE:ASM-SINK X64ASM:ENC-TEST-RR ;
: IMM, ( X64ASM:r64 n -- ) X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64 ;
: ADDI, ( X64ASM:r64 n -- ) X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-ADD-RI32 ;
: SUBI, ( X64ASM:r64 n -- ) X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-SUB-RI32 ;

\ r = the arena's base, read from the band.
: ARENA, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}  r BAND IMM,  r r PROF-ABI:PROF-ARENA X64ASM:MEM-OFF LOAD, ;

\ Replace rax with POISON when the condition holds. It clobbers r11; a mov
\ keeps the flags the condition reads.
: POISON-IF, ( X64ASM:condition -- ) {: c:X64ASM:condition :}
   X64ASM:R11 POISON IMM,
   c X64ASM:RAX X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CMOVCC ;

\ Poison rax unless the register holds n.
: KEEP-IF-EQ, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 v:n :}
   r v X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32
   X64ASM:C-NE POISON-IF, ;

\ ---- the spins ---------------------------------------------------------------
\ Spin until the cell `off` bytes past r8 moves from the value it held on
\ entry, at most SPIN-BOUND turns: rax = 1 when it moved, 0 when the bound ran
\ out. It clobbers rcx and rdx.
: WAIT-MOVE, ( n -- ) {: off:n :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: turn:label moved:label out:label :}
   X64ASM:RDX X64ASM:R8 off X64ASM:MEM-OFF LOAD,
   X64ASM:RCX SPIN-BOUND IMM,
   turn X64CODE:LBL,
      X64ASM:RDX X64ASM:R8 off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE moved X64CODE:JCC,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE turn X64CODE:JCC,
   X64ASM:RAX ZERO-REG,  out X64CODE:JMP,
   moved X64CODE:LBL,  X64ASM:RAX 1 IMM,
   out X64CODE:LBL, ;

\ The value a held register keeps across the ticks, one per register.
: SENTINEL ( X64ASM:r64 -- n ) X64ASM:R64>N $5E000000 + ;
: SEED, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}  r r SENTINEL X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32 ;
: HELD, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}  r r SENTINEL KEEP-IF-EQ, ;

\ B's body: spin until B's counter and A's inclusive count, in the first index
\ entry, both leave 0; then rax = 1 while every held register keeps its
\ sentinel, r11 checked first since the checks borrow it. rax = 0 when the
\ bound ran out.
: B-SPIN, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: turn:label next:label moved:label out:label :}
   X64ASM:R8 COUNTERS B-REC CELL * + IMM,
   X64ASM:R9 ARENA,  X64ASM:R9 PROF-ABI:ARN-IDX PROF-ABI:ENT-INCL + ADDI,
   X64ASM:RDX SEED,  X64ASM:RSI SEED,  X64ASM:RDI SEED,  X64ASM:R10 SEED,  X64ASM:R11 SEED,
   X64ASM:RCX SPIN-BOUND IMM,
   turn X64CODE:LBL,
      X64ASM:RAX X64ASM:R8 X64ASM:MEM-AT LOAD,  X64ASM:RAX TEST,  X64ASM:C-E next X64CODE:JCC,
      X64ASM:RAX X64ASM:R9 X64ASM:MEM-AT LOAD,  X64ASM:RAX TEST,  X64ASM:C-NE moved X64CODE:JCC,
   next X64CODE:LBL,
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE turn X64CODE:JCC,
   X64ASM:RAX ZERO-REG,  out X64CODE:JMP,
   moved X64CODE:LBL,
   X64ASM:RAX 1 IMM,
   X64ASM:R11 HELD,  X64ASM:RDX HELD,  X64ASM:RSI HELD,  X64ASM:RDI HELD,  X64ASM:R10 HELD,
   out X64CODE:LBL, ;

\ A and B behind a jump, each span its first label to its second. A is a `call
\ B` alone and B a spin, with A's `ret` in the gap between them, so the return
\ address the walk finds is A's end. The case's three other spins are no record:
\ one below A and two above B.
: RECORDS, ( -- label label label label )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: a:label a-end:label b:label b-end:label :}
   X64CODE:LBL {: past:label :}
   past X64CODE:JMP,
   a X64CODE:LBL,  b X64CODE:CALL,  a-end X64CODE:LBL,
   X64CODE:ASM-SINK X64ASM:ENC-RET
   b X64CODE:LBL,  B-SPIN,  X64CODE:ASM-SINK X64ASM:ENC-RET  b-end X64CODE:LBL,
   past X64CODE:LBL,
   a a-end b b-end ;

\ The spin above the index: once ARN-DEFER moves, the newest slot must hold a
\ pc in this routine and the cell at the interrupted rsp, this routine's return
\ address, since it pushes nothing. Its bounds are rip-relative, so its bytes
\ run wherever they are copied.
: HIGH, ( -- label label )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: start:label end:label past:label :}
   past X64CODE:JMP,
   start X64CODE:LBL,
   X64ASM:R8 ARENA,
   PROF-ABI:ARN-DEFER WAIT-MOVE,
   X64ASM:R9 X64ASM:R8 PROF-ABI:ARN-DEFER X64ASM:MEM-OFF LOAD,
   X64ASM:R9 X64ASM:R9 PROF-ABI:PROF-DEFER-ENT X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8
   X64ASM:R9 X64ASM:R8 X64ASM:R9 1 PROF-ABI:ARN-DEF PROF-ABI:PROF-DEFER-ENT - X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R10 X64ASM:R9 CELL X64ASM:MEM-OFF LOAD,
   X64ASM:R10 X64ASM:RSP X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-NE POISON-IF,
   X64ASM:R10 X64ASM:R9 X64ASM:MEM-AT LOAD,
   X64ASM:RCX start X64CODE:LEA-REL,  X64ASM:R10 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B POISON-IF,
   X64ASM:RCX end X64CODE:LEA-REL,  X64ASM:R10 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE POISON-IF,
   X64CODE:ASM-SINK X64ASM:ENC-RET
   end X64CODE:LBL,
   past X64CODE:LBL,
   start end ;

\ Copy the bytes [start, end) to CP through the RW alias, move CP past them
\ and call the copy, as code compiled after prof-on runs. It clobbers rax rcx
\ rdx rsi rdi r11 and whatever the copy clobbers.
: REGION-CALL, ( label label -- ) {: start:label end:label :}
   X64CODE:LBL {: byte:label :}
   X64ASM:RSI start X64CODE:LEA-REL,  X64ASM:RDX end X64CODE:LEA-REL,  X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-SUB-RR
   X64ASM:RCX X64ASM:RDX COPY,  X64ASM:RDI CP-REG COPY,  X64ASM:RDI X64KERNEL:WRITE-ALIAS,
   byte X64CODE:LBL,
      X64ASM:RAX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      X64ASM:RAX X64ASM:R64>N X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOV8-MR
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC
      X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE byte X64CODE:JCC,
   X64ASM:R11 CP-REG COPY,  CP-REG X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:R11 X64CODE:ASM-SINK X64ASM:ENC-CALL-REG ;

\ The foreign spin: rbp is not DATA while it waits for PROF-FOREIGN.
: FOREIGN-SPIN, ( -- )
   X64ASM:R10 DATA-REG COPY,  DATA-REG ZERO-REG,
   X64ASM:R8 BAND IMM,  PROF-ABI:PROF-FOREIGN WAIT-MOVE,
   DATA-REG X64ASM:R10 COPY, ;

\ Call a routine with rsp n bytes past the DO/LOOP stack's base, put rsp back
\ from the scratch cell and push the routine's rax.
: STACK-CALL, ( label n -- ) {: at:label off:n :}
   0 X64HARNESS:PUSH-SCRATCH,  X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RSP X64ASM:RCX X64ASM:MEM-AT STORE,
   X64ASM:RSP DATA-REG STACK-ABI:LOOP-BASE-CELL X64ASM:MEM-OFF LOAD,
   X64ASM:RSP off CELL + ADDI,                       \ the call's push leaves the routine at n
   at X64CODE:CALL,
   0 X64RT:G-PUSH
   0 X64HARNESS:PUSH-SCRATCH,  X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RSP X64ASM:RCX X64ASM:MEM-AT LOAD, ;

\ ---- the checks after the clock stops ---------------------------------------
\ rax = sum(counters) + ARN-NEW + the deferred slots' weights + ARN-SPILL +
\ PROF-OTHER + PROF-FOREIGN - PROF-TOT, over every profile ID: 0 while the
\ identity holds. It clobbers rcx, rdx and r8.
: SKEW, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: sum:label done:label slot:label slots:label :}
   X64ASM:RAX ZERO-REG,
   X64ASM:R8 ARENA,  X64ASM:RCX X64ASM:R8 PROF-ABI:ARN-IDS X64ASM:MEM-OFF LOAD,
   X64ASM:RDX COUNTERS IMM,
   sum X64CODE:LBL,
      X64ASM:RCX TEST,  X64ASM:C-E done X64CODE:JCC,
      X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
      X64ASM:RDX CELL ADDI,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  sum X64CODE:JMP,
   done X64CODE:LBL,
   X64ASM:R8 ARENA,
   X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-NEW X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RCX X64ASM:R8 PROF-ABI:ARN-DEFER X64ASM:MEM-OFF LOAD,  X64ASM:RDX X64ASM:R8 PROF-ABI:ARN-DEF PROF-ABI:PROF-SLOT-WEIGHT + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   slot X64CODE:LBL,
      X64ASM:RCX TEST,  X64ASM:C-E slots X64CODE:JCC,
      X64ASM:RAX X64ASM:RDX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
      X64ASM:RDX PROF-ABI:PROF-DEFER-ENT ADDI,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  slot X64CODE:JMP,
   slots X64CODE:LBL,
   X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-SPILL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:R8 BAND IMM,
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-OTHER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-FOREIGN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-ADD-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-TOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-SUB-RM ;

: IDENTITY, ( -- )  SKEW,  0 X64RT:G-PUSH  0 WANT ;

\ Poison rax unless the edge table holds the pair (B, A), keyed as EDGE keys
\ it, with a count, and B's own inclusive count, in the second index entry, is
\ nonzero: the walk's edge and the sample's count of itself. It clobbers rcx,
\ rdx and r8-r11.
: EDGE-SEEN, ( -- )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: turn:label hit:label out:label :}
   X64ASM:R8 ARENA,
   X64ASM:RCX X64ASM:R8 PROF-ABI:ARN-CALL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDX X64ASM:R8 PROF-ABI:ARN-INCL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:R9 B-REC PROF-ABI:PROF-REC-BITS lshift A-REC or 1+ IMM,
   X64ASM:R10 ZERO-REG,
   turn X64CODE:LBL,
      X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE out X64CODE:JCC,
      X64ASM:R9 X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-CMP-RM  X64ASM:C-E hit X64CODE:JCC,
      X64ASM:RCX PROF-ABI:PROF-CALL-ENT ADDI,  turn X64CODE:JMP,
   hit X64CODE:LBL,  X64ASM:R10 X64ASM:RCX CELL X64ASM:MEM-OFF LOAD,
   out X64CODE:LBL,
   X64ASM:R10 TEST,  X64ASM:C-E POISON-IF,
   X64ASM:R10 X64ASM:R8 PROF-ABI:ARN-IDX PROF-ABI:PROF-ENT + PROF-ABI:ENT-INCL + X64ASM:MEM-OFF LOAD,
   X64ASM:R10 TEST,  X64ASM:C-E POISON-IF, ;

\ prof-pc>rec names B for B's first byte and nothing for A's end, the gap,
\ and the edge (B, A) and B's inclusive count are nonzero.
: PC-CHECK, ( label label -- ) {: b:label gap:label :}
   b 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW
   gap 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW
   X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:R64>N X64RT:G-POP
   X64ASM:RCX -1 KEEP-IF-EQ,
   EDGE-SEEN,
   0 X64RT:G-PUSH  B-REC WANT ;

\ After prof-reset, every count the spins moved reads 0: the band's cells, B's
\ counter, the arena's counts, A's inclusive count, and every cell of the edge
\ table, the inclusive array and the stamps, which lie back to back.
: RESET-CHECK, ( -- )
   X64CODE:LBL {: next:label :}
   X64ASM:RAX ZERO-REG,
   X64ASM:R8 BAND IMM,
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-TOT X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-OTHER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:PROF-FOREIGN X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:R8 PROF-STATE-BYTES B-REC CELL * + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:R8 ARENA,
   X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-DEFER X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-FRAMES X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-IDX PROF-ABI:ENT-INCL + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM
   X64ASM:RCX X64ASM:R8 PROF-ABI:ARN-CALL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RDX X64ASM:R8 PROF-ABI:ARN-DEF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   next X64CODE:LBL,
      X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-OR-RM
      X64ASM:RCX CELL ADDI,  X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-B next X64CODE:JCC,
   0 X64RT:G-PUSH  0 WANT ;

\ getitimer(ITIMER_REAL) into the itimerval at rsp.
: TIMER?, ( -- )
   X64ASM:RDI ITIMER-REAL X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RSI X64ASM:RSP COPY,
   NR-GETITIMER SYS, ;

\ prof-on after prof-rate RATE: the timer's interval is RATE us and no
\ seconds; prof-off: every field of the timer reads 0.
: ARMED-CHECK, ( -- )
   0 N,  s" prof-on" ROW
   X64ASM:RSP ITIMER-BYTES SUBI,
   TIMER?,
   X64ASM:RAX X64ASM:RSP IT-USEC X64ASM:MEM-OFF LOAD,
   X64ASM:RCX X64ASM:RSP X64ASM:MEM-AT LOAD,  X64ASM:RCX 0 KEEP-IF-EQ,
   0 X64RT:G-PUSH
   s" prof-off" ROW
   TIMER?,
   X64ASM:RDX X64ASM:RSP X64ASM:MEM-AT LOAD,
   ITIMER-BYTES CELL ?do  X64ASM:RDX X64ASM:RSP i X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-OR-RM  CELL +loop
   X64ASM:RAX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX 0 KEEP-IF-EQ,
   X64ASM:RSP ITIMER-BYTES ADDI,
   0 X64RT:G-PUSH  RATE WANT ;

: RATE-CHECK, ( -- )  RATE N,  s" prof-rate" ROW  ARMED-CHECK, ;

\ ---- the cases ---------------------------------------------------------------
\ The routines first, so the spin below A precedes A in the text; the spin
\ above the index runs from its copy in the code region. A calls B with rsp
\ GUARD-GAP bytes above the DO/LOOP stack's lower guard, where a signal frame
\ pushed on that stack dies of SIGSEGV (139), and B holds five scratch
\ registers intact; the spin below A runs GUARD-GAP bytes under the stack's
\ upper guard.
: SAMPLES-CASE ( -- )
   [: X64ASM:R8 BAND IMM,  PROF-ABI:PROF-OTHER WAIT-MOVE, ;] X64HARNESS:ROUTINE, {: low:label :}
   RECORDS, {: a:label a-end:label b:label b-end:label :}
   HIGH, {: high:label high-end:label :}
   [: FOREIGN-SPIN, ;] X64HARNESS:ROUTINE, {: foreign:label :}
   s" prof-a" a a-end X64HARNESS:CODE-RECORD,
   s" prof-b" b b-end X64HARNESS:CODE-RECORD,
   0 N,  s" prof-on" ROW
   a GUARD-GAP CELL + STACK-CALL,  1 WANT
   low STACK-ABI:LOOP-BYTES GUARD-GAP - STACK-CALL,  1 WANT
   high high-end REGION-CALL,  0 X64RT:G-PUSH  1 WANT
   foreign X64CODE:CALL,  0 X64RT:G-PUSH  1 WANT
   s" prof-off" ROW
   IDENTITY,
   b a-end PC-CHECK,
   s" prof-reset" ROW  RESET-CHECK,
   RATE-CHECK, ;

\ The arena comes from prof-rate before the limit. With no address space left
\ for mmap, prof-on can only succeed by using the thread's runtime altstack.
: SHARED-STACK-CASE ( -- )
   RATE N,  s" prof-rate" ROW
   X64ASM:RSP 2 CELL * SUBI,
   X64ASM:RAX ZERO-REG,  X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT STORE,  X64ASM:RAX X64ASM:RSP CELL X64ASM:MEM-OFF STORE,
   X64ASM:RDI RLIMIT-AS X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI32
   X64ASM:RSI X64ASM:RSP COPY,
   NR-SETRLIMIT SYS,
   X64ASM:RSP 2 CELL * ADDI,
   0 N,  s" prof-on" ROW
   s" prof-off" ROW ;

\ ---- the host's own bytes --------------------------------------------------------
TEXTS TEXT-CAP * BUFFER: TEXT-BYTES
TEXTS TYPED-BUFFER TEXT-LEN n
variable GOT-N

: TEXT ( n -- str ) {: k:n :}  TEXT-BYTES k TEXT-CAP * +  k TEXT-LEN @ >str ;

\ Run the quotation with this host's fd 1 on a pipe, put fd 1 back, which
\ closes the pipe's last write end, and keep what the pipe holds as capture k.
: CAPTURE ( [ -- ] n -- ) {: k:n :}
   FD:STDOUT FD>N F-DUPFD FD-FLOOR fcntl {: saved:n :}
   pipe {: r:n w:n rc:n :}
   saved 0 <  rc 0<>  or if s" x86-64-kernel-prof: no pipe to capture fd 1" 1 die then
   w FD:STDOUT FD>N dup2 drop
   execute
   saved FD:STDOUT FD>N dup2 drop  saved close  w close
   0 GOT-N !
   begin
      r  TEXT-BYTES k TEXT-CAP * + GOT-N @ +  TEXT-CAP GOT-N @ -  read  dup 0 >
   while
      GOT-N +!
   repeat drop
   r close
   GOT-N @ TEXT-CAP >= if s" x86-64-kernel-prof: a capture outgrew its room" 1 die then
   GOT-N @ k TEXT-LEN ! ;

\ This host's reports before its profiler ever ran.
: HOST-CAPTURES ( -- )
   [: prof-report ;] 0 CAPTURE
   [: prof-json ;] 1 CAPTURE
   [: B-IX prof-row ;] 2 CAPTURE ;

\ ---- the report and limit cases --------------------------------------------------
: NDICT-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-NDICT X64ASM:>R64 ;
: DBASE-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-DBASE X64ASM:>R64 ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

\ ( x -- ) into the scratch cell at an offset, and ( -- x ) back out of it.
: KEEP, ( n -- ) AT,  1 X64RT:G-POP  0 X64RT:G-POP  X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT STORE, ;
: RECALL, ( n -- ) AT,  0 X64RT:G-POP  X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT LOAD,  0 X64RT:G-PUSH ;

: RECORD-CELL!, ( n n n -- ) {: v:n ix:n off:n :}
   X64ASM:RAX v IMM,
   X64ASM:RCX DBASE-REG ix DREC * off + X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64KERNEL:WRITE-ALIAS,
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT STORE, ;

\ A, B and C behind a jump, each span its first label to its second, in the
\ host's start order: A and C each a `call B` alone, whose return address is
\ its end, and B a `ret`. Nothing runs them; state S names their addresses.
: SPANS, ( -- label label label label label label )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: a:label a-end:label b:label b-end:label :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: c:label c-end:label past:label :}
   past X64CODE:JMP,
   a X64CODE:LBL,  b X64CODE:CALL,  a-end X64CODE:LBL,
   b X64CODE:LBL,  X64CODE:ASM-SINK X64ASM:ENC-RET  b-end X64CODE:LBL,
   c X64CODE:LBL,  b X64CODE:CALL,  c-end X64CODE:LBL,
   past X64CODE:LBL,
   a a-end b b-end c c-end ;

\ The records at the host's indices: A, B, the package row with the host's
\ two wids, and C in the host's wid for it; then r14 past the highest.
: HOST-RECORDS, ( label label label label label label -- )
   {: a:label a-end:label b:label b-end:label c:label c-end:label :}
   NDICT-REG A-IX IMM,  s" prof-a" a a-end X64HARNESS:CODE-RECORD,
   NDICT-REG B-IX IMM,  s" prof-b" b b-end X64HARNESS:CODE-RECORD,
   NDICT-REG Q-IX IMM,  s" X64K-PROF-Q" WL:NAMESPACE 0 X64HARNESS:RECORD,
   Q-PUBLIC Q-IX 0 RECORD-CELL!,  Q-PRIVATE Q-IX CELL RECORD-CELL!,
   NDICT-REG C-IX IMM,  s" CALLER-WITH-A-LONG-NAME" c c-end X64HARNESS:CODE-RECORD,
   C-WID C-IX PROF-ABI:DICT-WL-OFF RECORD-CELL!,
   NDICT-REG TOP-IX IMM, ;

\ Store n, or a label's address, into the cell at an offset past r8.
: R8-N!, ( n n -- ) {: v:n off:n :}  X64ASM:RAX v IMM,  X64ASM:RAX X64ASM:R8 off X64ASM:MEM-OFF STORE, ;
: R8-LABEL!, ( label n -- ) {: at:label off:n :}  X64ASM:RAX at X64CODE:MOVABS,  X64ASM:RAX X64ASM:R8 off X64ASM:MEM-OFF STORE, ;

\ Deferred slot i's offset in the arena: its pc, then the cell kept beside it,
\ then its weight.
: SLOT ( n -- n ) PROF-ABI:PROF-DEFER-ENT * PROF-ABI:ARN-DEF + ;

\ State S in the image's band and arena. The cells kept beside B's pc are A's
\ and C's return addresses, which the replay searches one byte back. A's
\ entry is the index's first: its span is the lowest the image seeds.
: IMAGE-STATE, ( label label label label -- )
   {: b:label a-end:label c:label c-end:label :}
   X64ASM:R8 BAND IMM,
   S-TOT PROF-ABI:PROF-TOT R8-N!,  S-OTHER PROF-ABI:PROF-OTHER R8-N!,  S-FOREIGN PROF-ABI:PROF-FOREIGN R8-N!,
   X64ASM:R8 ARENA,
   S-SPILL PROF-ABI:ARN-SPILL R8-N!,  S-FRAMES PROF-ABI:ARN-FRAMES R8-N!,  S-DROP PROF-ABI:ARN-DROP R8-N!,
   S-FCALLED PROF-ABI:ARN-FCALLED R8-N!,
   S-A-ARRAY  PROF-ABI:ARN-INCL A-IX CELL * +  R8-N!,
   S-A-ENTRY  PROF-ABI:ARN-IDX PROF-ABI:ENT-INCL +  R8-N!,
   c PROF-ABI:ARN-HI R8-LABEL!,
   FROM-A 0 ?do  b i SLOT R8-LABEL!,  a-end i SLOT CELL + R8-LABEL!,  loop
   NONE-AT FROM-A ?do  b i SLOT R8-LABEL!,  c-end i SLOT CELL + R8-LABEL!,  loop
   b NONE-AT SLOT R8-LABEL!,  0 NONE-AT SLOT CELL + R8-N!,
   1 PC1-AT SLOT R8-N!,  0 PC1-AT SLOT CELL + R8-N!,
   DEFERRED 0 ?do  i S-WEIGHT  i SLOT PROF-ABI:PROF-SLOT-WEIGHT +  R8-N!,  loop
   DEFERRED PROF-ABI:ARN-DEFER R8-N!, ;

\ Run the rows the quotation calls with fd 1 on a fresh pipe, put fd 1 back
\ from SAVED, which closes the pipe's last write end, and push the two reads
\ of the pipe into GOT: all the rows wrote, then EOF's 0.
: PIPED, ( [ -- ] -- )
   s" pipe" ROW  0 X64RT:G-POP  WFD KEEP,  RFD KEEP,
   WFD RECALL,  FD:STDOUT FD>N N,  s" dup2" ROW  0 X64RT:G-POP
   WFD RECALL,  s" close" ROW
   execute
   SAVED RECALL,  FD:STDOUT FD>N N,  s" dup2" ROW  0 X64RT:G-POP
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   RFD RECALL,  s" close" ROW ;

\ rax |= each of r9 bytes at rsi XORed with the one at rdi. It clobbers rcx
\ rdx rsi rdi r9.
: XOR-BYTES, ( -- )
   X64CODE:LBL X64CODE:LBL {: turn:label done:label :}
   turn X64CODE:LBL,
      X64ASM:R9 TEST,  X64ASM:C-E done X64CODE:JCC,
      X64ASM:RCX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      X64ASM:RDX X64ASM:RDI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDI X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-DEC
      turn X64CODE:JMP,
   done X64CODE:LBL, ;

\ Check the two reads PIPED, pushed against capture k: the OR of the first
\ count XORed with k's length, the second count and every byte XORed with
\ k's is 0 when the rows wrote exactly the host's bytes.
: SAME, ( n -- ) {: k:n :}
   k TEXT X64HARNESS:PUSH-TEXT,
   GOT AT,
   X64ASM:RSI X64ASM:R64>N X64RT:G-POP  X64ASM:R9 X64ASM:R64>N X64RT:G-POP  X64ASM:RDI X64ASM:R64>N X64RT:G-POP
   X64ASM:RDX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:R64>N X64RT:G-POP
   X64ASM:RAX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-XOR-RR  X64ASM:RAX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-OR-RR
   XOR-BYTES,
   0 X64RT:G-PUSH  0 WANT ;

\ Capture an image's report for the checks below, retaining the first read's
\ length. PIPED, has restored fd 1 and closed the pipe's write end.
: REPORT-TEXT, ( [ -- ] -- )
   PIPED,  0 X64RT:G-POP  CAP-LEN KEEP, ;

\ Check whether the latest captured report contains these bytes. This names
\ a failed check with the sought text instead of spending a numbered status.
: HAS, ( str bool -- )
   {: s:str present:bool :}
   X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL X64CODE:LBL {: next:label byte:label miss:label found:label done:label :}
   s X64HARNESS:PUSH-TEXT,
   X64ASM:R9 X64ASM:R64>N X64RT:G-POP  X64ASM:RDI X64ASM:R64>N X64RT:G-POP
   GOT AT,  X64ASM:RSI X64ASM:R64>N X64RT:G-POP
   CAP-LEN RECALL,  X64ASM:RDX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
   X64ASM:RAX ZERO-REG,
   next X64CODE:LBL,
      X64ASM:RCX X64ASM:RSI X64ASM:R9 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
      X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A done X64CODE:JCC,
      X64ASM:R8 ZERO-REG,
      byte X64CODE:LBL,
         X64ASM:R8 X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE found X64CODE:JCC,
         X64ASM:RCX X64ASM:RSI X64ASM:R8 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
         X64ASM:R10 X64ASM:RDI X64ASM:R8 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
         X64ASM:RCX X64ASM:R10 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-NE miss X64CODE:JCC,
         X64ASM:R8 X64CODE:ASM-SINK X64ASM:ENC-INC  byte X64CODE:JMP,
      miss X64CODE:LBL,  X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  next X64CODE:JMP,
   found X64CODE:LBL,  X64ASM:RAX 1 IMM,
   done X64CODE:LBL,
   X64ASM:RCX present if 1 else 0 then IMM,
   s X64HARNESS:CHECK-RCX, ;

\ ---- dictionary retirement and ordinal reuse -------------------------------------
\ A cold record's span behind a jump.
: COLD-SPAN, ( -- label label )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: start:label end:label past:label :}
   past X64CODE:JMP,
   start X64CODE:LBL,  X64CODE:ASM-SINK X64ASM:ENC-RET  end X64CODE:LBL,
   past X64CODE:LBL,
   start end ;

\ The donor waits for its own stable counter to move. Both cases put it
\ at ordinal 1 before prof-on gives that ordinal its initial identity.
: DONOR-SPAN, ( -- label label )
   X64CODE:LBL X64CODE:LBL X64CODE:LBL {: start:label end:label past:label :}
   past X64CODE:JMP,
   start X64CODE:LBL,
   X64ASM:R8 COUNTERS CELL + IMM,  0 WAIT-MOVE,
   X64CODE:ASM-SINK X64ASM:ENC-RET  end X64CODE:LBL,
   past X64CODE:LBL,
   start end ;

\ An ordinary count trim is HIDE's boundary. Reuse its ordinal before any report
\ can observe the lowered count: the donor keeps its samples under its own name
\ but no index entry, and the target gets its own copied name and none of them. A
\ second trim hides the target from the index and prof-row, with no replacement.
: TRIM-CASE ( -- )
   COLD-SPAN, {: keep:label keep-end:label :}
   DONOR-SPAN, {: donor:label donor-end:label :}
   COLD-SPAN, {: target:label target-end:label :}
   NDICT-REG ZERO-REG,
   s" prof-keeper" keep keep-end X64HARNESS:CODE-RECORD,
   s" prof-hidden-donor" donor donor-end X64HARNESS:CODE-RECORD,
   FD:STDOUT FD>N N,  F-DUPFD N,  FD-FLOOR N,  s" fcntl" ROW  SAVED KEEP,
   0 N,  s" prof-on" ROW
   donor X64CODE:CALL,  0 X64RT:G-PUSH  1 WANT
   s" prof-off" ROW
   1 N,  s" ndict!" ROW
   s" prof-reused-target" target target-end X64HARNESS:CODE-RECORD,
   [: s" prof-json" ROW ;] REPORT-TEXT,
   s\" {\"word\":\"prof-hidden-donor\",\"excl\":" true HAS,
   s\" {\"word\":\"prof-reused-target\",\"excl\":" false HAS,
   donor 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW  -1 WANT
   target 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW  1 WANT
   IDENTITY,
   [: 1 N,  s" prof-row" ROW ;] REPORT-TEXT,
   s" prof-reused-target" true HAS,
   s" prof-hidden-donor" false HAS,
   1 N,  s" ndict!" ROW
   [: s" prof-json" ROW ;] REPORT-TEXT,
   s\" ,\"indexed\":1," true HAS,
   target 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW  -1 WANT
   [: 1 N,  s" prof-row" ROW ;] REPORT-TEXT,
   s" profiler: no row for that record" true HAS, ;

\ The kernel has no undefine evaluator: stamp RETIRED in place, as XREF:RETIRE
\ does, after prof-on indexed the donor; the next report still indexes and lists
\ it, as its code stays.
: RETIRED-CASE ( -- )
   COLD-SPAN, {: keep:label keep-end:label :}
   DONOR-SPAN, {: donor:label donor-end:label :}
   NDICT-REG ZERO-REG,
   s" prof-keeper" keep keep-end X64HARNESS:CODE-RECORD,
   s" prof-retired-donor" donor donor-end X64HARNESS:CODE-RECORD,
   FD:STDOUT FD>N N,  F-DUPFD N,  FD-FLOOR N,  s" fcntl" ROW  SAVED KEEP,
   0 N,  s" prof-on" ROW
   donor X64CODE:CALL,  0 X64RT:G-PUSH  1 WANT
   s" prof-off" ROW
   WL:RETIRED 1 PROF-ABI:DICT-WL-OFF RECORD-CELL!,
   [: s" prof-json" ROW ;] REPORT-TEXT,
   s\" ,\"indexed\":2," true HAS,
   s\" {\"word\":\"prof-retired-donor\",\"excl\":" true HAS,
   donor 0 X64HARNESS:PUSH-LABEL,  s" prof-pc>rec" ROW  1 WANT
   IDENTITY, ;

\ The foreign spin at the limit: PROF-LIM 1 while rbp is not DATA, until
\ PROF-FOREIGN moves, so a tick reaches the limit on a foreign sample, which
\ must return; PROF-LIM 0 again before rbp comes back.
: FOREIGN-LIMIT, ( -- )
   X64ASM:R10 DATA-REG COPY,  DATA-REG ZERO-REG,
   X64ASM:R8 BAND IMM,
   X64ASM:R9 1 IMM,  X64ASM:R9 X64ASM:R8 PROF-ABI:PROF-LIM X64ASM:MEM-OFF STORE,
   PROF-ABI:PROF-FOREIGN WAIT-MOVE,
   X64ASM:R9 ZERO-REG,  X64ASM:R9 X64ASM:R8 PROF-ABI:PROF-LIM X64ASM:MEM-OFF STORE,
   DATA-REG X64ASM:R10 COPY, ;

\ After a report while armed, the timer's interval is prof-on's default and
\ no seconds.
: RESUMED, ( -- )
   X64ASM:RSP ITIMER-BYTES SUBI,
   TIMER?,
   X64ASM:RAX X64ASM:RSP IT-USEC X64ASM:MEM-OFF LOAD,
   X64ASM:RCX X64ASM:RSP X64ASM:MEM-AT LOAD,  X64ASM:RCX 0 KEEP-IF-EQ,
   X64ASM:RSP ITIMER-BYTES ADDI,
   0 X64RT:G-PUSH  DEFAULT-RATE WANT ;

\ State S's fields in the columns the host's reports print. (B, A) and (B, C)
\ tie at 3, and each prints a caller line.
: S-CALLERS, ( -- )
   s" <-        3  42.8 prof-a" true HAS,
   s" <-        3  42.8 X64K-PROF-Q:CALLER-WITH-A-LONG-NAME" true HAS,
   s" <-        1  14.2 (unknown)" true HAS, ;

: S-B-ROW, ( -- )
   s" 7  35.0       7  35.0  prof-b" true HAS,  S-CALLERS, ;

: S-REPORT, ( -- )
   [: s" prof-report" ROW ;] REPORT-TEXT,
   s"  new 1 " true HAS,
   s"  indexed 3 " true HAS,
   S-B-ROW,
   s" 0   0.0       9  45.0  prof-a" true HAS,
   s" 0   0.0       3  15.0  X64K-PROF-Q:CALLER-WITH-A-LONG-NAME" true HAS, ;

: S-ROW, ( -- )
   [: B-IX N,  s" prof-row" ROW ;] REPORT-TEXT,  S-B-ROW, ;

: S-JSON, ( -- )
   [: s" prof-json" ROW ;] REPORT-TEXT,
   s\" \"new\":1," true HAS,
   s\" \"indexed\":3," true HAS,
   s\" {\"word\":\"prof-a\",\"excl\":0,\"incl\":9}" true HAS,
   s\" {\"word\":\"prof-b\",\"excl\":7,\"incl\":7}" true HAS,
   s\" {\"word\":\"X64K-PROF-Q:CALLER-WITH-A-LONG-NAME\",\"excl\":0,\"incl\":3}" true HAS,
   s\" {\"word\":\"prof-b\",\"caller\":\"prof-a\",\"n\":3}" true HAS,
   s\" {\"word\":\"prof-b\",\"caller\":\"X64K-PROF-Q:CALLER-WITH-A-LONG-NAME\",\"n\":3}" true HAS,
   s\" {\"word\":\"prof-b\",\"caller\":\"(unknown)\",\"n\":1}" true HAS, ;

\ The report case compares every byte the image's prof-report, prof-json and
\ prof-row print on fd 1 with no arena with what this host's own rows print
\ before its profiler ran (HOST-CAPTURES), over records the image seeds at the
\ host's own record indices; then it checks the three reports' fields for
\ state S. Then a foreign sample at PROF-LIM 1 must return, and a report while
\ armed arms ITIMER_REAL again at the default rate.
: REPORT-CASE ( -- )
   SPANS, {: a:label a-end:label b:label b-end:label c:label c-end:label :}
   a a-end b b-end c c-end HOST-RECORDS,
   FD:STDOUT FD>N N,  F-DUPFD N,  FD-FLOOR N,  s" fcntl" ROW  SAVED KEEP,
   [: s" prof-report" ROW ;] PIPED,  0 SAME,
   [: s" prof-json" ROW ;] PIPED,  1 SAME,
   [: B-IX N,  s" prof-row" ROW ;] PIPED,  2 SAME,
   0 N,  s" prof-on" ROW  s" prof-off" ROW  s" prof-reset" ROW
   b a-end c c-end IMAGE-STATE,
   S-REPORT,  S-ROW,  S-JSON,
   0 N,  s" prof-on" ROW
   FOREIGN-LIMIT,  0 X64RT:G-PUSH  1 WANT
   [: s" prof-report" ROW ;] PIPED,  0 X64RT:G-POP  0 X64RT:G-POP
   RESUMED,
   s" prof-off" ROW ;

\ After fork: the child, fork's 0, takes the pipe's write end as fd 1, runs
\ LIMIT-SAMPLES prof-on and spins in Habu code, where the tick that reaches
\ the limit dumps and exits 99; a spin that outlasts the bound exits 0. The
\ parent goes on with the pid.
: LIMIT-CHILD, ( -- )
   X64CODE:LBL X64CODE:LBL {: turn:label parent:label :}
   0 X64RT:G-POP  X64ASM:RAX TEST,  X64ASM:C-NE parent X64CODE:JCC,
   WFD RECALL,  FD:STDOUT FD>N N,  s" dup2" ROW  0 X64RT:G-POP
   LIMIT-SAMPLES N,  s" prof-on" ROW
   X64ASM:RCX SPIN-BOUND IMM,
   turn X64CODE:LBL,  X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:C-NE turn X64CODE:JCC,
   X64ASM:RDI ZERO-REG,  NR-EXIT-GROUP SYS,
   parent X64CODE:LBL,
   0 X64RT:G-PUSH ;

\ Check the count read into GOT and its bytes: 0 when GOT starts with the
\ text, which the count must reach.
: STARTS, ( str -- ) {: s:str :}
   X64CODE:LBL {: enough:label :}
   s X64HARNESS:PUSH-TEXT,
   GOT AT,
   X64ASM:RSI X64ASM:R64>N X64RT:G-POP  X64ASM:R9 X64ASM:R64>N X64RT:G-POP  X64ASM:RDI X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:R64>N X64RT:G-POP
   X64ASM:RAX ZERO-REG,
   X64ASM:RDX X64ASM:R9 X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-GE enough X64CODE:JCC,  X64ASM:RAX 1 IMM,
   enough X64CODE:LBL,
   XOR-BYTES,
   0 X64RT:G-PUSH  0 WANT ;

\ Check the count read into GOT and its bytes: GOT starts with the head, the
\ digits after it make a total at least n, and the tail follows them.
: HEADER-TOTAL, ( str str n -- ) {: head:str tail:str least:n :}
   X64CODE:LBL X64CODE:LBL {: digit:label parsed:label :}
   CAP-LEN KEEP,  CAP-LEN RECALL,  head STARTS,
   tail X64HARNESS:PUSH-TEXT,
   X64ASM:R9 X64ASM:R64>N X64RT:G-POP  X64ASM:RDI X64ASM:R64>N X64RT:G-POP
   GOT AT,  X64ASM:RSI X64ASM:R64>N X64RT:G-POP
   CAP-LEN RECALL,  X64ASM:RDX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-ADD-RR     \ rdx = the end
   X64ASM:RSI head STR:LEN ADDI,
   X64ASM:R10 ZERO-REG,
   digit X64CODE:LBL,
      X64ASM:RSI X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-AE parsed X64CODE:JCC,
      X64ASM:RCX X64ASM:RSI X64ASM:MEM-AT X64CODE:ASM-SINK X64ASM:ENC-MOVZX-8-RM
      X64ASM:RCX [char] 0 SUBI,  X64ASM:RCX 9 X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-A parsed X64CODE:JCC,
      X64ASM:R10 X64ASM:R10 10 X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-IMUL-RRI8  X64ASM:R10 X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR
      X64ASM:RSI X64CODE:ASM-SINK X64ASM:ENC-INC  digit X64CODE:JMP,
   parsed X64CODE:LBL,
   X64ASM:RAX ZERO-REG,
   X64ASM:R10 least X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-L POISON-IF,
   X64ASM:RCX X64ASM:RSI X64ASM:R9 1 0 X64ASM:MEM-IDX X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RCX X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-A POISON-IF,
   XOR-BYTES,
   0 X64RT:G-PUSH  0 WANT ;

\ The limit's report is the text one, from its first line: at least LIMIT-SAMPLES samples,
\ since the sample that reaches the limit may count several intervals.
: LIMIT-CASE ( -- )
   s" pipe" ROW  0 X64RT:G-POP  WFD KEEP,  RFD KEEP,
   s" fork" ROW  LIMIT-CHILD,
   WFD RECALL,  s" close" ROW
   s" wait-status" ROW  99 8 lshift WANT
   RFD RECALL,  GOT AT,  TEXT-CAP N,  s" read" ROW
   s" profiler samples " s"  words " LIMIT-SAMPLES HEADER-TOTAL,
   RFD RECALL,  s" close" ROW ;

\ ---- the clock's bounds -----------------------------------------------------------
\ ( n -- ): spin until n more nanoseconds have passed by mono-ns.
: DELAY, ( -- )
   X64CODE:LBL {: turn:label :}
   s" mono-ns" ROW
   X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-ADD-RR  0 X64RT:G-PUSH
   turn X64CODE:LBL,
      s" mono-ns" ROW
      X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:R64>N X64RT:G-POP  0 X64RT:G-PUSH
      X64ASM:RCX X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-CMP-RR  X64ASM:C-L turn X64CODE:JCC,
   0 X64RT:G-POP ;

\ PROF-TOT is at least n.
: TOTAL-AT-LEAST, ( n -- ) {: least:n :}
   X64ASM:R8 BAND IMM,  X64ASM:RCX X64ASM:R8 PROF-ABI:PROF-TOT X64ASM:MEM-OFF LOAD,
   X64ASM:RAX 1 IMM,  X64ASM:RCX least X64ASM:>IMM32 X64CODE:ASM-SINK X64ASM:ENC-CMP-RI32  X64ASM:C-L POISON-IF,
   0 X64RT:G-PUSH  1 WANT ;

\ prof-rate SLOW-RATE, then prof-on: the timer's interval is one whole second
\ and the rest in microseconds; then a phase of SLOW-PHASE-NS, timed by
\ mono-ns, takes at least SLOW-TICKS samples.
: SLOW-CASE ( -- )
   SLOW-RATE N,  s" prof-rate" ROW
   0 N,  s" prof-on" ROW
   X64ASM:RSP ITIMER-BYTES SUBI,
   TIMER?,
   X64ASM:RAX X64ASM:RSP IT-USEC X64ASM:MEM-OFF LOAD,
   X64ASM:RCX X64ASM:RSP X64ASM:MEM-AT LOAD,  X64ASM:RCX SLOW-RATE PROF-ABI:USEC-PER-SEC / KEEP-IF-EQ,
   X64ASM:RSP ITIMER-BYTES ADDI,
   0 X64RT:G-PUSH  SLOW-RATE PROF-ABI:USEC-PER-SEC mod WANT
   SLOW-PHASE-NS N,  DELAY,
   s" prof-off" ROW
   SLOW-TICKS TOTAL-AT-LEAST, ;

\ rt_sigprocmask(how, {SIGALRM}, NULL, SIGSET-BYTES) on this thread, the set in
\ a cell below rsp: rax = 0 when the mask took.
: ALARM-MASK, ( n -- ) {: how:n :}
   X64ASM:RSP CELL SUBI,
   X64ASM:RAX 1 PROF-ABI:SIGALRM 1- lshift IMM,  X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT STORE,
   X64ASM:RDI how IMM,  X64ASM:RSI X64ASM:RSP COPY,  X64ASM:RDX ZERO-REG,  X64ASM:R10 SIGSET-BYTES IMM,
   NR-RT-SIGPROCMASK SYS,
   X64ASM:RSP CELL ADDI, ;

\ prof-on at DEFAULT-RATE, then SIGALRM blocked across a WAIT-NS spin by
\ mono-ns: the one signal the wait leaves pending weighs every interval it
\ spanned, so PROF-TOT reaches WAIT-TICKS.
: WEIGH-CASE ( -- )
   DEFAULT-RATE N,  s" prof-rate" ROW
   0 N,  s" prof-on" ROW
   SIG-BLOCK ALARM-MASK,  0 X64RT:G-PUSH  0 WANT
   WAIT-NS N,  DELAY,
   SIG-UNBLOCK ALARM-MASK,  0 X64RT:G-PUSH  0 WANT
   s" prof-off" ROW
   WAIT-TICKS TOTAL-AT-LEAST, ;

\ prof-on at DEFAULT-RATE, then CARRY-ROUNDS rounds of a CARRY-SPIN-NS spin, each
\ shorter than the interval, and a prof-row into a pipe, which stops the clock:
\ only the carry across the rows lets the spins reach CARRY-TICKS samples.
: CARRY-CASE ( -- )
   X64CODE:LBL {: turn:label :}
   FD:STDOUT FD>N N,  F-DUPFD N,  FD-FLOOR N,  s" fcntl" ROW  SAVED KEEP,
   DEFAULT-RATE N,  s" prof-rate" ROW
   0 N,  s" prof-on" ROW
   CARRY-ROUNDS N,  TURNS KEEP,
   turn X64CODE:LBL,
      CARRY-SPIN-NS N,  DELAY,
      [: -1 N,  s" prof-row" ROW ;] PIPED,  0 X64RT:G-POP  0 X64RT:G-POP
      TURNS AT,  X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT LOAD,  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT STORE,
      X64ASM:C-NE turn X64CODE:JCC,
   s" prof-off" ROW
   CARRY-TICKS TOTAL-AT-LEAST, ;

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
   X64CODE:LBL X64CODE:LBL {: turn:label same:label :}
   NDICT-REG TOP-IX IMM,
   s" prof-reset-alias" s" prof-reset" X64KERNEL:ENTRY-LABEL s" prof-rate" X64KERNEL:ENTRY-LABEL
   X64HARNESS:CODE-RECORD,
   NDICT-REG TOP-IX 1+ IMM,
   FAST-RATE N,  s" prof-rate" ROW
   RESET-TURNS N,  TURNS KEEP,  0 N,  SKEWED KEEP,
   turn X64CODE:LBL,
      0 N,  s" prof-on" ROW
      TURNS AT,  X64ASM:RAX X64ASM:R64>N X64RT:G-POP  X64ASM:RAX X64ASM:RAX X64ASM:MEM-AT LOAD,
      X64ASM:RAX PHASES 1- X64ASM:>IMM8 X64CODE:ASM-SINK X64ASM:ENC-AND-RI8
      X64ASM:RCX PHASE-STEP-NS IMM,  X64ASM:RAX X64ASM:RCX X64CODE:ASM-SINK X64ASM:ENC-IMUL-RR  0 X64RT:G-PUSH
      DELAY,
      s" prof-reset" ROW  s" prof-off" ROW
      SKEW,
      X64ASM:RAX TEST,  X64ASM:C-E same X64CODE:JCC,
      SKEWED AT,  X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT LOAD,  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-INC  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT STORE,
      same X64CODE:LBL,
      TURNS AT,  X64ASM:RCX X64ASM:R64>N X64RT:G-POP  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT LOAD,  X64ASM:RDX X64CODE:ASM-SINK X64ASM:ENC-DEC  X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT STORE,
      X64ASM:C-NE turn X64CODE:JCC,
   SKEWED RECALL,  0 WANT ;

\ prof-rate -1 with no handler.
: RATE-REJECTED-CASE ( -- )
   EXIT-STATUS:UNCAUGHT X64HARNESS:EXPECT-EXIT
   -1 N,  s" prof-rate" ROW ;

\ prof-rate RATE, then -1 under catch: the code is E-PROF-RATE, and the rate
\ the next prof-on arms is still RATE.
: RATE-CAUGHT-CASE ( -- )
   [: -1 N,  s" prof-rate" ROW ;] X64HARNESS:ROUTINE, {: rejected:label :}
   RATE N,  s" prof-rate" ROW
   X64ASM:RAX rejected X64CODE:MOVABS,  0 X64RT:G-PUSH  s" catch" ROW
   PROF-ABI:E-PROF-RATE WANT
   ARMED-CHECK, ;

\ -1 in ARN-USEC, which only a write past prof-rate can leave: prof-on's arm
\ is rejected, named on fd 2.
: ARM-REJECTED-CASE ( -- )
   EXIT-STATUS:SYSTEM X64HARNESS:EXPECT-EXIT
   RATE N,  s" prof-rate" ROW
   X64ASM:R8 ARENA,  X64ASM:RAX -1 IMM,  X64ASM:RAX X64ASM:R8 PROF-ABI:ARN-USEC X64ASM:MEM-OFF STORE,
   0 N,  s" prof-on" ROW ;

\ An image: the case, then the stack checks every case ends with.
: BUILD ( [ -- ] bool str -- ) {: negative:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   HOST-CAPTURES
   X64HARNESS:INIT
   [: SAMPLES-CASE ;] false s" hb-x64-kernel-prof" TMP-PATH BUILD
   [: SAMPLES-CASE ;] true s" hb-x64-kernel-prof-negative" TMP-PATH BUILD
   [: SHARED-STACK-CASE ;] false s" hb-x64-kernel-prof-shared-stack" TMP-PATH BUILD
   [: REPORT-CASE ;] false s" hb-x64-kernel-prof-report" TMP-PATH BUILD
   [: TRIM-CASE ;] false s" hb-x64-kernel-prof-trim" TMP-PATH BUILD
   [: RETIRED-CASE ;] false s" hb-x64-kernel-prof-retired" TMP-PATH BUILD
   [: LIMIT-CASE ;] false s" hb-x64-kernel-prof-limit" TMP-PATH BUILD
   [: SLOW-CASE ;] false s" hb-x64-kernel-prof-slow" TMP-PATH BUILD
   [: WEIGH-CASE ;] false s" hb-x64-kernel-prof-weigh" TMP-PATH BUILD
   [: CARRY-CASE ;] false s" hb-x64-kernel-prof-carry" TMP-PATH BUILD
   [: RESETS-CASE ;] false s" hb-x64-kernel-prof-resets" TMP-PATH BUILD
   [: RATE-REJECTED-CASE ;] false s" hb-x64-kernel-prof-rate-rejected" TMP-PATH BUILD
   [: RATE-CAUGHT-CASE ;] false s" hb-x64-kernel-prof-rate-caught" TMP-PATH BUILD
   [: ARM-REJECTED-CASE ;] false s" hb-x64-kernel-prof-arm-rejected" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-PROF:RUN
