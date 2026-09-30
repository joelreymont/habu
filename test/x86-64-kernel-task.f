\ x86-64-kernel-task.f - the task entry of the x86-64 kernel
\ (src/habu/kernel-x64.f TASK,) in the booted harness, cross-built for an
\ x86-64 peer. `task-entry` answers a SysV function whose one argument is a
\ TASK-ABI descriptor: it enters the task's VM state, calls the task's xt and
\ publishes DONE. Every image hands it the same descriptor and body. The body
\ records the registers it starts with and the status it finds through the TCB
\ its region holds, then pushes one cell through rax and zeroes every other
\ scratch register, as an xt may (docs/x86-64.md "Control rows"), so an entry
\ that reads the descriptor from one after the call faults. Each image is one
\ case, and the peer that runs it must see its status:
\
\    hb-x64-kernel-task            0  pthread_create and pthread_join, found by
\                                     DLSYM, and called through ffi-call, run the
\                                     entry on a thread: both answer 0, the
\                                     thread answers 0, the status is DONE, the
\                                     region's TASK-TCB-CELL holds the
\                                     descriptor and the stack base holds the
\                                     cell the body pushed
\    hb-x64-kernel-task-call       0  ffi-call of the entry answers 0, and the
\                                     caller's rbp, rbx, r13, r14, r15, depth
\                                     and balance survive it
\    hb-x64-kernel-task-body       0  ffi-call of the entry: the body starts
\                                     with the descriptor's r13, r14 and r15,
\                                     rbx 0 and the status HALT-REQ the
\                                     descriptor came with
\    hb-x64-kernel-task-negative  21  the thread case, expecting a wrong answer
\                                     from pthread_join
\
\ The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f
require src/habu/task-abi.f

package X64K-TASK
using X64ASM
using X64CODE
using X64RT

\ The task's cells, as DATA offsets past the heap floor and the harness's
\ scratch, where nothing these images run allocates: the descriptor, the cells
\ ffi-call passes, the thread, its answer, the task's data stack and its
\ region, which reaches TASK-TCB-CELL.
DATA-START $20000 + constant DESC
DESC TASK-ABI:TCB-BYTES + constant ARGS
ARGS 8 CELL * + constant THREAD
THREAD CELL + constant ANSWER
ANSWER CELL + constant STACK
STACK $100 + constant REGION

\ Where the body records what it starts with, as offsets into its region.
0 constant SAW-R13
8 constant SAW-R14
16 constant SAW-R15
24 constant SAW-RBX
32 constant SAW-STATUS

\ What the descriptor hands the task, and what the caller holds in rbx, which
\ the task starts at 0.
$1300000013 constant DBASE-MARK
$1400000014 constant NDICT-MARK
$1500000015 constant CP-MARK
$5ABE11 constant PUSHED
$B0B0B0B0B0 constant CALLER-RBX

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: FIELD ( n -- n ) DESC + ;
: ARG ( n -- n ) CELL * ARGS + ;

\ Push the DATA cell at an offset; pop into it.
: PUSH-CELL, ( n -- ) {: off:n :}
   RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-RM  0 G-PUSH ;
: POP-CELL, ( n -- ) {: off:n :}
   0 G-POP  RAX DATA-REG off MEM-OFF ASM-SINK ENC-MOV-MR ;

\ ---- the body and the descriptor ----------------------------------------------
: SAW!, ( r64 n -- ) {: r:r64 off:n :}  r RBP off MEM-OFF ASM-SINK ENC-MOV-MR ;

: BODY-WORDS, ( -- )
   R13 SAW-R13 SAW!,  R14 SAW-R14 SAW!,  R15 SAW-R15 SAW!,  RBX SAW-RBX SAW!,
   RAX RBP TASK-TCB-CELL MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX TASK-ABI:STATUS-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   RAX SAW-STATUS SAW!,
   RAX PUSHED >IMM64 ASM-SINK ENC-MOV-RI64  0 G-PUSH
   RCX ZERO-REG,  RDX ZERO-REG,  RSI ZERO-REG,  RDI ZERO-REG,
   R8 ZERO-REG,  R9 ZERO-REG,  R10 ZERO-REG,  R11 ZERO-REG, ;

: BODY ( -- label ) [: BODY-WORDS, ;] X64HARNESS:ROUTINE, ;

\ The descriptor a task's owner hands the entry. Its status is HALT-REQ, as a
\ TASK:HALT in the window before the thread starts leaves it, so a RUNNING the
\ entry wrote would show.
: DESCRIBE, ( label -- ) {: body:label :}
   body TASK-ABI:XT-OFF FIELD X64HARNESS:LABEL-CELL!,
   STACK TASK-ABI:STACK-OFF FIELD X64HARNESS:DATA-ADDR!,
   REGION TASK-ABI:REGION-OFF FIELD X64HARNESS:DATA-ADDR!,
   DBASE-MARK TASK-ABI:DBASE-OFF FIELD X64HARNESS:CELL!,
   NDICT-MARK TASK-ABI:NDICT-OFF FIELD X64HARNESS:CELL!,
   CP-MARK TASK-ABI:CP-OFF FIELD X64HARNESS:CELL!,
   TASK-ABI:HALT-REQ TASK-ABI:STATUS-OFF FIELD X64HARNESS:CELL!, ;

\ The caller's rbx is not the 0 the task starts with, so the entry must both
\ zero it and bring it back.
: CALLER-RBX, ( -- ) RBX CALLER-RBX >IMM64 ASM-SINK ENC-MOV-RI64 ;

\ ffi-call ( argbuf nargs fn -- ret ) of the entry on the descriptor.
: ENTER, ( -- )
   DESC 0 ARG X64HARNESS:DATA-ADDR!,
   ARGS X64HARNESS:PUSH-DATA,  1 N,
   s" task-entry" ROW  s" ffi-call" ROW ;

\ ---- the thread ---------------------------------------------------------------
\ Push the function dlsym finds for the name, whose bytes sit NUL-terminated
\ behind a jump.
: SYM, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: name:label past:label :}
   past JMP,
   name LBL,  a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN  0 ASM-SINK BUF:APPEND-BYTE
   past LBL,
   RSI name MOVABS,  X64KERNEL:DLSYM,  0 G-PUSH ;

\ pthread_create(&thread, NULL, the entry the row answers, &descriptor).
: CREATE, ( -- )
   THREAD 0 ARG X64HARNESS:DATA-ADDR!,
   0 1 ARG X64HARNESS:CELL!,
   s" task-entry" ROW  2 ARG POP-CELL,
   DESC 3 ARG X64HARNESS:DATA-ADDR!,
   ARGS X64HARNESS:PUSH-DATA,  4 N,  s" pthread_create" SYM,  s" ffi-call" ROW ;

\ pthread_join(thread, &answer).
: JOIN, ( -- )
   THREAD PUSH-CELL,  0 ARG POP-CELL,
   ANSWER 1 ARG X64HARNESS:DATA-ADDR!,
   ARGS X64HARNESS:PUSH-DATA,  2 N,  s" pthread_join" SYM,  s" ffi-call" ROW ;

\ ---- the cases ---------------------------------------------------------------
\ Both answers wait on the data stack until the thread is joined. A failed check
\ exits through exit(2), which ends only its own thread, and a process whose
\ other thread outlives that exits with the other thread's status.
: THREAD-CASE ( -- )
   BODY DESCRIBE,
   -1 ANSWER X64HARNESS:CELL!,
   CREATE,  JOIN,
   0 WANT  0 WANT
   0 ANSWER X64HARNESS:EXPECT-CELL,
   TASK-ABI:DONE TASK-ABI:STATUS-OFF FIELD X64HARNESS:EXPECT-CELL,
   REGION TASK-TCB-CELL + PUSH-CELL,  DESC X64HARNESS:EXPECT-POP-DATA,
   PUSHED STACK X64HARNESS:EXPECT-CELL, ;

\ The machine stack holds a copy of each callee-saved register across the call;
\ the check pops it and compares.
: HOLD, ( r64 -- ) ASM-SINK ENC-PUSH ;
: EXPECT-HELD, ( r64 -- ) {: r:r64 :}
   RAX ASM-SINK ENC-POP  RAX r ASM-SINK ENC-SUB-RR  0 G-PUSH  0 WANT ;

: CALL-CASE ( -- )
   BODY DESCRIBE,
   CALLER-RBX,
   RBP HOLD,  RBX HOLD,  R13 HOLD,  R14 HOLD,  R15 HOLD,
   ENTER,  0 WANT
   R15 EXPECT-HELD,  R14 EXPECT-HELD,  R13 EXPECT-HELD,
   RBX EXPECT-HELD,  RBP EXPECT-HELD, ;

\ The answer is CALL-CASE's to check; this case drops it.
: BODY-CASE ( -- )
   BODY DESCRIBE,
   CALLER-RBX,
   ENTER,  0 G-POP
   DBASE-MARK REGION SAW-R13 + X64HARNESS:EXPECT-CELL,
   NDICT-MARK REGION SAW-R14 + X64HARNESS:EXPECT-CELL,
   CP-MARK REGION SAW-R15 + X64HARNESS:EXPECT-CELL,
   0 REGION SAW-RBX + X64HARNESS:EXPECT-CELL,
   TASK-ABI:HALT-REQ REGION SAW-STATUS + X64HARNESS:EXPECT-CELL, ;

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
   [: THREAD-CASE ;] false s" hb-x64-kernel-task" TMP-PATH BUILD
   [: CALL-CASE ;] false s" hb-x64-kernel-task-call" TMP-PATH BUILD
   [: BODY-CASE ;] false s" hb-x64-kernel-task-body" TMP-PATH BUILD
   [: THREAD-CASE ;] true s" hb-x64-kernel-task-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-TASK:RUN
