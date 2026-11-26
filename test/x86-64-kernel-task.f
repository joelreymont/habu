\ x86-64-kernel-task.f - the task entry of the x86-64 kernel
\ (src/habu/kernel-x64.f TASK,) in the booted harness, cross-built for an
\ x86-64 peer. `task-entry` answers a SysV function taking a TASK-ABI
\ descriptor: it enters the task's VM state, calls the task's xt and publishes
\ DONE. The body pushes one cell through rax and zeroes every other scratch
\ register, as an xt may, so an entry reading the descriptor from one faults.
require test/x86-64-boot-harness.f
require src/habu/task-abi.f
require src/lib/vector.f
require src/arch/x86-64/rt.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require src/os/linux-x86-64/sys.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-TASK

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
24 constant SAW-STATUS

\ What the descriptor hands the task.
$1300000013 constant DBASE-MARK
$1400000014 constant NDICT-MARK
$1500000015 constant CP-MARK
$5ABE11 constant PUSHED

: DATA-REG ( -- X64ASM:r64 ) ENGINE-GPR:X64-RBASE X64ASM:>R64 ;
: ROW ( str -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: DESC-FIELD ( n -- n ) DESC + ;
: ARG ( n -- n ) CELL * ARGS + ;

: PUSH-CELL, ( n -- ) {: off:n :}
   X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM  0 X64RT:G-PUSH ;
: POP-CELL, ( n -- ) {: off:n :}
   0 X64RT:G-POP  X64ASM:RAX DATA-REG off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

\ ---- the body and the descriptor ----------------------------------------------
: SAW!, ( X64ASM:r64 n -- ) {: r:X64ASM:r64 off:n :}  r X64ASM:RBP off X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-MR ;

: BODY-WORDS, ( -- )
   X64ASM:R13 SAW-R13 SAW!,  X64ASM:R14 SAW-R14 SAW!,  X64ASM:R15 SAW-R15 SAW!,
   X64ASM:RAX X64ASM:RBP TASK-TCB-CELL X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX X64ASM:RAX TASK-ABI:STATUS-OFF X64ASM:MEM-OFF X64CODE:ASM-SINK X64ASM:ENC-MOV-RM
   X64ASM:RAX SAW-STATUS SAW!,
   X64ASM:RAX PUSHED X64ASM:>IMM64 X64CODE:ASM-SINK X64ASM:ENC-MOV-RI64  0 X64RT:G-PUSH
   X64ASM:RCX ZERO-REG,  X64ASM:RDX ZERO-REG,  X64ASM:RSI ZERO-REG,  X64ASM:RDI ZERO-REG,
   X64ASM:R8 ZERO-REG,  X64ASM:R9 ZERO-REG,  X64ASM:R10 ZERO-REG,  X64ASM:R11 ZERO-REG, ;

: BODY ( -- label ) [: BODY-WORDS, ;] X64HARNESS:ROUTINE, ;

\ The descriptor a task's owner hands the entry. Its status is HALT-REQ, as a
\ TASK:HALT in the window before the thread starts leaves it, so a RUNNING the
\ entry wrote would show.
: DESCRIBE, ( label -- ) {: body:label :}
   body TASK-ABI:XT-OFF DESC-FIELD X64HARNESS:LABEL-CELL!,
   STACK TASK-ABI:STACK-OFF DESC-FIELD X64HARNESS:DATA-ADDR!,
   REGION TASK-ABI:REGION-OFF DESC-FIELD X64HARNESS:DATA-ADDR!,
   DBASE-MARK TASK-ABI:DBASE-OFF DESC-FIELD X64HARNESS:CELL!,
   NDICT-MARK TASK-ABI:NDICT-OFF DESC-FIELD X64HARNESS:CELL!,
   CP-MARK TASK-ABI:CP-OFF DESC-FIELD X64HARNESS:CELL!,
   TASK-ABI:HALT-REQ TASK-ABI:STATUS-OFF DESC-FIELD X64HARNESS:CELL!, ;

\ ffi-call ( argbuf nargs fn -- ret ) of the entry on the descriptor.
: ENTER, ( -- )
   DESC 0 ARG X64HARNESS:DATA-ADDR!,
   ARGS X64HARNESS:PUSH-DATA,  1 N,
   s" task-entry" ROW  s" ffi-call" ROW ;

\ ---- the thread ---------------------------------------------------------------
\ Push the function dlsym finds for the name, whose bytes sit NUL-terminated
\ behind a jump.
: SYM, ( str -- ) {: s:str :}
   X64CODE:LBL X64CODE:LBL {: name:label past:label :}
   past X64CODE:JMP,
   name X64CODE:LBL,  s X64CODE:ASM-SINK VEC:APPEND  0 X64CODE:ASM-SINK VEC:PUSH c!
   past X64CODE:LBL,
   X64ASM:RSI name X64CODE:MOVABS,  X64KERNEL:DLSYM,  0 X64RT:G-PUSH ;

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
\ pthread_create's answer is checked while the thread may still run.
: THREAD-CASE ( -- )
   BODY DESCRIBE,
   -1 ANSWER X64HARNESS:CELL!,
   CREATE,  0 WANT
   JOIN,  0 WANT
   0 ANSWER X64HARNESS:EXPECT-CELL,
   TASK-ABI:DONE TASK-ABI:STATUS-OFF DESC-FIELD X64HARNESS:EXPECT-CELL,
   REGION TASK-TCB-CELL + PUSH-CELL,  DESC X64HARNESS:EXPECT-POP-DATA,
   PUSHED STACK X64HARNESS:EXPECT-CELL, ;

\ The machine stack holds a copy of each callee-saved register across the call;
\ the check pops it and compares.
: HOLD, ( X64ASM:r64 -- ) X64CODE:ASM-SINK X64ASM:ENC-PUSH ;
: EXPECT-HELD, ( X64ASM:r64 -- ) {: r:X64ASM:r64 :}
   X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-POP  X64ASM:RAX r X64CODE:ASM-SINK X64ASM:ENC-SUB-RR  0 X64RT:G-PUSH  0 WANT ;

: CALL-CASE ( -- )
   BODY DESCRIBE,
   X64ASM:RBP HOLD,  X64ASM:RBX HOLD,  X64ASM:R13 HOLD,  X64ASM:R14 HOLD,  X64ASM:R15 HOLD,
   ENTER,  0 WANT
   X64ASM:R15 EXPECT-HELD,  X64ASM:R14 EXPECT-HELD,  X64ASM:R13 EXPECT-HELD,
   X64ASM:RBX EXPECT-HELD,  X64ASM:RBP EXPECT-HELD, ;

\ The answer is CALL-CASE's to check; this case drops it.
: BODY-CASE ( -- )
   BODY DESCRIBE,
   ENTER,  0 X64RT:G-POP
   DBASE-MARK REGION SAW-R13 + X64HARNESS:EXPECT-CELL,
   NDICT-MARK REGION SAW-R14 + X64HARNESS:EXPECT-CELL,
   CP-MARK REGION SAW-R15 + X64HARNESS:EXPECT-CELL,
   TASK-ABI:HALT-REQ REGION SAW-STATUS + X64HARNESS:EXPECT-CELL, ;

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
   X64HARNESS:INIT
   [: THREAD-CASE ;] false s" hb-x64-kernel-task" TMP-PATH BUILD
   [: CALL-CASE ;] false s" hb-x64-kernel-task-call" TMP-PATH BUILD
   [: BODY-CASE ;] false s" hb-x64-kernel-task-body" TMP-PATH BUILD
   [: THREAD-CASE ;] true s" hb-x64-kernel-task-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-TASK:RUN
