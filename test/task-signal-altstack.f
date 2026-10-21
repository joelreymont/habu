\ task-signal-altstack.f - a runtime alternate signal stack belongs to each
\ Habu OS thread, and profiler arming must keep that thread's stack installed.
\ The x86-64 task entry registers its own band below the task's pushes
\ (src/habu/kernel-x64.f, X64BOOT:SIGNAL-STACK,), so a worker answers a
\ signal stack other than its creator's. The worker's failure exits the process
\ by name before its creator can join.
require src/lib/test.f
require src/lib/task.f
require src/lib/ffi-abi.f
require src/habu/stack-abi.f

package TASK-ALT-TEST

24 constant SS-BYTES
STACK-ABI:SIGNAL-BYTES constant SS-SIZE
2 constant SS-DISABLE

create SS-ROW SS-BYTES allot
variable SS-BEFORE
variable SS-MAIN

PROCESS-SYMBOLS
FUNCTION: SIGALTSTACK-CALL sigaltstack ( ptr u8 ptr u8 -- i32 )
   1 SS-BYTES WRITES-BYTES
;FUNCTION

: READ-SS ( -- )
   NULL-PTR SS-ROW SIGALTSTACK-CALL 0 <> if
      s" task-altstack: cannot query signal stack" 78 die
   then ;

: CHECK-SS ( -- )
   READ-SS
   SS-ROW CELL-VIEW @ 0= if
      s" task-altstack: no signal stack" 78 die
   then
   SS-ROW CELL + CELL-VIEW @ SS-DISABLE and 0 <> if
      s" task-altstack: disabled signal stack" 78 die
   then
   SS-ROW 2 CELL * + CELL-VIEW @ SS-SIZE <> if
      s" task-altstack: wrong signal stack size" 78 die
   then ;

: CHECK-PROF ( -- )
   CHECK-SS
   SS-ROW CELL-VIEW @ SS-BEFORE !
   0 prof-on
   CHECK-SS
   prof-off
   SS-ROW CELL-VIEW @ SS-BEFORE @ <> if
      s" task-altstack: profiler replaced signal stack" 78 die
   then ;

TASK:MIN-STACK TASK:TASK WORKER

: CHECK-WORKER ( -- )
   CHECK-SS
   SS-ROW CELL-VIEW @ SS-MAIN @ = if
      s" task-altstack: worker shares its creator's signal stack" 78 die
   then
   CHECK-PROF
   0 TASK:RETURN ;

: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if
      READ-SS  SS-ROW CELL-VIEW @ SS-MAIN !
      ['] CHECK-WORKER WORKER TASK:ACTIVATE
      WORKER TASK:JOIN MATCH result
         ok OF drop ENDOF
         err OF drop s" task-altstack: worker failed" 78 die ENDOF
      ;MATCH
      CHECK-PROF
   then
   s" test: ok" type cr ;

RUN
;package
