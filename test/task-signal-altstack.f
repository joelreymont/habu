\ task-signal-altstack.f - a runtime alternate signal stack belongs to each
\ Habu OS thread, and profiler arming must keep that thread's stack installed.
\ The worker's failure exits the process by name before its creator can join.
require lib/test.f
require lib/task.f
require lib/ffi-abi.f
require src/habu/task-abi.f

package TASK-ALT-TEST

24 constant SS-BYTES
$10000 constant SS-SIZE
2 constant SS-DISABLE

create SS-ROW SS-BYTES allot
variable SS-BEFORE

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

: ALT@ ( -- n )
   WORKER BYTE-VIEW TASK-ABI:ALTSTACK-OFF + CELL-VIEW @ ;

: NEED-ALT ( -- )
   ALT@ 0= if s" task-altstack: prepared task has no mapping" 78 die then ;

: NEED-RELEASE ( -- )
   ALT@ 0<> if s" task-altstack: released task kept mapping" 78 die then ;

: CHECK-WORKER ( -- )
   CHECK-SS
   SS-ROW CELL-VIEW @ ALT@ <> if
      s" task-altstack: worker used another signal stack" 78 die
   then
   CHECK-PROF ;

: RUN ( -- )
   HB-TARGET-LINUX-X86-64? if
      ['] CHECK-WORKER WORKER TASK:ACTIVATE
      NEED-ALT
      WORKER TASK:KILL
      NEED-RELEASE
      WORKER TASK:PREPARE
      NEED-ALT
      IMAGE-LIFECYCLE:PREPARE
      NEED-RELEASE
      CHECK-PROF
   then
   s" test: ok" type cr ;

RUN
;package
