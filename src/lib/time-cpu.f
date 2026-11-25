\ time-cpu.f - the calling thread's CPU time (user and system) in `package TIME`,
\ through libc clock_gettime. Unlike src/lib/time.f's wall clocks it excludes time
\ preempted, waiting or asleep, but still runs slower on a slower core. It is its
\ own file so time.f stays free of src/lib/ffi-abi.f and src/lib/task.f.
\ STORAGE CLASS. TASK-LOCAL: clock_gettime writes through a pointer, so the
\ timespec is a TASK:+USER record; tasks are OS threads and share nothing.

require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/task.f

package TIME

private

1000000000 constant NS-PER-S

\ CLOCK_THREAD_CPUTIME_ID is a different number on each host.
3 constant THREAD-CPU-LINUX
16 constant THREAD-CPU-MACOS

TASK:+USER CPU-SPEC TASK:timespec

PROCESS-SYMBOLS

FUNCTION: CLOCK-GETTIME-CALL clock_gettime ( n ^u8 -- i32 )
;FUNCTION

: THREAD-CPU-CLOCK ( -- n )
   HB-TARGET-LINUX? if THREAD-CPU-LINUX else THREAD-CPU-MACOS then ;

public

\ Throws E-TIME-CLOCK when the clock read fails.
: THREAD-CPU-NS ( -- n )
   THREAD-CPU-CLOCK CPU-SPEC BYTE-VIEW CLOCK-GETTIME-CALL
   0 <> if E-TIME-CLOCK throw then
   CPU-SPEC TASK-TIMESPEC:SEC @ NS-PER-S *
   CPU-SPEC TASK-TIMESPEC:NSEC @ + ;

;package
