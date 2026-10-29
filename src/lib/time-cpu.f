\ time-cpu.f - the calling thread's CPU time (user and system) in `package TIME`,
\ through libc clock_gettime. Unlike src/lib/time.f's wall clocks it excludes time
\ preempted, waiting or asleep, but still runs slower on a slower core. It is its
\ own file so time.f stays free of src/lib/ffi-abi.f and src/lib/task.f.
\ STORAGE CLASS. TASK-LOCAL: clock_gettime writes through a pointer, so the
\ timespec is a TASK:+USER row; tasks are OS threads and share nothing.

require src/lib/errors.f
require src/lib/ffi-abi.f
require src/lib/task.f

package TIME

private

$10 constant SPEC-BYTES                 \ struct timespec on an LP64 host
0 constant SPEC-SEC-OFF                 \ time_t tv_sec
8 constant SPEC-NSEC-OFF                \ long tv_nsec
1000000000 constant NS-PER-S

\ CLOCK_THREAD_CPUTIME_ID is a different number on each host.
3 constant THREAD-CPU-LINUX
16 constant THREAD-CPU-MACOS

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and SPEC-BYTES TASK:+USER CPU-SPEC drop

PROCESS-SYMBOLS

FUNCTION: CLOCK-GETTIME-CALL clock_gettime ( n ptr u8 -- i32 )
   1 SPEC-BYTES WRITES-BYTES
;FUNCTION

: THREAD-CPU-CLOCK ( -- n )
   HB-TARGET-LINUX-KERNEL? if THREAD-CPU-LINUX exit then
   HB-TARGET-MACOS? if THREAD-CPU-MACOS exit then
   E-PROC-HOST throw ;

public

\ Throws E-TIME-CLOCK when the host refuses the clock.
: THREAD-CPU-NS ( -- n )
   THREAD-CPU-CLOCK CPU-SPEC BYTE-VIEW CLOCK-GETTIME-CALL
   0 <> if E-TIME-CLOCK throw then
   CPU-SPEC SPEC-SEC-OFF + @ NS-PER-S *
   CPU-SPEC SPEC-NSEC-OFF + @ + ;

;package
