\ time-cpu.f - the calling thread's CPU time, in `package TIME`.
\
\ lib/time.f's clocks are engine primitives and read wall time: a window they
\ bound also holds every slice the host gave other processes. THREAD-CPU-NS is
\ the CPU time the calling thread has run, user and system, read through libc's
\ clock_gettime; time the thread spent preempted, waiting or asleep is not in
\ it, so a window it bounds holds the thread's own work at any host load. It
\ still runs slower on a slower core.
\
\ A FILE OF ITS OWN. The clock is a foreign call whose answer lives in a task
\ row, so it loads lib/ffi-abi.f and lib/task.f; lib/time.f stays free of both
\ for the files that only want wall time, lib/build-cache.f among them.
\
\ THE TIMESPEC IS THE CALLING TASK'S OWN. clock_gettime writes its answer
\ through a pointer, and a Habu task is an OS thread, so the record is a
\ TASK:+USER row: two tasks reading their clocks at once share nothing.

require lib/errors.f
require lib/ffi-abi.f
require lib/task.f

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
   HB-TARGET-LINUX? if THREAD-CPU-LINUX exit then
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
