\ process-cpu.f - the CPU time one process has run itself. CPU-NS answers the
\ user plus system time, in nanoseconds, that process pid has run, without
\ what it reaped from its children: a live or zombie child of the caller, or
\ any process the caller may read. A pid at or below 1, a pid that names no
\ process (reaped or never started) and any failed call throw E-PROC-OUTPUT.
\ It reads one process, with no walk of the process table and no lock.
\ Linux: clock_getcpuclockid(3) names pid's process CPU clock, answering an
\ errno value, ESRCH for no such process; clock_gettime(2) reads that clock as
\ a struct timespec. macOS: proc_pid_rusage with RUSAGE_INFO_V0 (0) fills a
\ struct rusage_info_v0 of 96 bytes (sys/resource.h) whose ri_user_time and
\ ri_system_time at 16 and 24 count mach absolute time units, which
\ mach_timebase_info's numer/denom scales to nanoseconds; a zombie answers its
\ final times, a reaped pid ESRCH and another user's process EPERM.
\ STORAGE CLASS. CALLER-OWNED: constants only; each call's out-parameters are
\ a MEM:WITH-BYTES scope, so tasks on several threads call it at once.

HB-TARGET-MACOS? HB-TARGET-LINUX? or ?TARGET

require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/memory.f

package PROC-CPU

private

1000000000 constant NS-PER-S

PROCESS-SYMBOLS

HB-TARGET-LINUX? [if]
FUNCTION: CPUCLOCKID clock_getcpuclockid ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: CLOCK-GET clock_gettime ( n ^u8 -- i32 ) ;FUNCTION

\ A struct timespec: tv_sec, then tv_nsec, eight bytes each. The clockid_t
\ out-parameter, a C int, takes its first four bytes before it is read.
16 constant BUF-BYTES

\ pid's process CPU clock; clock_getcpuclockid answers 0 or an errno.
: CLOCK ( n ^u8 -- n ) {: pid:n buf:^ :}
   pid buf CPUCLOCKID 0<> if E-PROC-OUTPUT throw then
   buf LE:S32@ ;

: READ ( n ^u8 NUM:alloc-byte-len -- n )
   drop {: pid:n buf:^ :}
   pid buf CLOCK buf CLOCK-GET 0<> if E-PROC-OUTPUT throw then
   buf LE:U64@ NS-PER-S * buf 8 + LE:U64@ + ;
[then]

HB-TARGET-MACOS? [if]
FUNCTION: PID-RUSAGE proc_pid_rusage ( n n ^u8 -- i32 ) ;FUNCTION
FUNCTION: TIMEBASE-INFO mach_timebase_info ( ^u8 -- i32 ) ;FUNCTION

0 constant RUSAGE-V0
16 constant RU-USER
24 constant RU-SYSTEM
96 constant TIMEBASE-AT             \ past the rusage_info_v0
TIMEBASE-AT 8 + constant BUF-BYTES  \ then u32 numer, u32 denom

: UNITS ( n ^u8 -- n ) {: pid:n buf:^ :}
   pid RUSAGE-V0 buf PID-RUSAGE 0<> if E-PROC-OUTPUT throw then
   buf RU-USER + LE:U64@ buf RU-SYSTEM + LE:U64@ + ;

: READ ( n ^u8 NUM:alloc-byte-len -- n )
   drop {: pid:n buf:^ :}
   pid buf UNITS {: units:n :}
   buf TIMEBASE-AT + {: base:^ :}
   base TIMEBASE-INFO 0<> if E-PROC-OUTPUT throw then
   units base LE:U32@ * base 4 + LE:U32@ / ;
[then]

public

: CPU-NS ( pid -- n ) {: pid:pid :}
   pid PID>N 1 <= if E-PROC-OUTPUT throw then
   pid PID>N BUF-BYTES MEM:BYTES-ALLOC-LEN [: READ ;] MEM:WITH-BYTES ;

;package
