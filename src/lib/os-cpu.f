\ os-cpu.f - processor facts the running host reports. ONLINE reads
\ sysconf(_SC_NPROCESSORS_ONLN); below one is E-ONLINE. ALLOWED counts the CPUs
\ the calling process may run on, as nproc(1) does: on Linux the set bits of
\ sched_getaffinity(2)'s mask for the calling thread, read as a 1024-CPU
\ cpu_set_t (glibc's and musl's CPU_SETSIZE); a failed call, which a host of
\ more CPUs than that makes, or a count below one is E-ALLOWED. macOS has no
\ affinity mask, so there ALLOWED is ONLINE. LOAD1 is getloadavg(3)'s first
\ sample as binary64; a return below one is E-LOAD. Each is logged with what
\ the call answered before it throws.
\ STORAGE CLASS. CALLER-OWNED: constants only; LOAD1's sample and ALLOWED's
\ mask are MEM:WITH-BYTES scopes.

require src/lib/ffi-abi.f
require src/lib/ieee754.f
require src/lib/memory.f
require src/lib/log.f

package OS-CPU
public

-9550 constant E-FIRST
-9559 constant E-LAST
-9550 s" sysconf reported no online processor, or failed" ERROR:FAULT E-ONLINE
-9551 s" getloadavg reported no sample, or failed" ERROR:FAULT E-LOAD
-9552 s" sched_getaffinity reported no allowed processor, or failed" ERROR:FAULT E-ALLOWED

private

\ _SC_NPROCESSORS_ONLN is 58 in the macOS SDK's unistd.h, 84 on Linux on both
\ architectures (musl unistd.h, glibc bits/confname.h).
58 constant ONLN-MACOS
84 constant ONLN-LINUX
8 constant SAMPLE-BYTES     \ one C double
128 constant MASK-BYTES     \ one cpu_set_t: CPU_SETSIZE 1024 bits

PROCESS-SYMBOLS
FUNCTION: SYSCONF-CALL sysconf ( n -- n ) ;FUNCTION
FUNCTION: GETLOADAVG-CALL getloadavg ( ^u8 n -- i32 ) ;FUNCTION
HB-TARGET-LINUX? [if]
FUNCTION: AFFINITY-CALL sched_getaffinity ( n n ^u8 -- i32 ) ;FUNCTION
[then]

: ONLN ( -- n )
   HB-TARGET-MACOS? if ONLN-MACOS else ONLN-LINUX then ;

\ A call's answer the host gave in place of a fact, logged.
: REJECTED ( str n -- ) {: what:str got:n :}
   LOG:ERROR s" os-cpu" LOG:LINE what LOG:TEXT+ s"  answered " LOG:TEXT+
      got LOG:INT+ LOG:;LINE ;

: SAMPLE ( ^u8 NUM:alloc-byte-len -- r )
   drop {: buf :}
   buf 1 GETLOADAVG-CALL {: got :}
   got 1 < if s" getloadavg" got REJECTED E-LOAD throw then
   buf CELL-VIEW @ IEEE754:BITS>F64 ;

\ The set bits of x.
: BITS ( n -- n )
   0 swap begin dup 0<> while dup 1- and swap 1+ swap repeat drop ;

HB-TARGET-LINUX? [if]
\ The set bits of the calling thread's affinity mask; pid 0 is the caller.
: MASK ( ^u8 NUM:alloc-byte-len -- n )
   drop {: buf :}
   0 MASK-BYTES buf AFFINITY-CALL {: got :}
   got 0<> if s" sched_getaffinity" got REJECTED E-ALLOWED throw then
   0 MASK-BYTES 0 ?do buf i + CELL-VIEW @ BITS + 1 cells +loop ;
[then]

public

: ONLINE ( -- n )
   ONLN SYSCONF-CALL {: count:n :}
   count 1 < if s" sysconf" count REJECTED E-ONLINE throw then
   count ;

HB-TARGET-LINUX? [if]
: ALLOWED ( -- n )
   MASK-BYTES MEM:BYTES-ALLOC-LEN [: MASK ;] MEM:WITH-BYTES {: count:n :}
   count 1 < if s" sched_getaffinity's mask" count REJECTED E-ALLOWED throw then
   count ;
[then]

HB-TARGET-MACOS? [if]
: ALLOWED ( -- n ) ONLINE ;
[then]

: LOAD1 ( -- r )
   SAMPLE-BYTES MEM:BYTES-ALLOC-LEN [: SAMPLE ;] MEM:WITH-BYTES ;

;package
