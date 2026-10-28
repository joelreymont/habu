\ os-cpu.f - processor facts the running host reports.
\
\ ONLINE answers how many processors are online now, from
\ sysconf(_SC_NPROCESSORS_ONLN). The name is 58 in the macOS SDK's
\ usr/include/unistd.h (`#define _SC_NPROCESSORS_ONLN 58`) and 84 on Linux on
\ both architectures: musl's unistd.h `#define _SC_NPROCESSORS_ONLN 84`, and
\ the same position in glibc's bits/confname.h enum. A count below one,
\ sysconf's -1 failure among them, throws E-ONLINE.
\
\ LOAD1 answers the one-minute load average, the first sample getloadavg(3)
\ writes (`int getloadavg(double loadavg[], int nelem)`, in libc on macOS,
\ glibc and musl). It asks for one sample into an eight-byte mapping the call
\ owns and reads that binary64 back through IEEE754:BITS>F64. A return below
\ one, getloadavg's -1 failure among them, throws E-LOAD.
\
\ The checked FFI bindings resolve on their first call and image preparation
\ forgets the addresses (src/lib/ffi-abi.f FORGET-SYMBOLS), so a captured image
\ never carries a process-owned callable address.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state (constants only);
\ LOAD1's sample buffer is a MEM:WITH-BYTES scope released on every exit.

require src/lib/ffi-abi.f
require src/lib/ieee754.f
require src/lib/memory.f

package OS-CPU
public

-9550 constant E-FIRST
-9559 constant E-LAST
-9550 constant E-ONLINE     \ sysconf reported no online processor, or failed
-9551 constant E-LOAD       \ getloadavg reported no sample, or failed

private

58 constant ONLN-MACOS
84 constant ONLN-LINUX
8 constant SAMPLE-BYTES     \ one C double

PROCESS-SYMBOLS
FUNCTION: SYSCONF-CALL sysconf ( n -- n ) ;FUNCTION
FUNCTION: GETLOADAVG-CALL getloadavg ( ptr u8 n -- i32 )
   0 SAMPLE-BYTES WRITES-BYTES          \ the one sample nelem 1 asks for
;FUNCTION

: ONLN ( -- n )
   HB-TARGET-MACOS? if ONLN-MACOS else ONLN-LINUX then ;


: SAMPLE ( ptr u8 NUM:alloc-byte-len -- r )
   drop {: buf :}
   buf 1 GETLOADAVG-CALL 1 < if E-LOAD throw then
   buf CELL-VIEW @ IEEE754:BITS>F64 ;

public

: ONLINE ( -- n )
   ONLN SYSCONF-CALL {: count:n :}
   count 1 < if E-ONLINE throw then
   count ;

: LOAD1 ( -- r )
   SAMPLE-BYTES MEM:BYTES-ALLOC-LEN [: SAMPLE ;] MEM:WITH-BYTES ;

;package
