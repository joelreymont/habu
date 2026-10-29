\ os-cpu.f - processor facts the running host reports. ONLINE reads
\ sysconf(_SC_NPROCESSORS_ONLN); below one is E-ONLINE. LOAD1 is getloadavg(3)'s
\ first sample as binary64; a return below one is E-LOAD. Image preparation
\ forgets the FFI addresses (src/lib/ffi-abi.f FORGET-SYMBOLS).
\ STORAGE CLASS. CALLER-OWNED: constants only; LOAD1's sample is a
\ MEM:WITH-BYTES scope.

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

\ _SC_NPROCESSORS_ONLN is 58 in the macOS SDK's unistd.h, 84 on Linux on both
\ architectures (musl unistd.h, glibc bits/confname.h).
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
