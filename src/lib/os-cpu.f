split-str
\ os-cpu.f - processor facts the running host reports. ONLINE reads
\ sysconf(_SC_NPROCESSORS_ONLN); below one is E-ONLINE. LOAD1 is getloadavg(3)'s
\ first sample as binary64; a return below one is E-LOAD. Each is logged with
\ what the call answered before it throws.
\ STORAGE CLASS. CALLER-OWNED: constants only; LOAD1's sample is a
\ MEM:WITH-BYTES scope.

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

private

\ _SC_NPROCESSORS_ONLN is 58 in the macOS SDK's unistd.h, 84 on Linux on both
\ architectures (musl unistd.h, glibc bits/confname.h).
58 constant ONLN-MACOS
84 constant ONLN-LINUX
8 constant SAMPLE-BYTES     \ one C double

PROCESS-SYMBOLS
FUNCTION: SYSCONF-CALL sysconf ( n -- n ) ;FUNCTION
FUNCTION: GETLOADAVG-CALL getloadavg ( ^u8 n -- i32 ) ;FUNCTION

: ONLN ( -- n )
   HB-TARGET-MACOS? if ONLN-MACOS else ONLN-LINUX then ;

\ A call's answer the host gave in place of a fact, logged.
: REFUSED ( str n -- ) {: what:^ u:n got:n :}
   LOG:ERROR s" os-cpu" LOG:LINE what u LOG:TEXT+ s"  answered " LOG:TEXT+
      got LOG:INT+ LOG:;LINE ;

: SAMPLE ( ^u8 NUM:alloc-byte-len -- r )
   drop {: buf :}
   buf 1 GETLOADAVG-CALL {: got :}
   got 1 < if s" getloadavg" got REFUSED E-LOAD throw then
   buf CELL-VIEW @ IEEE754:BITS>F64 ;

public

: ONLINE ( -- n )
   ONLN SYSCONF-CALL {: count:n :}
   count 1 < if s" sysconf" count REFUSED E-ONLINE throw then
   count ;

: LOAD1 ( -- r )
   SAMPLE-BYTES MEM:BYTES-ALLOC-LEN [: SAMPLE ;] MEM:WITH-BYTES ;

;package
