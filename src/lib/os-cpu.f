\ os-cpu.f - processor facts the running host reports.
\
\ ONLINE answers how many processors are online now, from
\ sysconf(_SC_NPROCESSORS_ONLN). The name is 58 in the macOS SDK's
\ usr/include/unistd.h (`#define _SC_NPROCESSORS_ONLN 58`) and 84 on Linux on
\ both architectures: musl's unistd.h `#define _SC_NPROCESSORS_ONLN 84`, and
\ the same position in glibc's bits/confname.h enum. A count below one,
\ sysconf's -1 failure among them, throws E-ONLINE. The checked FFI binding
\ resolves on its first call and image preparation forgets the address
\ (src/lib/ffi-abi.f FORGET-SYMBOLS), so a captured image never carries a
\ process-owned callable address.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state (constants only).

require src/lib/ffi-abi.f

package OS-CPU
public

-9550 constant E-FIRST
-9559 constant E-LAST
-9550 constant E-ONLINE     \ sysconf reported no online processor, or failed

private

58 constant ONLN-MACOS
84 constant ONLN-LINUX

PROCESS-SYMBOLS
FUNCTION: SYSCONF-CALL sysconf ( n -- n ) ;FUNCTION

: ONLN ( -- n )
   HB-TARGET-MACOS? if ONLN-MACOS else ONLN-LINUX then ;

public

: ONLINE ( -- n )
   ONLN SYSCONF-CALL {: count:n :}
   count 1 < if E-ONLINE throw then
   count ;

;package
