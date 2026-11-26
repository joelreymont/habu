\ fd-limit.f - a test's descriptor limit, package FD-LIMIT. RAISE lifts the
\ soft descriptor limit to what a test holds open.
\
\ STORAGE CLASS. PROCESS-WIDE: the limit belongs to the process, read and set
\ through one static buffer.

require src/lib/ffi-abi.f
require src/lib/test/assert.f

package FD-LIMIT

private

PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ^u8 -- i32 ) ;FUNCTION

create RLIMIT 16 allot

: NOFILE ( -- n ) HB-TARGET-MACOS? if 8 else 7 then ;

public

\ launchd gives a macOS login shell a soft limit of 256, under what a test
\ holds open. Linux's RLIM_INFINITY reads as -1 here, and is left as it is.
: RAISE ( n -- ) {: want:n :}
   NOFILE RLIMIT GET-LIMIT 0 T=
   RLIMIT @ {: soft:n :}
   soft 0< soft want >= or if exit then
   want RLIMIT !
   NOFILE RLIMIT SET-LIMIT 0 T= ;

;package
