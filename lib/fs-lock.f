\ fs-lock.f - advisory, nonblocking ownership of caller-held descriptors.
\
\ TRY does not own or close the descriptor. The caller holds the exclusive
\ flock until close (or process death). OPEN-DIR gives long-lived directory
\ holds an O_CLOEXEC descriptor, so an exec child cannot prolong ownership.

require lib/errors.f
require lib/fs.f
require lib/ffi-abi.f

package FS-LOCK

2 constant LOCK-EX
4 constant LOCK-NB
35 constant WOULD-BLOCK-MACOS
11 constant WOULD-BLOCK-LINUX

PROCESS-SYMBOLS
FUNCTION: OPEN-CALL open ( ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: FLOCK-CALL flock ( n n -- i32 ) ;FUNCTION

: OPEN-FLAGS ( -- n )
   HB-TARGET-MACOS? if $1100000 exit then    \ O_CLOEXEC | O_DIRECTORY.
   \ Linux arch/arm64/include/uapi/asm/fcntl.h: O_DIRECTORY bit 14, O_DIRECT bit 16.
   HB-TARGET-LINUX? if $84000 exit then      \ O_CLOEXEC | O_DIRECTORY.
   E-FS-OPEN throw ;

: WOULD-BLOCK ( -- n )
   HB-TARGET-MACOS? if WOULD-BLOCK-MACOS else WOULD-BLOCK-LINUX then ;

public

: OPEN-DIR ( ptr u8 n -- fd )
   FS-PATHZ OPEN-FLAGS OPEN-CALL {: opened:n :}
   opened 0 < if E-FS-OPEN throw then
   opened >FD ;

: TRY ( fd -- bool )
   FD>N LOCK-EX LOCK-NB or FLOCK-CALL 0= if true exit then
   FFI:ERRNO WOULD-BLOCK = if false exit then
   E-FS-IO throw ;

;package
