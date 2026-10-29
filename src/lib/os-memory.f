\ os-memory.f - process memory facts that are not image ABI constants.
\ PAGE-SIZE reports the running host's page size for OS mappings; Habu's guard
\ window uses the fixed STACK-ABI:PAGE-BYTES and PROT-PAGE-MAX instead.
\ The FFI binding resolves at its first call and image preparation forgets it
\ (src/lib/ffi-abi.f FORGET-SYMBOLS): a captured image carries no process address.
require src/lib/errors.f
require src/lib/ffi-abi.f

package OS-MEMORY
private

PROCESS-SYMBOLS
FUNCTION: PAGE-SIZE-CALL getpagesize ( -- i32 ) ;FUNCTION

: PAGE-SIZE-CHECK ( n -- n )
   dup 0 <= if drop E-MEM-SIZE throw then ;

public

: PAGE-SIZE ( -- n )
   PAGE-SIZE-CALL PAGE-SIZE-CHECK ;

;package
