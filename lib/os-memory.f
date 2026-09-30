\ os-memory.f - process memory facts that are not image ABI constants.
\
\ STACK-ABI:PAGE-BYTES and PROT-PAGE-MAX are Habu's fixed guard-window
\ contract. PAGE-SIZE is different: it reports the running process's host
\ page size for callers that need to describe or align an OS mapping. The
\ checked FFI binding resolves on its first call and image preparation forgets
\ the address (lib/ffi-abi.f FORGET-SYMBOLS), so a captured image never carries
\ a process-owned callable address.
require lib/errors.f
require lib/ffi-abi.f

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
