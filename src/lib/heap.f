\ heap.f - the heap: libc's calloc, realloc and free as an allocator, so a
\ container made with ALLOCATOR runs on the system allocator.
\ STORAGE CLASS. PROCESS-WIDE: one record, filled at load and only read after,
\ so tasks share it; CALLER-OWNED: the caller frees each block it takes.

require src/lib/error.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/allocator.f

package HEAP
private

PROCESS-SYMBOLS
FUNCTION: CALLOC-CALL calloc ( n n -- ^u8 ) ;FUNCTION
FUNCTION: REALLOC-CALL realloc ( ^u8 n -- ^u8 ) ;FUNCTION
FUNCTION: FREE-CALL free ( ^u8 -- ) ;FUNCTION

: ALLOC-BYTES ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   1 len CALLOC-CALL {: p:^ :}
   p NULL-PTR = if E-MEM-MAP throw then
   p ;


\ A refused realloc leaves the block as it was.
: RESIZE-BYTES ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   block new REALLOC-CALL {: moved:^ :}
   moved NULL-PTR = if E-MEM-MAP throw then
   new old > if 0 moved old + new old - SPAN:MAKE SPAN:FILL then
   moved ;


: FREE-BYTES ( ^u8 ^u8 -- ) {: st:^ p:^ :} p FREE-CALL ;

TYPED-VARIABLE RECORD ALLOC:allocator

: FILL ( -- )
   NULL-PTR [: ALLOC-BYTES ;] [: RESIZE-BYTES ;] [: FREE-BYTES ;]
   RECORD ALLOC:INIT ;

FILL

public

: ALLOCATOR ( -- ^ALLOC:allocator ) RECORD ;

seal-package
;package
