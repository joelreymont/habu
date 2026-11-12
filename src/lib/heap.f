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
FUNCTION: CALLOC-CALL calloc ( n n -- ptr u8 ) ;FUNCTION
FUNCTION: REALLOC-CALL realloc ( ptr u8 n -- ptr u8 ) ;FUNCTION
FUNCTION: FREE-CALL free ( ptr u8 -- ) ;FUNCTION

: ALLOC-BYTES ( ptr u8 n -- ptr u8 )
   {: st:ptr len:n :}
   1 len CALLOC-CALL {: p:ptr :}
   p NULL-PTR = if E-MEM-MAP throw then
   p ;


\ A refused realloc leaves the block as it was.
: RESIZE-BYTES ( ptr u8 ptr u8 n n -- ptr u8 )
   {: st:ptr block:ptr old:n new:n :}
   block new REALLOC-CALL {: moved:ptr :}
   moved NULL-PTR = if E-MEM-MAP throw then
   new old > if 0 moved old + new old - SPAN:MAKE SPAN:FILL then
   moved ;


: FREE-BYTES ( ptr u8 ptr u8 -- ) {: st:ptr p:ptr :} p FREE-CALL ;

TYPED-VARIABLE RECORD ALLOC:allocator

: FILL ( -- )
   NULL-PTR [: ALLOC-BYTES ;] [: RESIZE-BYTES ;] [: FREE-BYTES ;]
   RECORD ALLOC:INIT ;

FILL

public

: ALLOCATOR ( -- ptr ALLOC:allocator ) RECORD ;

seal-package
;package
