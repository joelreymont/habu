split-str
\ counting-allocator.f - test-only: an allocator over the heap that counts the
\ blocks it gives and frees, so a test sees storage a reader or source leaks.
\ TAKEN counts BYTES calls that answered and FREED its FREE calls; a resize
\ moves a block without counting.
\ From the BYTES call numbered REFUSED since TAKEN was last cleared, every call is
\ E-MEM-SIZE; 0 refuses none.
\ STORAGE CLASS. PROCESS-WIDE: the counters and the allocator record.

require src/lib/error.f
require src/lib/allocator.f
require src/lib/heap.f

package ALLOC-TEST
public
variable TAKEN
variable FREED
variable REFUSED

private
TYPED-VARIABLE COUNTER ALLOC:allocator

: C-ALLOC ( str -- ^u8 )
   {: st:^ len:n :}
   TAKEN @ 1+ REFUSED @ = if E-MEM-SIZE throw then
   HEAP:ALLOCATOR len ALLOC:BYTES drop  1 TAKEN +! ;

: C-RESIZE ( ^u8 str n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   HEAP:ALLOCATOR block old new ALLOC:RESIZE drop ;

: C-FREE ( ^u8 ^u8 -- )
   {: st:^ block:^ :}
   1 FREED +!  HEAP:ALLOCATOR block ALLOC:FREE ;

: COUNTER-FILL ( -- )
   NULL-PTR [: C-ALLOC ;] [: C-RESIZE ;] [: C-FREE ;] COUNTER ALLOC:INIT ;

COUNTER-FILL

public
: COUNTING ( -- ^ALLOC:allocator ) COUNTER ;

;package
