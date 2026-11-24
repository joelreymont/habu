\ counting-allocator.f - test-only allocators over the heap. COUNTING counts
\ the blocks it gives and frees, so a test sees storage a reader or source
\ leaks: TAKEN counts its BYTES calls that answered and FREED its FREE calls;
\ a resize moves a block without counting.
\ From the BYTES call numbered REFUSED since TAKEN was last cleared, every call is
\ E-MEM-SIZE; 0 refuses none.
\ MOVING's resize always takes a new block and fills the old one with $A5
\ before freeing it, so a read through a stale pointer shows.
\ STORAGE CLASS. PROCESS-WIDE: the counters and the allocator records.

require src/lib/error.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/span.f

package ALLOC-TEST
public
variable TAKEN
variable FREED
variable REFUSED

private
TYPED-VARIABLE COUNTER ALLOC:allocator

: C-ALLOC ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   TAKEN @ 1+ REFUSED @ = if E-MEM-SIZE throw then
   HEAP:ALLOCATOR len ALLOC:BYTES drop  1 TAKEN +! ;

: C-RESIZE ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   HEAP:ALLOCATOR block old new ALLOC:RESIZE drop ;

: C-FREE ( ^u8 ^u8 -- )
   {: st:^ block:^ :}
   1 FREED +!  HEAP:ALLOCATOR block ALLOC:FREE ;

: COUNTER-FILL ( -- )
   NULL-PTR [: C-ALLOC ;] [: C-RESIZE ;] [: C-FREE ;] COUNTER ALLOC:INIT ;

COUNTER-FILL

TYPED-VARIABLE MOVER ALLOC:allocator

: M-ALLOC ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   HEAP:ALLOCATOR len ALLOC:BYTES drop ;

: M-RESIZE ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   HEAP:ALLOCATOR new ALLOC:BYTES drop {: moved:^ :}
   block moved old new min BYTE-COPY
   $A5 block old SPAN:MAKE SPAN:FILL
   HEAP:ALLOCATOR block ALLOC:FREE  moved ;

: M-FREE ( ^u8 ^u8 -- )
   {: st:^ block:^ :}
   HEAP:ALLOCATOR block ALLOC:FREE ;

: MOVER-FILL ( -- )
   NULL-PTR [: M-ALLOC ;] [: M-RESIZE ;] [: M-FREE ;] MOVER ALLOC:INIT ;

MOVER-FILL

public
: COUNTING ( -- ^ALLOC:allocator ) COUNTER ;
: MOVING ( -- ^ALLOC:allocator ) MOVER ;

;package
