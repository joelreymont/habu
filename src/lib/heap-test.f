\ ALLOC through the heap's allocator: the cases every allocator meets; bytes
\ come back zeroed where a freed block's were dirty; small records share
\ pages; RESIZE keeps a block's bytes as it grows and shrinks and zeroes the
\ added ones, bytes a shrink cut included; a refused request is E-MEM-MAP and
\ leaves the block.
require src/lib/test.f
require src/lib/allocator.f
require src/lib/span.f
require src/lib/heap.f
require src/lib/test/allocator-cases.f

package HEAP-ALLOC-TEST

1000 constant RECORDS
RECORDS TYPED-BUFFER KEPT ptr u8
$7FFFFFFFFFFFFFF0 constant HUGE

: AL ( -- ptr ALLOC:allocator ) HEAP:ALLOCATOR ;


: DIRTY ( -- )
   s" ALLOC:BYTES answers zeroed bytes where a freed block's were dirty" T-LABEL
   AL 4096 ALLOC:BYTES drop {: block:ptr :}
   $FF block 4096 SPAN:MAKE SPAN:FILL
   AL block ALLOC:FREE
   AL 4096 ALLOC:BYTES drop {: again:ptr :}
   again 4096 ALLOC-TEST:ZEROED
   AL again ALLOC:FREE ;


: GAP ( n -- n )
   {: k:n :}
   k KEPT @ NULL-PTR -  k 1 - KEPT @ NULL-PTR -  - abs ;


\ A mapping per block puts each at least a 4 KiB page from the last: 0.
: SHARED ( -- n )
   0 RECORDS 1 ?do i GAP $1000 < if 1 + then loop ;


: PAGES ( -- )
   s" 1000 blocks of 16 bytes mostly lie within 4 KiB of the one before" T-LABEL
   RECORDS 0 ?do AL 16 ALLOC:BYTES drop i KEPT ! loop
   SHARED 500 >= TTRUE
   RECORDS 0 ?do AL i KEPT @ ALLOC:FREE loop ;


: GROWN ( -- ptr u8 )
   s" ALLOC:RESIZE grows a block, keeping its bytes" T-LABEL
   AL 16 ALLOC:BYTES drop {: block:ptr :}
   7 block c!  9 block 15 + c!
   AL block 16 $100000 ALLOC:RESIZE $100000 T= {: grown:ptr :}
   grown c@ 7 T=  grown 15 + c@ 9 T=
   grown ;


: SHRUNK ( ptr u8 -- ptr u8 )
   {: grown:ptr :}
   s" and shrinks it, keeping the bytes that remain" T-LABEL
   AL grown $100000 8 ALLOC:RESIZE 8 T= {: shrunk:ptr :}
   shrunk c@ 7 T=  shrunk 1 + 7 ALLOC-TEST:ZEROED
   shrunk ;


\ Dirtied first, so bytes the shrink cut read $FF unless the regrowth zeroes them.
: REGROWN ( -- )
   s" and zeroes bytes a shrink cut when the block grows again" T-LABEL
   AL 64 ALLOC:BYTES drop {: block:ptr :}
   $FF block 64 SPAN:MAKE SPAN:FILL
   AL block 64 16 ALLOC:RESIZE drop {: cut:ptr :}
   AL cut 16 64 ALLOC:RESIZE drop {: back:ptr :}
   back c@ $FF T=  back 15 + c@ $FF T=
   back 16 + 48 ALLOC-TEST:ZEROED
   AL back ALLOC:FREE ;


: RESIZES ( -- ) GROWN SHRUNK {: block:ptr :} AL block ALLOC:FREE  REGROWN ;


: REFUSED ( -- )
   s" a refused request throws E-MEM-MAP and leaves the block" T-LABEL
   [: AL HUGE ALLOC:BYTES drop drop ;] E-MEM-MAP TTHROWSQ
   AL 16 ALLOC:BYTES drop {: block:ptr :}
   5 block c!  6 block 15 + c!
   [: AL block 16 HUGE ALLOC:RESIZE drop drop ;] E-MEM-MAP TTHROWSQ
   block c@ 5 T=  block 15 + c@ 6 T=
   AL block ALLOC:FREE ;


: RUN ( -- )
   T-RESET
   AL ALLOC-TEST:TURNS DIRTY PAGES RESIZES REFUSED ;

RUN
T-REPORT

;package
