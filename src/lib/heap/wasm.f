\ heap/wasm.f - the heap on the Wasm target, which has no libc: a two-level
\ segregated-fit heap (TLSF) over regions from MEM-ALLOC-BYTES.
\
\ A block is a 16-byte header, the block before it in its region and its
\ payload size with FREE-BIT, then its payload, 16-aligned; a free block's
\ payload holds its free-list links. A region is one block when mapped and
\ ends in a header of size 0, which no block has, so no merge crosses it. A
\ freed block merges with a free neighbour on each side; a region that is one
\ free block again is unmapped. Free blocks are listed by class, 16 to each
\ power of two. A request, rounded up to 16 bytes, takes the first block on
\ the lowest list whose every block holds it, and gives back what it does not
\ use when that is at least MIN-BLOCK. A region is REGION-BYTES, or as many
\ 64 KiB pages as a larger request takes.
\ STORAGE CLASS. PROCESS-WIDE: the lists are mutable and unlocked; a Wasm
\ module runs one task. CALLER-OWNED: the caller frees each block it takes.

HB-TARGET-WASM? ?TARGET
require src/lib/error.f
require src/lib/span.f
require src/lib/memory.f

package HEAP
private

\ prev is the block before it, NULL for the first; size its payload bytes,
\ FREE-BIT or'ed in; next-free and prev-free its links, in the payload.
STRUCTURE block DERIVE addr
   FIELD prev ^block
   FIELD size n
   FIELD next-free ^block
   FIELD prev-free ^block
;STRUCTURE

CAST: >BLOCK ( ^u8 -- ^block )
CAST: BLOCK> ( ^block -- ^u8 )

16 constant HEAD                    \ the header: prev and size
16 constant GRAIN                   \ payload alignment and size step
1 constant FREE-BIT
HEAD GRAIN + constant MIN-BLOCK     \ the least block a split leaves
\ 1 MiB: test/wasm/heap-test.f's churn grew Wasm memory least with it,
\ 99.6 MB against 107.0 MB at 64 KiB and 109.1 MB at 8 MiB; its REGION copies it
$100000 constant REGION-BYTES
$100000000 constant MOST            \ a larger request is refused: Wasm memory32

4 constant SL-BITS                  \ 16 classes per power of two
1 SL-BITS lshift constant SL-COUNT
SL-COUNT GRAIN * constant SMALL     \ below it, classes are GRAIN apart
26 constant FL-COUNT                \ classes up to MOST rounded up

FL-COUNT SL-COUNT * TYPED-BUFFER HEADS ^block
FL-COUNT TYPED-BUFFER SL-MAPS n     \ per first level, the lists not empty
variable FL-MAP                     \ the first levels not empty


: LOG2 ( n -- n ) 0 swap begin 1 rshift dup 0<> while swap 1+ swap repeat drop ;

\ The lowest set bit of a nonzero n.
: LOWEST ( n -- n ) 0 begin 2dup rshift 1 and 0= while 1+ repeat nip ;

: SIZE-OF ( ^block -- n ) BLOCK-SIZE @ GRAIN negate and ;
: FREE? ( ^block -- bool ) BLOCK-SIZE @ FREE-BIT and 0<> ;
: PAYLOAD ( ^block -- ^u8 ) BLOCK> HEAD + ;
: OWNER ( ^u8 -- ^block ) HEAD - >BLOCK ;
: NEXT ( ^block -- ^block ) dup PAYLOAD swap SIZE-OF + >BLOCK ;
: END? ( ^block -- bool ) BLOCK-SIZE @ 0= ;

: ROUND ( n -- n ) GRAIN 1- + GRAIN negate and GRAIN max ;


\ The class of a size: its first level and its list's index.
: CLASS ( n -- n n )
   {: len:n :}
   len SMALL < if 0 len GRAIN / exit then
   len LOG2 {: f:n :}
   f SL-BITS 3 + - {: fl:n :}
   fl  fl SL-COUNT *  len f SL-BITS - rshift SL-COUNT - + ;


\ A size moved into the lowest class whose least size is at least it, so
\ every block of that class or above holds it.
: CLASS-UP ( n -- n )
   {: len:n :}
   len SMALL < if len exit then
   len  1 len LOG2 SL-BITS - lshift 1- + ;


: INSERT ( ^block -- )
   {: b:^ :}
   b SIZE-OF CLASS {: fl:n idx:n :}
   idx HEADS @ {: head:^ :}
   head b BLOCK-NEXT-FREE !
   NULL-PTR b BLOCK-PREV-FREE !
   head NULL-PTR <> if b head BLOCK-PREV-FREE ! then
   b idx HEADS !
   fl SL-MAPS @  1 idx fl SL-COUNT * - lshift or  fl SL-MAPS !
   FL-MAP @  1 fl lshift or  FL-MAP ! ;


: REMOVE ( ^block -- )
   {: b:^ :}
   b SIZE-OF CLASS {: fl:n idx:n :}
   b BLOCK-NEXT-FREE @ {: next:^ :}
   b BLOCK-PREV-FREE @ {: prev:^ :}
   next NULL-PTR <> if prev next BLOCK-PREV-FREE ! then
   prev NULL-PTR <> if next prev BLOCK-NEXT-FREE ! exit then
   next idx HEADS !
   next NULL-PTR <> if exit then
   fl SL-MAPS @  1 idx fl SL-COUNT * - lshift invert and  dup fl SL-MAPS !
   0= if FL-MAP @  1 fl lshift invert and  FL-MAP ! then ;


\ The first block on the lists from the size's own class up when the size sits
\ on a class boundary, else from the next class up; NULL when none.
: FIND ( n -- ^block )
   CLASS-UP CLASS {: fl:n idx:n :}
   fl SL-MAPS @  -1 idx fl SL-COUNT * - lshift and {: here:n :}
   here 0<> if fl SL-COUNT * here LOWEST + HEADS @ exit then
   FL-MAP @  -1 fl 1+ lshift and {: above:n :}
   above 0= if NULL-PTR exit then
   above LOWEST {: up:n :}
   up SL-COUNT *  up SL-MAPS @ LOWEST + HEADS @ ;


\ A region holding a size, as one used block before its end header.
: MAP-REGION ( n -- ^block )
   {: len:n :}
   len HEAD 2 * + MEM-64K-SPAN-BYTES REGION-BYTES max {: bytes:n :}
   bytes MEM-ALLOC-BYTES drop >BLOCK {: b:^ :}
   NULL-PTR b BLOCK-PREV !
   bytes HEAD 2 * - b BLOCK-SIZE !
   b NEXT {: end:^ :}
   b end BLOCK-PREV !
   0 end BLOCK-SIZE !
   b ;


\ A region that is one free block again goes back; any other free block goes
\ on its list.
: SETTLE ( ^block -- )
   {: b:^ :}
   b BLOCK-PREV @ NULL-PTR =  b NEXT END?  and if
      b BLOCK>  b SIZE-OF HEAD 2 * +  MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
      exit
   then
   b INSERT ;


\ Marks a block free, merges it with each free neighbour and settles it.
: RELEASE ( ^block -- )
   {: b:^ :}
   b NEXT {: next:^ :}
   next FREE? if
      next REMOVE
      b SIZE-OF HEAD + next SIZE-OF + b BLOCK-SIZE !
      b b NEXT BLOCK-PREV !
   then
   b BLOCK-PREV @ {: prev:^ :}
   prev NULL-PTR <> if prev FREE? if
      prev REMOVE
      prev SIZE-OF HEAD + b SIZE-OF + prev BLOCK-SIZE !
      prev prev NEXT BLOCK-PREV !
      prev SIZE-OF FREE-BIT or prev BLOCK-SIZE !
      prev SETTLE exit
   then then
   b SIZE-OF FREE-BIT or b BLOCK-SIZE !
   b SETTLE ;


\ Cuts a used block to a size, freeing a remainder of at least MIN-BLOCK.
: TRIM ( ^block n -- )
   {: b:^ len:n :}
   b SIZE-OF len - {: spare:n :}
   spare MIN-BLOCK < if exit then
   b PAYLOAD len + >BLOCK {: rest:^ :}
   b rest BLOCK-PREV !
   spare HEAD - rest BLOCK-SIZE !
   len b BLOCK-SIZE !
   rest rest NEXT BLOCK-PREV !
   rest RELEASE ;


\ A used block of a rounded size, from the lists or a new region.
: TAKE ( n -- ^block )
   {: len:n :}
   len FIND {: found:^ :}
   found NULL-PTR = if len MAP-REGION else found REMOVE found then {: b:^ :}
   b SIZE-OF b BLOCK-SIZE !
   b len TRIM
   b ;


: CHECK ( n -- ) MOST > if E-MEM-MAP throw then ;


: ALLOC-BYTES ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   len CHECK
   len ROUND TAKE PAYLOAD {: p:^ :}
   0 p len SPAN:MAKE SPAN:FILL
   p ;


\ Grows in place into the block's slack or a free next block, else moves; a
\ refused move leaves the block as it was.
: RESIZE-BYTES ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ p:^ old:n new:n :}
   new CHECK
   p OWNER {: b:^ :}
   new ROUND {: want:n :}
   b NEXT {: next:^ :}
   want b SIZE-OF > next FREE? and if
      b SIZE-OF HEAD + next SIZE-OF + want >= if
         next REMOVE
         b SIZE-OF HEAD + next SIZE-OF + b BLOCK-SIZE !
         b b NEXT BLOCK-PREV !
      then
   then
   want b SIZE-OF <= if
      new old > if 0 p old + new old - SPAN:MAKE SPAN:FILL then
      b want TRIM
      p exit
   then
   st new ALLOC-BYTES {: moved:^ :}
   p moved old BYTE-COPY
   b RELEASE
   moved ;


: FREE-BYTES ( ^u8 ^u8 -- ) {: st:^ p:^ :} p OWNER RELEASE ;

;package
