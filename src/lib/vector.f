split-str
\ vector.f - package VEC: a typed vector<t> that takes its elements from an
\ allocator it keeps and grows through it. COUNT elements are live; growth
\ doubles the room the block holds, so n pushes make about log2 n resize calls.
\ A bad count is E-VEC-CAPACITY. AT trusts its index: the caller keeps it within
\ 0 to COUNT - 1.
\ STORAGE CLASS. CALLER-OWNED: the caller frees a vector with DELETE.

require src/lib/error.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/span.f

package VEC
private

MEM-MAX-CELLS constant VEC-MAX-CELLS

public

\ A typed vector: its allocator, its elements' bytes, their count, the elements
\ the bytes hold and the bytes each takes. The bytes of elements from count to
\ room are zero. No field holds a `t`, so every vector<t> has one layout.
STRUCTURE vector<t> OPAQUE DERIVE addr
   FIELD allocator ^ALLOC:allocator
   FIELD data ^u8
   FIELD count n
   FIELD room n
   FIELD width n
;STRUCTURE

private

CAST: >ELEM ( ^u8 -- ^t )
CAST: >VECTOR ( ^u8 -- ^vector<t> )
CAST: VECTOR> ( ^vector<t> -- ^u8 )

\ The most elements of n bytes that VEC-MAX-CELLS cells hold. COUNT and the
\ room passed CHECK-COUNT when they were set, so RESERVE and ENSURE skip its
\ division for an n within them.
: MOST ( n -- n ) VEC-MAX-CELLS CELL * swap 1 max / ;

: CHECK-COUNT ( n n -- )
   {: k:n w:n :}
   k 0 <  k w MOST >  or if E-VEC-CAPACITY throw then ;

\ Twice the room, or k when that is more or twice the room passes the ceiling.
: ROOM-FOR ( ^vector<t> n -- n )
   {: v:^ k:n :}
   v VECTOR-ROOM @ {: r:n :}
   r  v VECTOR-WIDTH @ MOST 2 /  > if k exit then
   k  r 2 *  max ;

: GROW ( ^vector<t> n -- )
   {: v:^ k:n :}
   v k ROOM-FOR {: r:n :}
   v VECTOR-WIDTH @ {: w:n :}
   v VECTOR-ALLOCATOR @  v VECTOR-DATA @  v VECTOR-ROOM @ w *  r w *
   ALLOC:RESIZE drop  v VECTOR-DATA !
   r v VECTOR-ROOM ! ;

: CUT ( ^vector<t> n -- )
   {: v:^ k:n :}
   v VECTOR-WIDTH @ {: w:n :}
   v VECTOR-DATA @ k w * +  v VECTOR-COUNT @ k - w *  SPAN:MAKE {: s :}
   0 s SPAN:FILL
   k v VECTOR-COUNT ! ;

public

\ k zeroed elements, or E-VEC-CAPACITY for a negative k or more bytes than
\ VEC-MAX-CELLS cells; a refused data request frees the header, then rethrows.
: NEW ( ^ALLOC:allocator n width<t> -- ^vector<t> )
   {: al:^ k:n w :}
   w width>n {: wn:n :}
   k wn CHECK-COUNT
   al [type] vector<u8> width>n ALLOC:BYTES drop >VECTOR {: v:^ :}
   al v VECTOR-ALLOCATOR !
   [: al k wn * ALLOC:BYTES drop v VECTOR-DATA ! ;] catch {: code:n :}
   code 0<> if al v VECTOR> ALLOC:FREE code throw then
   k v VECTOR-COUNT !  k v VECTOR-ROOM !  wn v VECTOR-WIDTH !
   v ;

: COUNT ( ^vector<t> -- n ) VECTOR-COUNT @ ;

: AT ( ^vector<t> n -- ^t )
   {: v:^ i:n :}
   v VECTOR-DATA @  i v VECTOR-WIDTH @ * +  >ELEM ;

: DELETE ( ^vector<t> -- )
   {: v:^ :}
   v NULL-PTR = if exit then
   v VECTOR-ALLOCATOR @ {: al:^ :}
   al v VECTOR-DATA @ ALLOC:FREE
   al v VECTOR> ALLOC:FREE ;

\ Room for n elements, COUNT kept; the block may move. Until COUNT passes n,
\ ENSURE, RESIZE, PUSH and APPEND neither allocate nor move the block.
: RESERVE ( ^vector<t> n -- )
   {: v:^ k:n :}
   k 0 >=  k v VECTOR-ROOM @ <=  and if exit then
   k v VECTOR-WIDTH @ CHECK-COUNT
   v k GROW ;

\ n below 0 or past NEW's ceiling is E-VEC-CAPACITY; the block may move when n
\ passes the room it holds.
: ENSURE ( ^vector<t> n -- )
   {: v:^ k:n :}
   k 0 >=  k v VECTOR-COUNT @ <=  and if exit then
   k v VECTOR-WIDTH @ CHECK-COUNT
   k v VECTOR-ROOM @ > if v k GROW then
   k v VECTOR-COUNT ! ;

\ n below 0 is E-VEC-CAPACITY; below COUNT the block stays where it is.
: RESIZE ( ^vector<t> n -- )
   {: v:^ k:n :}
   k 0 < if E-VEC-CAPACITY throw then
   k v VECTOR-COUNT @ < if v k CUT exit then
   v k ENSURE ;

\ Answers the new last element, its bytes zero, growing as ENSURE does.
: PUSH ( ^vector<t> -- ^t )
   {: v:^ :}
   v VECTOR-COUNT @ {: i:n :}
   v i 1 + ENSURE
   v i AT ;

\ A move of the block and DELETE end the span, as they end an AT address.
: SPAN ( ^vector<t> -- SPAN:span<t> )
   {: v:^ :}
   v VECTOR-DATA @ >ELEM  v VECTOR-COUNT @ v VECTOR-WIDTH @ *  SPAN:MAKE ;

\ COUNT grown by n, answering the span of the n new elements, which are
\ zero; the block may move, as ENSURE's does.
: EXTEND ( ^vector<t> n -- SPAN:span<t> )
   {: v:^ k:n :}
   v VECTOR-COUNT @ {: old:n :}
   v old k + ENSURE
   v SPAN old v VECTOR-WIDTH @ * SPAN:SKIP ;

\ The elements the block holds.
: CAP ( ^vector<t> -- n ) VECTOR-ROOM @ ;

\ The block holds exactly n elements, or COUNT when that is more; the block may
\ move.
: TRIM ( ^vector<t> n -- )
   {: v:^ k:n :}
   v VECTOR-WIDTH @ {: w:n :}
   k w CHECK-COUNT
   k v VECTOR-COUNT @ max {: r:n :}
   v VECTOR-ALLOCATOR @  v VECTOR-DATA @  v VECTOR-ROOM @ w *  r w *
   ALLOC:RESIZE drop  v VECTOR-DATA !
   r v VECTOR-ROOM ! ;

\ Reserves room for n elements and answers where the bytes at the address lie
\ afterwards: an address in the old block, live elements or not, keeps its offset.
: MAKE-ROOM ( ^vector<t> n ^u8 -- ^u8 )
   {: v:^ k:n s:^ :}
   s v VECTOR-DATA @ - {: off:n :}
   v VECTOR-ROOM @ v VECTOR-WIDTH @ * {: held:n :}
   v k RESERVE
   off 0 >=  off held <  and if v VECTOR-DATA @ off + exit then
   s ;

\ n elements read at the address, appended; a negative n is E-VEC-CAPACITY.
\ They may be the vector's own elements, though growth moves the block.
: APPEND ( ^t n ^vector<t> -- )
   {: src:^ k:n v:^ :}
   k 0 < if E-VEC-CAPACITY throw then
   v VECTOR-COUNT @ {: old:n :}
   v VECTOR-WIDTH @ {: w:n :}
   v old k + src BYTE-VIEW MAKE-ROOM {: s:^ :}
   s  v VECTOR-DATA @ old w * +  k w * BYTE-COPY
   old k + v VECTOR-COUNT ! ;

seal-package
;package
