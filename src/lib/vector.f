\ vector.f - package VEC: a typed vector<t> that takes its elements from an
\ allocator it keeps and grows through it. COUNT elements are live; growth
\ doubles the room the block holds, so n pushes make about log2 n resize calls.
\ A bad count is E-VEC-CAPACITY, an index outside the elements E-VEC-BOUNDS.
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
   FIELD allocator ptr ALLOC:allocator
   FIELD data ptr u8
   FIELD count n
   FIELD room n
   FIELD width n
;STRUCTURE

private

CAST: >ELEM ( ptr u8 -- ptr t )
CAST: >VECTOR ( ptr u8 -- ptr vector<t> )
CAST: VECTOR> ( ptr vector<t> -- ptr u8 )

\ The most elements of n bytes that VEC-MAX-CELLS cells hold.
: MOST ( n -- n ) VEC-MAX-CELLS CELL * swap 1 max / ;

: CHECK-COUNT ( n n -- )
   {: k:n w:n :}
   k 0 <  k w MOST >  or if E-VEC-CAPACITY throw then ;

\ Twice the room, or k when that is more or twice the room passes the ceiling.
: ROOM-FOR ( ptr vector<t> n -- n )
   {: v:ptr k:n :}
   v VECTOR-ROOM @ {: r:n :}
   r  v VECTOR-WIDTH @ MOST 2 /  > if k exit then
   k  r 2 *  max ;

: GROW ( ptr vector<t> n -- )
   {: v:ptr k:n :}
   v k ROOM-FOR {: r:n :}
   v VECTOR-WIDTH @ {: w:n :}
   v VECTOR-ALLOCATOR @  v VECTOR-DATA @  v VECTOR-ROOM @ w *  r w *
   ALLOC:RESIZE drop  v VECTOR-DATA !
   r v VECTOR-ROOM ! ;

: CUT ( ptr vector<t> n -- )
   {: v:ptr k:n :}
   v VECTOR-WIDTH @ {: w:n :}
   v VECTOR-DATA @ k w * +  v VECTOR-COUNT @ k - w *  SPAN:MAKE {: s :}
   0 s SPAN:FILL
   k v VECTOR-COUNT ! ;

public

\ k zeroed elements, or E-VEC-CAPACITY for a negative k or more bytes than
\ VEC-MAX-CELLS cells; a refused data request frees the header, then rethrows.
: NEW ( ptr ALLOC:allocator n width<t> -- ptr vector<t> )
   {: al:ptr k:n w :}
   w width>n {: wn:n :}
   k wn CHECK-COUNT
   al [type] vector<u8> width>n ALLOC:BYTES drop >VECTOR {: v:ptr :}
   al v VECTOR-ALLOCATOR !
   [: al k wn * ALLOC:BYTES drop v VECTOR-DATA ! ;] catch {: code:n :}
   code 0<> if al v VECTOR> ALLOC:FREE code throw then
   k v VECTOR-COUNT !  k v VECTOR-ROOM !  wn v VECTOR-WIDTH !
   v ;

: COUNT ( ptr vector<t> -- n ) VECTOR-COUNT @ ;

\ An index outside 0 to COUNT - 1 is E-VEC-BOUNDS.
: AT ( ptr vector<t> n -- ptr t )
   {: v:ptr i:n :}
   i 0 <  i v VECTOR-COUNT @ >=  or if E-VEC-BOUNDS throw then
   v VECTOR-DATA @  i v VECTOR-WIDTH @ * +  >ELEM ;

: DELETE ( ptr vector<t> -- )
   {: v:ptr :}
   v NULL-PTR = if exit then
   v VECTOR-ALLOCATOR @ {: al:ptr :}
   al v VECTOR-DATA @ ALLOC:FREE
   al v VECTOR> ALLOC:FREE ;

\ Room for n elements, COUNT kept; the block may move. Until COUNT passes n,
\ ENSURE, RESIZE and PUSH neither allocate nor move the block.
: RESERVE ( ptr vector<t> n -- )
   {: v:ptr k:n :}
   k v VECTOR-WIDTH @ CHECK-COUNT
   k v VECTOR-ROOM @ > if v k GROW then ;

\ n below 0 or past NEW's ceiling is E-VEC-CAPACITY; the block may move when n
\ passes the room it holds.
: ENSURE ( ptr vector<t> n -- )
   {: v:ptr k:n :}
   k v VECTOR-WIDTH @ CHECK-COUNT
   k v VECTOR-COUNT @ <= if exit then
   k v VECTOR-ROOM @ > if v k GROW then
   k v VECTOR-COUNT ! ;

\ n below 0 is E-VEC-CAPACITY; below COUNT the block stays where it is.
: RESIZE ( ptr vector<t> n -- )
   {: v:ptr k:n :}
   k 0 < if E-VEC-CAPACITY throw then
   k v VECTOR-COUNT @ < if v k CUT exit then
   v k ENSURE ;

\ Answers the new last element, its bytes zero, growing as ENSURE does.
: PUSH ( ptr vector<t> -- ptr t )
   {: v:ptr :}
   v VECTOR-COUNT @ {: i:n :}
   v i 1 + ENSURE
   v i AT ;

\ Growth and DELETE end the span, as they end an AT address.
: SPAN ( ptr vector<t> -- SPAN:span<t> )
   {: v:ptr :}
   v VECTOR-DATA @ >ELEM  v VECTOR-COUNT @ v VECTOR-WIDTH @ *  SPAN:MAKE ;

seal-package
;package
