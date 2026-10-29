\ vector.f - package VEC: checked growable cell vectors over NUM roles. A
\ caller-owned header of HEADER-CELLS cells (data, length, capacity); the
\ capacity cell is the liveness token, a dead header reads length and capacity
\ zero. Lengths are NUM:item-count and positions NUM:index, so a swap is a
\ checker reject. Bad capacity is E-VEC-CAPACITY, a bad index E-VEC-BOUNDS,
\ growing a dead header E-VEC-STATE. STORAGE CLASS. CALLER-OWNED.

require src/lib/errors.f
require src/lib/memory.f

\ The private words keep the header cells and address arithmetic on bare `count`,
\ `len` and `idx` roles; the public surface converts to NUM roles at its boundary
\ through the two checked casts ITEM-COUNT>N and INDEX>N. Every allocation goes
\ through CAP-ALLOC, so a zero or overflowing capacity is refused before mmap.

package VEC
private

BEGIN-STRUCTURE VEC-HEADER-BYTES
   PTR-FIELD: VEC.DATA
   CELL +FIELD VEC.LEN
   CELL +FIELD VEC.CAP
END-STRUCTURE

2 constant VEC-GROWTH
MEM-MAX-CELLS constant VEC-MAX-CELLS

: VEC-COUNT ( n -- count )
   dup 0 < if E-VEC-CAPACITY throw then
   dup VEC-MAX-CELLS > if E-VEC-CAPACITY throw then
   >COUNT ;

: VEC-LEN ( n -- len )
   dup 0 < if E-VEC-BOUNDS throw then
   dup VEC-MAX-CELLS > if E-VEC-CAPACITY throw then
   >LEN ;

: VEC-IDX ( n -- idx )
   dup 0 < if E-VEC-BOUNDS throw then
   >IDX ;

: VEC-CHECK-NEED ( count -- ) {: need :}
   need COUNT>N 0 < if E-VEC-CAPACITY throw then
   need COUNT>N VEC-MAX-CELLS > if E-VEC-CAPACITY throw then ;

: VEC-CHECK-CAP ( count -- )
   dup VEC-CHECK-NEED
   COUNT>N 0= if E-VEC-CAPACITY throw then ;

: VEC-CHECK-LEN ( len -- ) {: len :}
   len LEN>N 0 < if E-VEC-BOUNDS throw then
   len LEN>N VEC-MAX-CELLS > if E-VEC-CAPACITY throw then ;

: VEC-CELL-FIELD ( ptr a n -- ptr a ) {: base:ptr off :}
   off 0 < if E-VEC-BOUNDS throw then
   base off cells + ;

: VEC-DATA-FIELD ( ptr a -- ptr ptr a )
   VEC.DATA ;

: VEC-DATA@ ( ptr a -- ptr a )
   VEC-DATA-FIELD @ ;

: VEC-DATA! ( ptr a ptr a -- ) {: data:ptr vec:ptr :}
   data vec VEC-DATA-FIELD ! ;

: VEC-LEN-FIELD ( ptr a -- ptr n )
   VEC.LEN BYTE-VIEW CELL-VIEW ;

: VEC-LEN@ ( ptr a -- len )
   VEC-LEN-FIELD @ VEC-LEN ;

: VEC-CAP-FIELD ( ptr a -- ptr n )
   VEC.CAP BYTE-VIEW CELL-VIEW ;

: VEC-CAP@ ( ptr a -- count )
   VEC-CAP-FIELD @ VEC-COUNT ;

: VEC-CAP! ( count ptr a -- ) {: cap vec:ptr :}
   cap VEC-CHECK-CAP
   cap COUNT>N vec VEC-CAP-FIELD ! ;

\ A positive VEC.CAP means the header owns a mapping of that many cells at
\ VEC.DATA; zero means none (fresh or disposed). Touching a dead vector throws
\ E-VEC-STATE instead of dereferencing freed storage. Reads the raw cell:
\ VEC-CAP@ would refuse a zero.
: VEC-CHECK-LIVE ( ptr a -- ) {: vec:ptr :}
   vec VEC-CAP-FIELD @ 0= if E-VEC-STATE throw then ;

\ Initializing over a live header would orphan its mapping: callers dispose an
\ owned vector first.
: VEC-CHECK-DEAD ( ptr a -- ) {: vec:ptr :}
   vec VEC-CAP-FIELD @ 0 <> if E-VEC-STATE throw then ;

: VEC-LEN! ( len ptr a -- ) {: len vec:ptr :}
   len VEC-CHECK-LEN
   len LEN>N vec VEC-CAP@ COUNT>N > if E-VEC-BOUNDS throw then
   len LEN>N vec VEC-LEN-FIELD ! ;

: VEC-CHECK-INDEX ( ptr a idx -- ) {: vec:ptr ix :}
   vec VEC-CHECK-LIVE
   ix IDX>N 0 < if E-VEC-BOUNDS throw then
   ix IDX>N vec VEC-LEN@ LEN>N >= if E-VEC-BOUNDS throw then ;

: VEC@ ( ptr a idx -- a ) {: vec:ptr ix :}
   vec ix VEC-CHECK-INDEX
   vec VEC-DATA@ ix IDX>N VEC-CELL-FIELD @ ;

: VEC! ( a ptr a idx -- ) {: value vec:ptr ix :}
   vec ix VEC-CHECK-INDEX
   value vec VEC-DATA@ ix IDX>N VEC-CELL-FIELD ! ;

: VEC-COPY-CELLS ( ptr a ptr a len -- ) {: src:ptr dst:ptr len :}
   len VEC-CHECK-LEN
   len LEN>N 0 ?do
      src i VEC-CELL-FIELD @ dst i VEC-CELL-FIELD !
   loop ;

\ The capacity is the mapping's cell length, so MEM-CELLS>BYTES gives the exact
\ byte extent the allocation minted.
: VEC-RELEASE-STORAGE ( ptr a count -- ) {: data:ptr cap :}
   data BYTE-VIEW  cap MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;

\ The release is last and the caller allocates before this runs, so a failed
\ grow leaves the old storage owned and intact.
: VEC-INSTALL-RESIZE ( ptr a count ptr a -- ) {: vec:ptr cap data:ptr :}
   vec VEC-DATA@ {: old:ptr :}
   vec VEC-CAP@ {: oldcap :}
   old data vec VEC-LEN@ VEC-COPY-CELLS
   data vec VEC-DATA!
   cap vec VEC-CAP!
   old oldcap VEC-RELEASE-STORAGE ;

: VEC-CHECK-RESIZE-CAP ( ptr a count -- ) {: vec:ptr cap :}
   vec VEC-CHECK-LIVE
   cap VEC-CHECK-CAP
   cap COUNT>N vec VEC-LEN@ LEN>N < if E-VEC-BOUNDS throw then ;

: VEC-GROW-CAP ( ptr a count -- count ) {: vec:ptr need :}
   vec VEC-CHECK-LIVE
   need VEC-CHECK-NEED
   vec VEC-CAP@ COUNT>N
   begin dup need COUNT>N < while
      dup VEC-MAX-CELLS VEC-GROWTH / > if
         drop need COUNT>N
      else
         VEC-GROWTH *
      then
   repeat VEC-COUNT ;

CAST: ITEM-COUNT>N ( NUM:item-count -- n )
CAST: INDEX>N ( NUM:index -- n )

\ The derived cell is provably nonnegative, so the refusal arms are unreachable
\ (E-VEC-BOUNDS), as in MEM's exhaustive E-MEM-TOTALITY match.
: OK-CELL-COUNT ( NUM:numeric-result<NUM:cell-count> -- NUM:cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: OK-ALLOC-CELL-COUNT
      ( NUM:numeric-result<NUM:alloc-cell-count> -- NUM:alloc-cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;

\ One cell per item. MEM:CELLS-ALLOC-COUNT has this shape but throws E-MEM-SIZE;
\ the vector's refusal is E-VEC-CAPACITY, so the two narrowing steps are here.
: CAP-ALLOC ( NUM:item-count -- NUM:alloc-cell-count )
   ITEM-COUNT>N NUM:CELL-COUNT OK-CELL-COUNT
   NUM:AS-ALLOC-CELL-COUNT OK-ALLOC-CELL-COUNT ;

: ITEM>RC  ( NUM:item-count -- count )  ITEM-COUNT>N >COUNT ;
: IX>RI    ( NUM:index -- idx )         INDEX>N >IDX ;

public

VEC-HEADER-BYTES CELL / constant HEADER-CELLS   \ cells a caller allots for one header

\ A length or capacity as an item count; a negative n throws E-VEC-CAPACITY.
: N>ITEM ( n -- NUM:item-count )
   NUM:ITEM-COUNT
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
\ An element position as an index; a negative n throws E-VEC-BOUNDS.
: N>INDEX ( n -- NUM:index )
   NUM:INDEX
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: INIT ( ptr a NUM:item-count -- ) {: vec:ptr cap:NUM:item-count :}
   vec VEC-CHECK-DEAD
   cap CAP-ALLOC MEM:ALLOC-CELLS vec VEC-DATA!
   cap ITEM>RC vec VEC-CAP!
   0 VEC-LEN vec VEC-LEN! ;
: CLEAR ( ptr a -- )
   0 VEC-LEN swap VEC-LEN! ;

\ Clear the entire header before release, so retries cannot free the mapping
\ twice and a captured dead header cannot retain its former process address.
: DISPOSE ( ptr a -- ) {: vec:ptr :}
   vec VEC-CAP-FIELD @ {: cap :}
   vec VEC-DATA@ {: data:ptr :}
   NULL-PTR vec VEC-DATA!
   0 vec VEC-CAP-FIELD !
   0 vec VEC-LEN-FIELD !
   cap 0= if exit then
   data cap >COUNT VEC-RELEASE-STORAGE ;

: LEN@ ( ptr a -- NUM:item-count )  VEC-LEN@ LEN>N N>ITEM ;
: CAP@ ( ptr a -- NUM:item-count )  VEC-CAP@ COUNT>N N>ITEM ;
\ Any length from zero through the capacity; above it throws E-VEC-BOUNDS.
: LEN! ( NUM:item-count ptr a -- )  swap ITEM-COUNT>N VEC-LEN swap VEC-LEN! ;

\ Borrowed base of the contiguous cells: growth (PUSH, ENSURE), RESIZE and DISPOSE invalidate it.
: DATA@ ( ptr a -- ptr a )  VEC-DATA@ ;

: RESIZE ( ptr a NUM:item-count -- ) {: vec:ptr cap:NUM:item-count :}
   cap ITEM>RC {: rc:count :}
   vec rc VEC-CHECK-RESIZE-CAP
   vec rc  cap CAP-ALLOC MEM:ALLOC-CELLS  VEC-INSTALL-RESIZE ;
: ENSURE ( ptr a NUM:item-count -- ) {: vec:ptr need:NUM:item-count :}
   need ITEM-COUNT>N {: nn:n :}
   nn >COUNT VEC-CHECK-NEED
   nn vec VEC-CAP@ COUNT>N <= if exit then
   vec  vec nn >COUNT VEC-GROW-CAP COUNT>N N>ITEM  RESIZE ;

\ Answers the index the value landed at; the capacity doubles when the length
\ reaches it.
: PUSH ( a ptr a -- NUM:index ) {: value:a vec:ptr :}
   vec VEC-LEN@ LEN>N {: old:n :}
   vec old 1 + N>ITEM ENSURE
   value vec VEC-DATA@ old VEC-CELL-FIELD !
   old 1 + VEC-LEN vec VEC-LEN!
   old N>INDEX ;

\ The quotation's effect [ R NUM:index a -- R ] is one a local annotation cannot
\ express.
: EACH ( R ptr a [ R NUM:index a -- R ] -- R ) {: vec:ptr q :}
   vec VEC-LEN@ LEN>N 0 ?do
      i N>INDEX  vec i VEC-IDX VEC@  q execute
   loop ;

\ Defined last: `@`/`!` name the package fetch/store VEC:@ / VEC:!, which would
\ shadow the core cell primitives inside later VEC bodies.
: @ ( ptr a NUM:index -- a ) {: vec:ptr ix:NUM:index :}
   vec ix IX>RI VEC@ ;
: ! ( a ptr a NUM:index -- ) {: value:a vec:ptr ix:NUM:index :}
   value vec ix IX>RI VEC! ;

;package
