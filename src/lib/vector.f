\ vector.f - package VEC: checked growable cell vectors over NUM roles.
\
\ A vector is a caller-owned header of HEADER-CELLS cells (data pointer, active
\ length, capacity). INIT binds a dead (zero or disposed) header to a fresh
\ src/lib/memory.f mapping and DISPOSE releases it; the capacity cell is the
\ liveness token. A length or a capacity is a `NUM:item-count` and an element
\ position a `NUM:index`, so a count/index swap is a checker reject. PUSH doubles
\ the capacity when the length reaches it. Invalid or overflowing capacity
\ requests throw E-VEC-CAPACITY, invalid indexes or impossible lengths
\ E-VEC-BOUNDS, and growing or resizing a dead header E-VEC-STATE; a dead
\ header reads as length and capacity zero.
\
\ STORAGE CLASS. CALLER-OWNED: the header and its mapping belong to the caller.

require src/lib/errors.f
require src/lib/memory.f

\ ---- the header and its raw cells ----------------------------------------------
\
\ The private words below keep the header cells and the cell-address arithmetic
\ on `count`, `len` and `idx` roles over bare cells; the public surface after
\ them converts those to NUM roles at its boundary. Every allocation goes through
\ the private one-cell-per-item adapter CAP-ALLOC, which produces a `cell-count`
\ then an `alloc-cell-count` and calls MEM:ALLOC-CELLS, so a zero or overflowing
\ capacity is refused at validation before mmap.
\
\ VEC owns exactly two representation projections (ITEM-COUNT>N, INDEX>N), both
\ checked casts, mirroring MEM's ALLOC-CELLS>N: the header cells and the
\ cell-address arithmetic consume a bare `n`, so the typed boundary reads a
\ validated role's cell here. Raw storage rejects nominal roles, so these
\ projections are the explicit boundary to the numeric header and address
\ arithmetic.

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

\ Ownership / liveness token. A positive VEC.CAP means the header owns a live
\ mapping of that many cells at VEC.DATA; a zero VEC.CAP means no mapping is owned
\ (a fresh all-zero header or a disposed one). DISPOSE clears the capacity to
\ void ownership, so this single cell is the liveness marker every storage-touching
\ use consults: touching a dead vector throws E-VEC-STATE instead of dereferencing
\ freed storage. Reads the raw cell (VEC-CAP@ would refuse a zero elsewhere).
: VEC-CHECK-LIVE ( ptr a -- ) {: vec:ptr :}
   vec VEC-CAP-FIELD @ 0= if E-VEC-STATE throw then ;

\ Init-time inverse of VEC-CHECK-LIVE: a positive VEC.CAP means the header already
\ owns a live mapping, so initializing over it would overwrite VEC.DATA and orphan
\ that mapping (the one remaining leak path after resize/dispose). Fail closed -
\ callers dispose an owned vector before re-initializing it.
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

\ Return a mapping the vector owns (data pointer + its cell capacity) to the OS via
\ the typed munmap inverse: the capacity IS the mapping's cell length, so
\ MEM-CELLS>BYTES projects it to the exact byte extent the allocation minted and
\ MEM:BYTES-ALLOC-LEN narrows that to the alloc-byte-len MEM:RELEASE-BYTES demands.
: VEC-RELEASE-STORAGE ( ptr a count -- ) {: data:ptr cap :}
   data BYTE-VIEW  cap MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;

\ Copy into the new mapping, install it, then release the prior one. The release is
\ LAST and the caller allocates BEFORE this word runs, so a failed grow (the alloc
\ throws upstream) never reaches here and leaves the old storage owned and intact.
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

\ ---- ok-extractors: the read/derived cell is provably nonnegative, so the -------
\ refusal arms are unreachable invariants (E-VEC-BOUNDS; mirrors MEM's
\ E-MEM-TOTALITY discipline of an exhaustive MATCH over an impossible result).
: OK-ITEM-COUNT ( NUM:numeric-result<NUM:item-count> -- NUM:item-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: OK-CELL-COUNT ( NUM:numeric-result<NUM:cell-count> -- NUM:cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: OK-INDEX ( NUM:numeric-result<NUM:index> -- NUM:index )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- capacity narrowing: the plan's named owner boundary. A zero-admitting ------
\ cell-count becomes the positive alloc role, or maps zero/overflow explicitly to
\ the existing named vector-capacity throw (byte-identical to VEC-CHECK-CAP).
: OK-ALLOC-CELL-COUNT
      ( NUM:numeric-result<NUM:alloc-cell-count> -- NUM:alloc-cell-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;

\ ---- the private one-cell-per-item allocation adapter --------------------------
\ item-count -> cell-count (one cell per item) -> alloc-cell-count -> MEM:ALLOC-CELLS.
\ (MEM:CELLS-ALLOC-COUNT has this shape but throws E-MEM-SIZE; the plan pins the
\ vector-capacity refusal here, so the two narrowing steps are re-stated with the
\ E-VEC-CAPACITY owner boundary.)
: CAP-ALLOC ( NUM:item-count -- NUM:alloc-cell-count )
   ITEM-COUNT>N NUM:CELL-COUNT OK-CELL-COUNT      \ N items -> N cells (1 cell/item)
   NUM:AS-ALLOC-CELL-COUNT OK-ALLOC-CELL-COUNT ;  \ positive alloc role or E-VEC-CAPACITY

\ ---- role bridges between NUM roles, raw cells, and the header roles --------
: N>ITEM   ( n -- NUM:item-count )  NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX  ( n -- NUM:index )       NUM:INDEX OK-INDEX ;
: ITEM>RC  ( NUM:item-count -- count )  ITEM-COUNT>N >COUNT ;   \ role -> header count
: IX>RI    ( NUM:index -- idx )         INDEX>N >IDX ;          \ role -> header index

public

VEC-HEADER-BYTES CELL / constant HEADER-CELLS   \ cells a caller allots for one header

\ ---- init / clear -------------------------------------------------------------
: INIT ( ptr a NUM:item-count -- ) {: vec:ptr cap:NUM:item-count :}
   vec VEC-CHECK-DEAD                             \ reject re-init of a live header (no leak)
   cap CAP-ALLOC MEM:ALLOC-CELLS vec VEC-DATA!   \ typed alloc; 0/overflow -> E-VEC-CAPACITY
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

\ ---- length / capacity (raw cell <-> item-count role) -------------------------
: LEN@ ( ptr a -- NUM:item-count )  VEC-LEN@ LEN>N N>ITEM ;
: CAP@ ( ptr a -- NUM:item-count )  VEC-CAP@ COUNT>N N>ITEM ;
\ Any length from zero through the capacity; above it throws E-VEC-BOUNDS.
: LEN! ( NUM:item-count ptr a -- )  swap ITEM-COUNT>N VEC-LEN swap VEC-LEN! ;

\ Borrowed base of the contiguous cells: growth (PUSH, ENSURE), RESIZE and DISPOSE invalidate it.
: DATA@ ( ptr a -- ptr a )  VEC-DATA@ ;

\ ---- resize / ensure (every allocation flows through the typed adapter) -------
: RESIZE ( ptr a NUM:item-count -- ) {: vec:ptr cap:NUM:item-count :}
   cap ITEM>RC {: rc:count :}
   vec rc VEC-CHECK-RESIZE-CAP                     \ reject 0 / cap < active length
   vec rc  cap CAP-ALLOC MEM:ALLOC-CELLS  VEC-INSTALL-RESIZE ;
: ENSURE ( ptr a NUM:item-count -- ) {: vec:ptr need:NUM:item-count :}
   need ITEM-COUNT>N {: nn:n :}
   nn >COUNT VEC-CHECK-NEED                        \ reject negative / above max
   nn vec VEC-CAP@ COUNT>N <= if exit then         \ no-op ensure (need <= capacity)
   vec  vec nn >COUNT VEC-GROW-CAP COUNT>N N>ITEM  RESIZE ;

\ ---- append (returns the index the value landed at) ---------------------------
: PUSH ( a ptr a -- NUM:index ) {: value:a vec:ptr :}
   vec VEC-LEN@ LEN>N {: old:n :}
   vec old 1 + N>ITEM ENSURE                       \ growth through the typed adapter
   value vec VEC-DATA@ old VEC-CELL-FIELD !
   old 1 + VEC-LEN vec VEC-LEN!
   old N>INDEX ;

\ ---- iterate active cells, index-first (index lifted to the role) -------------
\ effect [ R NUM:index a -- R ], which a local annotation cannot express.
: EACH ( R ptr a [ R NUM:index a -- R ] -- R ) {: vec:ptr q :}
   vec VEC-LEN@ LEN>N 0 ?do
      i N>INDEX  vec i VEC-IDX VEC@  q execute
   loop ;

\ ---- element access (index role -> checked raw cell) --------------------------
\ Defined last: `@`/`!` name the package fetch/store VEC:@ / VEC:!, which would
\ shadow the core cell primitives inside later VEC bodies.
: @ ( ptr a NUM:index -- a ) {: vec:ptr ix:NUM:index :}
   vec ix IX>RI VEC@ ;
: ! ( a ptr a NUM:index -- ) {: value:a vec:ptr ix:NUM:index :}
   value vec ix IX>RI VEC! ;

;package
