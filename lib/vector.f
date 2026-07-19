\ vector.f - checked growable cell-vector helpers.
\
require lib/errors.f
require lib/memory.f

BEGIN-STRUCTURE VEC-HEADER-BYTES
   PTR-FIELD: VEC.DATA
   CELL +FIELD VEC.LEN
   CELL +FIELD VEC.CAP
END-STRUCTURE

VEC-HEADER-BYTES CELL / constant VEC-HEADER-CELLS

2 constant VEC-GROWTH
MEM-MAX-CELLS constant VEC-MAX-CELLS

: VEC-COUNT ( n -- count )
   dup 0 < if E-VEC-CAPACITY throw then
   dup VEC-MAX-CELLS > if E-VEC-CAPACITY throw then
   >COUNT ;

: VEC-CAP-COUNT ( n -- count )
   dup 0= if E-VEC-CAPACITY throw then
   VEC-COUNT ;

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

: VEC-CELLS>BYTES ( count -- n )
   dup VEC-CHECK-CAP
   MEM-CELLS>BYTES ;

: VEC-ALLOC-CELLS ( count -- ptr a )
   dup VEC-CHECK-CAP
   MEM-ALLOC-CELLS ;

: VEC-CELL-FIELD ( ptr a n -- ptr a ) {: base:ptr off :}
   off 0 < if E-VEC-BOUNDS throw then
   base off cells + ;

: VEC-DATA-FIELD ( ptr a -- ptr ptr a )
   VEC.DATA ;

: VEC-DATA@ ( ptr a -- ptr a )
   VEC-DATA-FIELD @ ;

: VEC-DATA! ( ptr a ptr a -- ) {: data:ptr vec:ptr :}
   data vec VEC-DATA-FIELD ! ;

: VEC-LEN-FIELD ( ptr a -- ptr a )
   VEC.LEN ;

: VEC-LEN@ ( ptr a -- len )
   VEC-LEN-FIELD @ VEC-LEN ;

: VEC-CAP-FIELD ( ptr a -- ptr a )
   VEC.CAP ;

: VEC-CAP@ ( ptr a -- count )
   VEC-CAP-FIELD @ VEC-COUNT ;

: VEC-CAP! ( count ptr a -- ) {: cap vec:ptr :}
   cap VEC-CHECK-CAP
   cap COUNT>N vec VEC-CAP-FIELD ! ;

: VEC-LEN! ( len ptr a -- ) {: len vec:ptr :}
   len VEC-CHECK-LEN
   len LEN>N vec VEC-CAP@ COUNT>N > if E-VEC-BOUNDS throw then
   len LEN>N vec VEC-LEN-FIELD ! ;

: VEC-INIT ( ptr a count -- ) {: vec:ptr cap :}
   cap VEC-ALLOC-CELLS vec VEC-DATA!
   cap vec VEC-CAP!
   0 VEC-LEN vec VEC-LEN! ;

: VEC-CLEAR ( ptr a -- )
   0 VEC-LEN swap VEC-LEN! ;

: VEC-CHECK-INDEX ( ptr a idx -- ) {: vec:ptr ix :}
   ix IDX>N 0 < if E-VEC-BOUNDS throw then
   ix IDX>N vec VEC-LEN@ LEN>N >= if E-VEC-BOUNDS throw then ;

: VEC@ ( ptr a idx -- a ) {: vec:ptr ix :}
   vec ix VEC-CHECK-INDEX
   vec VEC-DATA@ ix IDX>N VEC-CELL-FIELD @ ;

: VEC! ( a ptr a idx -- ) {: value vec:ptr ix :}
   vec ix VEC-CHECK-INDEX
   value vec VEC-DATA@ ix IDX>N VEC-CELL-FIELD ! ;

: VEC-N@ ( ptr a idx -- n )
   VEC@ ;

: VEC-N! ( n ptr a idx -- )
   VEC! ;

: VEC-A@ ( ptr a idx -- ptr u8 )
   VEC@ ;

: VEC-A! ( ptr u8 ptr a idx -- )
   VEC! ;

: VEC-COPY-CELLS ( ptr a ptr a len -- ) {: src:ptr dst:ptr len :}
   len VEC-CHECK-LEN
   len LEN>N 0 ?do
      src i VEC-CELL-FIELD @ dst i VEC-CELL-FIELD !
   loop ;

: VEC-INSTALL-RESIZE ( ptr a count ptr a -- ) {: vec:ptr cap data:ptr :}
   vec VEC-DATA@ data vec VEC-LEN@ VEC-COPY-CELLS
   data vec VEC-DATA!
   cap vec VEC-CAP! ;

: VEC-CHECK-RESIZE-CAP ( ptr a count -- ) {: vec:ptr cap :}
   cap VEC-CHECK-CAP
   cap COUNT>N vec VEC-LEN@ LEN>N < if E-VEC-BOUNDS throw then ;

: VEC-RESIZE ( ptr a count -- ) {: vec:ptr cap :}
   vec cap VEC-CHECK-RESIZE-CAP
   vec cap cap VEC-ALLOC-CELLS VEC-INSTALL-RESIZE ;

: VEC-GROW-CAP ( ptr a count -- count ) {: vec:ptr need :}
   need VEC-CHECK-NEED
   vec VEC-CAP@ COUNT>N
   begin dup need COUNT>N < while
      dup VEC-MAX-CELLS VEC-GROWTH / > if
         drop need COUNT>N
      else
         VEC-GROWTH *
      then
   repeat VEC-COUNT ;

: VEC-ENSURE ( ptr a count -- ) {: vec:ptr need :}
   need VEC-CHECK-NEED
   need COUNT>N vec VEC-CAP@ COUNT>N <= if exit then
   vec vec need VEC-GROW-CAP VEC-RESIZE ;

: VEC-PUSH-AT ( a ptr a n -- idx ) {: value vec:ptr old :}
   old VEC-IDX drop
   vec old 1 + VEC-COUNT VEC-ENSURE
   value vec VEC-DATA@ old VEC-CELL-FIELD !
   old 1 + VEC-LEN vec VEC-LEN!
   old VEC-IDX ;

: VEC-PUSH ( a ptr a -- idx ) {: value vec:ptr :}
   value vec vec VEC-LEN@ LEN>N VEC-PUSH-AT ;

: VEC-PUSH-N ( n ptr a -- idx )
   VEC-PUSH ;

: VEC-PUSH-A ( ptr u8 ptr a -- idx )
   VEC-PUSH ;

: VEC-EACH ( R ptr a [ R idx a -- R ] -- R ) {: vec:ptr q :}
   vec VEC-LEN@ LEN>N 0 ?do
      i VEC-IDX vec i VEC-IDX VEC@ q execute
   loop ;

\ ---- B5.5 typed VEC surface over CAD-NUM roles (MODEL-CAD-V2-PLAN.md B5.5) -----
\
\ The raw VEC-* words above conflate item counts, capacities, and indices on one
\ interchangeable `n` and allocate through the raw MEM-ALLOC-CELLS sink. Package
\ VEC re-states the same growable cell vector over CAD-NUM roles: a length or a
\ capacity is a `CAD-NUM:item-count`, an element position is a `CAD-NUM:index`, and
\ the ONLY allocation path is the private one-cell-per-item adapter, which produces
\ a `cell-count` (one cell per item) then an `alloc-cell-count` and calls the
\ packaged MEM:ALLOC-CELLS - so a count/index swap is a checker reject and a zero or
\ overflowing capacity is refused at VALIDATION before mmap. Positive behavior is
\ byte-identical to the raw words (same storage, same E-VEC-CAPACITY/E-VEC-BOUNDS
\ throws); the raw words stay for their not-yet-migrated callers.
\
\ VEC owns exactly two audited representation projections (ITEM-COUNT>N, INDEX>N),
\ mirroring MEM's ALLOC-CELLS>N: the raw vector header cell and the cell-address
\ arithmetic still consume a bare `n`, so the typed boundary reads a validated
\ role's cell here. Private, no public export, confined to lib/vector.f like the MEM
\ pair; retire when TVK-RAW (habu-nominal-storage-raw-a3430ef2) lets the header and
\ address primitives take the nominal role directly.

package VEC
private

TRUSTED: ITEM-COUNT>N ( CAD-NUM:item-count -- n ) ;
TRUSTED: INDEX>N ( CAD-NUM:index -- n ) ;

\ ---- ok-extractors: the read/derived cell is provably nonnegative, so the -------
\ refusal arms are unreachable invariants (E-VEC-BOUNDS; mirrors MEM's
\ E-MEM-TOTALITY discipline of an exhaustive MATCH over an impossible result).
: OK-ITEM-COUNT ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- CAD-NUM:item-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: OK-CELL-COUNT ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- CAD-NUM:cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: OK-INDEX ( CAD-NUM:numeric-result<CAD-NUM:index> -- CAD-NUM:index )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- capacity narrowing: the plan's named owner boundary. A zero-admitting ------
\ cell-count becomes the positive alloc role, or maps zero/overflow explicitly to
\ the existing named vector-capacity throw (byte-identical to raw VEC-CHECK-CAP).
: OK-ALLOC-CELL-COUNT
      ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- CAD-NUM:alloc-cell-count )
   MATCH CAD-NUM:numeric-result
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
: CAP-ALLOC ( CAD-NUM:item-count -- CAD-NUM:alloc-cell-count )
   ITEM-COUNT>N CAD-NUM:CELL-COUNT OK-CELL-COUNT      \ N items -> N cells (1 cell/item)
   CAD-NUM:AS-ALLOC-CELL-COUNT OK-ALLOC-CELL-COUNT ;  \ positive alloc role or E-VEC-CAPACITY

\ ---- role bridges between CAD-NUM roles, raw cells, and the legacy helpers ------
: N>ITEM   ( n -- CAD-NUM:item-count )  CAD-NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX  ( n -- CAD-NUM:index )       CAD-NUM:INDEX OK-INDEX ;
: ITEM>RC  ( CAD-NUM:item-count -- count )  ITEM-COUNT>N >COUNT ;   \ role -> legacy count
: IX>RI    ( CAD-NUM:index -- idx )         INDEX>N >IDX ;          \ role -> legacy index

public

\ ---- init / clear -------------------------------------------------------------
: INIT ( ptr a CAD-NUM:item-count -- ) {: vec:ptr cap:CAD-NUM:item-count :}
   cap CAP-ALLOC MEM:ALLOC-CELLS vec VEC-DATA!   \ typed alloc; 0/overflow -> E-VEC-CAPACITY
   cap ITEM>RC vec VEC-CAP!
   0 VEC-LEN vec VEC-LEN! ;
: CLEAR ( ptr a -- )  VEC-CLEAR ;

\ ---- length / capacity readers (raw cell -> item-count role) ------------------
: LEN@ ( ptr a -- CAD-NUM:item-count )  VEC-LEN@ LEN>N N>ITEM ;
: CAP@ ( ptr a -- CAD-NUM:item-count )  VEC-CAP@ COUNT>N N>ITEM ;

\ ---- resize / ensure (every allocation flows through the typed adapter) -------
: RESIZE ( ptr a CAD-NUM:item-count -- ) {: vec:ptr cap:CAD-NUM:item-count :}
   cap ITEM>RC {: rc:count :}
   vec rc VEC-CHECK-RESIZE-CAP                     \ reject 0 / cap < active length
   vec rc  cap CAP-ALLOC MEM:ALLOC-CELLS  VEC-INSTALL-RESIZE ;
: ENSURE ( ptr a CAD-NUM:item-count -- ) {: vec:ptr need:CAD-NUM:item-count :}
   need ITEM-COUNT>N {: nn:n :}
   nn >COUNT VEC-CHECK-NEED                        \ reject negative / above max
   nn vec VEC-CAP@ COUNT>N <= if exit then         \ no-op ensure (need <= capacity)
   vec  vec nn >COUNT VEC-GROW-CAP COUNT>N N>ITEM  RESIZE ;

\ ---- append (returns the index the value landed at) ---------------------------
: PUSH ( a ptr a -- CAD-NUM:index ) {: value:a vec:ptr :}
   value vec VEC-PUSH IDX>N N>INDEX ;

\ ---- iterate active cells, index-first (index lifted to the role) -------------
\ typed-local-lint: allow-bare-local - q preserves the row-polymorphic quotation
\ effect [ R CAD-NUM:index a -- R ], which a local annotation cannot express.
: EACH ( R ptr a [ R CAD-NUM:index a -- R ] -- R ) {: vec:ptr q :}
   vec VEC-LEN@ LEN>N 0 ?do
      i N>INDEX  vec i VEC-IDX VEC@  q execute
   loop ;

\ ---- element access (index role -> checked raw cell) --------------------------
\ Defined last: `@`/`!` name the package fetch/store VEC:@ / VEC:!, which would
\ shadow the core cell primitives inside later VEC bodies.
: @ ( ptr a CAD-NUM:index -- a ) {: vec:ptr ix:CAD-NUM:index :}
   vec ix IX>RI VEC@ ;
: ! ( a ptr a CAD-NUM:index -- ) {: value:a vec:ptr ix:CAD-NUM:index :}
   value vec ix IX>RI VEC! ;

;package
