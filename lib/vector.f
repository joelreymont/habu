\ vector.f - checked growable cell-vector helpers.
\
require lib/errors.f
require lib/memory.f

package VEC
private

BEGIN-STRUCTURE HEADER-BYTES
   PTR-FIELD: DATA-PTR
   CELL +FIELD LEN-PTR
   CELL +FIELD CAP-PTR
END-STRUCTURE

public
HEADER-BYTES CELL / constant HEADER-CELLS
private

2 constant GROWTH
MEM-MAX-CELLS constant MAX-CELLS

: RAW-COUNT ( n -- count )
   dup 0 < if E-VEC-CAPACITY throw then
   dup MAX-CELLS > if E-VEC-CAPACITY throw then
   >COUNT ;

: RAW-LEN ( n -- len )
   dup 0 < if E-VEC-BOUNDS throw then
   dup MAX-CELLS > if E-VEC-CAPACITY throw then
   >LEN ;

: RAW-IDX ( n -- idx )
   dup 0 < if E-VEC-BOUNDS throw then
   >IDX ;

: CHECK-NEED ( count -- ) {: need:count :}
   need COUNT>N 0 < if E-VEC-CAPACITY throw then
   need COUNT>N MAX-CELLS > if E-VEC-CAPACITY throw then ;

: CHECK-CAP ( count -- )
   dup CHECK-NEED
   COUNT>N 0= if E-VEC-CAPACITY throw then ;

: CHECK-LEN ( len -- ) {: len:len :}
   len LEN>N 0 < if E-VEC-BOUNDS throw then
   len LEN>N MAX-CELLS > if E-VEC-CAPACITY throw then ;

: CELL-PTR ( ptr a n -- ptr a ) {: base:ptr off:n :}
   off 0 < if E-VEC-BOUNDS throw then
   base off cells + ;

: RAW-DATA@ ( ptr h -- ptr a )
   DATA-PTR @ ;

: RAW-DATA! ( ptr a ptr h -- ) {: data:ptr vec:ptr :}
   data vec DATA-PTR ! ;

: RAW-LEN@ ( ptr h -- len )
   LEN-PTR @ RAW-LEN ;

: RAW-CAP@ ( ptr h -- count )
   CAP-PTR @ RAW-COUNT ;

: RAW-CAP! ( count ptr h -- ) {: cap:count vec:ptr :}
   cap CHECK-CAP
   cap COUNT>N vec CAP-PTR ! ;

\ Capacity is the ownership token: zero means fresh or disposed.
: CHECK-LIVE ( ptr h -- ) {: vec:ptr :}
   vec CAP-PTR @ 0= if E-VEC-STATE throw then ;

: CHECK-DEAD ( ptr h -- ) {: vec:ptr :}
   vec CAP-PTR @ 0 <> if E-VEC-STATE throw then ;

: RAW-LEN! ( len ptr h -- ) {: len:len vec:ptr :}
   len CHECK-LEN
   len LEN>N vec RAW-CAP@ COUNT>N > if E-VEC-BOUNDS throw then
   len LEN>N vec LEN-PTR ! ;

: CHECK-INDEX ( ptr h idx -- ) {: vec:ptr ix:idx :}
   vec CHECK-LIVE
   ix IDX>N 0 < if E-VEC-BOUNDS throw then
   ix IDX>N vec RAW-LEN@ LEN>N >= if E-VEC-BOUNDS throw then ;

: RAW@ ( ptr h idx -- a ) {: vec:ptr ix:idx :}
   vec ix CHECK-INDEX
   vec RAW-DATA@ ix IDX>N CELL-PTR @ ;

: RAW! ( a ptr h idx -- ) {: value:a vec:ptr ix:idx :}
   vec ix CHECK-INDEX
   value vec RAW-DATA@ ix IDX>N CELL-PTR ! ;

: COPY-CELLS ( ptr a ptr a len -- ) {: src:ptr dst:ptr len:len :}
   len CHECK-LEN
   len LEN>N 0 ?do
      src i CELL-PTR @ dst i CELL-PTR !
   loop ;

\ Release the exact cell extent allocated for the backing store.
: RELEASE-STORAGE ( ptr a count -- ) {: data:ptr cap:count :}
   data  cap MEM-CELLS>BYTES MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;

\ Allocation precedes this word, so failed growth leaves the old store intact.
: INSTALL-RESIZE ( ptr h count ptr a -- ) {: vec:ptr cap:count data:ptr :}
   vec RAW-DATA@ {: old:ptr :}
   vec RAW-CAP@ {: oldcap:count :}
   old data vec RAW-LEN@ COPY-CELLS
   data vec RAW-DATA!
   cap vec RAW-CAP!
   old oldcap RELEASE-STORAGE ;

: CHECK-RESIZE ( ptr h count -- ) {: vec:ptr cap:count :}
   vec CHECK-LIVE
   cap CHECK-CAP
   cap COUNT>N vec RAW-LEN@ LEN>N < if E-VEC-BOUNDS throw then ;

: GROW-CAP ( ptr h count -- count ) {: vec:ptr need:count :}
   vec CHECK-LIVE
   need CHECK-NEED
   vec RAW-CAP@ COUNT>N
   begin dup need COUNT>N < while
      dup MAX-CELLS GROWTH / > if
         drop need COUNT>N
      else
         GROWTH *
      then
   repeat RAW-COUNT ;

\ Private proof erasure; VEC owns the checked role boundary.
TRUSTED: ITEM-COUNT>N ( CAD-NUM:item-count -- n ) ;
TRUSTED: INDEX>N ( CAD-NUM:index -- n ) ;

\ Validated internal values make refusal arms unreachable.
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

: OK-ALLOC-CELL-COUNT
      ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- CAD-NUM:alloc-cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;

\ One cell per item; VEC owns E-VEC-CAPACITY at this boundary.
: CAP-ALLOC ( CAD-NUM:item-count -- CAD-NUM:alloc-cell-count )
   ITEM-COUNT>N CAD-NUM:CELL-COUNT OK-CELL-COUNT
   CAD-NUM:AS-ALLOC-CELL-COUNT OK-ALLOC-CELL-COUNT ;

: N>ITEM   ( n -- CAD-NUM:item-count )  CAD-NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX  ( n -- CAD-NUM:index )       CAD-NUM:INDEX OK-INDEX ;
: ITEM>RC  ( CAD-NUM:item-count -- count )  ITEM-COUNT>N >COUNT ;
: IX>RI    ( CAD-NUM:index -- idx )         INDEX>N >IDX ;

public

: INIT ( ptr h CAD-NUM:item-count -- ) {: vec:ptr cap:CAD-NUM:item-count :}
   vec CHECK-DEAD
   cap CAP-ALLOC MEM:ALLOC-CELLS vec RAW-DATA!
   cap ITEM>RC vec RAW-CAP!
   0 RAW-LEN vec RAW-LEN! ;
: CLEAR ( ptr h -- )  0 RAW-LEN swap RAW-LEN! ;

\ Clear ownership before release so repeated disposal is a no-op and a failed
\ release cannot double-free on retry.
: DISPOSE ( ptr h -- ) {: vec:ptr :}
   vec CAP-PTR @ {: cap:n :}
   cap 0= if exit then
   vec RAW-DATA@ {: data:ptr :}
   0 vec CAP-PTR !
   0 vec LEN-PTR !
   data cap >COUNT RELEASE-STORAGE ;

: LEN@ ( ptr h -- CAD-NUM:item-count )  RAW-LEN@ LEN>N N>ITEM ;
: CAP@ ( ptr h -- CAD-NUM:item-count )  RAW-CAP@ COUNT>N N>ITEM ;

\ Invalidated by resize or disposal.
: DATA@ ( ptr h -- ptr a )  dup CHECK-LIVE RAW-DATA@ ;

: RESIZE ( ptr h CAD-NUM:item-count -- ) {: vec:ptr cap:CAD-NUM:item-count :}
   cap ITEM>RC {: rc:count :}
   vec rc CHECK-RESIZE
   vec rc  cap CAP-ALLOC MEM:ALLOC-CELLS  INSTALL-RESIZE ;
: ENSURE ( ptr h CAD-NUM:item-count -- ) {: vec:ptr need:CAD-NUM:item-count :}
   need ITEM-COUNT>N {: nn:n :}
   nn >COUNT CHECK-NEED
   nn vec RAW-CAP@ COUNT>N <= if exit then
   vec  vec nn >COUNT GROW-CAP COUNT>N N>ITEM  RESIZE ;

: PUSH ( a ptr h -- CAD-NUM:index ) {: value:a vec:ptr :}
   vec RAW-LEN@ LEN>N {: old:n :}
   vec old 1 + N>ITEM ENSURE
   value vec RAW-DATA@ old CELL-PTR !
   old 1 + RAW-LEN vec RAW-LEN!
   old N>INDEX ;

\ typed-local-lint: allow-bare-local - q preserves the row-polymorphic quotation
\ effect [ R CAD-NUM:index a -- R ], which a local annotation cannot express.
: EACH ( R ptr h [ R CAD-NUM:index a -- R ] -- R ) {: vec:ptr q :}
   vec RAW-LEN@ LEN>N 0 ?do
      i N>INDEX  vec i RAW-IDX RAW@  q execute
   loop ;

\ Defined last: `@`/`!` name the package fetch/store VEC:@ / VEC:!, which would
\ shadow the core cell primitives inside later VEC bodies.
: @ ( ptr h CAD-NUM:index -- a ) {: vec:ptr ix:CAD-NUM:index :}
   vec ix IX>RI RAW@ ;
: ! ( a ptr h CAD-NUM:index -- ) {: value:a vec:ptr ix:CAD-NUM:index :}
   value vec ix IX>RI RAW! ;

;package
