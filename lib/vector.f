\ vector.f - checked growable cell-vector helpers.
\
\ Load after lib/errors.f and lib/memory.f.

0 constant VEC-DATA-OFF
1 constant VEC-LEN-OFF
2 constant VEC-CAP-OFF
3 constant VEC-HEADER-CELLS

2 constant VEC-GROWTH
MEM-MAX-CELLS constant VEC-MAX-CELLS

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
   base off 0 ?do cell+ loop ;

: VEC-DATA-FIELD ( ptr a -- ptr ptr a )
   VEC-DATA-OFF ptr-field ;

: VEC-DATA@ ( ptr a -- ptr a )
   VEC-DATA-FIELD @ ;

: VEC-DATA! ( ptr a ptr a -- ) {: data:ptr vec:ptr :}
   data vec VEC-DATA-FIELD ! ;

: VEC-LEN-FIELD ( ptr a -- ptr a )
   VEC-LEN-OFF VEC-CELL-FIELD ;

: VEC-LEN@ ( ptr a -- len )
   VEC-LEN-FIELD @ >LEN ;

: VEC-CAP-FIELD ( ptr a -- ptr a )
   VEC-CAP-OFF VEC-CELL-FIELD ;

: VEC-CAP@ ( ptr a -- count )
   VEC-CAP-FIELD @ >COUNT ;

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
   0 >LEN vec VEC-LEN! ;

: VEC-CLEAR ( ptr a -- )
   0 >LEN swap VEC-LEN! ;

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
   repeat >COUNT ;

: VEC-ENSURE ( ptr a count -- ) {: vec:ptr need :}
   need VEC-CHECK-NEED
   need COUNT>N vec VEC-CAP@ COUNT>N <= if exit then
   vec vec need VEC-GROW-CAP VEC-RESIZE ;

: VEC-PUSH-AT ( a ptr a n -- idx ) {: value vec:ptr old :}
   vec old 1 + >COUNT VEC-ENSURE
   value vec VEC-DATA@ old VEC-CELL-FIELD !
   old 1 + >LEN vec VEC-LEN!
   old >IDX ;

: VEC-PUSH ( a ptr a -- idx ) {: value vec:ptr :}
   value vec vec VEC-LEN@ LEN>N VEC-PUSH-AT ;

: VEC-PUSH-N ( n ptr a -- idx )
   VEC-PUSH ;

: VEC-PUSH-A ( ptr u8 ptr a -- idx )
   VEC-PUSH ;

: VEC-EACH ( R ptr a [ R idx a -- R ] -- R ) {: vec:ptr q :}
   vec VEC-LEN@ LEN>N 0 ?do
      i >IDX vec i >IDX VEC@ q execute
   loop ;
s" VEC-EACH" s" R ptr a [ R idx a -- R ] -- R" TRUST
