\ arena.f - growable index-addressed cell arena, the storage substrate under the
\ sealed immutable nominal collection (dot habu-add-immutable-nominal-9290a81f).
\
\ Every path node, binding record, chunk run, and row record lives in one of
\ these arenas and is named by an INDEX, never by a pointer: a copy-on-grow
\ reallocation moves the backing memory but leaves every index valid, which is
\ why the public handles can be one-cell nominals over indices instead of raw
\ arena pointers. Storage is grow-only (the platform has no MEM-FREE): a fresh
\ AR-INIT drops the old backing span and mmaps a new one, so re-initialisation
\ after a snapshot restore also resets the process-local base pointer.
\
\ A "seal" high-water makes committed spans immutable: AR-CELL! refuses any write
\ below the seal (E-NOM-SEALED), so once a transaction commits (AR-SEAL-COMMIT)
\ its records can never be overwritten by a later raw store. Rollback truncates
\ the used high-water back to the seal (AR-TRUNC), discarding only the provisional
\ tail. This one mechanism serves both "protected spans prevent raw mutation" and
\ "rollback restores high-water/index state".
\
\ Control block = 4 cells: [0] base (ptr a) [1] cap (cells) [2] used [3] seal.
\ Load after lib/errors.f and lib/memory.f. Private helpers of package NOM.

require lib/errors.f
require lib/memory.f

-5500 constant E-NOM-SEALED        \ raw store aimed below a committed seal boundary
-5501 constant E-NOM-OVERFLOW      \ index/extent arithmetic would exceed the machine max
-5502 constant E-NOM-HANDLE        \ forged, stale, or out-of-range nominal handle
-5503 constant E-NOM-CONFLICT      \ two bindings share a path but disagree on the slot
-5504 constant E-NOM-BUDGET        \ a resource budget (not semantic capacity) was exceeded
-5505 constant E-NOM-WIRE          \ canonical byte stream is malformed / corrupt
-5506 constant E-NOM-OPEN          \ a second builder opened while one was already live
-5507 constant E-NOM-CAPACITY      \ a caller output buffer was too small

8 constant NOM-AR-SEED             \ initial arena capacity in cells

package NOM
private

: AR-BASE-FIELD ( ptr a -- ptr ptr a )   0 ptr-field ;
: AR-BASE@ ( ptr a -- ptr a )   AR-BASE-FIELD @ ;
: AR-BASE! ( ptr a ptr a -- ) {: v:ptr ctl:ptr :}
   v ctl AR-BASE-FIELD ! ;
: AR-CAP@ ( ptr a -- n )   1 cells + @ ;
: AR-CAP! ( n ptr a -- )   1 cells + ! ;
: AR-USED@ ( ptr a -- n )   2 cells + @ ;
: AR-USED! ( n ptr a -- )   2 cells + ! ;
: AR-SEAL@ ( ptr a -- n )   3 cells + @ ;
: AR-SEAL! ( n ptr a -- )   3 cells + ! ;

: AR-INIT ( ptr a -- ) {: ctl:ptr :}
   NOM-AR-SEED >COUNT MEM-ALLOC-CELLS ctl AR-BASE!
   NOM-AR-SEED ctl AR-CAP!
   0 ctl AR-USED!
   0 ctl AR-SEAL! ;

: AR-COPY-INTO ( ptr a ptr a -- ) {: ctl:ptr nb:ptr :}
   ctl AR-USED@ 0 ?do
      ctl AR-BASE@ i cells + @  nb i cells + !
   loop ;

: AR-NEXT-CAP ( n n -- n ) {: cap:n need:n :}
   cap MEM-MAX-CELLS 2 / > if need else cap 2 * need max then ;

: AR-GROW ( ptr a n -- ) {: ctl:ptr need:n :}
   need MEM-MAX-CELLS > if E-NOM-OVERFLOW throw then
   ctl AR-CAP@ need AR-NEXT-CAP {: ncap:n :}
   ncap need < ncap MEM-MAX-CELLS > or if E-NOM-OVERFLOW throw then
   ncap >COUNT MEM-ALLOC-CELLS {: nb:ptr :}
   ctl nb AR-COPY-INTO
   nb ctl AR-BASE!
   ncap ctl AR-CAP! ;

: AR-BOUNDS ( ptr a n -- ) {: ctl:ptr idx:n :}
   idx 0 < idx ctl AR-USED@ >= or if E-NOM-HANDLE throw then ;

: AR-CELL@ ( ptr a n -- a ) {: ctl:ptr idx:n :}
   ctl idx AR-BOUNDS
   idx cells ctl AR-BASE@ + @ ;

: AR-CELL! ( a ptr a n -- ) {: v:n ctl:ptr idx:n :}
   idx ctl AR-SEAL@ < if E-NOM-SEALED throw then
   ctl idx AR-BOUNDS
   v idx cells ctl AR-BASE@ + ! ;

: AR-PUSH ( a ptr a -- n ) {: v:n ctl:ptr :}
   ctl AR-USED@ 1 + MEM-MAX-CELLS > if E-NOM-OVERFLOW throw then
   ctl AR-USED@ 1 + ctl AR-CAP@ > if ctl ctl AR-USED@ 1 + AR-GROW then
   ctl AR-USED@ {: idx:n :}
   v idx cells ctl AR-BASE@ + !
   idx 1 + ctl AR-USED!
   idx ;

: AR-SEAL-COMMIT ( ptr a -- ) {: ctl:ptr :}
   ctl AR-USED@ ctl AR-SEAL! ;

: AR-RESET ( ptr a -- ) {: ctl:ptr :}    \ scratch reset: only legal on an unsealed arena
   ctl AR-SEAL@ 0 <> if E-NOM-SEALED throw then
   0 ctl AR-USED! ;

: AR-TRUNC ( ptr a n -- ) {: ctl:ptr mark:n :}
   mark ctl AR-SEAL@ < if E-NOM-SEALED throw then
   mark ctl AR-USED@ > if E-NOM-HANDLE throw then
   mark ctl AR-USED! ;

;package
