\ vector-test.f - focused tests for checked growable cell vectors, and for the
\ frees a typed vector's DELETE and refused NEW make through its allocator.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/vector.f src/lib/vector-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/vector.f
require src/lib/arena.f

create VECT-VEC VEC:HEADER-CELLS cells allot
\ The header of a vector OF POINTERS is declared storage: its base carries the
\ element type (`ptr ptr u8`), which a `create`d cell cannot publish.
VEC:HEADER-CELLS TYPED-BUFFER VECT-PTR-VEC ptr u8
create VECT-BIG-VEC VEC:HEADER-CELLS cells allot
create VECT-REL-VEC VEC:HEADER-CELLS cells allot   \ dedicated header for release/dispose lifecycle tests

141000 constant VECT-BIG-N

: VECT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

CAST: VECT-IC>RAW ( NUM:item-count -- n )

: VECT-TLEN ( ptr a -- n )  VEC:LEN@ VECT-IC>RAW ;
: VECT-TCAP ( ptr a -- n )  VEC:CAP@ VECT-IC>RAW ;
: VECT-TAT  ( ptr a n -- a ) VEC:N>INDEX VEC:@ ;
: VECT-TPUT ( a ptr a n -- ) VEC:N>INDEX VEC:! ;

\ Re-init of a live header is refused, so each test opens from a disposed one.
: VECT-T-FRESH ( n -- )  VECT-VEC VEC:DISPOSE  VECT-VEC swap VEC:N>ITEM VEC:INIT ;

: VECT-REL-INIT ( n -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC swap VEC:N>ITEM VEC:INIT ;

: VECT-T-INIT ( -- )                                    \ zero length valid, capacity honoured
   2 VECT-T-FRESH
   VECT-VEC VECT-TLEN 0 T=
   VECT-VEC VECT-TCAP 2 T= ;

: VECT-T-PUSH ( -- )                                    \ push returns the landing index; growth survives
   2 VECT-T-FRESH
   11 VECT-VEC VEC:PUSH NUM:ORDINAL 0 T=
   22 VECT-VEC VEC:PUSH NUM:ORDINAL 1 T=
   33 VECT-VEC VEC:PUSH NUM:ORDINAL 2 T=                   \ third push grows through the typed adapter
   VECT-VEC VECT-TLEN 3 T=
   VECT-VEC 0 VECT-TAT 11 T=
   VECT-VEC 1 VECT-TAT 22 T=
   VECT-VEC 2 VECT-TAT 33 T= ;

: VECT-T-SET ( -- )                                     \ store overwrites a live cell
   2 VECT-T-FRESH
   77 VECT-VEC VEC:PUSH drop
   123 VECT-VEC 0 VECT-TPUT
   VECT-VEC 0 VECT-TAT 123 T= ;

: VECT-T-CLEAR ( -- )                                   \ clear zeroes length, keeps capacity
   2 VECT-T-FRESH
   7 VECT-VEC VEC:PUSH drop  8 VECT-VEC VEC:PUSH drop
   VECT-VEC VEC:CLEAR
   VECT-VEC VECT-TLEN 0 T=
   VECT-VEC VECT-TCAP 2 T=
   9 VECT-VEC VEC:PUSH NUM:ORDINAL 0 T= ;

: VECT-T-RESIZE ( -- )                                  \ resize grows capacity, preserves cells
   2 VECT-T-FRESH
   41 VECT-VEC VEC:PUSH drop  42 VECT-VEC VEC:PUSH drop
   VECT-VEC 5 VEC:N>ITEM VEC:RESIZE
   VECT-VEC VECT-TLEN 2 T=
   VECT-VEC VECT-TCAP 5 T=
   VECT-VEC 0 VECT-TAT 41 T=
   VECT-VEC 1 VECT-TAT 42 T= ;

: VECT-T-ENSURE ( -- )                                  \ ensure is a no-op below cap, grows above
   4 VECT-T-FRESH
   VECT-VEC 3 VEC:N>ITEM VEC:ENSURE   VECT-VEC VECT-TCAP 4 T=  \ need <= cap: no reallocation
   VECT-VEC 9 VEC:N>ITEM VEC:ENSURE   VECT-VEC VECT-TCAP 16 T= ;  \ need > cap: double 4 -> 8 -> 16

: VECT-T-POINTER ( -- )                                 \ a vector of pointers keeps its element type
   0 VECT-PTR-VEC VEC:DISPOSE  0 VECT-PTR-VEC 1 VEC:N>ITEM VEC:INIT
   s" alpha" drop 0 VECT-PTR-VEC VEC:PUSH NUM:ORDINAL 0 T=
   0 VECT-PTR-VEC 0 VECT-TAT 5 s" alpha" T$=
   s" beta" drop 0 VECT-PTR-VEC 0 VECT-TPUT
   0 VECT-PTR-VEC 0 VECT-TAT 4 s" beta" T$= ;

variable VECT-TSUM   variable VECT-TIXSUM
: VECT-T-EACH-ACC ( NUM:index n -- ) {: ix:NUM:index value:n :}
   VECT-TSUM @ value + VECT-TSUM !
   VECT-TIXSUM @ ix NUM:ORDINAL + VECT-TIXSUM ! ;
: VECT-T-EACH ( -- )                                    \ EACH visits cells index-first
   2 VECT-T-FRESH
   0 VECT-TSUM !  0 VECT-TIXSUM !
   5 VECT-VEC VEC:PUSH drop  6 VECT-VEC VEC:PUSH drop  7 VECT-VEC VEC:PUSH drop
   VECT-VEC [: VECT-T-EACH-ACC ;] VEC:EACH
   VECT-TSUM @ 18 T=
   VECT-TIXSUM @ 3 T= ;

\ Large fill stays within the suite budget only while slot access is O(1);
\ an O(index) cell-address regression makes this fill take minutes.
: VECT-T-BIG-FILL ( -- )
   VECT-BIG-VEC 2 VEC:N>ITEM VEC:INIT
   VECT-BIG-N 0 ?do i VECT-BIG-VEC VEC:PUSH drop loop
   VECT-BIG-VEC VECT-TLEN VECT-BIG-N T=
   VECT-BIG-VEC 0 VECT-TAT 0 T=
   VECT-BIG-VEC VECT-BIG-N 2 / VECT-TAT VECT-BIG-N 2 / T=
   VECT-BIG-VEC VECT-BIG-N 1 - VECT-TAT VECT-BIG-N 1 - T= ;

: VECT-T-DATA ( -- )                                    \ DATA@ is the contiguous cell base, reread after growth
   2 VECT-T-FRESH
   11 VECT-VEC VEC:PUSH drop  22 VECT-VEC VEC:PUSH drop
   VECT-VEC VEC:DATA@ {: old:ptr :}
   old @ 11 T=
   old cell+ @ 22 T=
   33 VECT-VEC VEC:PUSH drop                              \ third push grows into new storage
   VECT-VEC VEC:DATA@ {: new:ptr :}
   new old = TFALSE
   new @ 11 T=
   new cell+ @ 22 T=
   new 2 cells + @ 33 T= ;

: VECT-T-LEN-SET ( -- )                                 \ LEN! truncates, grows within capacity, empties
   4 VECT-T-FRESH
   11 VECT-VEC VEC:PUSH drop  22 VECT-VEC VEC:PUSH drop  33 VECT-VEC VEC:PUSH drop
   3 VEC:N>ITEM VECT-VEC VEC:LEN!                        \ same length: no change
   VECT-VEC VECT-TLEN 3 T=
   VECT-VEC 2 VECT-TAT 33 T=
   1 VEC:N>ITEM VECT-VEC VEC:LEN!                        \ truncate to the first cell
   VECT-VEC VECT-TLEN 1 T=
   VECT-VEC VECT-TCAP 4 T=
   VECT-VEC 0 VECT-TAT 11 T=
   [: VECT-VEC 1 VECT-TAT drop ;] E-VEC-BOUNDS TTHROWSQ  \ the dropped cells are out of range
   44 VECT-VEC VEC:PUSH NUM:ORDINAL 1 T=
   4 VEC:N>ITEM VECT-VEC VEC:LEN!                        \ grow within capacity
   VECT-VEC VECT-TLEN 4 T=
   VECT-VEC 1 VECT-TAT 44 T=
   0 VEC:N>ITEM VECT-VEC VEC:LEN!                        \ empty
   VECT-VEC VECT-TLEN 0 T=
   [: VECT-VEC 0 VECT-TAT drop ;] E-VEC-BOUNDS TTHROWSQ ;

: VECT-T-LEN-HIGH ( -- )                                \ a length above capacity rejects
   2 VECT-T-FRESH
   3 VEC:N>ITEM VECT-VEC VEC:LEN! ;

: VECT-T-CAP-ZERO ( -- )   0 VECT-T-FRESH ;
: VECT-T-CAP-OVER ( -- )   MEM-MAX-CELLS 1 + VECT-T-FRESH ;
: VECT-T-ENSURE-HIGH ( -- )                               \ a need above the cell ceiling
   2 VECT-T-FRESH  VECT-VEC MEM-MAX-CELLS 1 + VEC:N>ITEM VEC:ENSURE ;
: VECT-T-RESIZE-ZERO ( -- )
   2 VECT-T-FRESH  VECT-VEC 0 VEC:N>ITEM VEC:RESIZE ;        \ resize to zero capacity
: VECT-T-RESIZE-SHRINK ( -- )
   2 VECT-T-FRESH
   41 VECT-VEC VEC:PUSH drop  42 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VEC:N>ITEM VEC:RESIZE ;                                     \ cap below active length
: VECT-T-GET-HIGH ( -- )
   2 VECT-T-FRESH  1 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-TAT drop ;                                             \ index at length rejects
: VECT-T-SET-HIGH ( -- )
   2 VECT-T-FRESH  1 VECT-VEC VEC:PUSH drop
   2 VECT-VEC 1 VECT-TPUT ;                                               \ store at length rejects

\ ---- release / dispose lifecycle ----------------------------------------------
\ Release is proved by ADDRESS REUSE: a mapping freed to the OS leaves a hole that
\ the very next same-size mmap refills, so a fresh allocation lands on the freed
\ address. On the unreleased base the old mapping is retained, so the fresh
\ allocation lands elsewhere and the equality assertion fails.
: VECT-T-RESIZE-RELEASES ( -- )                        \ resize frees the prior mapping
   2 VECT-REL-INIT
   VECT-REL-VEC VEC:DATA@ {: old:ptr :}
   VECT-REL-VEC 4 VEC:N>ITEM VEC:RESIZE               \ prior 2-cell mapping released after copy/install
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}             \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: VECT-T-DISPOSE-RELEASES ( -- )                       \ dispose frees the owned mapping
   2 VECT-REL-INIT
   VECT-REL-VEC VEC:DATA@ {: old:ptr :}
   VECT-REL-VEC VEC:DISPOSE
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}
   reuse old = TTRUE ;

\ A dead header from an earlier image can still hold its old data address (cell
\ 0) and length (cell 1); a dispose clears both without releasing anything.
: VECT-T-DISPOSE-DOUBLE ( -- )
   2 VECT-REL-INIT
   VECT-REL-VEC VEC:DISPOSE  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC VECT-TCAP 0 T=
   VECT-REL-VEC VECT-TLEN 0 T=
   VECT-REL-VEC VEC:DATA@ 0= TTRUE
   $1000 VECT-REL-VEC !                                \ a stale data address
   7 VECT-REL-VEC cell+ !
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC VEC:DATA@ 0= TTRUE
   VECT-REL-VEC VECT-TLEN 0 T= ;

: VECT-T-DISPOSE-CYCLE-BOUNDED ( -- )                  \ init/dispose cycles keep ONE resident mapping
   2 VECT-REL-INIT
   VECT-REL-VEC VEC:DATA@ {: base:ptr :}
   VECT-REL-VEC VEC:DISPOSE
   8 0 ?do
      VECT-REL-VEC 2 VEC:N>ITEM VEC:INIT
      VECT-REL-VEC VEC:DATA@ base = TTRUE               \ every cycle refills the same freed hole
      VECT-REL-VEC VEC:DISPOSE
   loop ;

: VECT-T-RESIZE-FAIL-ATOMIC ( -- )                     \ a failed grow leaves the old vector valid
   2 VECT-REL-INIT
   41 VECT-REL-VEC VEC:PUSH drop  42 VECT-REL-VEC VEC:PUSH drop
   VECT-REL-VEC VEC:DATA@ {: before:ptr :}
   [: VECT-REL-VEC MEM-MAX-CELLS VEC:N>ITEM VEC:RESIZE ;] E-MEM-MAP TTHROWSQ  \ mmap of the whole space fails
   VECT-REL-VEC VEC:DATA@ before = TTRUE                \ mapping not swapped out
   VECT-REL-VEC VECT-TLEN 2 T=
   VECT-REL-VEC VECT-TCAP 2 T=
   VECT-REL-VEC 0 VECT-TAT 41 T=
   VECT-REL-VEC 1 VECT-TAT 42 T= ;

: VECT-T-DISPOSE-USE ( -- )
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 0 VECT-TAT drop ;
: VECT-T-DISPOSE-USE-PUSH ( -- )
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   9 VECT-REL-VEC VEC:PUSH drop ;
: VECT-T-DISPOSE-USE-RESIZE ( -- )
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 5 VEC:N>ITEM VEC:RESIZE ;
: VECT-T-INIT-LIVE-REJECTS ( -- )
   2 VECT-REL-INIT                                     \ header now owns a live mapping
   VECT-REL-VEC 2 VEC:N>ITEM VEC:INIT ;               \ re-init without dispose throws (else old mapping leaks)

DEFLINEAR vect-lin

package VECT-TYPED

STRUCTURE turn DERIVE addr FIELD id n FIELD step n ;STRUCTURE

: CELLS-N ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" VEC:NEW makes zeroed elements of n that keep what is written" T-LABEL
   al 3 [type] n VEC:NEW {: v:ptr :}
   v VEC:COUNT 3 T=
   v 0 VEC:AT @ 0 T=  v 2 VEC:AT @ 0 T=
   5 v 0 VEC:AT !  6 v 2 VEC:AT !
   v 0 VEC:AT @ 5 T=  v 1 VEC:AT @ 0 T=  v 2 VEC:AT @ 6 T=
   v VEC:DELETE ;

: CELLS-R ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so of r" T-LABEL
   al 2 [type] r VEC:NEW {: v:ptr :}
   2.5 v 1 VEC:AT !
   v 0 VEC:AT @ 0.0 f= TTRUE  v 1 VEC:AT @ 2.5 f= TTRUE ;

: RECORDS ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" and so of a record, past an index's error" T-LABEL
   al 4 [type] turn VEC:NEW {: v:ptr :}
   7 v 3 VEC:AT TURN-ID !  8 v 3 VEC:AT TURN-STEP !
   v 3 VEC:AT TURN-ID @ 7 T=  v 3 VEC:AT TURN-STEP @ 8 T=
   v 2 VEC:AT TURN-STEP @ 0 T=
   [: v 4 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   [: v -1 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ ;

: EMPTY ( ptr ALLOC:allocator -- )
   {: al:ptr :}
   s" a vector of no elements has no index; a negative count is refused" T-LABEL
   al 0 [type] n VEC:NEW {: v:ptr :}
   v VEC:COUNT 0 T=
   [: v 0 VEC:AT drop ;] E-VEC-BOUNDS TTHROWSQ
   v VEC:DELETE
   [: al -1 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $2000000000000001 [type] n VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ
   [: al $7FFFFFFFFFFFFFFF [type] u8 VEC:NEW drop ;] E-VEC-CAPACITY TTHROWSQ ;

\ An allocator bumping through POOL that counts its calls and frees, keeps the
\ last length asked, records each freed offset in POOL and refuses over 2000.
4096 BUFFER: POOL
variable TOP
variable ALLOCS
variable FREES
variable LAST-N
8 TYPED-BUFFER FREED n
TYPED-VARIABLE AL ALLOC:allocator

: P-ALLOC ( ptr u8 n -- ptr u8 )
   {: st:ptr len:n :}
   1 ALLOCS +!  len LAST-N !
   len 2000 > if E-MEM-SIZE throw then
   st TOP @ +  len 7 + -8 and TOP +! ;

: P-FREE ( ptr u8 ptr u8 -- )
   {: st:ptr p:ptr :}
   p NULL-PTR - st NULL-PTR - - FREES @ FREED !  1 FREES +! ;

: DELETE-SOME ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 3 [type] n VEC:NEW {: v:ptr :}
   ALLOCS @ a0 - 2 T=  LAST-N @ 3 cells T=
   v VEC:DELETE  FREES @ f0 - 2 T=
   f0 FREED @  h [type] VEC:vector<n> width>n +  T=
   f0 1 + FREED @ h T= ;

: DELETE-NONE ( -- )
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   AL 0 [type] n VEC:NEW {: v:ptr :}
   ALLOCS @ a0 - 1 T=
   v VEC:DELETE  FREES @ f0 - 1 T=  f0 FREED @ h T= ;

: DELETES ( -- )
   s" VEC:DELETE frees the data, then the header, through the vector's allocator" T-LABEL
   DELETE-SOME  DELETE-NONE ;

: REFUSED ( -- )
   s" a refused data request frees the header" T-LABEL
   FREES @ {: f0:n :}  TOP @ {: h:n :}  ALLOCS @ {: a0:n :}
   [: AL 1000 [type] n VEC:NEW drop ;] E-MEM-SIZE TTHROWSQ
   ALLOCS @ a0 - 2 T=  FREES @ f0 - 1 T=  f0 FREED @ h T= ;

public

: RUN ( -- )
   POOL [: P-ALLOC ;] [: P-FREE ;] AL ALLOC:INIT
   ARENA:NEW {: a:ptr :}
   a ARENA:ALLOCATOR {: al:ptr :}
   al CELLS-N  al CELLS-R  al RECORDS  al EMPTY
   DELETES  REFUSED
   a ARENA:RELEASE ;

;package

: VECT-RUN ( -- )
   T-RESET
   \ ---- positive behavior -----------------------------------------------------
   VECT-T-INIT
   VECT-T-PUSH
   VECT-T-SET
   VECT-T-CLEAR
   VECT-T-RESIZE
   VECT-T-ENSURE
   VECT-T-POINTER
   VECT-T-EACH
   VECT-T-BIG-FILL
   VECT-T-DATA
   VECT-T-LEN-SET
   \ ---- runtime allocation/bounds refusals ------------------------------------
   [: VECT-T-CAP-ZERO ;] E-VEC-CAPACITY TTHROWSQ       \ zero capacity allocation rejects
   [: VECT-T-CAP-OVER ;] E-VEC-CAPACITY TTHROWSQ       \ growth overflow rejects
   [: VECT-T-ENSURE-HIGH ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-RESIZE-ZERO ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-RESIZE-SHRINK ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-T-GET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-T-SET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-T-LEN-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   [: -1 VEC:N>ITEM drop ;] E-VEC-CAPACITY TTHROWSQ    \ a negative count rejects
   [: -1 VEC:N>INDEX drop ;] E-VEC-BOUNDS TTHROWSQ     \ a negative index rejects
   \ ---- release / dispose lifecycle -------------------------------------------
   VECT-T-RESIZE-RELEASES
   VECT-T-DISPOSE-RELEASES
   VECT-T-DISPOSE-DOUBLE
   VECT-T-DISPOSE-CYCLE-BOUNDED
   VECT-T-RESIZE-FAIL-ATOMIC
   [: VECT-T-DISPOSE-USE ;] E-VEC-STATE TTHROWSQ
   [: VECT-T-DISPOSE-USE-PUSH ;] E-VEC-STATE TTHROWSQ
   [: VECT-T-DISPOSE-USE-RESIZE ;] E-VEC-STATE TTHROWSQ
   [: VECT-T-INIT-LIVE-REJECTS ;] E-VEC-STATE TTHROWSQ  \ live re-init rejects; no owned mapping leaks
   \ ---- static role-swap rejections (0) + resolving positives (-1) ------------
   s" VOK-INIT ( ptr a NUM:item-count -- ) VEC:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-AT ( ptr a NUM:index -- a ) VEC:@"        CHECK-QUIET-CANDIDATE! -1 T=
   s" VSWAP-IDX-FOR-CNT ( ptr a NUM:index -- ) VEC:INIT"  VECT-CHECK-REJECTS
   s" VSWAP-CNT-FOR-IDX ( ptr a NUM:item-count -- a ) VEC:@" VECT-CHECK-REJECTS
   s" VSWAP-IDX-FOR-LEN ( NUM:index ptr a -- ) VEC:LEN!"     VECT-CHECK-REJECTS
   s" VSWAP-RAW-CAP ( ptr a n -- ) VEC:INIT"                  VECT-CHECK-REJECTS
   s" VSWAP-RAW-IDX ( ptr a n -- a ) VEC:@"                   VECT-CHECK-REJECTS
   \ ---- the typed vector ---------------------------------------------------------
   VECT-TYPED:RUN
   s" VOK-TAT ( ptr VEC:vector<n> -- ptr n ) 0 VEC:AT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VLIN-AT ( ptr VEC:vector<vect-lin> -- ptr vect-lin ) 0 VEC:AT" VECT-CHECK-REJECTS
   T-REPORT ;

VECT-RUN
