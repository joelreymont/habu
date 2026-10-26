\ vector-test.f - focused tests for checked growable cell vectors.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/vector.f src/lib/vector-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/vector.f

create VECT-VEC VEC:HEADER-CELLS cells allot
\ The header of a vector OF POINTERS is declared storage: its base carries the
\ element type (`ptr ptr u8`), which a `create`d cell cannot publish.
VEC:HEADER-CELLS TYPED-BUFFER VECT-PTR-VEC ptr u8
create VECT-BIG-VEC VEC:HEADER-CELLS cells allot
create VECT-REL-VEC VEC:HEADER-CELLS cells allot   \ dedicated header for release/dispose lifecycle tests

141000 constant VECT-BIG-N

: VECT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ White-box count reader, declared at this test's own scope: a length/capacity
\ assertion compares against a literal n. Checked cast, and projection out of a
\ cell family needs no ownership (checker.f CAST-OWNER?). A pushed index is read
\ through the package's own public NUM:ORDINAL. (VEC's own ITEM-COUNT>N /
\ INDEX>N stay VEC-private.)
CAST: VECT-IC>RAW ( NUM:item-count -- n )    \ typed length/capacity for T=

\ role builders from a raw cell (the refusal arms are unreachable for the test
\ inputs; an unexpected refusal throws the vector's own invariant code).
: VECT-N>ITEM ( n -- NUM:item-count )
   NUM:ITEM-COUNT
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: VECT-N>INDEX ( n -- NUM:index )
   NUM:INDEX
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: VECT-TLEN ( ptr a -- n )  VEC:LEN@ VECT-IC>RAW ;   \ typed length as raw n
: VECT-TCAP ( ptr a -- n )  VEC:CAP@ VECT-IC>RAW ;   \ typed capacity as raw n
: VECT-TAT  ( ptr a n -- a ) VECT-N>INDEX VEC:@ ;              \ typed fetch at a raw index
: VECT-TPUT ( a ptr a n -- ) VECT-N>INDEX VEC:! ;              \ typed store at a raw index

\ dispose-then-init the shared typed header at raw cap n: re-init of a live header
\ now rejects, so each test opens from a disposed header.
: VECT-T-FRESH ( n -- )  VECT-VEC VEC:DISPOSE  VECT-VEC swap VECT-N>ITEM VEC:INIT ;

\ the same for the release/dispose lifecycle header
: VECT-REL-INIT ( n -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC swap VECT-N>ITEM VEC:INIT ;

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
   VECT-VEC 5 VECT-N>ITEM VEC:RESIZE
   VECT-VEC VECT-TLEN 2 T=
   VECT-VEC VECT-TCAP 5 T=
   VECT-VEC 0 VECT-TAT 41 T=
   VECT-VEC 1 VECT-TAT 42 T= ;

: VECT-T-ENSURE ( -- )                                  \ ensure is a no-op below cap, grows above
   4 VECT-T-FRESH
   VECT-VEC 3 VECT-N>ITEM VEC:ENSURE   VECT-VEC VECT-TCAP 4 T=  \ need <= cap: no reallocation
   VECT-VEC 9 VECT-N>ITEM VEC:ENSURE   VECT-VEC VECT-TCAP 16 T= ;  \ need > cap: double 4 -> 8 -> 16

: VECT-T-POINTER ( -- )                                 \ a vector of pointers keeps its element type
   0 VECT-PTR-VEC VEC:DISPOSE  0 VECT-PTR-VEC 1 VECT-N>ITEM VEC:INIT
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
   VECT-BIG-VEC 2 VECT-N>ITEM VEC:INIT
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
   3 VECT-N>ITEM VECT-VEC VEC:LEN!                        \ same length: no change
   VECT-VEC VECT-TLEN 3 T=
   VECT-VEC 2 VECT-TAT 33 T=
   1 VECT-N>ITEM VECT-VEC VEC:LEN!                        \ truncate to the first cell
   VECT-VEC VECT-TLEN 1 T=
   VECT-VEC VECT-TCAP 4 T=
   VECT-VEC 0 VECT-TAT 11 T=
   [: VECT-VEC 1 VECT-TAT drop ;] E-VEC-BOUNDS TTHROWSQ  \ the dropped cells are out of range
   44 VECT-VEC VEC:PUSH NUM:ORDINAL 1 T=
   4 VECT-N>ITEM VECT-VEC VEC:LEN!                        \ grow within capacity
   VECT-VEC VECT-TLEN 4 T=
   VECT-VEC 1 VECT-TAT 44 T=
   0 VECT-N>ITEM VECT-VEC VEC:LEN!                        \ empty
   VECT-VEC VECT-TLEN 0 T=
   [: VECT-VEC 0 VECT-TAT drop ;] E-VEC-BOUNDS TTHROWSQ ;

: VECT-T-LEN-HIGH ( -- )                                \ a length above capacity rejects
   2 VECT-T-FRESH
   3 VECT-N>ITEM VECT-VEC VEC:LEN! ;

: VECT-T-CAP-ZERO ( -- )   0 VECT-T-FRESH ;               \ zero capacity allocation
: VECT-T-CAP-OVER ( -- )   MEM-MAX-CELLS 1 + VECT-T-FRESH ; \ overflowing capacity
: VECT-T-ENSURE-HIGH ( -- )                               \ a need above the cell ceiling
   2 VECT-T-FRESH  VECT-VEC MEM-MAX-CELLS 1 + VECT-N>ITEM VEC:ENSURE ;
: VECT-T-RESIZE-ZERO ( -- )
   2 VECT-T-FRESH  VECT-VEC 0 VECT-N>ITEM VEC:RESIZE ;        \ resize to zero capacity
: VECT-T-RESIZE-SHRINK ( -- )
   2 VECT-T-FRESH
   41 VECT-VEC VEC:PUSH drop  42 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-N>ITEM VEC:RESIZE ;                                     \ cap below active length
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
   VECT-REL-VEC 4 VECT-N>ITEM VEC:RESIZE               \ prior 2-cell mapping released after copy/install
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}             \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: VECT-T-DISPOSE-RELEASES ( -- )                       \ dispose frees the owned mapping
   2 VECT-REL-INIT
   VECT-REL-VEC VEC:DATA@ {: old:ptr :}
   VECT-REL-VEC VEC:DISPOSE
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}
   reuse old = TTRUE ;

\ A second dispose is a proved no-op. The header layout is data pointer at cell 0,
\ length at cell 1: a dead header from an earlier image can still hold its old data
\ address and length, and a dispose clears both without releasing anything.
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
      VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
      VECT-REL-VEC VEC:DATA@ base = TTRUE               \ every cycle refills the same freed hole
      VECT-REL-VEC VEC:DISPOSE
   loop ;

: VECT-T-RESIZE-FAIL-ATOMIC ( -- )                     \ a failed grow leaves the old vector valid
   2 VECT-REL-INIT
   41 VECT-REL-VEC VEC:PUSH drop  42 VECT-REL-VEC VEC:PUSH drop
   VECT-REL-VEC VEC:DATA@ {: before:ptr :}
   [: VECT-REL-VEC MEM-MAX-CELLS VECT-N>ITEM VEC:RESIZE ;] E-MEM-MAP TTHROWSQ  \ mmap of the whole space fails
   VECT-REL-VEC VEC:DATA@ before = TTRUE                \ mapping not swapped out
   VECT-REL-VEC VECT-TLEN 2 T=
   VECT-REL-VEC VECT-TCAP 2 T=
   VECT-REL-VEC 0 VECT-TAT 41 T=
   VECT-REL-VEC 1 VECT-TAT 42 T= ;

: VECT-T-DISPOSE-USE ( -- )                            \ access after dispose rejects
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 0 VECT-TAT drop ;
: VECT-T-DISPOSE-USE-PUSH ( -- )                       \ push after dispose rejects
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   9 VECT-REL-VEC VEC:PUSH drop ;
: VECT-T-DISPOSE-USE-RESIZE ( -- )                     \ resize after dispose rejects
   2 VECT-REL-INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 5 VECT-N>ITEM VEC:RESIZE ;
: VECT-T-INIT-LIVE-REJECTS ( -- )                      \ re-init of a live header rejects
   2 VECT-REL-INIT                                     \ header now owns a live mapping
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT ;               \ re-init without dispose throws (else old mapping leaks)

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
   T-REPORT ;

VECT-RUN
