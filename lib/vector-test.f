\ vector-test.f - focused tests for checked growable cell vectors.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/vector-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require test/checker-assert.f

create VECT-VEC VEC-HEADER-CELLS cells allot
create VECT-PTR-VEC VEC-HEADER-CELLS cells allot
create VECT-BIG-VEC VEC-HEADER-CELLS cells allot
create VECT-REL-VEC VEC-HEADER-CELLS cells allot   \ dedicated header for release/dispose lifecycle tests

141000 constant VECT-BIG-N

variable VECT-SUM
variable VECT-IDX-SUM

: VECT-RESET ( -- )                     \ dispose-then-init: re-init of a live header now rejects
   VECT-VEC VEC-DISPOSE  VECT-VEC 2 VEC-CAP-COUNT VEC-INIT
   VECT-PTR-VEC VEC-DISPOSE  VECT-PTR-VEC 1 VEC-CAP-COUNT VEC-INIT
   0 VECT-SUM !
   0 VECT-IDX-SUM ! ;

: VECT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: VECT-GROW ( -- )
   VECT-RESET
   0 VEC-COUNT VEC-CHECK-NEED
   1 VEC-CAP-COUNT VEC-CELLS>BYTES 1 cells T=
   1 VEC-LEN VEC-CHECK-LEN
   VECT-VEC 1 VEC-CELL-FIELD VECT-VEC cell+ = TTRUE
   VECT-VEC VEC-CAP@ VEC-CHECK-CAP
   VECT-VEC 2 VEC-COUNT VEC-ENSURE
   VECT-VEC VEC-CAP@ COUNT>N 2 T=
   11 VECT-VEC VEC-PUSH-N IDX>N 0 T=
   22 VECT-VEC VEC-PUSH-N IDX>N 1 T=
   33 VECT-VEC VEC-PUSH-N IDX>N 2 T=
   VECT-VEC VEC-LEN@ LEN>N 3 T=
   VECT-VEC VEC-CAP@ COUNT>N 4 T=
   VECT-VEC 0 VEC-IDX VEC-N@ 11 T=
   VECT-VEC 1 VEC-IDX VEC-N@ 22 T=
   VECT-VEC 2 VEC-IDX VEC-N@ 33 T= ;

: VECT-SET ( -- )
   VECT-RESET
   99 VECT-VEC VEC-PUSH-N drop
   123 VECT-VEC 0 VEC-IDX VEC-N!
   VECT-VEC 0 VEC-IDX VEC-N@ 123 T= ;

: VECT-RESIZE ( -- )
   VECT-RESET
   41 VECT-VEC VEC-PUSH-N drop
   42 VECT-VEC VEC-PUSH-N drop
   VECT-VEC 5 VEC-CAP-COUNT VEC-RESIZE
   VECT-VEC VEC-LEN@ LEN>N 2 T=
   VECT-VEC VEC-CAP@ COUNT>N 5 T=
   VECT-VEC 0 VEC-IDX VEC-N@ 41 T=
   VECT-VEC 1 VEC-IDX VEC-N@ 42 T= ;

: VECT-RESIZE-SHRINK-BAD ( -- )
   VECT-RESET
   41 VECT-VEC VEC-PUSH-N drop
   42 VECT-VEC VEC-PUSH-N drop
   VECT-VEC 1 VEC-CAP-COUNT VEC-RESIZE ;

\ ---- release / dispose lifecycle (raw surface) --------------------------------
\ Release is proved by ADDRESS REUSE: a mapping freed to the OS leaves a hole that
\ the very next same-size mmap refills, so a fresh allocation lands on the freed
\ address. On the unreleased base the old mapping is retained, so the fresh
\ allocation lands elsewhere and the equality assertion fails.
: VECT-REL-INIT ( n -- )                               \ free any prior mapping, then init fresh
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC swap VEC-CAP-COUNT VEC-INIT ;

: VECT-RESIZE-RELEASES ( -- )                          \ resize frees the prior mapping (address reuse)
   2 VECT-REL-INIT
   VECT-REL-VEC VEC-DATA@ {: old:ptr :}
   VECT-REL-VEC 4 VEC-CAP-COUNT VEC-RESIZE              \ prior 2-cell mapping released after copy/install
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}             \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: VECT-DISPOSE-RELEASES ( -- )                         \ dispose frees the owned mapping (address reuse)
   2 VECT-REL-INIT
   VECT-REL-VEC VEC-DATA@ {: old:ptr :}
   VECT-REL-VEC VEC-DISPOSE
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}
   reuse old = TTRUE ;

: VECT-DISPOSE-DOUBLE ( -- )                           \ a second dispose is a proved no-op (no throw)
   2 VECT-REL-INIT
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC VEC-CAP-FIELD @ 0 T= ;                  \ stays dead; reaching here proves no throw

: VECT-DISPOSE-CYCLE-BOUNDED ( -- )                    \ init/dispose cycles keep ONE resident mapping
   2 VECT-REL-INIT
   VECT-REL-VEC VEC-DATA@ {: base:ptr :}
   VECT-REL-VEC VEC-DISPOSE
   8 0 ?do
      VECT-REL-VEC 2 VEC-CAP-COUNT VEC-INIT
      VECT-REL-VEC VEC-DATA@ base = TTRUE               \ every cycle refills the same freed hole
      VECT-REL-VEC VEC-DISPOSE
   loop ;

: VECT-DISPOSE-USE-GET ( -- )                          \ read after dispose
   2 VECT-REL-INIT  VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 0 VEC-IDX VEC-N@ drop ;
: VECT-DISPOSE-USE-PUSH ( -- )                         \ push after dispose
   2 VECT-REL-INIT  VECT-REL-VEC VEC-DISPOSE
   9 VECT-REL-VEC VEC-PUSH-N drop ;
: VECT-DISPOSE-USE-RESIZE ( -- )                       \ resize after dispose
   2 VECT-REL-INIT  VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 5 VEC-CAP-COUNT VEC-RESIZE ;

: VECT-INIT-LIVE-REJECTS ( -- )                        \ re-init of a live header rejects (fail-closed)
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 2 VEC-CAP-COUNT VEC-INIT               \ header now owns a live mapping
   VECT-REL-VEC 2 VEC-CAP-COUNT VEC-INIT ;             \ re-init without dispose throws (else old mapping leaks)

: VECT-RESIZE-FAIL-ATOMIC ( -- )                       \ a failed grow leaves the old vector valid
   2 VECT-REL-INIT
   41 VECT-REL-VEC VEC-PUSH-N drop  42 VECT-REL-VEC VEC-PUSH-N drop
   VECT-REL-VEC VEC-DATA@ {: before:ptr :}
   [: VECT-REL-VEC VEC-MAX-CELLS VEC-COUNT VEC-RESIZE ;] E-MEM-MAP TTHROWSQ  \ mmap of the whole space fails
   VECT-REL-VEC VEC-DATA@ before = TTRUE                \ mapping not swapped out
   VECT-REL-VEC VEC-LEN@ LEN>N 2 T=
   VECT-REL-VEC VEC-CAP@ COUNT>N 2 T=
   VECT-REL-VEC 0 VEC-IDX VEC-N@ 41 T=
   VECT-REL-VEC 1 VEC-IDX VEC-N@ 42 T= ;

: VECT-CLEAR ( -- )
   VECT-RESET
   7 VECT-VEC VEC-PUSH-N drop
   8 VECT-VEC VEC-PUSH-N drop
   VECT-VEC VEC-CLEAR
   VECT-VEC VEC-LEN@ LEN>N 0 T=
   VECT-VEC VEC-CAP@ COUNT>N 2 T=
   9 VECT-VEC VEC-PUSH-N IDX>N 0 T=
   VECT-VEC 0 VEC-IDX VEC-N@ 9 T= ;

: VECT-POINTER ( -- )
   VECT-RESET
   s" alpha" drop VECT-PTR-VEC VEC-PUSH-A IDX>N 0 T=
   VECT-PTR-VEC 0 VEC-IDX VEC-A@ 5 s" alpha" T$=
   s" beta" drop VECT-PTR-VEC 0 VEC-IDX VEC-A!
   VECT-PTR-VEC 0 VEC-IDX VEC-A@ 4 s" beta" T$= ;

: VECT-EACH-ACC ( idx n -- ) {: ix value :}
   VECT-SUM @ value + VECT-SUM !
   VECT-IDX-SUM @ ix IDX>N + VECT-IDX-SUM ! ;

: VECT-EACH-TEST ( -- )
   VECT-RESET
   5 VECT-VEC VEC-PUSH-N drop
   6 VECT-VEC VEC-PUSH-N drop
   7 VECT-VEC VEC-PUSH-N drop
   VECT-VEC [: VECT-EACH-ACC ;] VEC-EACH
   VECT-SUM @ 18 T=
   VECT-IDX-SUM @ 3 T= ;

\ Large fill stays within the suite budget only while slot access is O(1);
\ an O(index) VEC-CELL-FIELD regression makes this fill take minutes.
: VECT-BIG-FILL ( -- )
   VECT-BIG-VEC 2 VEC-CAP-COUNT VEC-INIT
   VECT-BIG-N 0 ?do i VECT-BIG-VEC VEC-PUSH-N drop loop
   VECT-BIG-VEC VEC-LEN@ LEN>N VECT-BIG-N T=
   VECT-BIG-VEC 0 VEC-IDX VEC-N@ 0 T=
   VECT-BIG-VEC VECT-BIG-N 2 / VEC-IDX VEC-N@ VECT-BIG-N 2 / T=
   VECT-BIG-VEC VECT-BIG-N 1 - VEC-IDX VEC-N@ VECT-BIG-N 1 - T= ;

: VECT-BAD-CAP-ZERO ( -- )
   0 VEC-CAP-COUNT drop ;

: VECT-BAD-CAP-NEG ( -- )
   -1 VEC-CAP-COUNT drop ;

: VECT-BAD-NEED-HIGH ( -- )
   VEC-MAX-CELLS 1 + VEC-COUNT drop ;

: VECT-BAD-LEN-NEG ( -- )
   -1 VEC-LEN drop ;

: VECT-BAD-LEN-HIGH ( -- )
   VECT-RESET
   3 VEC-LEN VECT-VEC VEC-LEN! ;

: VECT-GET-HIGH ( -- )
   VECT-RESET
   1 VECT-VEC VEC-PUSH-N drop
   VECT-VEC 1 VEC-IDX VEC-N@ drop ;

: VECT-GET-NEG ( -- )
   VECT-RESET
   1 VECT-VEC VEC-PUSH-N drop
   VECT-VEC -1 VEC-IDX VEC-N@ drop ;

: VECT-SET-HIGH ( -- )
   VECT-RESET
   1 VECT-VEC VEC-PUSH-N drop
   2 VECT-VEC 1 VEC-IDX VEC-N! ;

\ ---- B5.5 typed VEC surface over CAD-NUM roles --------------------------------
\ White-box role readers: reopen the unsealed CAD-NUM package to project a role's
\ raw cell for scalar assertions, the pattern lib/cad-num-arithmetic-test.f and
\ lib/memory-test.f use. (VEC's own ITEM-COUNT>N/INDEX>N stay VEC-private.)
package CAD-NUM
public
: VECT-IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;
: VECT-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

\ role builders from a raw cell (the refusal arms are unreachable for the test
\ inputs; an unexpected refusal throws the vector's own invariant code).
: VECT-N>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;
: VECT-N>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: VECT-TLEN ( ptr a -- n )  VEC:LEN@ CAD-NUM:VECT-IC>RAW ;   \ typed length as raw n
: VECT-TCAP ( ptr a -- n )  VEC:CAP@ CAD-NUM:VECT-IC>RAW ;   \ typed capacity as raw n
: VECT-TAT  ( ptr a n -- a ) VECT-N>INDEX VEC:@ ;              \ typed fetch at a raw index
: VECT-TPUT ( a ptr a n -- ) VECT-N>INDEX VEC:! ;              \ typed store at a raw index

\ dispose-then-init the shared typed header at raw cap n: re-init of a live header
\ now rejects, so each test opens from a disposed header.
: VECT-T-FRESH ( n -- )  VECT-VEC VEC-DISPOSE  VECT-VEC swap VECT-N>ITEM VEC:INIT ;

: VECT-T-INIT ( -- )                                    \ zero length valid, capacity honoured
   2 VECT-T-FRESH
   VECT-VEC VECT-TLEN 0 T=
   VECT-VEC VECT-TCAP 2 T= ;

: VECT-T-PUSH ( -- )                                    \ push returns the landing index; growth survives
   2 VECT-T-FRESH
   11 VECT-VEC VEC:PUSH CAD-NUM:VECT-IX>RAW 0 T=
   22 VECT-VEC VEC:PUSH CAD-NUM:VECT-IX>RAW 1 T=
   33 VECT-VEC VEC:PUSH CAD-NUM:VECT-IX>RAW 2 T=                   \ third push grows through the typed adapter
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
   9 VECT-VEC VEC:PUSH CAD-NUM:VECT-IX>RAW 0 T= ;

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

variable VECT-TSUM   variable VECT-TIXSUM
: VECT-T-EACH-ACC ( CAD-NUM:index a -- ) {: ix:CAD-NUM:index value:n :}
   VECT-TSUM @ value + VECT-TSUM !
   VECT-TIXSUM @ ix CAD-NUM:VECT-IX>RAW + VECT-TIXSUM ! ;
: VECT-T-EACH ( -- )                                    \ EACH visits cells index-first
   2 VECT-T-FRESH
   0 VECT-TSUM !  0 VECT-TIXSUM !
   5 VECT-VEC VEC:PUSH drop  6 VECT-VEC VEC:PUSH drop  7 VECT-VEC VEC:PUSH drop
   VECT-VEC [: VECT-T-EACH-ACC ;] VEC:EACH
   VECT-TSUM @ 18 T=
   VECT-TIXSUM @ 3 T= ;

: VECT-T-CAP-ZERO ( -- )   0 VECT-T-FRESH ;               \ zero capacity allocation
: VECT-T-CAP-OVER ( -- )   VEC-MAX-CELLS 1 + VECT-T-FRESH ; \ overflowing capacity
: VECT-T-RESIZE-ZERO ( -- )
   2 VECT-T-FRESH  VECT-VEC 0 VECT-N>ITEM VEC:RESIZE ;        \ resize to zero capacity
: VECT-T-RESIZE-SHRINK ( -- )
   2 VECT-T-FRESH
   41 VECT-VEC VEC:PUSH drop  42 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-N>ITEM VEC:RESIZE ;                                     \ cap below active length
: VECT-T-GET-HIGH ( -- )
   2 VECT-T-FRESH  1 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-TAT drop ;                                             \ index at length rejects

: VECT-T-DISPOSE-RELEASES ( -- )                       \ typed dispose frees the owned mapping
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC VEC-DATA@ {: old:ptr :}
   VECT-REL-VEC VEC:DISPOSE
   2 >COUNT MEM-ALLOC-CELLS {: reuse:ptr :}
   reuse old = TTRUE ;
: VECT-T-DISPOSE-DOUBLE ( -- )                         \ typed double dispose is a proved no-op
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC VEC:DISPOSE  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC VEC-CAP-FIELD @ 0 T= ;
: VECT-T-DISPOSE-USE ( -- )                            \ typed access after dispose rejects
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 0 VECT-TAT drop ;
: VECT-T-INIT-LIVE-REJECTS ( -- )                      \ typed re-init of a live header rejects
   VECT-REL-VEC VEC-DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT                 \ header now owns a live mapping
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT ;               \ re-init without dispose throws (else old mapping leaks)

: VECT-RUN ( -- )
   T-RESET
   VECT-GROW
   VECT-SET
   VECT-RESIZE
   VECT-CLEAR
   VECT-POINTER
   VECT-EACH-TEST
   VECT-BIG-FILL
   [: VECT-RESIZE-SHRINK-BAD ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-BAD-CAP-ZERO ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-BAD-CAP-NEG ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-BAD-NEED-HIGH ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-BAD-LEN-NEG ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-BAD-LEN-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-GET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-GET-NEG ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-SET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   s" BAD-VEC-FETCH ( ptr a len -- a ) VEC@" VECT-CHECK-REJECTS
   s" BAD-VEC-LEN! ( count ptr a -- ) VEC-LEN!" VECT-CHECK-REJECTS
   \ ---- release / dispose lifecycle (raw) -------------------------------------
   VECT-RESIZE-RELEASES
   VECT-DISPOSE-RELEASES
   VECT-DISPOSE-DOUBLE
   VECT-DISPOSE-CYCLE-BOUNDED
   VECT-RESIZE-FAIL-ATOMIC
   [: VECT-DISPOSE-USE-GET ;] E-VEC-STATE TTHROWSQ
   [: VECT-DISPOSE-USE-PUSH ;] E-VEC-STATE TTHROWSQ
   [: VECT-DISPOSE-USE-RESIZE ;] E-VEC-STATE TTHROWSQ
   [: VECT-INIT-LIVE-REJECTS ;] E-VEC-STATE TTHROWSQ   \ live re-init rejects; no owned mapping leaks
   \ ---- typed VEC surface: positive behavior ----------------------------------
   VECT-T-INIT
   VECT-T-PUSH
   VECT-T-SET
   VECT-T-CLEAR
   VECT-T-RESIZE
   VECT-T-ENSURE
   VECT-T-EACH
   \ ---- typed VEC surface: runtime allocation/bounds refusals -----------------
   [: VECT-T-CAP-ZERO ;] E-VEC-CAPACITY TTHROWSQ       \ zero capacity allocation rejects
   [: VECT-T-CAP-OVER ;] E-VEC-CAPACITY TTHROWSQ       \ growth overflow rejects
   [: VECT-T-RESIZE-ZERO ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-RESIZE-SHRINK ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-T-GET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   \ ---- typed release / dispose lifecycle -------------------------------------
   VECT-T-DISPOSE-RELEASES
   VECT-T-DISPOSE-DOUBLE
   [: VECT-T-DISPOSE-USE ;] E-VEC-STATE TTHROWSQ
   [: VECT-T-INIT-LIVE-REJECTS ;] E-VEC-STATE TTHROWSQ  \ typed live re-init rejects; no owned mapping leaks
   \ ---- typed VEC surface: static role-swap rejections (0) + resolving positives (-1)
   s" VOK-INIT ( ptr a CAD-NUM:item-count -- ) VEC:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-AT ( ptr a CAD-NUM:index -- a ) VEC:@"        CHECK-QUIET-CANDIDATE! -1 T=
   s" VSWAP-IDX-FOR-CNT ( ptr a CAD-NUM:index -- ) VEC:INIT"  VECT-CHECK-REJECTS
   s" VSWAP-CNT-FOR-IDX ( ptr a CAD-NUM:item-count -- a ) VEC:@" VECT-CHECK-REJECTS
   s" VSWAP-RAW-CAP ( ptr a n -- ) VEC:INIT"                  VECT-CHECK-REJECTS
   s" VSWAP-RAW-IDX ( ptr a n -- a ) VEC:@"                   VECT-CHECK-REJECTS
   T-REPORT ;

VECT-RUN
