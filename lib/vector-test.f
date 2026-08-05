\ vector-test.f - focused tests for checked growable cell vectors.
\ Run: bin/hb --load lib/vector-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require test/checker-assert.f

package CAD-NUM
public
: VECT-IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;
: VECT-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

package VEC-TEST
private

create VECT-VEC VEC:HEADER-CELLS cells allot
create VECT-PTR-VEC VEC:HEADER-CELLS cells allot
create VECT-BIG-VEC VEC:HEADER-CELLS cells allot
create VECT-REL-VEC VEC:HEADER-CELLS cells allot
create VECT-TMP-VEC VEC:HEADER-CELLS cells allot

141000 constant VECT-BIG-N

: VECT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Test-only CAD role projections and builders.
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

: VECT-TLEN ( ptr h -- n )  VEC:LEN@ CAD-NUM:VECT-IC>RAW ;
: VECT-TCAP ( ptr h -- n )  VEC:CAP@ CAD-NUM:VECT-IC>RAW ;
: VECT-TAT  ( ptr h n -- a ) VECT-N>INDEX VEC:@ ;
: VECT-TPUT ( a ptr h n -- ) VECT-N>INDEX VEC:! ;

\ dispose-then-init the shared typed header at raw cap n: re-init of a live header
\ now rejects, so each test opens from a disposed header.
: VECT-T-FRESH ( n -- )  VECT-VEC VEC:DISPOSE  VECT-VEC swap VECT-N>ITEM VEC:INIT ;

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
: VECT-T-EACH-ACC ( CAD-NUM:index n -- ) {: ix:CAD-NUM:index value:n :}
   VECT-TSUM @ value + VECT-TSUM !
   VECT-TIXSUM @ ix CAD-NUM:VECT-IX>RAW + VECT-TIXSUM ! ;
: VECT-T-EACH ( -- )                                    \ EACH visits cells index-first
   2 VECT-T-FRESH
   0 VECT-TSUM !  0 VECT-TIXSUM !
   5 VECT-VEC VEC:PUSH drop  6 VECT-VEC VEC:PUSH drop  7 VECT-VEC VEC:PUSH drop
   VECT-VEC [: VECT-T-EACH-ACC ;] VEC:EACH
   VECT-TSUM @ 18 T=
   VECT-TIXSUM @ 3 T= ;

: VECT-POINTER ( -- )
   VECT-PTR-VEC VEC:DISPOSE
   VECT-PTR-VEC 1 VECT-N>ITEM VEC:INIT
   s" alpha" drop VECT-PTR-VEC VEC:PUSH drop
   VECT-PTR-VEC 0 VECT-TAT 5 s" alpha" T$=
   s" beta" drop VECT-PTR-VEC 0 VECT-TPUT
   VECT-PTR-VEC 0 VECT-TAT 4 s" beta" T$= ;

: VECT-BIG-FILL ( -- )
   VECT-BIG-VEC VEC:DISPOSE
   VECT-BIG-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-BIG-N 0 ?do i VECT-BIG-VEC VEC:PUSH drop loop
   VECT-BIG-VEC VECT-TLEN VECT-BIG-N T=
   VECT-BIG-VEC 0 VECT-TAT 0 T=
   VECT-BIG-VEC VECT-BIG-N 2 / VECT-TAT VECT-BIG-N 2 / T=
   VECT-BIG-VEC VECT-BIG-N 1 - VECT-TAT VECT-BIG-N 1 - T= ;

: VECT-T-CAP-ZERO ( -- )   0 VECT-T-FRESH ;               \ zero capacity allocation
: VECT-T-CAP-OVER ( -- )   MEM-MAX-CELLS 1 + VECT-T-FRESH ; \ overflowing capacity
: VECT-T-RESIZE-ZERO ( -- )
   2 VECT-T-FRESH  VECT-VEC 0 VECT-N>ITEM VEC:RESIZE ;        \ resize to zero capacity
: VECT-T-RESIZE-SHRINK ( -- )
   2 VECT-T-FRESH
   41 VECT-VEC VEC:PUSH drop  42 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-N>ITEM VEC:RESIZE ;                                     \ cap below active length
: VECT-T-GET-HIGH ( -- )
   2 VECT-T-FRESH  1 VECT-VEC VEC:PUSH drop
   VECT-VEC 1 VECT-TAT drop ;                                             \ index at length rejects

: VECT-EXPECT-REUSED ( ptr a -- ) {: old:ptr :}
   VECT-TMP-VEC VEC:DISPOSE
   VECT-TMP-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-TMP-VEC VEC:DATA@ old = TTRUE
   VECT-TMP-VEC VEC:DISPOSE ;

: VECT-RESIZE-RELEASES ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC VEC:DATA@ {: old:ptr :}
   VECT-REL-VEC 4 VECT-N>ITEM VEC:RESIZE
   old VECT-EXPECT-REUSED ;

: VECT-DISPOSE-RELEASES ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC VEC:DATA@ {: old:ptr :}
   VECT-REL-VEC VEC:DISPOSE
   old VECT-EXPECT-REUSED ;

: VECT-DISPOSE-CYCLE ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   41 VECT-REL-VEC VEC:PUSH drop
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   42 VECT-REL-VEC VEC:PUSH drop
   VECT-REL-VEC 0 VECT-TAT 42 T= ;
: VECT-T-DISPOSE-DOUBLE ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC VEC:DISPOSE  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC VECT-TCAP 0 T= ;
: VECT-T-DISPOSE-USE ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 0 VECT-TAT drop ;
: VECT-DISPOSE-USE-PUSH ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT  VECT-REL-VEC VEC:DISPOSE
   1 VECT-REL-VEC VEC:PUSH drop ;
: VECT-DISPOSE-USE-RESIZE ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 4 VECT-N>ITEM VEC:RESIZE ;
: VECT-DISPOSE-USE-DATA ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT  VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC VEC:DATA@ drop ;
: VECT-T-INIT-LIVE-REJECTS ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT ;

: VECT-RESIZE-FAIL-ATOMIC ( -- )
   VECT-REL-VEC VEC:DISPOSE
   VECT-REL-VEC 2 VECT-N>ITEM VEC:INIT
   41 VECT-REL-VEC VEC:PUSH drop  42 VECT-REL-VEC VEC:PUSH drop
   VECT-REL-VEC VEC:DATA@ {: before:ptr :}
   [: VECT-REL-VEC MEM-MAX-CELLS VECT-N>ITEM VEC:RESIZE ;] E-MEM-MAP TTHROWSQ
   VECT-REL-VEC VEC:DATA@ before = TTRUE
   VECT-REL-VEC VECT-TLEN 2 T=
   VECT-REL-VEC VECT-TCAP 2 T=
   VECT-REL-VEC 0 VECT-TAT 41 T=
   VECT-REL-VEC 1 VECT-TAT 42 T= ;

: VECT-RUN ( -- )
   T-RESET
   VECT-T-INIT
   VECT-T-PUSH
   VECT-T-SET
   VECT-T-CLEAR
   VECT-T-RESIZE
   VECT-T-ENSURE
   VECT-T-EACH
   VECT-POINTER
   VECT-BIG-FILL
   [: VECT-T-CAP-ZERO ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-CAP-OVER ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-RESIZE-ZERO ;] E-VEC-CAPACITY TTHROWSQ
   [: VECT-T-RESIZE-SHRINK ;] E-VEC-BOUNDS TTHROWSQ
   [: VECT-T-GET-HIGH ;] E-VEC-BOUNDS TTHROWSQ
   VECT-RESIZE-RELEASES
   VECT-DISPOSE-RELEASES
   VECT-DISPOSE-CYCLE
   VECT-T-DISPOSE-DOUBLE
   VECT-RESIZE-FAIL-ATOMIC
   [: VECT-T-DISPOSE-USE ;] E-VEC-STATE TTHROWSQ
   [: VECT-DISPOSE-USE-PUSH ;] E-VEC-STATE TTHROWSQ
   [: VECT-DISPOSE-USE-RESIZE ;] E-VEC-STATE TTHROWSQ
   [: VECT-DISPOSE-USE-DATA ;] E-VEC-STATE TTHROWSQ
   [: VECT-T-INIT-LIVE-REJECTS ;] E-VEC-STATE TTHROWSQ
   s" VOK-INIT ( ptr h CAD-NUM:item-count -- ) VEC:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-AT ( ptr h CAD-NUM:index -- a ) VEC:@"        CHECK-QUIET-CANDIDATE! -1 T=
   s" VOK-DATA ( ptr h -- ptr a ) VEC:DATA@"             CHECK-QUIET-CANDIDATE! -1 T=
   s" VSWAP-IDX-FOR-CNT ( ptr h CAD-NUM:index -- ) VEC:INIT"  VECT-CHECK-REJECTS
   s" VSWAP-CNT-FOR-IDX ( ptr h CAD-NUM:item-count -- a ) VEC:@" VECT-CHECK-REJECTS
   s" VSWAP-RAW-CAP ( ptr h n -- ) VEC:INIT"                  VECT-CHECK-REJECTS
   s" VSWAP-RAW-IDX ( ptr h n -- a ) VEC:@"                   VECT-CHECK-REJECTS
   T-REPORT ;

VECT-RUN

;package
