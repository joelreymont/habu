\ Appended to a private copy of the production chain producer by its suite.
\ The copy adds null and externally located DATA-pointer fixtures around the
\ window, captures the real compiler, then runs these exact-row controls.
package AOT-CHAIN-ROW-TEST
using AOT-WINDOW

: ROW ( n -- ptr u8 ) XTOFF-ROW * XTOFF-BUF@ + ;
: U32! ( n ptr u8 -- ) {: value:n dst:ptr :}
   4 0 ?do value i 8 * rshift dst i + c! loop ;
: CASE? ( ptr u8 n -- bool ) 0 SCRIPT-ARGV$ STR= ;

: FIXTURE-REFUSE ( -- )
   s" chain-address-rows: fixture does not exercise every intended row kind"
   75 die ;

\ Source declarations must give the producer both location coordinates, both
\ target kinds, and a null DATA target; these are independent of today's count.
: ?POPULATIONS ( -- )
   0
   XTOFF-N @ 0 ?do
      i ROW AOT-CHAIN:ROW-U32@ XTOFF-WINDOW-TAG and 0= if 1 or then
      i ROW 4 + AOT-CHAIN:ROW-U32@ {: meta:n :}
      meta XTOFF-DATA-TAG and 0<> if
         meta XTOFF-VALUE-MASK and 0= if 4 or else 8 or then
      else 2 or then
   loop
   15 <> if FIXTURE-REFUSE then ;

: SWAP-ROWS ( -- )
   0 ROW AOT-CHAIN:ROW-U32@ 0 ROW 4 + AOT-CHAIN:ROW-U32@
   1 ROW AOT-CHAIN:ROW-U32@ 0 ROW U32!
   1 ROW 4 + AOT-CHAIN:ROW-U32@ 0 ROW 4 + U32!
   1 ROW 4 + U32! 1 ROW U32! ;

: ALTER ( -- )
   s" reorder" CASE? if SWAP-ROWS exit then
   s" missing" CASE? if -1 XTOFF-N +! exit then
   s" duplicate" CASE? if 0 ROW 1 ROW XTOFF-ROW BYTE-COPY exit then
   s" location" CASE? if 0 ROW AOT-CHAIN:ROW-U32@ 8 + 0 ROW U32! exit then
   s" kind" CASE? if
      0 ROW 4 + dup AOT-CHAIN:ROW-U32@ XTOFF-DATA-TAG xor swap U32! exit
   then
   s" target" CASE? if 0 ROW 4 + dup AOT-CHAIN:ROW-U32@ 1+ swap U32! exit then
   s" null-target" CASE? if
      0 ROW 4 + dup AOT-CHAIN:ROW-U32@ XTOFF-DATA-TAG and swap U32! exit
   then
   s" valid" CASE? 0= if FIXTURE-REFUSE then ;

\ Exercise the index at the format's actual row capacity, with both signed
\ halves of the packed key space. These are index inputs, not a fake capture.
: LARGE-KEY ( n -- n n ) {: k:n :}
   k CELL * XTOFF-WINDOW-TAG or
   k 1+ k 1 and 0= if XTOFF-DATA-TAG or then ;

: LARGE-INDEX ( -- )
   XTOFF-MAX XTOFF-N !
   XTOFF-N @ 0 ?do
      i LARGE-KEY i ROW 4 + U32! i ROW U32!
   loop
   AOT-CHAIN:INDEX-ROWS
   XTOFF-N @ 0 ?do i LARGE-KEY AOT-CHAIN:?EXACT-ROW loop
   AOT-CHAIN:ROW-INDEX-RELEASE ;

public
: RUN ( -- )
   AOT-CHAIN:RUN
   ?POPULATIONS
   s" index-scale" CASE? if LARGE-INDEX else
      ALTER
      AOT-CHAIN:?XTOFF
   then
   s" chain-address-rows: ok" type cr ;
;package

AOT-CHAIN-ROW-TEST:RUN
