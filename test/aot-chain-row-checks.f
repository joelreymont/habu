\ Appended to a private copy of the production chain producer by
\ test/aot-chain-producer-suite.f. The copy adds null and externally located
\ DATA-pointer fixtures around the window and captures the real compiler once.
\ Each case then forks this captured process: the fork changes the live rows
\ and runs the producer's own check, and this process asserts the fork's exit
\ code and output with the words the suite asserts its host with. The row table
\ is private anonymous memory and the dictionary is copied on write, so every
\ fork starts from the captured rows and no change reaches the next case.
require lib/process.f
require lib/test.f
require lib/test/subject.f
require test/aot-chain-capture-lib.f

package AOT-CHAIN-SUITE
using AOT-WINDOW

: ROW ( n -- ptr u8 ) XTOFF-ROW * XTOFF-BUF@ + ;
: U32! ( n ptr u8 -- ) {: value:n dst:ptr :}
   4 0 ?do value i 8 * rshift dst i + c! loop ;

: FIXTURE-REFUSE ( -- )
   s" chain-address-rows: fixture does not exercise every intended row kind"
   75 die ;

: ?PORTABLE-CLOSURE ( -- )
   AOT-IDENT:COUNT 0= if FIXTURE-REFUSE then
   AOT-IDENT:COUNT 0 ?do
      i AOT-IDENT:PATH$ 0= if FIXTURE-REFUSE then
      c@ 47 = if
         s" chain-closure: captured a build-tree absolute path" 75 die
      then
   loop
   s" chain-closure: portable" type cr ;

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

\ The row changes, each made in a fork just before the producer's check.
: SWAP-ROWS ( -- )
   0 ROW AOT-CHAIN:ROW-U32@ 0 ROW 4 + AOT-CHAIN:ROW-U32@
   1 ROW AOT-CHAIN:ROW-U32@ 0 ROW U32!
   1 ROW 4 + AOT-CHAIN:ROW-U32@ 0 ROW 4 + U32!
   1 ROW 4 + U32! 1 ROW U32! ;
: DROP-ROW ( -- ) -1 XTOFF-N +! ;
: DUP-ROW ( -- ) 0 ROW 1 ROW XTOFF-ROW BYTE-COPY ;
: MOVE-LOCATION ( -- ) 0 ROW AOT-CHAIN:ROW-U32@ 8 + 0 ROW U32! ;
: FLIP-KIND ( -- )
   0 ROW 4 + dup AOT-CHAIN:ROW-U32@ XTOFF-DATA-TAG xor swap U32! ;
: MOVE-TARGET ( -- ) 0 ROW 4 + dup AOT-CHAIN:ROW-U32@ 1+ swap U32! ;
: NULL-TARGET ( -- )
   0 ROW 4 + dup AOT-CHAIN:ROW-U32@ XTOFF-DATA-TAG and swap U32! ;

\ Exercise the index beyond the former fixed row limit, with both signed
\ halves of the packed key space. These are index inputs, not a fake capture.
: LARGE-KEY ( n -- n n ) {: k:n :}
   k CELL * k 1 and 0<> if XTOFF-WINDOW-TAG or then
   k 1+ k 1 and 0= if XTOFF-DATA-TAG or then ;

: LARGE-INDEX ( -- )
   75900 XTOFF-RESERVE
   75900 XTOFF-N !
   XTOFF-N @ 0 ?do
      i LARGE-KEY i ROW 4 + U32! i ROW U32!
   loop
   AOT-CHAIN:INDEX-ROWS
   XTOFF-N @ 0 ?do i LARGE-KEY AOT-CHAIN:?EXACT-ROW loop
   AOT-CHAIN:ROW-INDEX-RELEASE ;

TYPED-VARIABLE CASE-XT [ -- ]

: FORK-CASE ( -- )
   s" AOT-CHAIN-SUITE:CASE-CHILD" OUT CAP >LEN ERR CAP >LEN CHILD-TIMEOUT-MS >MS
   SUBJECT:RUN PROC-OUTCOME>RC RC>N RC !
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: PRODUCER-CASE ( [ -- ] ptr u8 n n -- ) {: body a:ptr u:n want:n :}
   body CASE-XT !
   a u T-LABEL
   FORK-CASE
   want ROW-RC
   want 0= if s" chain-address-rows: ok" SAID? else
      s" declared address rows do not match the live window" ERR-SAID?
   then ;

\ The accepting controls run after the refusals, so a change that outlived its
\ fork would fail them.
: PRODUCER-CASES ( -- )
   [: DROP-ROW AOT-CHAIN:?XTOFF ;] s" missing" REFUSE-RC PRODUCER-CASE
   [: DUP-ROW AOT-CHAIN:?XTOFF ;] s" duplicate" REFUSE-RC PRODUCER-CASE
   [: MOVE-LOCATION AOT-CHAIN:?XTOFF ;] s" location" REFUSE-RC PRODUCER-CASE
   [: FLIP-KIND AOT-CHAIN:?XTOFF ;] s" kind" REFUSE-RC PRODUCER-CASE
   [: MOVE-TARGET AOT-CHAIN:?XTOFF ;] s" target" REFUSE-RC PRODUCER-CASE
   [: NULL-TARGET AOT-CHAIN:?XTOFF ;] s" null-target" REFUSE-RC PRODUCER-CASE
   [: AOT-CHAIN:?XTOFF ;] s" valid" 0 PRODUCER-CASE
   [: SWAP-ROWS AOT-CHAIN:?XTOFF ;] s" reorder" 0 PRODUCER-CASE
   [: LARGE-INDEX ;] s" index-scale" 0 PRODUCER-CASE ;

public
\ A fork's entry, which SUBJECT:RUN names in the child.
: CASE-CHILD ( -- )
   CASE-XT @ execute
   s" chain-address-rows: ok" type cr ;

: HOST-RUN ( -- )
   AOT-CHAIN:RUN
   ?PORTABLE-CLOSURE
   ?POPULATIONS
   T-RESET
   PRODUCER-CASES
   T-REPORT
   s" chain-address-rows: every case" type cr ;
;package

AOT-CHAIN-SUITE:HOST-RUN
