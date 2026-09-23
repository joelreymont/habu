\ The exact carrier grammar shared by capture, artifact merge and stripped link.
require lib/test.f
require src/habu/layout.f
require src/habu/address-carrier.f
require lib/task.f

package DATA-ADDRESS-CODEC-TEST
using SNAP-RELOC
20 BUFFER: CODE

: W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   4 0 ?do w i 8 * rshift p i + c! loop ;
: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: PREFIX ( n -- ) {: rd:n :}
   $D2800000 rd or CODE W32!
   $F2A00000 rd or CODE 4 + W32! ;
: COMPACT ( n -- ) {: rd:n :}
   $D2C00000 rd or CODE W32!
   $F2A00000 rd or CODE 4 + W32!
   $F2800000 rd or CODE 8 + W32!
   $D503201F CODE 12 + W32! ;
: ABSOLUTE ( n -- ) {: rd:n :}
   rd PREFIX
   $F2C00000 rd or CODE 8 + W32!
   $F2E00000 rd or CODE 12 + W32! ;

: COMPACT-CASE ( n -- ) {: rd:n :}
   rd COMPACT
   CODE CODE 12 + CHAIN-SIZE DATA-CHAIN-BYTES T=
   CODE DATA-VA VA>N DATA-SIZE + DATA-CHAIN-BYTES SET-CHAIN-VALUE
   CODE DATA-CHAIN-BYTES CHAIN-VALUE DATA-VA VA>N DATA-SIZE + T=
   CODE $FFFFFFFFFFFF DATA-CHAIN-BYTES SET-CHAIN-VALUE
   CODE DATA-CHAIN-BYTES CHAIN-VALUE $FFFFFFFFFFFF T=
   \ Canonical capture offsets are raw, even when numerically below DATA-VA.
   CODE 17 DATA-CHAIN-BYTES SET-CHAIN-VALUE
   CODE DATA-CHAIN-BYTES CHAIN-VALUE 17 T=
   CODE 12 + W32@ $D503201F T=
   CODE CODE 12 + CHAIN-SIZE DATA-CHAIN-BYTES T= ;

: ABSOLUTE-CASE ( n -- ) {: rd:n :}
   rd ABSOLUTE
   CODE $123456789ABCDEF0 ADDR-CHAIN-BYTES SET-CHAIN-VALUE
   CODE CODE 16 + CHAIN-SIZE ADDR-CHAIN-BYTES T=
   CODE ADDR-CHAIN-BYTES CHAIN-VALUE $123456789ABCDEF0 T= ;

: MALFORMED ( -- )
   9 COMPACT
   12 0 ?do CODE CODE i + CHAIN-SIZE 0 T= loop
   \ A different register, halfword or opcode is not this carrier.
   $F2800008 CODE 8 + W32! CODE CODE 16 + CHAIN-SIZE 0 T=
   $F2A00009 CODE 8 + W32! CODE CODE 16 + CHAIN-SIZE 0 T=
   $D2800009 CODE 8 + W32! CODE CODE 16 + CHAIN-SIZE 0 T=
   9 COMPACT $F2A00008 CODE 4 + W32! CODE CODE 16 + CHAIN-SIZE 0 T=
   9 ABSOLUTE CODE CODE 15 + CHAIN-SIZE 0 T=
   $F2E00008 CODE 12 + W32! CODE CODE 16 + CHAIN-SIZE 0 T= ;

\ A worker's x20 points at its own header, not shared dictionary storage.
\ The rejected x20-relative carrier crashed this case inside TASK:ACTIVATE.
variable SHARED
TASK:MIN-STACK TASK:TASK WORKER
: WORK ( -- ) 1 SHARED atomic-add drop 0 TASK:RETURN ;
: SHARED-CASE ( -- )
   s" a worker reaches the same DATA cell as its caller" T-LABEL
   41 SHARED !
   ['] WORK WORKER TASK:ACTIVATE
   WORKER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH
   SHARED @ 42 T= ;

: MAIN ( -- )
   T-RESET
   19 0 ?do i COMPACT-CASE i ABSOLUTE-CASE loop
   MALFORMED SHARED-CASE
   T-REPORT ;
MAIN
;package
