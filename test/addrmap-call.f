\ Direct JIT calls preserve the callee's address records without copying chains.
require lib/errors.f
require lib/test.f

0 set-tier
package ADDRMAP-CALL-TEST
private

\ Inspection is limited to emitted code and the engine's relocation bitmap.
: DATA-A ( -- ptr u8 ) data-base ;
: REGION-BASE ( -- n ) dbase@ ;
TRUSTED: CODE-A ( n -- ptr u8 ) ;

: ADDR-BIT@ ( n -- n ) {: at:n :}
   at REGION-BASE - {: off:n :}
   DATA-A SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

variable COUNT

: MARKS ( n n -- n ) {: from:n to:n :}
   0 COUNT !
   to from ?do i ADDR-BIT@ COUNT @ + COUNT ! 4 +loop
   COUNT @ ;

: W32@ ( n -- n ) {: at:n :}
   at CODE-A c@
   at 1+ CODE-A c@ 8 lshift or
   at 2 + CODE-A c@ 16 lshift or
   at 3 + CODE-A c@ 24 lshift or ;

\ AArch64 BL has top six bits $25 and a signed 26-bit instruction displacement.
: BL? ( n -- bool ) W32@ 26 rshift $25 = ;

: BL-TARGET ( n -- n ) {: at:n :}
   at W32@ $3FFFFFF and {: d:n :}
   d $2000000 >= if d $4000000 - else d then
   2 lshift at + ;

: CALLS-TO ( n n n -- n ) {: from:n to:n target:n :}
   0 COUNT !
   to from ?do
      i BL? if i BL-TARGET target = if 1 COUNT +! then then
   4 +loop
   COUNT @ ;

\ Each span starts at the live code cursor. No frame or body length is assumed.
variable P0  variable P1  variable P2  variable P3
variable P4  variable P5  variable P6

cp@ P0 !
create AMC-DATA 8 allot
cp@ P1 !
: AMC-ONE ( -- ptr n ) AMC-DATA ;
cp@ P2 !
: AMC-TWO ( -- ptr n ptr n ) AMC-DATA AMC-DATA ;
cp@ P3 !
: AMC-STEP ( n -- n ) 1 + ;
cp@ P4 !
: AMC-PLAIN ( n -- n ) AMC-STEP ;
cp@ P5 !
: AMC-NESTED ( -- ptr n ptr n ) AMC-TWO ;
cp@ P6 !

: TEST-CALLEE ( -- )
   s" the created word owns exactly one recorded address chain" T-LABEL
   P0 @ ADDR-BIT@ 1 T=
   P0 @ P1 @ MARKS 1 T= ;

: TEST-CALLS ( -- )
   s" direct calls reach the exact data word once or twice" T-LABEL
   P1 @ P2 @ P0 @ CALLS-TO 1 T=
   P2 @ P3 @ P0 @ CALLS-TO 2 T=
   s" ordinary and nested callees keep their own call boundaries" T-LABEL
   P4 @ P5 @ P3 @ CALLS-TO 1 T=
   P5 @ P6 @ P2 @ CALLS-TO 1 T= ;

: TEST-MAPS ( -- )
   s" calls do not invent address chains in their callers" T-LABEL
   P1 @ P2 @ MARKS 0 T=
   P2 @ P3 @ MARKS 0 T=
   P4 @ P5 @ MARKS 0 T=
   P5 @ P6 @ MARKS 0 T=
   P0 @ P1 @ MARKS 1 T= ;

: TEST-VALUES ( -- )
   s" data calls return the live storage and ordinary calls execute" T-LABEL
   $12345678 AMC-DATA !
   AMC-ONE @ $12345678 T=
   $23456789 AMC-ONE !
   AMC-DATA @ $23456789 T=
   AMC-TWO AMC-DATA = TTRUE AMC-DATA = TTRUE
   AMC-NESTED AMC-DATA = TTRUE AMC-DATA = TTRUE
   16 AMC-PLAIN 17 T= ;

public
: RUN ( -- )
   T-RESET
   TEST-CALLEE
   TEST-CALLS
   TEST-MAPS
   TEST-VALUES
   T-REPORT ;
;package

ADDRMAP-CALL-TEST:RUN
