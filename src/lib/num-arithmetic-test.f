\ num-arithmetic-test.f - the NUM arithmetic boundary and static rejection
\ matrix, suite `num-arithmetic` in the standard-library gate.
\ Run: bin/hb --load src/lib/num-arithmetic-test.f
\ Each classifier answers the some payload's raw value (nonnegative) or T-NONE,
\ so one `T=` pins both.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/property.f
require src/lib/adt/option.f
require src/lib/num-types.f
require src/lib/num-arithmetic.f

\ The bounds mirror the library's independently.
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell
$4000000000000000 constant T-LARGEST-POW2          \ the highest nonnegative bit
-1 constant T-NONE                                 \ a classifier's answer for none

\ The roles are read out here rather than by reopening the package under test.
CAST: T-BL>RAW ( NUM:byte-len -- n )
CAST: T-IC>RAW ( NUM:item-count -- n )
CAST: T-BO>RAW ( NUM:byte-off -- n )

: BL-CODE ( option<NUM:byte-len> -- n )
   MATCH option
      none OF T-NONE ENDOF
      some OF T-BL>RAW ENDOF
   ;MATCH ;
: IC-CODE ( option<NUM:item-count> -- n )
   MATCH option
      none OF T-NONE ENDOF
      some OF T-IC>RAW ENDOF
   ;MATCH ;
: BO-CODE ( option<NUM:byte-off> -- n )
   MATCH option
      none OF T-NONE ENDOF
      some OF T-BO>RAW ENDOF
   ;MATCH ;

\ A bad seed aborts the run.
: OKBL ( n -- NUM:byte-len ) NUM:BYTE-LEN MATCH option
   none OF 90 throw ENDOF some OF ENDOF ;MATCH ;
: OKIC ( n -- NUM:item-count ) NUM:ITEM-COUNT MATCH option
   none OF 90 throw ENDOF some OF ENDOF ;MATCH ;
: OKBO ( n -- NUM:byte-off ) NUM:BYTE-OFF MATCH option
   none OF 90 throw ENDOF some OF ENDOF ;MATCH ;

: RT-ADVANCE ( -- )
   5 OKBO 0 OKBL NUM:ADVANCE-BYTE-OFF BO-CODE 5 T=
   5 OKBO 7 OKBL NUM:ADVANCE-BYTE-OFF BO-CODE 12 T=
   T-MAX-N OKBO 0 OKBL NUM:ADVANCE-BYTE-OFF BO-CODE T-MAX-N T=
   T-MAX-N OKBO 1 OKBL NUM:ADVANCE-BYTE-OFF BO-CODE T-NONE T= ;

: RT-SUB ( -- )
   5 OKBL 5 OKBL NUM:SUB-BYTES BL-CODE 0 T=
   9 OKBL 4 OKBL NUM:SUB-BYTES BL-CODE 5 T=
   3 OKBL 4 OKBL NUM:SUB-BYTES BL-CODE T-NONE T=
   5 OKIC 5 OKIC NUM:SUB-ITEMS IC-CODE 0 T=
   9 OKIC 4 OKIC NUM:SUB-ITEMS IC-CODE 5 T=
   3 OKIC 4 OKIC NUM:SUB-ITEMS IC-CODE T-NONE T= ;

: RT ( -- )
   T-RESET
   RT-ADVANCE  RT-SUB
   T-REPORT ;
RT

\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected, 1 uncheckable.
: STAT ( -- )
   T-RESET
   s" GOOD-SUB ( NUM:byte-len NUM:byte-len -- option<NUM:byte-len> ) NUM:SUB-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-ADVANCE ( NUM:byte-off NUM:byte-len -- option<NUM:byte-off> ) NUM:ADVANCE-BYTE-OFF"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" BAD-ADVANCE-REV ( NUM:byte-len NUM:byte-off -- option<NUM:byte-off> ) NUM:ADVANCE-BYTE-OFF"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-SUB ( n n -- option<NUM:byte-len> ) NUM:SUB-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-CROSS-SUB ( NUM:byte-len NUM:item-count -- option<NUM:byte-len> ) NUM:SUB-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT

package NUM-ARITH-TEST
private

: OFFSET-BOUNDS ( -- )
   0 OKBO 0 OKBL NUM:BYTE-OFF-IN-LEN? TFALSE
   0 OKBO 1 OKBL NUM:BYTE-OFF-IN-LEN? TTRUE
   1 OKBO 1 OKBL NUM:BYTE-OFF-IN-LEN? TFALSE
   $10000 OKBO 1 OKBL NUM:BYTE-OFF-IN-LEN? TFALSE
   1 OKBO $10000 OKBL NUM:BYTE-OFF-IN-LEN? TTRUE
   T-MAX-N 1- OKBO T-MAX-N OKBL NUM:BYTE-OFF-IN-LEN? TTRUE
   T-MAX-N OKBO T-MAX-N OKBL NUM:BYTE-OFF-IN-LEN? TFALSE ;

variable LOWER-PAIR-N
variable UPPER-PAIR-N
variable CROSS-PAIR-N
variable ONE-BITS
variable ZERO-BITS

: COVERAGE-RESET ( -- )
   0 LOWER-PAIR-N !
   0 UPPER-PAIR-N !
   0 CROSS-PAIR-N !
   0 ONE-BITS !
   0 ZERO-BITS ! ;

: HIGH-BAND? ( n -- bool )
   T-LARGEST-POW2 >= ;

: NOTE-PAIR-BAND ( n n -- )
   {: x:n y:n :}
   x HIGH-BAND? if
      y HIGH-BAND? if
         1 UPPER-PAIR-N +!
      else
         1 CROSS-PAIR-N +!
      then
   else
      y HIGH-BAND? if
         1 CROSS-PAIR-N +!
      else
         1 LOWER-PAIR-N +!
      then
   then ;

: NOTE-BIT-COVERAGE ( n -- )
   {: value:n :}
   value ONE-BITS @ or ONE-BITS !
   value T-MAX-N xor ZERO-BITS @ or ZERO-BITS ! ;

: COVERAGE-CHECK ( -- )
   LOWER-PAIR-N @ 0 > TTRUE
   UPPER-PAIR-N @ 0 > TTRUE
   CROSS-PAIR-N @ 0 > TTRUE
   ONE-BITS @ T-MAX-N T=
   ZERO-BITS @ T-MAX-N T= ;

: CMP-PROP-ROW ( n n -- )
   {: x:n y:n :}
   x y < if
      x OKBO y OKBL NUM:BYTE-OFF-IN-LEN? TTRUE
   else
      x OKBO y OKBL NUM:BYTE-OFF-IN-LEN? TFALSE
   then ;

\ Three 31-bit draws span 0..T-MAX-N. The third draw's high bit supplies
\ cell bit 62; the alternating low bit is not used.
: RND-N ( -- n )
   PROP:RND
   PROP:RND 31 lshift or
   PROP:RND 32 lshift T-LARGEST-POW2 and or
   dup NOTE-BIT-COVERAGE ;

: PROPERTY-ROW ( -- )
   RND-N RND-N {: x:n y:n :}
   x y NOTE-PAIR-BAND
   x y CMP-PROP-ROW ;

: PROPERTIES ( -- )
   314159 512 PROP:RUN-RESET
   COVERAGE-RESET
   PROP:COUNT@ 0 ?do
      PROPERTY-ROW
   loop
   COVERAGE-CHECK ;

\ The advance is called by prefix throughout, as NUM:BYTE+: a bare `BYTE+` is
\ the raw primitive.
\ Byte i of the buffer carries the value i, so the byte read back after advancing
\ by i IS the offset that was advanced by - a wrong or ignored offset shows up as
\ a wrong byte rather than as a crash.
create BP-BUF 16 allot

: BP-FILL ( -- )
   16 0 ?do  i BP-BUF i + c!  loop ;

: BP-AT ( n -- n ) {: off:n :}
   BP-BUF off OKBO NUM:BYTE+ c@ ;

: POINTER-ADVANCE ( -- )
   BP-FILL
   0 BP-AT 0 T=                                 \ a zero advance stays put
   1 BP-AT 1 T=
   7 BP-AT 7 T=
   15 BP-AT 15 T= ;                             \ the last byte of the buffer

: STATIC-GOOD ( -- )
   s" GOOD-BYTE-BOUND ( NUM:byte-off NUM:byte-len -- bool ) NUM:BYTE-OFF-IN-LEN?"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-BYTE-ADVANCE ( ^u8 NUM:byte-off -- ^u8 ) NUM:BYTE+"
      CHECK-QUIET-CANDIDATE! -1 T= ;

: STATIC-BAD ( -- )
   s" BAD-BYTE-BOUND-SWAP ( NUM:byte-len NUM:byte-off -- bool ) NUM:BYTE-OFF-IN-LEN?"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ADVANCE-RAW-OFF ( ^u8 n -- ^u8 ) NUM:BYTE+"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ADVANCE-BYTE-LEN ( ^u8 NUM:byte-len -- ^u8 ) NUM:BYTE+"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ADVANCE-INDEX ( ^u8 NUM:index -- ^u8 ) NUM:BYTE+"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ADVANCE-RAW-PTR ( n NUM:byte-off -- ^u8 ) NUM:BYTE+"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: STATIC-PRIVATE ( -- )
   s" PRIVATE-INDEX-PROJ ( NUM:index -- n ) NUM:INDEX>N"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" PRIVATE-BYTE-OFF-PROJ ( NUM:byte-off -- n ) NUM:BYTE-OFF>N"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   OFFSET-BOUNDS POINTER-ADVANCE PROPERTIES
   STATIC-GOOD STATIC-BAD STATIC-PRIVATE
   T-REPORT ;

RUN
;package
