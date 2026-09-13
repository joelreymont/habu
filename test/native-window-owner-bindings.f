\ The fresh window source-loads the adapter at the ordinary tier. Internal
\ pre-hook callbacks must bind through their owner without taking a public tick.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/compiler/native/checker-owner.f

package OWNER-BINDING-CHECK

: TRUE! ( bool -- ) 0= if 79 throw then ;
: EQ! ( n n -- ) <> if 79 throw then ;

: RUN ( -- )
   tier@ 0 EQ!
   CHECKER-OWNER:BY-NAME? TRUE!
   s" BINDING-SCAN ( n -- n ) 1 +" CHECKER-OWNER:CHECK-UNJUDGED -1 EQ!
   s" BINDING-SCAN" CHECKER-OWNER:QUERY TRUE!
   CHECKER-OWNER:DIN-CELLS 1 EQ!
   CHECKER-OWNER:DOUT-CELLS 1 EQ! ;

RUN
;package
