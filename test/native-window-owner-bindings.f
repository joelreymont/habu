\ The fresh window source-loads the adapter at the ordinary tier. Internal
\ pre-hook callbacks must bind through their owner without taking a public tick.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/compiler/native/checker-owner.f

package OWNER-BINDING-CHECK

: TRUE! ( bool -- ) 0= if 79 throw then ;
: EQ! ( n n -- ) <> if 79 throw then ;
: CLAUSE-SUFFIX$ ( -- ptr u8 n ) DOES-CLAUSE:SUFFIX$ ;

\ A row a check records by hand binds nowhere in compiled code (src/core/checker.f
\ CK-CLOSE!), so the query asks about a word this window compiled.
: BINDING-WORD ( n -- n ) 1 + ;

: RUN ( -- )
   tier@ 0 EQ!
   CLAUSE-SUFFIX$ s" ;does" CORE-STR= TRUE!
   CHECKER-OWNER:BY-NAME? TRUE!
   s" BINDING-SCAN ( n -- n ) 1 +" CHECKER-OWNER:CHECK-UNJUDGED -1 EQ!
   s" BINDING-WORD" CHECKER-OWNER:QUERY TRUE!
   CHECKER-OWNER:DIN-CELLS 1 EQ!
   CHECKER-OWNER:DOUT-CELLS 1 EQ! ;

RUN
;package
