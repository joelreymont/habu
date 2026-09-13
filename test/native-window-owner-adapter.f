\ The child installed a fresh checker and the current layout/include surface.
\ Explicit source loading prevents a product host's provided-file list from
\ turning the tier1 adapter regression into a baked compiler no-op.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/compiler/native/checker-owner.f

package OWNER-ADAPTER-CHECK

: TRUE! ( bool -- ) 0= if 79 throw then ;
: EQ! ( n n -- ) <> if 79 throw then ;

: RUN ( -- )
   tier@ 1 EQ!
   CHECKER-OWNER:BY-NAME? TRUE!
   CHECKER-OWNER:RECORD? TRUE!
   s" ADAPTER-SOURCE ( n -- n ) 1 +" CHECKER-OWNER:CHECK-UNJUDGED -1 EQ!
   s" ADAPTER-SOURCE" CHECKER-OWNER:QUERY TRUE!
   CHECKER-OWNER:DIN-CELLS 1 EQ!
   CHECKER-OWNER:DOUT-CELLS 1 EQ!
   CHECKER-OWNER:CAPTURE-PREPARE
   CHECKER-OWNER:BY-NAME? 0= TRUE!
   s" ADAPTER-SCAN ( n -- n ) 1 +" CHECKER-OWNER:CHECK -1 EQ!
   s" ADAPTER-SCAN" CHECKER-OWNER:QUERY TRUE!
   CHECKER-OWNER:DIN-N 1 EQ!
   CHECKER-OWNER:DOUT-N 1 EQ!
   CHECKER-OWNER:DIN-CELLS 1 EQ!
   CHECKER-OWNER:DOUT-CELLS 1 EQ!
   s" @" s" -- n" CHECKER-OWNER:DOES-CHECK -1 EQ!
   CHECKER-OWNER:DOES-IN 1 EQ!
   CHECKER-OWNER:DOES-OUT 1 EQ!
   CHECKER-OWNER:TAPE-DISARM ;

RUN
;package
