\ Plant and verify canaries in the live return stack before APP-IMAGE:SAVE.
\ The stack is a guarded mapping outside DATA (src/habu/stack-abi.f), so the
\ parent proves the image carries neither canary and zero base cells.
require lib/memory.f
require src/habu/stack-abi.f
require test/snapshot-writer-poison-canaries.f

package SNAP-WRITER-POISON

\ The live return stack: the mapping the engine published in its base cell.
: RETURN-STACK ( -- ptr u8 )
   data-base STACK-ABI:RETURN-BASE-CELL + @ MEM-MAPPED>PTR ;

\ The bottom slot is free at the top level (depth 0) and the top slot is never
\ reached by a fixture this shallow. The planted values are the constants
\ inverted, so the constants' own cells in DATA never match the parent's scan.
: LO-SLOT ( -- ptr n ) RETURN-STACK CELL-VIEW ;
: HI-SLOT ( -- ptr n ) RETURN-STACK STACK-ABI:RETURN-BYTES 8 - + CELL-VIEW ;

: PLANT ( -- )
   LO-CANARY invert LO-SLOT !
   HI-CANARY invert HI-SLOT ! ;

: PROVE-PLANTED ( -- )
   LO-SLOT @ LO-CANARY invert <> if
      s" snapshot writer low return-stack poison failed" 70 die
   then
   HI-SLOT @ HI-CANARY invert <> if
      s" snapshot writer high return-stack poison failed" 70 die
   then ;

: POISON ( -- )
   PLANT
   PROVE-PLANTED ;

POISON

;package
