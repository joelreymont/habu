\ Direct JIT templates check each complete physical transfer, including spills.
require test/engine-stack-lifecycle.f

package STACK-LIFECYCLE-TEST

: JIT-SPILLS ( -- )
   s" a complete virtual-stack flush fits exactly" T-LABEL
   s" create BUF 32 allot : TWO ( -- ) 11 22 2drop ; : GO ( -- ) ['] TWO BUF 16 run-in-stack ; GO"
   CHILD-RC 0 T=
   s" a two-cell flush refuses one remaining cell" T-LABEL
   s" create BUF 32 allot : TWO ( -- ) 11 22 2drop ; : GO ( -- ) ['] TWO BUF 8 run-in-stack ; GO" REFUSED
   s" compiled address push needs one cell" T-LABEL
   s" create BUF 32 allot : ADDR ( -- ) BUF drop ; : GO ( -- ) ['] ADDR BUF 0 run-in-stack ; GO" REFUSED ;

: JIT-RETURNS ( -- )
   s" direct return transfer uses the last slot" T-LABEL
   s" : MOVE ( -- ) 11 >r r@ drop r> drop ; STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! MOVE data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 1 - = ."
   CHILD-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" direct return push refuses a full stack" T-LABEL
   s" : MOVE ( -- ) 11 >r r> drop ; STACK-ABI:RETURN-CELLS data-base RSP-CELL + ! MOVE" REFUSED
   s" direct return pop validates before indexing" T-LABEL
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r> drop ; MOVE" REFUSED
   s" direct return peek validates before indexing" T-LABEL
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" REFUSED ;

: JIT-LOOPS ( -- )
   s" a loop uses the last frame" T-LABEL
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES 1 - data-base LOOPSP-CELL + ! ONE data-base LOOPSP-CELL + @ STACK-ABI:LOOP-FRAMES 1 - = ."
   CHILD-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" loop entry refuses a full frame stack" T-LABEL
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES data-base LOOPSP-CELL + ! ONE" REFUSED
   s" loop index validates the live frame" T-LABEL
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! i drop loop ; ONE" REFUSED
   s" outer index validates both frames" T-LABEL
   s" : TWO ( -- ) 1 0 do 1 0 do 1 data-base LOOPSP-CELL + ! j drop loop loop ; TWO" REFUSED
   s" unloop validates before decrement" T-LABEL
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! unloop exit loop ; ONE" REFUSED ;

public
: RUN-JIT ( -- )
   T-RESET JIT-SPILLS JIT-RETURNS JIT-LOOPS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-JIT
