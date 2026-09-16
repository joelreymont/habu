\ Direct JIT templates check each complete physical transfer, including spills.
require test/engine-stack-lifecycle.f

package STACK-LIFECYCLE-TEST

\ The two overflow cases below use the ratchet test/engine-stack-wide.f
\ RATCHET-TAIL builds and documents: `begin dup <transfer> recurse again`
\ declared `( n -- )` leaves the stack one cell deeper per level, so the
\ transfer runs thousands of times and is itself what fills the guarded
\ stack, and the first write past the capacity is the transfer's own -- the
\ ratchet still fits at that depth and the transfer's whole width does not.
\ Spelled out here rather than shared: this fixture builds its child sources
\ as literals and does not carry lib/string.f.
: JIT-SPILLS ( -- )
   s" a complete virtual-stack flush fits on a guarded stack" T-LABEL
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : TWO ( -- ) 11 22 2drop ; : GO ( -- ) ['] TWO BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   CHILD-RC 0 T=
   s" a two-cell flush past the capacity faults the data guard page" T-LABEL
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : TWO ( -- ) 11 22 2drop ; : RATCHET ( n -- ) begin dup TWO recurse again ; : RATCHET0 ( -- ) 1 RATCHET ; : GO ( -- ) ['] RATCHET0 BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   REFUSED-DATA
   s" a compiled address push past the capacity faults the data guard page" T-LABEL
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : ADDR ( -- ) BUF drop ; : RATCHET ( n -- ) begin dup ADDR recurse again ; : RATCHET0 ( -- ) 1 RATCHET ; : GO ( -- ) ['] RATCHET0 BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   REFUSED-DATA ;

: JIT-RETURNS ( -- )
   s" direct return transfer uses the last slot" T-LABEL
   s" : MOVE ( -- ) 11 >r r@ drop r> drop ; STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! MOVE data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 1 - = ."
   CHILD-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" direct return push refuses a full stack" T-LABEL
   s" : MOVE ( -- ) 11 >r r> drop ; STACK-ABI:RETURN-CELLS data-base RSP-CELL + ! MOVE" REFUSED-RETURN
   s" direct return pop validates before indexing" T-LABEL
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r> drop ; MOVE" REFUSED-RETURN
   s" direct return peek validates before indexing" T-LABEL
   s" : MOVE ( -- ) 11 >r 0 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" REFUSED-RETURN ;

: JIT-LOOPS ( -- )
   s" a loop uses the last frame" T-LABEL
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES 1 - data-base LOOPSP-CELL + ! ONE data-base LOOPSP-CELL + @ STACK-ABI:LOOP-FRAMES 1 - = ."
   CHILD-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" loop entry refuses a full frame stack" T-LABEL
   s" : ONE ( -- ) 1 0 do i drop loop ; STACK-ABI:LOOP-FRAMES data-base LOOPSP-CELL + ! ONE" REFUSED-LOOP
   s" loop index validates the live frame" T-LABEL
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! i drop loop ; ONE" REFUSED-LOOP
   s" outer index validates both frames" T-LABEL
   s" : TWO ( -- ) 1 0 do 1 0 do 1 data-base LOOPSP-CELL + ! j drop loop loop ; TWO" REFUSED-LOOP
   \ unloop only moves the loop cursor (J-UNLOOP decrements LOOPSP-CELL, a
   \ plain DATA-region cell), so a cursor below the base is not a fault by
   \ itself: the same source with nothing after the unloop returns cleanly.
   \ What faults is the next operation that touches the frame stack at the
   \ moved cursor -- and in this source that is the inner DO opening a frame
   \ one below the base, a WRITE, before `i` ever reads one. Measured: with
   \ the inner body reduced to `1 0 do loop` the child still exits
   \ ENGINE-ERROR:STACK-BOUNDS (loop). `i drop` stays in the case so the read
   \ is covered too if the write ever stops faulting.
   s" unloop then a frame write below the base" T-LABEL
   s" : ONE ( -- ) 1 0 do 0 data-base LOOPSP-CELL + ! unloop 1 0 do i drop loop exit loop ; ONE" REFUSED-LOOP ;

public
: RUN-JIT ( -- )
   T-RESET JIT-SPILLS JIT-RETURNS JIT-LOOPS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-JIT
