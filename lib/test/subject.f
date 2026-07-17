\ subject.f - isolated evaluation of the running test subject.

require lib/errors.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f

package SUBJECT

1 constant OUT-FD
2 constant ERR-FD

PTR-VARIABLE SRC-A
variable SRC-U
PTR-VARIABLE STACK-A

\ RUN is synchronous in the parent. The child inherits these cells by COW and
\ exits before the next RUN, so no source or stack state is shared across cases.

: SRC! ( ptr u8 n -- )
   SRC-U !
   SRC-A ! ;

: SRC$ ( -- ptr u8 n )
   SRC-A @
   SRC-U @ LEN>N ;

: STACK! ( ptr u8 -- )
   STACK-A ! ;

: STACK@ ( -- ptr u8 )
   STACK-A @ ;

: STACK ( -- ptr u8 n )
   STACK@ 0= if
      MEM-ALLOC-64K drop STACK!
   then
   STACK@ 1 MEM-64K-BYTES ;

: DUP2! ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if
      drop E-PROC-OUTPUT throw
   then
   drop ;

: CHILD-FDS ( -- )
   PROC-OUT-R PROC-CLOSE-CELL
   PROC-ERR-R PROC-CLOSE-CELL
   PROC-OUT-W @ OUT-FD DUP2!
   PROC-ERR-W @ ERR-FD DUP2!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

\ `evaluate` is deliberately checker-rejected; this is the single dynamic-source
\ boundary. The active capability dot is habu-type-isolated-dynamic-244c0e2c.
TRUSTED: EVAL ( -- )
   SRC$ evaluate ;

\ `run-in-stack` installs S0 but cannot clear inherited engine recovery state.
\ The fork child must enter evaluation with no catch or TTY recovery handler.
\ This raw runtime-cell boundary is tracked by the same active capability dot.
TRUSTED: STACK-ARM ( -- )
   STACK@ data-base S0-CELL + !
   0 data-base HND-CELL + !
   0 data-base REPLH-CELL + ! ;

: EVAL-STACK ( -- )
   ['] EVAL STACK run-in-stack ;

: CHILD-EXIT ( n -- ) {: rc:n :}
   s" " rc die ;

: CHILD ( -- )
   STACK-ARM
   CHILD-FDS
   EVAL-STACK
   0 CHILD-EXIT ;

: SPAWN ( -- )
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PID>N 0= if CHILD then
   pid PROC-CAPTURE-PID!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

public

: RUN ( ptr u8 n ptr u8 len ptr u8 len ms -- len len outcome )
   {: src:ptr srcu:n out:ptr outcap:len err:ptr errcap:len timeout:ms :}
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   src srcu SRC!
   STACK 2drop
   timeout PROC-CAPTURE-BEGIN
   SPAWN
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

;package
