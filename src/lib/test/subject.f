\ subject.f - isolated evaluation of the running test subject (package SUBJECT).
\ RUN evaluates one counted source in a fork child, its own process root
\ (PROC-FORK:CHILD-MAIN), captures bounded stdout and stderr into the caller's
\ spans, enforces a deadline and answers the outcome. Copy-on-write keeps the
\ child's changes from the parent; `evaluate-closed` fails a source leaving cells.
\ STORAGE CLASS. PROCESS-WIDE: static buffers; one task runs a subject at a time.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/fd.f

package SUBJECT

PTR-VARIABLE SRC-A
variable SRC-U
PTR-VARIABLE STACK-A
$4000 constant STATUS-CAP
10000 constant STATUS-MS
create STATUS-OUT STATUS-CAP allot
create STATUS-ERR STATUS-CAP allot

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
      STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop STACK!
   then
   STACK@ STACK-ABI:PAGE-BYTES ;

: DUP2! ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if
      drop E-PROC-OUTPUT throw
   then
   drop ;

: CHILD-FDS ( -- )
   PROC-OUT-R PROC-CLOSE-CELL
   PROC-ERR-R PROC-CLOSE-CELL
   PROC-OUT-W @ >FD FD:STDOUT FD>N DUP2!
   PROC-ERR-W @ >FD FD:STDERR FD>N DUP2!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: EVAL ( -- )
   SRC$ evaluate-closed ;

\ `run-in-stack` owns the active allocation, including its floor and capacity.
: EVAL-STACK ( -- )
   ['] EVAL STACK run-in-stack ;

: CHILD ( -- )
   [: CHILD-FDS EVAL-STACK ;] PROC-FORK:CHILD-MAIN ;

: SPAWN ( -- )
   PROC-FORK:RAW {: pid:pid :}
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

: SHOW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n :}
   s" program:" type cr src srcu type cr
   s" stdout:" type cr out outu type cr
   s" stderr:" type cr err erru type cr ;

\ The capture is the only evidence of where a child stopped before its deadline,
\ so the timeout arm prints the program and its streams, then throws
\ E-PROC-TIMEOUT so the row still reports a timeout.
: TIMED-OUT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n :}
   s" timeout: the child passed its deadline" type cr
   src srcu out outu err erru SHOW
   E-PROC-TIMEOUT throw ;

\ A throw code cannot cross a process boundary, so a child that met a deadline
\ exits PROC-TIMEOUT-RC. Its parent passes the program, the captured stdout and
\ stderr and the exit code; on that code TIMED-OUT prints them and throws
\ E-PROC-TIMEOUT. Any other code is the caller's to assert.
: RC-TIMED-OUT ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n rc:n :}
   rc PROC-TIMEOUT-RC = if src srcu out outu err erru TIMED-OUT then ;

private

\ A signal is not an exit. A deadline is no verdict (docs/gate.md "How a suite
\ runs, and what that demands of its files").
: STATUS ( ptr u8 n n -- len len bool )
   {: src:ptr srcu:n want:n :}
   src srcu STATUS-OUT STATUS-CAP >LEN STATUS-ERR STATUS-CAP >LEN STATUS-MS >MS RUN
   {: outu:len erru:len oc :}
   outu erru
   oc MATCH outcome
      exited OF want = ENDOF
      signaled OF drop false ENDOF
      timeout OF
         src srcu STATUS-OUT outu LEN>N STATUS-ERR erru LEN>N TIMED-OUT
      ENDOF
   ;MATCH ;

public

\ The capture is kept only for this answer; a case that reads the child's
\ streams runs RUN itself.
: EXITED? ( ptr u8 n n -- bool )
   STATUS >r 2drop r> ;

\ Whether src, run as the subject, exited with code want, wrote nothing to
\ stdout and named why on stderr: a refusal the engine or the checker reports
\ before anything runs.
: REFUSED? ( ptr u8 n n ptr u8 n -- bool )
   {: src:ptr srcu:n want:n why:ptr whyu:n :}
   src srcu want STATUS {: outu:len erru:len exited:bool :}
   exited outu LEN>N 0= and
   STATUS-ERR erru LEN>N why whyu CONTAINS? and ;

;package
