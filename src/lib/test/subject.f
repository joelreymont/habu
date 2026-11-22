split-str
\ subject.f - isolated evaluation of the running test subject (package SUBJECT).
\ RUN evaluates one counted source in a fork child, its own process root
\ (ERROR:CHILD-TOP), captures bounded stdout and stderr into the caller's
\ spans, enforces a deadline and answers the outcome. Copy-on-write keeps the
\ child's changes from the parent; `evaluate-closed` fails a source leaving cells.
\ STORAGE CLASS. PROCESS-WIDE: static buffers; one task runs a subject at a time.

require src/lib/error.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/fd.f

package SUBJECT

PTR-VARIABLE SRC-A
variable SRC-U
$4000 constant STATUS-CAP
10000 constant STATUS-MS
create STATUS-OUT STATUS-CAP allot
create STATUS-ERR STATUS-CAP allot

\ RUN is synchronous in the parent. The child inherits these cells by COW and
\ exits before the next RUN, so no source state is shared across cases.

: SRC! ( str -- )
   SRC-U !
   SRC-A ! ;

: SRC$ ( -- str )
   SRC-A @
   SRC-U @ LEN>N ;

: DUP2! ( fd n -- ) {: fd:fd dst:n :}
   fd FD>N dst dup2 dup 0 < if
      drop E-PROC-OUTPUT throw
   then
   drop ;

: CHILD-FDS ( -- )
   PROCESS:OUT-R PROCESS:CLOSE-CELL
   PROCESS:ERR-R PROCESS:CLOSE-CELL
   PROCESS:OUT-W @ >FD FD:STDOUT FD>N DUP2!
   PROCESS:ERR-W @ >FD FD:STDERR FD>N DUP2!
   PROCESS:OUT-W PROCESS:CLOSE-CELL
   PROCESS:ERR-W PROCESS:CLOSE-CELL ;

\ The child's diagnostics are its stderr, whatever buffer its parent printed
\ rejections into.
: EVAL ( -- )
   REJECTION:BUFFER-OFF
   SRC$ evaluate-closed ;

: CHILD ( -- )
   [: CHILD-FDS EVAL ;] ERROR:CHILD-TOP ;

: SPAWN ( -- )
   [: PROC-FORK:RAW dup PID>N 0= if CHILD then ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

public

: RUN ( str ^u8 len ^u8 len ms -- len len PROCESS:ending )
   {: src:^ srcu:n out:^ outcap:len err:^ errcap:len timeout:ms :}
   outcap errcap PROCESS:CAPTURE-CHECK-CAPS
   src srcu SRC!
   timeout PROCESS:CAPTURE-BEGIN
   SPAWN
   out outcap err errcap PROCESS:RUN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

: SHOW ( str str str -- )
   {: src:^ srcu:n out:^ outu:n err:^ erru:n :}
   s" program:" type cr src srcu type cr
   s" stdout:" type cr out outu type cr
   s" stderr:" type cr err erru type cr ;

\ The capture is the only evidence of where a child stopped before its deadline,
\ so the timeout arm prints the program and its streams, then throws
\ E-PROC-TIMEOUT so the row still reports a timeout.
: TIMED-OUT ( str str str -- )
   {: src:^ srcu:n out:^ outu:n err:^ erru:n :}
   s" timeout: the child passed its deadline" type cr
   src srcu out outu err erru SHOW
   E-PROC-TIMEOUT throw ;

\ A throw code cannot cross a process boundary, so a child that met a deadline
\ exits 124. Its parent passes the program, the captured stdout and
\ stderr and the exit code; on that code TIMED-OUT prints them and throws
\ E-PROC-TIMEOUT. Any other code is the caller's to assert.
: RC-TIMED-OUT ( str str str n -- )
   {: src:^ srcu:n out:^ outu:n err:^ erru:n rc:n :}
   rc 124 = if src srcu out outu err erru TIMED-OUT then ;

private

\ A signal is not an exit. A deadline is no verdict.
: STATUS ( str n -- len len bool )
   {: src:^ srcu:n want:n :}
   src srcu STATUS-OUT STATUS-CAP >LEN STATUS-ERR STATUS-CAP >LEN STATUS-MS >MS RUN
   {: outu:len erru:len oc :}
   outu erru
   oc MATCH PROCESS:ending
      exited OF want = ENDOF
      signaled OF drop false ENDOF
      timeout OF
         src srcu STATUS-OUT outu LEN>N STATUS-ERR erru LEN>N TIMED-OUT
      ENDOF
   ;MATCH ;

public

\ The capture is kept only for this answer; a case that reads the child's
\ streams runs RUN itself, or REJECTED for stderr when stdout stays empty.
: EXITED? ( str n -- bool )
   STATUS >r 2drop r> ;

\ src's stderr, valid until the next run, and whether src, run as the subject,
\ exited with code want and wrote nothing to stdout.
: REJECTED ( str n -- str bool )
   STATUS {: outu:len erru:len exited:bool :}
   STATUS-ERR erru LEN>N  exited outu LEN>N 0= and ;

\ Whether src, run as the subject, exited with code want, wrote nothing to
\ stdout and named why on stderr: a refusal the engine or the checker reports
\ before anything runs.
: REFUSED? ( str n str -- bool )
   {: why:^ whyu:n :}
   REJECTED {: err:^ erru:n ok:bool :}
   ok  err erru why whyu CONTAINS? and ;

;package
