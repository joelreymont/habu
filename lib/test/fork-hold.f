\ fork-hold.f - fork while another task holds process-wide state.
\
\ fork copies only the thread that calls it. A lock another task holds, or a
\ one-time setup another task is part way through, is copied into the child as
\ it stands, and the child has no thread to finish it: the child's first use
\ waits for ever, unless the fork held it or the child reset it
\ (lib/fork-child.f). A case here holds that state in a task of its own (HOLD),
\ forks children that reach it through its owner's public words (FORK-CHECK)
\ and lets go (RELEASE). lib/fork-child-test.f and lib/pg-fork-test.f are the
\ cases.
\
\ NOTHING IS LEFT. A child that does not end inside CHILD-MS is sent SIGKILL,
\ every child is reaped, and the holder lets go before it is joined, whatever
\ the children did. The parent closes its copy of a pipe's write end, so the
\ children's copies are the last: the last child's exit closes it, which is how
\ the parent sees them end without a wait that could block.

require lib/errors.f
require lib/test.f
require lib/task.f
require lib/process.f
require lib/process-fork.f

package FORK-HOLD

private

10000 constant CHILD-MS                  \ the children's work and exit; a held lock never ends them
30000000000 constant HOLD-WAIT-NS        \ one handshake wait, past CHILD-MS and the reap
1 constant CHILD-THREW-RC
32 constant CHILD-MAX

variable HELD                            \ 1 once the holder holds its state
variable LET-GO                          \ 1 once the children are reaped
TYPED-VARIABLE BODY [ -- ]
CHILD-MAX TYPED-BUFFER PIDS pid

TASK:MIN-STACK TASK:TASK HOLDER

\ A wait the other side ends; past HOLD-WAIT-NS it throws E-PROC-TIMEOUT
\ rather than hang the suite.
: FLAG-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns HOLD-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ Runs INSIDE the holder, so it asserts nothing: RELEASE's join answers a throw.
: HOLDER-RUN ( -- )
   BODY @ execute
   0 TASK:RETURN ;

\ A child's whole life. A throw must not leave it: it would unwind the child's
\ copy of the forking thread's stack, so the exit code carries it.
: CHILD ( [ -- ] -- )
   catch {: code:n :}
   s" " code 0= if 0 else CHILD-THREW-RC then die ;

: SPAWN ( [ -- ] n -- ) {: op k :}
   k 0 ?do
      PROC-FORK:CHECKED dup PID>N 0= if drop op CHILD then
      i PIDS !
   loop ;

: END-WAIT ( fd ms -- fd ms ) {: rd:fd t :}
   rd t POLL-IN-OR-TIMEOUT drop
   rd t ;

\ Zero when every child's end of the pipe closed - every child exited - inside
\ CHILD-MS; else what the wait threw, E-PROC-TIMEOUT for a child still running.
: END-CODE ( fd -- n ) {: rd:fd :}
   rd CHILD-MS >MS [: END-WAIT ;] catch {: code:n :}
   2drop
   code ;

: KILL-ALL ( n -- )
   0 ?do i PIDS @ SIGKILL PROC-KILL-RAW drop loop ;

\ Reaps the first n children and answers how many of them exited 0.
: REAP ( n -- n )
   0 swap 0 ?do
      i PIDS @ PROC-WAIT-RC MATCH result
         ok OF ENDOF
         err OF ENDOF
      ;MATCH
      0= if 1 + then
   loop ;

public

\ Runs body in the holder task and returns once body has called HOLDING or
\ HAMMER, which is when it holds its state.
: HOLD ( [ -- ] -- )
   BODY !
   0 HELD atomic!
   0 LET-GO atomic!
   ['] HOLDER-RUN HOLDER TASK:ACTIVATE
   HELD 1 FLAG-WAIT ;

\ A holder body calls this while it holds its state: HOLD returns, and this
\ returns once RELEASE lets go.
: HOLDING ( -- )
   1 HELD atomic!
   LET-GO 1 FLAG-WAIT ;

\ A holder body for state no public word holds still: op takes it and lets go
\ again, over and over until RELEASE, so a fork lands inside it some of the time.
: HAMMER ( [ -- ] -- ) {: op :}
   1 HELD atomic!
   begin LET-GO atomic@ 0= while op execute repeat ;

\ Forks k children, at most CHILD-MAX, that each run op and exit, and asserts
\ under label that all of them ended inside CHILD-MS and that every one exited 0.
: FORK-CHECK ( ptr u8 n [ -- ] n -- ) {: label:ptr u op k :}
   PIPE-PAIR {: rd:fd wr:fd :}
   op k SPAWN
   wr FD>N close
   rd END-CODE {: ended:n :}
   ended 0<> if k KILL-ALL then
   k REAP {: answered:n :}
   rd FD>N close
   label u T-LABEL
   ended 0 T=
   label u T-LABEL
   answered k T= ;

\ Lets the holder go and joins it; its body must have ended without a throw.
: RELEASE ( -- )
   1 LET-GO atomic!
   s" the holder let go" T-LABEL
   HOLDER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

;package
