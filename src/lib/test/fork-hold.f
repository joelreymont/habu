\ fork-hold.f - fork while another task holds process-wide state. fork copies
\ only its calling thread, so a lock or one-time setup another task holds is
\ copied as it stands, and the child's first use waits for ever unless the fork
\ held it or the child reset it (src/lib/fork-child.f). Cases:
\ src/lib/fork-child-test.f, src/lib/pg-fork-test.f. STORAGE CLASS. PROCESS-WIDE:
\ HELD and LET-GO are the handshake with the holder task; one case at a time.

require src/lib/errors.f
require src/lib/test.f
require src/lib/task.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/test/wait.f

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
\ rather than hang the suite. It prints nothing, because the holder waits here
\ too and a task asserts nothing: main names the deadline where it reads it.
: FLAG-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns HOLD-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ A FLAG-WAIT deadline main reads, its own or the holder's from the join, is
\ the row's own deadline: it is named and thrown, never asserted. Any other
\ code is answered.
: FLAG-EXPIRED ( n -- n )
   {: code:n :}
   code E-PROC-TIMEOUT = if s" fork-hold FLAG-WAIT" TEST-WAIT:EXPIRED then
   code ;

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
\ The parent closes its own write end, so the last child's exit closes the pipe:
\ the parent sees the children end without a wait that could block.
: END-CODE ( fd -- n ) {: rd:fd :}
   rd CHILD-MS >MS [: END-WAIT ;] catch {: code:n :}
   2drop
   code ;

: KILL-ALL ( n -- )
   0 ?do i PIDS @ SIGKILL PROC-KILL-RAW drop loop ;

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
   [: HELD 1 FLAG-WAIT ;] catch FLAG-EXPIRED {: code:n :}
   code 0<> if code throw then ;

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

\ Forks k children, at most CHILD-MAX, that each run op and exit. A child still
\ running at CHILD-MS is the row's own deadline, thrown once every child is
\ killed and reaped. Only a wait that completed has its status asserted under
\ label, with every child's exit 0.
: FORK-CHECK ( ptr u8 n [ -- ] n -- ) {: label:ptr u op k :}
   PIPE-PAIR {: rd:fd wr:fd :}
   op k SPAWN
   wr FD>N close
   rd END-CODE {: ended:n :}
   ended 0<> if k KILL-ALL then
   k REAP {: answered:n :}
   rd FD>N close
   ended E-PROC-TIMEOUT = if s" fork-hold child exit" TEST-WAIT:EXPIRED then
   label u T-LABEL
   ended 0 T=
   label u T-LABEL
   answered k T= ;

\ Lets the holder go and joins it; its body must have ended without a throw. A
\ holder whose FLAG-WAIT expired is the row's deadline, thrown before any
\ assertion. That holder has already let go of its state, so a case reads the
\ held state before RELEASE and asserts what it read after.
: RELEASE ( -- )
   1 LET-GO atomic!
   s" the holder let go" T-LABEL
   HOLDER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF FLAG-EXPIRED 0 T= ENDOF
   ;MATCH ;

;package
