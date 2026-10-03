\ process-tree-fork-test.f - a forked child does not inherit a walk.
\
\     bin/hb --load lib/process-tree-fork-test.f
\
\ fork copies only the thread that calls it. While another task holds WALKING
\ - inside a KILL-TREE, a CATCHES? or a CPU-NS - the child's copy of the cell
\ stays held with no thread there to release it, so the child's first walk or
\ query waits for ever: a process forked while another task's capture ended
\ its child early, say, could never end a capture of its own.
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. THE CHILD WAITS FOR EVER. Nothing releases the inherited lock, and the
\     child's first query sleeps in WALK-GET.
\  2. THE PARENT LOSES ITS LOCK. A reset run on the parent's side of the fork
\     clears the holder's WALKING, and a third task could then walk beside it.
\
\ Asserted: a child forked while a task of this process holds WALKING ends its
\ CPU-NS of itself inside CHILD-MS and exits 0; and the parent still finds
\ WALKING held by that task afterwards. The holder takes the lock through
\ WALK-GET, the word every walk takes it with, and the parent reads WALKING
\ itself: both are PROC-TREE's own, so this file reopens the package.
\
\ NOTHING IS LEFT. A child that does not end inside CHILD-MS is sent SIGKILL
\ and reaped, and the holder lets go before it is joined, whatever the child
\ did. The parent closes its copy of a pipe's write end, so the child's is the
\ last: the child's exit closes it, which is how the parent sees it end
\ without a wait that could block.

require lib/errors.f
require lib/test.f
require lib/task.f
require lib/process.f
require lib/process-fork.f
require lib/process-tree.f

package PROC-TREE

10000 constant CHILD-MS                  \ a child's query and exit; a held lock never ends them
30000000000 constant HOLD-WAIT-NS        \ one handshake wait, past CHILD-MS and the reap
1 constant CHILD-THREW-RC

variable HELD                            \ 1 once the holder has the lock
variable LET-GO                          \ 1 once the child is reaped

TASK:MIN-STACK TASK:TASK HOLDER

\ A wait the other side ends; past HOLD-WAIT-NS it throws E-PROC-TIMEOUT
\ rather than hang the suite.
: FLAG-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns HOLD-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ Runs INSIDE the holder, so it asserts nothing: the join answers its throw.
: HOLDER-BODY ( -- )
   WALK-GET
   1 HELD atomic!
   [: LET-GO 1 FLAG-WAIT ;] [: WALK-RELEASE ;] finally
   0 TASK:RETURN ;

: CHILD-QUERY ( -- )
   getpid >PID CPU-NS drop ;

\ The child's whole life. A throw must not leave it: it would unwind the
\ child's copy of this thread's stack, so the exit code carries it.
: CHILD ( -- )
   [: CHILD-QUERY ;] catch {: code:n :}
   s" " code 0= if 0 else CHILD-THREW-RC then die ;

: END-WAIT ( fd ms -- fd ms ) {: rd:fd t :}
   rd t POLL-IN-OR-TIMEOUT drop
   rd t ;

\ Zero when the child's end of the pipe closed - the child exited - inside
\ CHILD-MS; else what the wait threw, E-PROC-TIMEOUT for a child still running.
: END-CODE ( fd -- n ) {: rd:fd :}
   rd CHILD-MS >MS [: END-WAIT ;] catch {: code:n :}
   2drop
   code ;

: JOIN-HOLDER ( -- )
   HOLDER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: FORK-UNDER-WALK ( -- )
   0 HELD !  0 LET-GO !
   ['] HOLDER-BODY HOLDER TASK:ACTIVATE
   HELD 1 FLAG-WAIT
   PIPE-PAIR {: rd:fd wr:fd :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if CHILD then
   wr FD>N close
   rd END-CODE {: ended:n :}
   ended 0<> if pid SIGKILL PROC-KILL-RAW drop then
   pid PROC-WAIT-RC MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH {: rc:n :}
   rd FD>N close
   WALKING atomic@ {: held:n :}
   1 LET-GO atomic!
   s" the holder kept the lock and let it go" T-LABEL
   JOIN-HOLDER
   s" a child forked under a held walk ended its query in time" T-LABEL
   ended 0 T=
   s" the child's query answered" T-LABEL
   rc 0 T=
   s" the parent's lock was still the holder's" T-LABEL
   held 1 T= ;

: FORK-TEST-MAIN ( -- )
   T-RESET
   FORK-UNDER-WALK
   T-REPORT
   s" process-tree-fork-test: ok" type cr ;

FORK-TEST-MAIN

;package
