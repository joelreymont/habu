\ process-task-test.f - two tasks through the per-task process row.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f src/lib/process.f src/lib/process-argv.f src/lib/process-env.f src/lib/process-command.f src/lib/task.f src/lib/process-task-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/task.f
require src/lib/test/outcome.f
require src/lib/test/wait.f

package PROCESS-TASK-TEST

\ One task captures children through CMD:RUN-OUTCOME while this thread
\ polls its own always-ready pipe through POLL-IN. Both reach src/lib/process.f's
\ row. While that row was one set of dictionary cells the two shared pollfd
\ slot 0: the poller armed its pipe over the capture's stdout descriptor and
\ wrote its own revents back over the capture's, so the capture loop read a
\ descriptor that was not ready and blocked - measured before the row moved as
\ this same program running past 90 s without finishing one capture, against
\ under a second afterwards (dot habu-make-the-process-6e615161). A shared slot
\ spoils a capture only when a poll lands inside it: with one pollfd array for
\ both tasks, twelve children failed or hung in each of twelve runs, where four
\ passed one run in nine: at that rate twelve miss it about once in 700 runs.
12 constant PTT-CAPTURES                 \ children the captor runs
5000 constant PTT-CAPTURE-MS             \ one child's capture deadline
50 constant PTT-POLL-MS                  \ one POLL-IN wait
20000000000 constant PTT-BUDGET-NS       \ the captor's whole run: 20 s

create PTT-BYTE 120 c,                   \ the one byte that keeps the pipe ready

variable PTT-DONE                        \ the captor ran every child
variable PTT-DID                         \ children the captor actually ran to the end
variable PTT-CAP-BAD                     \ children that did not exit 0 with "hello"
variable PTT-POLLS
variable PTT-POLL-BAD                    \ polls that did not answer the one ready byte
variable PTT-PIPE-R
variable PTT-PIPE-W

TASK:MIN-STACK TASK:TASK PTT-CAPTOR

: PTT-CAP-BAD+ ( -- )
   PTT-CAP-BAD @ 1 + PTT-CAP-BAD ! ;

\ Runs INSIDE the captor task, so it asserts nothing: it counts, and this
\ thread reads the counts after the join. A deadline throws E-PROC-TIMEOUT,
\ which the join records and this thread names and throws (PTT-WORKER-ERR).
: PTT-ONE-CAPTURE ( CMD:command -- CMD:command )
   CMD:RESET
   s" hello" CMD:ARG+
   s" /bin/echo" PTT-CAPTURE-MS >MS CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N 0<> if PTT-CAP-BAD+ then
   CMD:OUT$ S\" hello\n" STR= 0= if PTT-CAP-BAD+ then
   PTT-DID @ 1 + PTT-DID ! ;

: PTT-CAPTURE-ALL ( CMD:command -- CMD:command )
   0 begin dup PTT-CAPTURES < while
      swap PTT-ONE-CAPTURE swap
      1 +
   repeat drop ;

\ The answer is stored BEFORE the done flag, so this thread never reaches
\ TASK:JOIN between the last capture and TASK:RETURN.
: PTT-CAPTOR-BODY ( -- )
   [: PTT-CAPTURE-ALL ;] CMD:WITH-COMMAND
   PTT-DID @ TASK:RETURN
   1 PTT-DONE ! ;

: PTT-POLL-ONCE ( -- )
   PTT-PIPE-R @ >FD PTT-POLL-MS >MS POLL-IN COUNT>N 1 <> if
      PTT-POLL-BAD @ 1 + PTT-POLL-BAD !
   then
   PTT-POLLS @ 1 + PTT-POLLS ! ;

\ Polls this thread's own pipe while the captor runs, for at most the budget:
\ true once the captor has ended, false when the budget ran out first. Only a
\ captor that has ended is joined, so a shared row that stalls a capture ends
\ in the named captor wait and never in a join that waits without a bound.
: PTT-POLL-UNTIL-DONE ( -- bool )
   mono-ns PTT-BUDGET-NS + {: deadline:n :}
   begin PTT-CAPTOR TASK:DONE? 0= while
      mono-ns deadline > if 0 0= 0= exit then
      PTT-POLL-ONCE
   repeat
   0 0= ;

\ A worker's code, read here because a task asserts nothing. E-PROC-TIMEOUT is
\ a deadline the worker recorded: this thread names it and throws it before
\ any assertion on the result. Any other code is not 0 and fails.
: PTT-WORKER-ERR ( n -- )
   {: code:n :}
   code E-PROC-TIMEOUT = if
      s" process-task-test command task wait" TEST-WAIT:EXPIRED
   then
   code 0 T= ;

\ Joins a worker: the answer it returned must be want, and a code it threw is
\ read by PTT-WORKER-ERR.
: PTT-JOIN= ( ptr n n -- )
   {: t:ptr want:n :}
   t TASK:JOIN MATCH result
     ok  OF want T= ENDOF
     err OF PTT-WORKER-ERR ENDOF
   ;MATCH ;

\ A worker's answer: what it returned, or the code it threw.
: PTT-JOIN-CODE ( ptr n -- n )
   TASK:JOIN MATCH result
     ok  OF ENDOF
     err OF ENDOF
   ;MATCH ;

\ A code that is neither 0 nor a deadline the worker recorded.
: PTT-FAILED? ( n -- bool )
   {: code:n :}
   code 0<> code E-PROC-TIMEOUT <> and ;

\ Joins two workers that wait on each other before reading either, and answers
\ true when both answered 0. A worker that fails leaves its peer to wait out
\ PTT-WAIT: only the failure is asserted, never that deadline, and the case
\ reads nothing the workers left. When neither failed, a deadline is named and
\ thrown.
: PTT-JOIN-PAIR ( ptr n ptr n -- bool )
   {: ta:ptr tb:ptr :}
   ta PTT-JOIN-CODE tb PTT-JOIN-CODE {: a:n b:n :}
   a PTT-FAILED? b PTT-FAILED? or if
      a PTT-FAILED? if a 0 T= then
      b PTT-FAILED? if b 0 T= then
      false exit
   then
   a E-PROC-TIMEOUT = b E-PROC-TIMEOUT = or if
      s" process-task-test command task wait" TEST-WAIT:EXPIRED
   then
   true ;

: PTT-CONCURRENT-CAPTURE-AND-POLL ( -- )
   0 PTT-DONE !  0 PTT-DID !  0 PTT-CAP-BAD !  0 PTT-POLLS !  0 PTT-POLL-BAD !
   PIPE-PAIR {: rd:fd wr:fd :}
   rd FD>N PTT-PIPE-R !
   wr FD>N PTT-PIPE-W !
   wr FD>N PTT-BYTE 1 write 1 T=
   ['] PTT-CAPTOR-BODY PTT-CAPTOR TASK:ACTIVATE
   PTT-POLL-UNTIL-DONE {: finished:bool :}
   PTT-PIPE-R @ close
   PTT-PIPE-W @ close
   finished 0= if s" process-task-test captor wait" TEST-WAIT:EXPIRED then
   PTT-CAPTOR PTT-CAPTURES PTT-JOIN=
   PTT-CAP-BAD @ 0 T=
   PTT-POLL-BAD @ 0 T=
   PTT-POLLS @ 0 > TTRUE ;

\ ---- two tasks, two commands ------------------------------------------------
\ The shape the shared command could not survive: task A states its arguments,
\ task B states its own in between, and A's run must still be A's command. With
\ one process-wide command A ran B's arguments (dot habu-give-proc-cmd-c1f51174,
\ reproducer habu-gaps/process-call-state/repro.f). Two scopes are two
\ contexts, so the interleave is no longer a race at all - and this
\ file's other rows keep proving that the src/lib/process.f capture row underneath
\ them is already per task. In the two-command case the handshake forces the
\ interleave every round; the four-command case relies on four tasks
\ interleaving over its spawns.
3 constant PTT-ROUNDS
5000 constant PTT-RUN-MS
5000000000 constant PTT-WAIT-NS          \ one handshake wait: 5 s

TASK:MIN-STACK TASK:TASK PTT-A
TASK:MIN-STACK TASK:TASK PTT-B

variable PTT-A-READY                     \ rounds task A has stated its arguments for
variable PTT-B-DONE                      \ rounds task B has run its own command for

\ Runs INSIDE a worker: a handshake that does not arrive in time throws
\ E-PROC-TIMEOUT, which the join records and this thread names and throws.
: PTT-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns PTT-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ Runs INSIDE a worker, so it asserts nothing: a bad completion throws and the
\ join reports the code, a deadline throws E-PROC-TIMEOUT, which the join
\ records and this thread names and throws; a wrong capture is counted and
\ returned.
: PTT-PRINT ( CMD:command -- CMD:command )
   s" /usr/bin/printf" PTT-RUN-MS >MS CMD:RUN-OUTCOME
   MATCH outcome
      exited OF 0<> if E-PROC-OUTPUT throw then ENDOF
      signaled OF drop E-PROC-OUTPUT throw ENDOF
      timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

\ ( bad round -- bad round+1 ): the counter rides the stack because a local
\ binds once per definition, so a round cannot bind one of its own.
: PTT-A-ROUND ( CMD:command n n -- CMD:command n n )
   {: bad:n r:n :}
   CMD:RESET
   s" alpha" CMD:ARG+
   r 1 + PTT-A-READY atomic!
   PTT-B-DONE r 1 + PTT-WAIT
   PTT-PRINT
   CMD:OUT$ s" alpha" STR= if bad else bad 1 + then
   r 1 + ;

: PTT-A-RUN ( CMD:command -- n CMD:command )
   0 0 begin dup PTT-ROUNDS < while
      PTT-A-ROUND
   repeat drop swap ;

: PTT-A-BODY ( -- ) [: PTT-A-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-B-ROUND ( CMD:command n n -- CMD:command n n )
   {: bad:n r:n :}
   PTT-A-READY r 1 + PTT-WAIT
   CMD:RESET
   s" beta" CMD:ARG+
   PTT-PRINT
   CMD:OUT$ s" beta" STR= {: kept:bool :}
   r 1 + PTT-B-DONE atomic!
   kept if bad else bad 1 + then
   r 1 + ;

: PTT-B-RUN ( CMD:command -- n CMD:command )
   0 0 begin dup PTT-ROUNDS < while
      PTT-B-ROUND
   repeat drop swap ;

: PTT-B-BODY ( -- ) [: PTT-B-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-TWO-COMMANDS ( -- )
   0 PTT-A-READY !  0 PTT-B-DONE !
   ['] PTT-A-BODY PTT-A TASK:ACTIVATE
   ['] PTT-B-BODY PTT-B TASK:ACTIVATE
   PTT-A PTT-B PTT-JOIN-PAIR drop ;      \ each answers its rounds whose capture was not its own

\ ---- four tasks, four commands ----------------------------------------------
\ Each task holds its own environment row and its own stdin across a run, so
\ every part of a context the tasks could have shared is read back per task.
TASK:MIN-STACK TASK:TASK PTT-T0
TASK:MIN-STACK TASK:TASK PTT-T1
TASK:MIN-STACK TASK:TASK PTT-T2
TASK:MIN-STACK TASK:TASK PTT-T3

: PTT-VALUE$ ( n -- ptr u8 n ) {: ix:n :}
   ix 0 = if s" zero" exit then
   ix 1 = if s" one" exit then
   ix 2 = if s" two" exit then
   s" three" ;

: PTT-ENV-OUT$ ( n -- ptr u8 n ) {: ix:n :}
   ix 0 = if S\" HABU_CMD_TASK=zero\n" exit then
   ix 1 = if S\" HABU_CMD_TASK=one\n" exit then
   ix 2 = if S\" HABU_CMD_TASK=two\n" exit then
   S\" HABU_CMD_TASK=three\n" ;

: PTT-STDIN$ ( n -- ptr u8 n ) {: ix:n :}
   ix 0 = if s" stdin for zero" exit then
   ix 1 = if s" stdin for one" exit then
   ix 2 = if s" stdin for two" exit then
   s" stdin for three" ;

: PTT-RUN-OK ( CMD:command ptr u8 n -- CMD:command )
   PTT-RUN-MS >MS CMD:RUN-OUTCOME PROC-OUTCOME>RC RC>N {: rc:n :}
   rc 0<> if rc throw then ;

: PTT-ENV-ROUND ( CMD:command n n -- CMD:command n )
   {: bad:n ix:n :}
   CMD:RESET
   CMD:ENV-HERMETIC
   s" HABU_CMD_TASK" ix PTT-VALUE$ CMD:ENV+
   s" /usr/bin/env" PTT-RUN-OK
   CMD:OUT$ ix PTT-ENV-OUT$ STR= if bad else bad 1 + then ;

: PTT-STDIN-ROUND ( CMD:command n n -- CMD:command n )
   {: bad:n ix:n :}
   CMD:RESET
   ix PTT-STDIN$ CMD:IN!
   s" /bin/cat" PTT-RUN-OK
   CMD:OUT$ ix PTT-STDIN$ STR= if bad else bad 1 + then ;

: PTT-T-ROUND ( CMD:command n n n -- CMD:command n n ) {: bad:n r:n ix:n :}
   bad ix PTT-ENV-ROUND ix PTT-STDIN-ROUND
   r 1 + ;

\ Task ix answers the rounds whose capture was not its own.
: PTT-T-RUN ( n CMD:command -- n CMD:command )
   >r {: ix:n :} r>
   0 0 begin dup PTT-ROUNDS < while
      ix PTT-T-ROUND
   repeat drop swap ;

: PTT-T0-BODY ( -- ) 0 [: PTT-T-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;
: PTT-T1-BODY ( -- ) 1 [: PTT-T-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;
: PTT-T2-BODY ( -- ) 2 [: PTT-T-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;
: PTT-T3-BODY ( -- ) 3 [: PTT-T-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-FOUR-COMMANDS ( -- )
   ['] PTT-T0-BODY PTT-T0 TASK:ACTIVATE
   ['] PTT-T1-BODY PTT-T1 TASK:ACTIVATE
   ['] PTT-T2-BODY PTT-T2 TASK:ACTIVATE
   ['] PTT-T3-BODY PTT-T3 TASK:ACTIVATE
   PTT-T0 0 PTT-JOIN=
   PTT-T1 0 PTT-JOIN=
   PTT-T2 0 PTT-JOIN=
   PTT-T3 0 PTT-JOIN= ;

\ ---- bounded waits beside each other and beside a capture -------------------
\ This thread spawns both children before a task starts, because the argv table
\ is process-wide. Then it waits out a window on the held child while one task
\ reaps the sleeper and another captures a command of its own context. Each
\ answers for its own pid: the sleeper's exit, the capture's output and status,
\ and the window's timeout, after which the held child is still ours to reap.
300 constant PTT-HOLD-MS                 \ outlasts the sleeper and the captured child

TASK:MIN-STACK TASK:TASK PTT-WAITER
TASK:MIN-STACK TASK:TASK PTT-CAPTURER

variable PTT-SLEEPER                     \ exits 22 a fifth of a second after its spawn
variable PTT-HELD                        \ exits 21 once its stdin closes
variable PTT-HELD-W                      \ that stdin's write end

: PTT-SH-ARGS ( ptr u8 n -- )
   {: s:ptr su:n :}
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s su >LEN PROC-ARGV+ ;

: PTT-HOLD-SCRIPT ( -- ptr u8 n )
   s" read x; exit 21" ;

\ Both pipe ends are close-on-exec, so the held child's stdin is the one copy
\ on its fd 0 and closing the write end here releases it.
: PTT-SPAWN-CHILDREN ( -- )
   s" sleep 0.2; exit 22" PTT-SH-ARGS
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N PTT-SLEEPER !
   PIPE-PAIR {: in-r:fd in-w:fd :}
   in-r FD-CLOEXEC!  in-w FD-CLOEXEC!
   PTT-HOLD-SCRIPT PTT-SH-ARGS
   s" /bin/sh" >LEN in-r -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N PTT-HELD !
   in-r FD>N close
   in-w FD>N PTT-HELD-W ! ;

\ Runs INSIDE a worker: the sleeper's rc, or E-PROC-TIMEOUT once PTT-RUN-MS
\ passes without its exit, which the join records and this thread names and
\ throws.
: PTT-WAITER-BODY ( -- )
   PTT-SLEEPER @ >PID PTT-RUN-MS >MS PROC-WAIT-BOUNDED
   PROC-OUTCOME>RC RC>N TASK:RETURN ;

\ 0 when `/bin/sh -c script` printed exactly want, else 1.
: PTT-SH-PRINTS ( CMD:command ptr u8 n ptr u8 n -- n CMD:command )
   {: script:ptr scriptu:n want:ptr wantu:n :}
   s" -c" CMD:ARG+
   script scriptu CMD:ARG+
   s" /bin/sh" PTT-RUN-OK
   CMD:OUT$ want wantu STR= if 0 else 1 then swap ;

: PTT-CAPTURER-RUN ( CMD:command -- n CMD:command )
   s" sleep 0.2; printf capture-ok" s" capture-ok" PTT-SH-PRINTS ;

: PTT-CAPTURER-BODY ( -- ) [: PTT-CAPTURER-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-WAITS-BESIDE-CAPTURE ( -- )
   PTT-SPAWN-CHILDREN
   ['] PTT-WAITER-BODY PTT-WAITER TASK:ACTIVATE
   ['] PTT-CAPTURER-BODY PTT-CAPTURER TASK:ACTIVATE
   PTT-HELD @ >PID PTT-HOLD-MS >MS PROC-WAIT-BOUNDED T-OUTCOME-TIMEOUT
   PTT-WAITER 22 PTT-JOIN=
   PTT-CAPTURER 0 PTT-JOIN=
   PTT-HELD-W @ close
   PTT-HELD @ >PID PTT-RUN-MS >MS PROC-WAIT-BOUNDED {: done :}
   PTT-HOLD-SCRIPT s" " s" " done 21 T-OUTCOME-EXITED= ;

\ ---- two growing captures -----------------------------------------------------
\ Each child receives a different argv, environment and 70 KiB stdin, and its
\ stdout, the mark line and that stdin, outgrows the capture's first region. A
\ task asserts nothing: it records what its scope read, and this thread checks.
70000 constant PTT-GROW-U
create PTT-GROW-IN-A PTT-GROW-U allot
create PTT-GROW-IN-B PTT-GROW-U allot
variable PTT-GROW-OUT-A-U
variable PTT-GROW-OUT-B-U
variable PTT-GROW-ERR-A-U
variable PTT-GROW-ERR-B-U
variable PTT-GROW-A-BAD                  \ 1: stdout was not the mark line and the stdin
variable PTT-GROW-B-BAD
TASK:MIN-STACK TASK:TASK PTT-GROW-TA
TASK:MIN-STACK TASK:TASK PTT-GROW-TB

: PTT-GROW-STAGE ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: v:ptr vu:n script:ptr scriptu:n :}
   CMD:ENV-HERMETIC
   s" PTT_MARK" v vu CMD:ENV+
   s" -c" CMD:ARG+
   script scriptu CMD:ARG+ ;

\ The rc of /bin/sh run with the PTT-GROW-U bytes at in as its stdin.
: PTT-GROW-RUN ( CMD:command ptr u8 -- CMD:command n )
   PTT-GROW-U CMD:IN!
   s" /bin/sh" PTT-RUN-MS >MS CMD:RUN-OUTCOME PROC-OUTCOME>RC RC>N ;

\ 0 when stdout is the mark line followed by the whole stdin at in, else 1.
: PTT-GROW-BAD ( CMD:command ptr u8 n ptr u8 -- CMD:command n )
   {: mark:ptr marku:n in:ptr :}
   CMD:OUT$ {: out:ptr outu:n :}
   outu marku PTT-GROW-U + <> if 1 exit then
   out marku mark marku STR=
   out marku + PTT-GROW-U in PTT-GROW-U STR= and if 0 else 1 then ;

: PTT-GROW-A-RUN ( CMD:command -- n CMD:command )
   s" alpha" s" echo A:$PTT_MARK; /bin/cat" PTT-GROW-STAGE
   1 PTT-A-READY atomic!
   PTT-B-DONE 1 PTT-WAIT
   PTT-GROW-IN-A PTT-GROW-RUN {: rc:n :}
   CMD:OUT$ nip PTT-GROW-OUT-A-U !
   CMD:ERR$ nip PTT-GROW-ERR-A-U !
   S\" A:alpha\n" PTT-GROW-IN-A PTT-GROW-BAD PTT-GROW-A-BAD !
   rc swap ;

: PTT-GROW-B-RUN ( CMD:command -- n CMD:command )
   s" beta" s" echo B:$PTT_MARK; /bin/cat" PTT-GROW-STAGE
   1 PTT-B-DONE atomic!
   PTT-A-READY 1 PTT-WAIT
   PTT-GROW-IN-B PTT-GROW-RUN {: rc:n :}
   CMD:OUT$ nip PTT-GROW-OUT-B-U !
   CMD:ERR$ nip PTT-GROW-ERR-B-U !
   S\" B:beta\n" PTT-GROW-IN-B PTT-GROW-BAD PTT-GROW-B-BAD !
   rc swap ;

: PTT-GROW-A-BODY ( -- ) [: PTT-GROW-A-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;
: PTT-GROW-B-BODY ( -- ) [: PTT-GROW-B-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-GROW-CAPTURES ( -- )
   PTT-GROW-U 0 ?do
      97 PTT-GROW-IN-A i + c!
      98 PTT-GROW-IN-B i + c!
   loop
   0 PTT-A-READY ! 0 PTT-B-DONE !
   ['] PTT-GROW-A-BODY PTT-GROW-TA TASK:ACTIVATE
   ['] PTT-GROW-B-BODY PTT-GROW-TB TASK:ACTIVATE
   PTT-GROW-TA PTT-GROW-TB PTT-JOIN-PAIR 0= if exit then
   PTT-GROW-OUT-A-U @ PTT-GROW-U 8 + T=
   PTT-GROW-OUT-B-U @ PTT-GROW-U 7 + T=
   PTT-GROW-ERR-A-U @ 0 T= PTT-GROW-ERR-B-U @ 0 T=
   PTT-GROW-A-BAD @ 0 T= PTT-GROW-B-BAD @ 0 T= ;

\ ---- descriptors a concurrent spawn must not inherit ------------------------
\ Three tasks capture /usr/bin/true beside one whose shells leave a five-second
\ sleep behind and one that lists /dev/fd. Failure cases: a capture's pipe or
\ /dev/null that another task's spawn inherits before it is marked
\ close-on-exec reaches the sleep, which holds a capture's write end past its
\ deadline (a timeout), and shows in a listing as a descriptor the quiet
\ baseline lacks (a leak). Detection is probabilistic: without FD-LOCK:WITH
\ (src/lib/fd-lock.f) about one capture in three hundred times out.
100 constant PTT-INHERIT-CAPTURES        \ captures per capture task
3000 constant PTT-INHERIT-MS             \ one capture's deadline, inside the sleep's 5 s
256 constant PTT-FD-BASE-CAP

TASK:MIN-STACK TASK:TASK PTT-I0
TASK:MIN-STACK TASK:TASK PTT-I1
TASK:MIN-STACK TASK:TASK PTT-I2
TASK:MIN-STACK TASK:TASK PTT-SH
TASK:MIN-STACK TASK:TASK PTT-LS

variable PTT-INHERIT-ON                  \ the shell and listing tasks run while set
variable PTT-INHERIT-TIMEOUTS
variable PTT-INHERIT-BAD                 \ children that did not exit 0
variable PTT-INHERIT-LEAKS               \ listings unlike the baseline
create PTT-FD-BASE PTT-FD-BASE-CAP allot
variable PTT-FD-BASE-U

\ Runs INSIDE a worker, so it counts and asserts nothing.
: PTT-INHERIT-COUNT ( outcome -- )
   MATCH outcome
      exited OF 0<> if 1 PTT-INHERIT-BAD atomic-add drop then ENDOF
      signaled OF drop 1 PTT-INHERIT-BAD atomic-add drop ENDOF
      timeout OF 1 PTT-INHERIT-TIMEOUTS atomic-add drop ENDOF
   ;MATCH ;

: PTT-INHERIT-RUN ( CMD:command -- CMD:command )
   PTT-INHERIT-CAPTURES 0 ?do
      CMD:RESET
      s" /usr/bin/true" PTT-INHERIT-MS >MS CMD:RUN-OUTCOME
      PTT-INHERIT-COUNT
   loop ;

: PTT-I-BODY ( -- ) [: PTT-INHERIT-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

\ The shell exits at once; its sleep keeps every descriptor the shell inherited.
: PTT-SH-RUN ( CMD:command -- CMD:command )
   begin
      CMD:RESET
      s" -c" CMD:ARG+
      s" /bin/sleep 5 </dev/null >/dev/null 2>&1 &" CMD:ARG+
      s" /bin/sh" PTT-INHERIT-MS >MS CMD:RUN-OUTCOME PTT-INHERIT-COUNT
      PTT-INHERIT-ON atomic@ 0=
   until ;

: PTT-SH-BODY ( -- ) [: PTT-SH-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

: PTT-LIST-FDS ( CMD:command -- CMD:command outcome )
   CMD:RESET
   s" /dev/fd" CMD:ARG+
   s" /bin/ls" PTT-INHERIT-MS >MS CMD:RUN-OUTCOME ;

: PTT-LS-RUN ( CMD:command -- CMD:command )
   begin
      PTT-LIST-FDS PTT-INHERIT-COUNT
      CMD:OUT$ PTT-FD-BASE PTT-FD-BASE-U @ STR= 0= if
         1 PTT-INHERIT-LEAKS atomic-add drop
      then
      PTT-INHERIT-ON atomic@ 0=
   until ;

: PTT-LS-BODY ( -- ) [: PTT-LS-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

: PTT-FD-BASE! ( CMD:command -- CMD:command )
   PTT-LIST-FDS PROC-OUTCOME>RC RC>N 0 T=
   CMD:OUT$ {: a:ptr u:n :}
   u PTT-FD-BASE-CAP <= TTRUE
   a PTT-FD-BASE u BYTE-COPY
   u PTT-FD-BASE-U ! ;

: PTT-FD-BASELINE ( -- ) [: PTT-FD-BASE! ;] CMD:WITH-COMMAND ;

: PTT-NO-INHERIT ( -- )
   0 PTT-INHERIT-TIMEOUTS !  0 PTT-INHERIT-BAD !  0 PTT-INHERIT-LEAKS !
   PTT-FD-BASELINE
   1 PTT-INHERIT-ON !
   ['] PTT-SH-BODY PTT-SH TASK:ACTIVATE
   ['] PTT-LS-BODY PTT-LS TASK:ACTIVATE
   ['] PTT-I-BODY PTT-I0 TASK:ACTIVATE
   ['] PTT-I-BODY PTT-I1 TASK:ACTIVATE
   ['] PTT-I-BODY PTT-I2 TASK:ACTIVATE
   PTT-I0 0 PTT-JOIN=
   PTT-I1 0 PTT-JOIN=
   PTT-I2 0 PTT-JOIN=
   0 PTT-INHERIT-ON atomic!
   PTT-SH 0 PTT-JOIN=
   PTT-LS 0 PTT-JOIN=
   PTT-INHERIT-TIMEOUTS @ 0 T=
   PTT-INHERIT-LEAKS @ 0 T=
   PTT-INHERIT-BAD @ 0 T= ;

\ ---- cancelling one call, while another completes --------------------------
\ Failure cases: a ready cancellation must wake a capture blocked writing its
\ stdin, run caller cleanup, and leave the other command alone. Descendants can
\ hold capture pipes open; EOF can precede child exit. Neither may turn
\ cancellation into a blocking reap. HALT remains cooperative: explicit
\ cancellation is the pipe, never PAUSE. The throw ends the command's scope, so
\ the caller catches it outside CMD:WITH-COMMAND.
variable PTT-CANCEL-R
variable PTT-CANCEL-W
variable PTT-CANCEL-MODE
variable PTT-CANCEL-CLEAN
variable PTT-CANCEL-CODE
variable PTT-CANCEL-PID
create PTT-CANCEL-BYTE 99 c,

: PTT-CANCEL-SCRIPT ( -- ptr u8 n )
   PTT-CANCEL-MODE @ 1 = if
      s" sleep 10 & printf out; printf err >&2; wait" exit
   then
   PTT-CANCEL-MODE @ 2 = if
      s" printf out; printf err >&2; exec 0<&- 1>&- 2>&-; exec sleep 10" exit
   then
   s" printf out; printf err >&2; exec sleep 10" ;

\ Mode 3 runs through RUN-RC, the others through RUN-OUTCOME.
: PTT-CANCEL-RUN ( CMD:command -- CMD:command )
   PTT-CANCEL-R @ >FD CMD:CANCEL-FD!
   s" -c" CMD:ARG+
   PTT-CANCEL-SCRIPT CMD:ARG+
   PTT-GROW-IN-A PTT-GROW-U CMD:IN!
   PTT-CANCEL-MODE @ 3 = if
      s" /bin/sh" PTT-RUN-MS >MS CMD:RUN-RC drop exit
   then
   s" /bin/sh" PTT-RUN-MS >MS CMD:RUN-OUTCOME drop ;

: PTT-CANCEL-CLEANUP ( -- )
   1 PTT-CANCEL-CLEAN atomic! ;

: PTT-CANCEL-BODY ( -- )
   [: [: [: PTT-CANCEL-RUN ;] CMD:WITH-COMMAND ;] [: PTT-CANCEL-CLEANUP ;] finally ;]
   catch PTT-CANCEL-CODE !
   PROC-PID @ PTT-CANCEL-PID !
   0 TASK:RETURN ;

: PTT-CANCEL-CONTROL-RUN ( CMD:command -- n CMD:command )
   s" sleep 0.2; printf unaffected" s" unaffected" PTT-SH-PRINTS ;

: PTT-CANCEL-CONTROL ( -- )
   [: PTT-CANCEL-CONTROL-RUN ;] CMD:WITH-COMMAND TASK:RETURN ;

: PTT-CANCEL-OPEN ( -- )
   PIPE-PAIR {: rd:fd wr:fd :}
   rd FD-CLOEXEC! wr FD-CLOEXEC!
   rd FD>N PTT-CANCEL-R ! wr FD>N PTT-CANCEL-W ! ;

: PTT-CANCEL-SIGNAL ( -- )
   PTT-CANCEL-W @ PTT-CANCEL-BYTE 1 write 1 T= ;

\ Inspect only this live task's process row, before JOIN releases its region.
\ Copied bytes establish the capture state, rather than assuming a scheduler
\ delay is long enough. The tree child starts before those bytes are printed.
\ Closed owned descriptors additionally establish the EOF/reap boundary.
: PTT-CANCEL-READY? ( -- bool )
   PTT-A TASK:DONE? if
      PTT-A 0 PTT-JOIN=
      PTT-CANCEL-CODE @ dup 0<> if throw then drop
      E-PROC-OUTPUT throw
   then
   PTT-A PROC-PID TASK:HIS atomic@ 0 <= if false exit then
   PTT-A PROC-OUT-LEN TASK:HIS atomic@ 3 < if false exit then
   PTT-A PROC-ERR-LEN TASK:HIS atomic@ 3 < if false exit then
   PTT-CANCEL-MODE @ 2 <> if true exit then
   PTT-A PROC-OUT-R TASK:HIS atomic@ 0 <
   PTT-A PROC-ERR-R TASK:HIS atomic@ 0 < and
   PTT-A PROC-IN-W TASK:HIS atomic@ 0 < and ;

: PTT-CANCEL-ONE ( n -- )
   PTT-CANCEL-MODE !
   PTT-CANCEL-OPEN
   0 PTT-CANCEL-CLEAN ! 0 PTT-CANCEL-CODE !
   ['] PTT-CANCEL-BODY PTT-A TASK:ACTIVATE
   ['] PTT-CANCEL-CONTROL PTT-B TASK:ACTIVATE
   [: PTT-CANCEL-READY? ;] mono-ns PTT-WAIT-NS +
   s" process-task-test capture cancellation boundary" TEST-WAIT:POLL
   mono-ns {: start:n :}
   PTT-CANCEL-SIGNAL
   PTT-A 0 PTT-JOIN=
   mono-ns start - 1000000000 < TTRUE
   PTT-B 0 PTT-JOIN=
   PTT-CANCEL-CODE @ E-PROC-CANCELLED T=
   PTT-CANCEL-CLEAN atomic@ 1 T=
   PTT-CANCEL-PID @ PROC-NO-PID T=
   \ The borrowed fd is still open and its byte was never consumed.
   PTT-CANCEL-W @ close
   PTT-CANCEL-R @ PTT-CANCEL-BYTE 1 read 1 T=
   PTT-CANCEL-R @ close ;

create PTT-CANCEL-MARK FS-PATH-CAP allot
variable PTT-CANCEL-MARK-U

: PTT-CANCEL-MARK$ ( -- ptr u8 n )
   PTT-CANCEL-MARK PTT-CANCEL-MARK-U @ ;

: PTT-CANCEL-PRE-RUN ( CMD:command -- CMD:command )
   s" /bin/sh" PTT-RUN-MS >MS CMD:RUN-OUTCOME drop ;

: PTT-CANCEL-PRE-SETUP ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   S\" printf spawned > \q$PC_CANCEL_MARK\q" CMD:ARG+
   s" PC_CANCEL_MARK" PTT-CANCEL-MARK$ CMD:ENV+ ;

\ Run the mark command with fd lent to it.
: PTT-CANCEL-PRE-LENT ( n CMD:command -- CMD:command )
   >r {: fd:n :} r>
   PTT-CANCEL-PRE-SETUP
   fd >FD CMD:CANCEL-FD!
   PTT-CANCEL-PRE-RUN ;

\ RESET and a lent PROC-NO-FD each disconnect a lent pipe that stays readable.
: PTT-CANCEL-PRE-RELENT ( CMD:command -- CMD:command )
   PTT-CANCEL-PRE-SETUP
   PTT-CANCEL-PRE-RUN
   PTT-CANCEL-MARK$ FILE? TTRUE
   CMD:RESET
   CMD:OUTCOME@ PROC-OUTCOME>RC RC>N 0 T=
   PTT-CANCEL-R @ >FD CMD:CANCEL-FD!
   CMD:RESET
   s" /usr/bin/true" PTT-RUN-OK
   PTT-CANCEL-R @ >FD CMD:CANCEL-FD!
   PROC-NO-FD >FD CMD:CANCEL-FD!
   s" /usr/bin/true" PTT-RUN-OK ;

\ A cancelled run leaves its code: OUTCOME@ and RC@ throw it again. The catch
\ sits inside the scope, the shape dot habu-settle-stale-linear-0bcc904c questions.
: PTT-CANCEL-PRE-KEPT ( CMD:command -- CMD:command )
   PTT-CANCEL-PRE-SETUP
   PTT-CANCEL-R @ >FD CMD:CANCEL-FD!
   [: PTT-CANCEL-PRE-RUN ;] catch E-PROC-CANCELLED T=
   [: CMD:OUTCOME@ drop ;] catch E-PROC-CANCELLED T=
   [: CMD:RC@ drop ;] catch E-PROC-CANCELLED T= ;

: PTT-CANCEL-PRE ( -- )
   CLEANUP-RESET
   s" cmd-cancel" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-TREE+
   root rootu s" spawned" PTT-CANCEL-MARK JOIN-PATH PTT-CANCEL-MARK-U !
   PTT-CANCEL-OPEN PTT-CANCEL-SIGNAL
   [: PTT-CANCEL-R @ [: PTT-CANCEL-PRE-LENT ;] CMD:WITH-COMMAND ;]
   E-PROC-CANCELLED TTHROWSQ
   [: PTT-CANCEL-PRE-KEPT ;] CMD:WITH-COMMAND
   PTT-CANCEL-MARK$ FILE? TFALSE
   [: PTT-CANCEL-PRE-RELENT ;] CMD:WITH-COMMAND
   \ POLLHUP cancels too, without a byte to read or a writer left alive.
   PTT-CANCEL-W @ close
   PTT-CANCEL-R @ PTT-CANCEL-BYTE 1 read 1 T=
   [: PTT-CANCEL-R @ [: PTT-CANCEL-PRE-LENT ;] CMD:WITH-COMMAND ;]
   E-PROC-CANCELLED TTHROWSQ
   PTT-CANCEL-R @ close
   [: 1000000 [: PTT-CANCEL-PRE-LENT ;] CMD:WITH-COMMAND ;] E-PROC-OUTPUT TTHROWSQ
   CLEANUP-RUN ;

public

: MAIN ( -- )
   T-RESET
   PTT-CONCURRENT-CAPTURE-AND-POLL
   s" a task's own command arguments survive another task's" T-LABEL
   PTT-TWO-COMMANDS
   s" four tasks keep their own environment row and stdin" T-LABEL
   PTT-FOUR-COMMANDS
   s" bounded waits answer for their own pids beside a capture" T-LABEL
   PTT-WAITS-BESIDE-CAPTURE
   s" growing captures keep argv, env, stdin and large stdout apart" T-LABEL
   PTT-GROW-CAPTURES
   s" a capture's descriptors never reach another task's child" T-LABEL
   PTT-NO-INHERIT
   s" cancelling plain, tree-held, EOF and RUN-RC captures isolates another call" T-LABEL
   4 0 ?do i PTT-CANCEL-ONE loop
   s" pre-signalled cancellation never spawns and reset disconnects the lent pipe" T-LABEL
   PTT-CANCEL-PRE
   T-REPORT
   s" process-task-test: ok" type cr ;

MAIN

;package
