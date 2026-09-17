\ process-task-test.f - two tasks through the per-task process row.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-command.f lib/task.f lib/process-task-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-command.f
require lib/task.f

package PROCESS-TASK-TEST

\ One task captures children through PROC-CMD:RUN-OUTCOME while this thread
\ polls its own always-ready pipe through POLL-IN. Both reach lib/process.f's
\ row. While that row was one set of dictionary cells the two shared pollfd
\ slot 0: the poller armed its pipe over the capture's stdout descriptor and
\ wrote its own revents back over the capture's, so the capture loop read a
\ descriptor that was not ready and blocked - measured before the row moved as
\ this same program running past 90 s without finishing one capture, against
\ under a second afterwards (dot habu-make-the-process-6e615161).
40 constant PTT-CAPTURES                 \ children the captor runs
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
\ thread reads the counts after the join.
: PTT-ONE-CAPTURE ( -- )
   PROC-CMD:RESET
   s" hello" >LEN PROC-CMD:ARG+
   s" /usr/bin/echo" >LEN PTT-CAPTURE-MS >MS PROC-CMD:RUN-RC
   MATCH result
     ok  OF drop ENDOF
     err OF drop PTT-CAP-BAD+ ENDOF
   ;MATCH
   PROC-CMD:OUT$ S\" hello\n" STR= 0= if PTT-CAP-BAD+ then
   PTT-DID @ 1 + PTT-DID ! ;

\ The answer is stored BEFORE the done flag, so this thread never reaches
\ TASK:JOIN between the last capture and TASK:RETURN.
: PTT-CAPTOR-BODY ( -- )
   0 begin dup PTT-CAPTURES < while
      PTT-ONE-CAPTURE
      1 +
   repeat drop
   PTT-DID @ TASK:RETURN
   1 PTT-DONE ! ;

: PTT-POLL-ONCE ( -- )
   PTT-PIPE-R @ >FD PTT-POLL-MS >MS POLL-IN COUNT>N 1 <> if
      PTT-POLL-BAD @ 1 + PTT-POLL-BAD !
   then
   PTT-POLLS @ 1 + PTT-POLLS ! ;

\ True when the captor finished inside its budget. A shared row makes this
\ false instead of hanging forever, so a regression reports a failed case.
: PTT-POLL-UNTIL-DONE ( -- bool )
   mono-ns PTT-BUDGET-NS + {: deadline:n :}
   begin PTT-DONE @ 0= while
      mono-ns deadline > if 0 0= 0= exit then
      PTT-POLL-ONCE
   repeat
   0 0= ;

: PTT-JOIN-CAPTOR ( -- )
   begin PTT-DONE @ 0= while TASK:PAUSE repeat   \ stop polling: let a wedged captor out
   PTT-CAPTOR TASK:JOIN
   MATCH result
     ok  OF PTT-CAPTURES T= ENDOF
     err OF 0 T= ENDOF                           \ the captor threw; its code is not 0
   ;MATCH ;

: PTT-CONCURRENT-CAPTURE-AND-POLL ( -- )
   0 PTT-DONE !  0 PTT-DID !  0 PTT-CAP-BAD !  0 PTT-POLLS !  0 PTT-POLL-BAD !
   PIPE-PAIR {: rd:fd wr:fd :}
   rd FD>N PTT-PIPE-R !
   wr FD>N PTT-PIPE-W !
   wr FD>N PTT-BYTE 1 write 1 T=
   ['] PTT-CAPTOR-BODY PTT-CAPTOR TASK:ACTIVATE
   PTT-POLL-UNTIL-DONE {: finished:bool :}
   PTT-JOIN-CAPTOR
   PTT-PIPE-R @ close
   PTT-PIPE-W @ close
   finished TTRUE
   PTT-CAP-BAD @ 0 T=
   PTT-POLL-BAD @ 0 T=
   PTT-POLLS @ 0 > TTRUE ;

\ The row is a claim on USER-BAND, so its size is part of the contract: a slot
\ added without a band budget is caught here rather than at a caller's E-TASK-USER.
: PTT-ROW-SIZE ( -- )
   PROC-STORAGE-BYTES 1184 T= ;

: PROCESS-TASK-TEST-MAIN ( -- )
   T-RESET
   PTT-ROW-SIZE
   PTT-CONCURRENT-CAPTURE-AND-POLL
   T-REPORT
   s" process-task-test: ok" type cr ;

PROCESS-TASK-TEST-MAIN

;package
