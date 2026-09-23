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

\ ---- two tasks, two commands ------------------------------------------------
\ The shape the shared command could not survive: task A states its arguments,
\ task B states its own in between, and A's run must still be A's command. With
\ one process-wide command A ran B's arguments (dot habu-give-proc-cmd-c1f51174,
\ reproducer habu-gaps/process-call-state/repro.f). Two declared contexts are
\ two sets of storage, so the interleave is no longer a race at all - and this
\ file's other rows keep proving that the lib/process.f capture row underneath
\ them is already per task.
20 constant PTT-ROUNDS
5000 constant PTT-RUN-MS
5000000000 constant PTT-WAIT-NS          \ one handshake wait: 5 s

CMD:COMMAND PTT-CMD-A
CMD:COMMAND PTT-CMD-B

TASK:MIN-STACK TASK:TASK PTT-A
TASK:MIN-STACK TASK:TASK PTT-B

variable PTT-A-READY                     \ rounds task A has stated its arguments for
variable PTT-B-DONE                      \ rounds task B has run its own command for

: PTT-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns PTT-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ Runs INSIDE a worker, so it asserts nothing: a bad completion throws and the
\ join reports the code; a wrong capture is counted and returned.
: PTT-PRINT ( ptr ptr u8 -- ) {: h:ptr :}
   h s" /usr/bin/printf" >LEN PTT-RUN-MS >MS CMD:RUN-OUTCOME
   MATCH outcome
      exited OF 0<> if E-PROC-OUTPUT throw then ENDOF
      signaled OF drop E-PROC-OUTPUT throw ENDOF
      timeout OF E-PROC-OUTPUT throw ENDOF
   ;MATCH ;

\ ( bad round -- bad round+1 ): the counter rides the stack because a local
\ binds once per definition, so a round cannot bind one of its own.
: PTT-A-ROUND ( n n -- n n ) {: bad:n r:n :}
   PTT-CMD-A CMD:RESET
   PTT-CMD-A s" alpha" >LEN CMD:ARG+
   r 1 + PTT-A-READY atomic!
   PTT-B-DONE r 1 + PTT-WAIT
   PTT-CMD-A PTT-PRINT
   PTT-CMD-A CMD:OUT$ s" alpha" STR= if bad else bad 1 + then
   r 1 + ;

: PTT-A-BODY ( -- )
   0 0 begin dup PTT-ROUNDS < while
      PTT-A-ROUND
   repeat drop TASK:RETURN ;

: PTT-B-ROUND ( n n -- n n ) {: bad:n r:n :}
   PTT-A-READY r 1 + PTT-WAIT
   PTT-CMD-B CMD:RESET
   PTT-CMD-B s" beta" >LEN CMD:ARG+
   PTT-CMD-B PTT-PRINT
   PTT-CMD-B CMD:OUT$ s" beta" STR= {: kept:bool :}
   r 1 + PTT-B-DONE atomic!
   kept if bad else bad 1 + then
   r 1 + ;

: PTT-B-BODY ( -- )
   0 0 begin dup PTT-ROUNDS < while
      PTT-B-ROUND
   repeat drop TASK:RETURN ;

: PTT-JOIN-ZERO ( ptr n -- )
   TASK:JOIN MATCH result
     ok  OF 0 T= ENDOF                            \ rounds whose capture was not its own
     err OF 0 T= ENDOF                            \ the worker threw; its code is not 0
   ;MATCH ;

: PTT-TWO-COMMANDS ( -- )
   0 PTT-A-READY !  0 PTT-B-DONE !
   ['] PTT-A-BODY PTT-A TASK:ACTIVATE
   ['] PTT-B-BODY PTT-B TASK:ACTIVATE
   PTT-A PTT-JOIN-ZERO
   PTT-B PTT-JOIN-ZERO ;

\ ---- four tasks, four commands ----------------------------------------------
\ Each task holds its own environment row and its own stdin across a run, so
\ every part of a context the tasks could have shared is read back per task.
CMD:COMMAND PTT-CMD-0
CMD:COMMAND PTT-CMD-1
CMD:COMMAND PTT-CMD-2
CMD:COMMAND PTT-CMD-3

TASK:MIN-STACK TASK:TASK PTT-T0
TASK:MIN-STACK TASK:TASK PTT-T1
TASK:MIN-STACK TASK:TASK PTT-T2
TASK:MIN-STACK TASK:TASK PTT-T3

: PTT-SLOT ( n -- ptr ptr u8 ) {: ix:n :}
   ix 0 = if PTT-CMD-0 exit then
   ix 1 = if PTT-CMD-1 exit then
   ix 2 = if PTT-CMD-2 exit then
   PTT-CMD-3 ;

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

: PTT-RUN-OK ( ptr ptr u8 ptr u8 len -- ) {: h:ptr path:ptr pathu:len :}
   h path pathu PTT-RUN-MS >MS CMD:RUN-RC
   MATCH result
     ok  OF drop ENDOF
     err OF throw ENDOF
   ;MATCH ;

: PTT-ENV-ROUND ( n n -- n ) {: bad:n ix:n :}
   ix PTT-SLOT CMD:RESET
   ix PTT-SLOT CMD:ENV-HERMETIC
   ix PTT-SLOT s" HABU_CMD_TASK" >LEN ix PTT-VALUE$ >LEN CMD:ENV+
   ix PTT-SLOT s" /usr/bin/env" >LEN PTT-RUN-OK
   ix PTT-SLOT CMD:OUT$ ix PTT-ENV-OUT$ STR= if bad else bad 1 + then ;

: PTT-STDIN-ROUND ( n n -- n ) {: bad:n ix:n :}
   ix PTT-SLOT CMD:RESET
   ix PTT-SLOT ix PTT-STDIN$ >LEN CMD:IN!
   ix PTT-SLOT s" /bin/cat" >LEN PTT-RUN-OK
   ix PTT-SLOT CMD:OUT$ ix PTT-STDIN$ STR= if bad else bad 1 + then ;

: PTT-T-ROUND ( n n n -- n n ) {: bad:n r:n ix:n :}
   bad ix PTT-ENV-ROUND ix PTT-STDIN-ROUND
   r 1 + ;

: PTT-T-RUN ( n -- n ) {: ix:n :}
   0 0 begin dup PTT-ROUNDS < while
      ix PTT-T-ROUND
   repeat drop ;

: PTT-T0-BODY ( -- ) 0 PTT-T-RUN TASK:RETURN ;
: PTT-T1-BODY ( -- ) 1 PTT-T-RUN TASK:RETURN ;
: PTT-T2-BODY ( -- ) 2 PTT-T-RUN TASK:RETURN ;
: PTT-T3-BODY ( -- ) 3 PTT-T-RUN TASK:RETURN ;

: PTT-FOUR-COMMANDS ( -- )
   ['] PTT-T0-BODY PTT-T0 TASK:ACTIVATE
   ['] PTT-T1-BODY PTT-T1 TASK:ACTIVATE
   ['] PTT-T2-BODY PTT-T2 TASK:ACTIVATE
   ['] PTT-T3-BODY PTT-T3 TASK:ACTIVATE
   PTT-T0 PTT-JOIN-ZERO
   PTT-T1 PTT-JOIN-ZERO
   PTT-T2 PTT-JOIN-ZERO
   PTT-T3 PTT-JOIN-ZERO ;

: PROCESS-TASK-TEST-MAIN ( -- )
   T-RESET
   PTT-ROW-SIZE
   PTT-CONCURRENT-CAPTURE-AND-POLL
   s" a task's own command arguments survive another task's" T-LABEL
   PTT-TWO-COMMANDS
   s" four tasks keep their own environment row and stdin" T-LABEL
   PTT-FOUR-COMMANDS
   T-REPORT
   s" process-task-test: ok" type cr ;

PROCESS-TASK-TEST-MAIN

;package
