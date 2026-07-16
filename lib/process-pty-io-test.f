\ process-pty-io-test.f - real PTY supervisor lifecycle coverage.

require lib/process-pty-io.f
require lib/test.f
require lib/engine-candidate.f
require test/checker-assert.f

package PROCESS-PTY

$100 constant BUF-CAP
$3E8 constant IO-WAIT-MS
$C8 constant OWNER-WAIT-POLLS
$A constant OWNER-WAIT-MS
$1388 constant OWNER-EXIT-MS

create BUF BUF-CAP allot
create OWNER-PIDS 2 cells allot

variable SUP
variable TARGET
variable OWNER-R
variable OWNER-W
variable OWNER-GATE-R
variable OWNER-GATE-W
variable RAW

: SAVE-PIDS ( process-pty-handle pid pid -- process-pty-handle )
   PID>N TARGET !
   PID>N SUP ! ;

: TEST-PIDS ( process-pty-handle -- process-pty-handle pid pid )
   VIEW >r >r >r r> drop r> drop r> drop ;

: CHECK-PIDS ( process-pty-handle -- process-pty-handle )
   TEST-PIDS SAVE-PIDS
   SUP @ 0 > TTRUE
   TARGET @ 0 > TTRUE
   SUP @ TARGET @ T<>
   SUP @ getpid T<>
   TARGET @ getpid T<> ;

: CHECK-IO ( process-pty-handle -- process-pty-handle )
   s" habu-pty" PROCESS-PTY:WRITE
   IO-WAIT-MS >MS PROCESS-PTY:POLL-IN COUNT>N 0 > TTRUE
   BUF BUF-CAP PROCESS-PTY:READ
   0 > TTRUE ;

: CHECK-KILLED ( outcome -- )
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF SIGKILL T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: CHECK-EXITED ( outcome -- )
   MATCH outcome
     exited OF 0 T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: CHECK-HB ( -- )
   ENGINE-CANDIDATE:PATH$ >LEN PROCESS-PTY:START
   IO-WAIT-MS >MS PROCESS-PTY:POLL-IN COUNT>N 0 > TTRUE
   BUF BUF-CAP PROCESS-PTY:READ 0 > TTRUE
   4 BUF c!
   BUF 1 PROCESS-PTY:WRITE
   PROCESS-PTY:WAIT CHECK-EXITED ;

: CHECK-RAW-SPAWN ( -- )
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO
   PROC-WAIT-STATUS 0 T= ;

: CLOSE-FD ( n -- )
   >FD FD-CLOSE ;

: OWNER-PIDS! ( process-pty-handle pid pid -- process-pty-handle )
   PID>N OWNER-PIDS cell+ !
   PID>N OWNER-PIDS ! ;

: REQUIRE-EOF ( fd -- )
   READ-EOF? 0= if E-PROC-OUTPUT throw then ;

: OWNER-CHILD ( -- )
   OWNER-R @ CLOSE-FD
   OWNER-GATE-W @ CLOSE-FD
   s" /bin/cat" >LEN PROCESS-PTY:START
   TEST-PIDS OWNER-PIDS!
   OWNER-W @ >FD OWNER-PIDS 2 cells WRITE-EXACT
   OWNER-W @ CLOSE-FD
   OWNER-GATE-R @ >FD REQUIRE-EOF
   OWNER-GATE-R @ CLOSE-FD
   HANDLE>N drop   \ test-only authority loss models abrupt owner exit
   s" " 0 die ;

: OWNER-SPAWN ( -- pid )
   PIPE-PAIR OWNER-W ! OWNER-R !
   PIPE-PAIR OWNER-GATE-W ! OWNER-GATE-R !
   OWNER-R @ >FD FD-CLOEXEC!
   OWNER-W @ >FD FD-CLOEXEC!
   OWNER-GATE-R @ >FD FD-CLOEXEC!
   OWNER-GATE-W @ >FD FD-CLOEXEC!
   PROC-FORK dup PID>N 0= if drop OWNER-CHILD then ;

: PID-GONE? ( pid -- bool )
   PID-ALIVE? 0= ;

: WAIT-GONE? {: pid:pid :} ( pid -- bool )
   0 begin dup OWNER-WAIT-POLLS < while
      pid PID-GONE? if drop true exit then
      NULL$ drop 0 OWNER-WAIT-MS poll 0 T=
      1+
   repeat drop
   false ;

: WATCH-PID ( pid -- fd )
   PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then >FD ;

: WATCH-EXIT ( fd -- ) {: watch:fd :}
   watch OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   watch FD-CLOSE ;

: CHECK-OWNER-DEATH ( -- )
   OWNER-SPAWN {: owner:pid :}
   OWNER-W @ CLOSE-FD
   OWNER-GATE-R @ CLOSE-FD
   OWNER-R @ >FD OWNER-PIDS 2 cells READ-EXACT
   OWNER-R @ CLOSE-FD
   OWNER-PIDS @ >PID WATCH-PID {: sup-watch:fd :}
   OWNER-PIDS cell+ @ >PID WATCH-PID {: target-watch:fd :}
   OWNER-GATE-W @ CLOSE-FD
   owner PROC-WAIT-STATUS 0 T=
   sup-watch WATCH-EXIT
   target-watch WATCH-EXIT
   OWNER-PIDS cell+ @ >PID WAIT-GONE? TTRUE ;

: SAVE-RAW ( process-pty-handle -- process-pty-handle )
   HANDLE>N dup RAW ! N>HANDLE ;

: OPEN-SAVED ( -- )
   RAW @ OPEN-RAW drop ;

: SET-SUP ( process-pty-handle pid -- process-pty-handle )
   >r HANDLE>N dup UNPACK-IDX r> swap SUP! N>HANDLE ;

: SET-TARGET ( process-pty-handle pid -- process-pty-handle )
   >r HANDLE>N dup UNPACK-IDX r> swap TARGET! N>HANDLE ;

: BREAK-MASTER ( process-pty-handle -- process-pty-handle )
   HANDLE>N dup UNPACK-IDX MASTER@ FD>N close-rc 0 T= N>HANDLE ;

: BAD-WAIT ( -- )
   s" /usr/bin/true" >LEN PROCESS-PTY:START
   SAVE-RAW
   CHECK-PIDS
   getpid >PID SET-SUP
   PROCESS-PTY:WAIT drop ;

: BAD-KILL-PID ( -- )
   s" /usr/bin/true" >LEN PROCESS-PTY:START
   SAVE-RAW
   -1 >PID SET-TARGET
   PROCESS-PTY:KILL drop ;

: BAD-MASTER ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START
   SAVE-RAW
   BREAK-MASTER
   PROCESS-PTY:KILL drop ;

: BAD-WRITE ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START
   SAVE-RAW
   BREAK-MASTER
   s" x" PROCESS-PTY:WRITE
   PROCESS-PTY:KILL drop ;

: BAD-READ ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START
   SAVE-RAW
   BREAK-MASTER
   BUF 1 PROCESS-PTY:READ drop
   PROCESS-PTY:KILL drop ;

: BAD-POLL ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START
   SAVE-RAW
   BREAK-MASTER
   IO-WAIT-MS >MS PROCESS-PTY:POLL-IN drop
   PROCESS-PTY:KILL drop ;

: CHECK-INACTIVE ( -- )
   [: OPEN-SAVED ;] E-PROC-PTY-HANDLE TTHROWSQ ;

: CHECK-RECOVERY ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL CHECK-KILLED ;

: CHECK-SYSCALL-ERRORS ( -- )
   [: BAD-WAIT ;] E-PROC-WAIT TTHROWSQ
   SUP @ >PID PROC-WAIT-STATUS 0 T=
   CHECK-INACTIVE
   [: BAD-KILL-PID ;] E-PROC-PTY-HANDLE TTHROWSQ
   CHECK-INACTIVE
   [: BAD-MASTER ;] E-PROC-OUTPUT TTHROWSQ
   CHECK-INACTIVE
   [: BAD-WRITE ;] E-PROC-OUTPUT TTHROWSQ
   CHECK-INACTIVE
   [: BAD-READ ;] E-PROC-OUTPUT TTHROWSQ
   CHECK-INACTIVE
   [: BAD-POLL ;] E-PROC-OUTPUT TTHROWSQ
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: RUN ( -- )
   T-RESET
   s" /bin/cat" >LEN PROCESS-PTY:START
   CHECK-PIDS
   CHECK-IO
   PROCESS-PTY:KILL CHECK-KILLED
   CHECK-HB
   CHECK-RAW-SPAWN
   CHECK-OWNER-DEATH
   CHECK-SYSCALL-ERRORS
   T-REPORT
   s" process-pty-io-test: ok" type cr ;

RUN

;package
