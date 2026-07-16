\ process-pty-io-test.f - real PTY supervisor lifecycle coverage.

require lib/process-pty-io.f
require lib/test.f
require lib/engine-candidate.f
require lib/fs-mutate.f
require test/checker-assert.f

package PROCESS-PTY

$100 constant BUF-CAP
$3E8 constant IO-WAIT-MS
$1388 constant OWNER-EXIT-MS

create BUF BUF-CAP allot
create OWNER-READY 1 allot
create ROOT-BUF FS-PATH-CAP allot
create SCRIPT-BUF FS-PATH-CAP allot
create RACE-BUF FS-PATH-CAP allot

variable SUP
variable TARGET
variable OWNER-R
variable OWNER-W
variable OWNER-GATE-R
variable OWNER-GATE-W
variable KEEP-R
variable KEEP-W
variable KEEP-SENT-R
variable KEEP-SENT-W
variable SUP-SENT-R
variable SUP-SENT-W
variable RAW
variable ROOT-U
variable SCRIPT-U
variable RACE-U
variable SENT-R
variable SENT-W
variable OWNER-RACE
variable MASTER-R
variable MASTER-W
variable LIFE-R
variable LIFE-W
variable DONE-R
variable DONE-W
variable FRAME-DETAIL

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

: PATH! ( ptr u8 n ptr u8 ptr a -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SCRIPT$ ( -- ptr u8 n )
   SCRIPT-BUF SCRIPT-U @ ;

: RACE$ ( -- ptr u8 n )
   RACE-BUF RACE-U @ ;

: FIXTURE-SETUP ( -- )
   CLEANUP-RESET
   s" habu-pty-io" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U PATH!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hold.sh" SCRIPT-BUF JOIN-PATH SCRIPT-U !
   ROOT$ s" race.sh" RACE-BUF JOIN-PATH RACE-U !
   SCRIPT$ S\" #!/bin/sh\ntrap '' HUP\n( sleep 30 ) &\nwait\n" WRITE-ALL
   SCRIPT$ CHMOD-X
   RACE$ S\" #!/bin/sh\ntrap '' HUP\nread line\n( sleep 30 ) &\nkill -9 $$\n" WRITE-ALL
   RACE$ CHMOD-X ;

: REQUIRE-EOF ( fd -- )
   READ-EOF? 0= if E-PROC-OUTPUT throw then ;

: REQUIRE-NOT-READY ( fd -- )
   0 >MS POLL-ONE 0 T= ;

: SENTINEL-OPEN ( -- )
   PIPE-PAIR SENT-W ! SENT-R ! ;

: SUP-SENTINEL-OPEN ( -- )
   PIPE-PAIR SUP-SENT-W ! SUP-SENT-R !
   SUP-SENT-W @ >FD FD-CLOEXEC! ;

: KEEP-SENTINEL-OPEN ( -- )
   PIPE-PAIR KEEP-SENT-W ! KEEP-SENT-R !
   KEEP-SENT-W @ >FD FD-CLOEXEC! ;

: KEEPER-CHILD ( -- )
   SENT-W @ CLOSE-FD
   SUP-SENT-W @ CLOSE-FD
   OWNER-W @ CLOSE-FD
   OWNER-GATE-R @ CLOSE-FD
   KEEP-R @ >FD REQUIRE-EOF
   KEEP-R @ CLOSE-FD
   s" " 0 die ;

: OWNER-KEEPER ( -- pid )
   PROC-FORK dup PID>N 0= if drop KEEPER-CHILD then ;

: OWNER-CHILD ( -- )
   OWNER-R @ CLOSE-FD
   OWNER-GATE-W @ CLOSE-FD
   SENT-R @ CLOSE-FD
   KEEP-SENT-R @ CLOSE-FD
   SUP-SENT-R @ CLOSE-FD
   KEEP-W @ CLOSE-FD
   OWNER-RACE @ if RACE$ else SCRIPT$ then >LEN PROCESS-PTY:START
   HANDLE>N RAW !
   OWNER-KEEPER drop
   RAW @ N>HANDLE
   SENT-W @ CLOSE-FD
   KEEP-SENT-W @ CLOSE-FD
   SUP-SENT-W @ CLOSE-FD
   KEEP-R @ CLOSE-FD
   1 OWNER-READY c!
   OWNER-W @ >FD OWNER-READY 1 WRITE-EXACT
   OWNER-W @ CLOSE-FD
   OWNER-GATE-R @ >FD REQUIRE-EOF
   OWNER-GATE-R @ CLOSE-FD
   OWNER-RACE @ if S\" go\n" PROCESS-PTY:WRITE then
   HANDLE>N drop   \ test-only authority loss models abrupt owner exit
   s" " 0 die ;

: OWNER-SPAWN ( -- pid )
   PIPE-PAIR OWNER-W ! OWNER-R !
   PIPE-PAIR OWNER-GATE-W ! OWNER-GATE-R !
   PIPE-PAIR KEEP-W ! KEEP-R !
   OWNER-R @ >FD FD-CLOEXEC!
   OWNER-W @ >FD FD-CLOEXEC!
   OWNER-GATE-R @ >FD FD-CLOEXEC!
   OWNER-GATE-W @ >FD FD-CLOEXEC!
   KEEP-R @ >FD FD-CLOEXEC!
   KEEP-W @ >FD FD-CLOEXEC!
   PROC-FORK dup PID>N 0= if drop OWNER-CHILD then ;

: CHECK-OWNER-MODE ( bool -- )
   OWNER-RACE !
   SENTINEL-OPEN
   KEEP-SENTINEL-OPEN
   SUP-SENTINEL-OPEN
   OWNER-SPAWN {: owner:pid :}
   OWNER-W @ CLOSE-FD
   OWNER-GATE-R @ CLOSE-FD
   KEEP-R @ CLOSE-FD
   SENT-W @ CLOSE-FD
   KEEP-SENT-W @ CLOSE-FD
   SUP-SENT-W @ CLOSE-FD
   OWNER-R @ >FD OWNER-READY 1 READ-EXACT
   OWNER-R @ CLOSE-FD
   OWNER-GATE-W @ CLOSE-FD
   owner PROC-WAIT-STATUS 0 T=
   SUP-SENT-R @ >FD OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   SUP-SENT-R @ >FD REQUIRE-EOF
   SUP-SENT-R @ CLOSE-FD
   SENT-R @ >FD OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   SENT-R @ >FD REQUIRE-EOF
   SENT-R @ CLOSE-FD
   KEEP-SENT-R @ >FD REQUIRE-NOT-READY
   KEEP-W @ CLOSE-FD
   KEEP-SENT-R @ >FD OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   KEEP-SENT-R @ >FD REQUIRE-EOF
   KEEP-SENT-R @ CLOSE-FD ;

: CHECK-OWNER-DEATH ( -- )
   false CHECK-OWNER-MODE
   true CHECK-OWNER-MODE ;

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

: BAD-AUTH-WAIT ( -- )
   RAW @ N>HANDLE PROCESS-PTY:WAIT drop ;

: BAD-AUTH-KILL ( -- )
   RAW @ N>HANDLE PROCESS-PTY:KILL drop ;

: CHECK-AUTH-RECOVERY ( -- )
   [: BAD-AUTH-WAIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   CHECK-RECOVERY
   [: BAD-AUTH-KILL ;] E-PROC-PTY-HANDLE TTHROWSQ
   CHECK-RECOVERY ;

: BREAK-COMMIT ( -- )
   TX-SUP CELL-PID@ PID>N SUP !
   TX-MASTER CELL-FD@ FD>N close-rc 0 <> if E-PROC-OUTPUT throw then ;

: INJECT-COMMIT ( -- )
   [: BREAK-COMMIT ;] is BEFORE-COMMIT ;

: RESET-COMMIT ( -- )
   COMMIT-DEFAULT ;

: BAD-COMMIT ( -- )
   INJECT-COMMIT
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-COMMIT-RECOVERY ( -- )
   [: BAD-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   RESET-COMMIT
   CLEAN-MASK @ CLEAN-MASTER T=
   SUP @ wait-status 0 < TTRUE
   CHECK-RECOVERY ;

: BREAK-READY ( -- )
   TX-SUP CELL-PID@ PID>N SUP !
   TX-DONE-R CELL-FD@ FD>N close-rc 0 <> if E-PROC-OUTPUT throw then ;

: INJECT-READY ( -- )
   [: BREAK-READY ;] is BEFORE-READY ;

: BAD-READY ( -- )
   INJECT-READY
   SCRIPT$ >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-DEAD-READER ( -- )
   SENTINEL-OPEN
   [: BAD-READY ;] E-PROC-OUTPUT TTHROWSQ
   READY-DEFAULT
   SENT-W @ CLOSE-FD
   SENT-R @ >FD OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   SENT-R @ >FD REQUIRE-EOF
   SENT-R @ CLOSE-FD
   CLEAN-MASK @ CLEAN-DONE-R and CLEAN-DONE-R T=
   SUP @ wait-status 0 < TTRUE
   CHECK-RECOVERY ;

: BAD-EXEC ( -- )
   s" /no/such/habu-pty-executable" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-EXEC-ERROR ( -- )
   [: BAD-EXEC ;] E-PROC-SPAWN TTHROWSQ
   SUP-CLEAN-MASK @ 0 T=
   CLEAN-MASK @ 0 T=
   CHECK-RECOVERY ;

: FRAME-PIPES ( -- )
   PIPE-PAIR MASTER-W ! MASTER-R !
   PIPE-PAIR LIFE-W ! LIFE-R !
   PIPE-PAIR DONE-W ! DONE-R ! ;

: FRAME-TARGET ( -- pid )
   PROC-FORK dup PID>N 0= if drop s" " 0 die then ;

: FRAME-SUP-CHILD ( -- )
   DONE-R @ CLOSE-FD
   DONE-W @ >FD E-PROC-SPAWN FRAME-WRITE
   FRAME-DETAIL @ if DONE-W @ >FD SUP-CLEAN-WAIT FRAME-WRITE then
   DONE-W @ CLOSE-FD
   s" " 0 die ;

: FRAME-SUP ( -- pid )
   PROC-FORK dup PID>N 0= if drop FRAME-SUP-CHILD then ;

: FRAME-START ( bool -- process-pty-handle )
   FRAME-DETAIL !
   FRAME-PIPES
   FRAME-TARGET dup PID>N TARGET ! {: target:pid :}
   FRAME-SUP dup PID>N SUP ! {: sup:pid :}
   MASTER-W @ CLOSE-FD
   LIFE-R @ CLOSE-FD
   DONE-W @ CLOSE-FD
   RESERVE sup target MASTER-R @ >FD LIFE-W @ >FD DONE-R @ >FD COMMIT ;

: BAD-FRAME ( -- )
   true FRAME-START PROCESS-PTY:WAIT drop ;

: BAD-SHORT-FRAME ( -- )
   false FRAME-START PROCESS-PTY:WAIT drop ;

: CHECK-FRAME-CHILDREN ( -- )
   TARGET @ >PID PROC-WAIT-STATUS 0 T=
   SUP @ wait-status 0 < TTRUE ;

: CHECK-FRAME-CLEAN ( n -- )
   SUP-CLEAN-MASK @ swap T=
   CHECK-FRAME-CHILDREN
   CHECK-RECOVERY ;

: CHECK-FRAME-ERROR ( -- )
   [: BAD-FRAME ;] E-PROC-SPAWN TTHROWSQ
   SUP-CLEAN-WAIT CHECK-FRAME-CLEAN
   [: BAD-SHORT-FRAME ;] E-PROC-SPAWN TTHROWSQ
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN ;

: BAD-FRAME-LIVE ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START HANDLE>N RAW !
   true FRAME-START PROCESS-PTY:WAIT drop ;

: CHECK-FRAME-RESET ( -- )
   [: BAD-FRAME-LIVE ;] E-PROC-SPAWN TTHROWSQ
   SUP-CLEAN-MASK @ SUP-CLEAN-WAIT T=
   CHECK-FRAME-CHILDREN
   RAW @ N>HANDLE PROCESS-PTY:KILL CHECK-KILLED
   SUP-CLEAN-MASK @ 0 T= ;

: PRIVATE? ( ptr u8 n -- )
   XREF-FIND XREF-FOUND? TFALSE ;

: CHECK-PRIVATE ( -- )
   s" PROCESS-PTY:START-RAW" PRIVATE?
   s" PROCESS-PTY:START-SAVE" PRIVATE?
   s" PROCESS-PTY:START-GUARD" PRIVATE?
   s" PROCESS-PTY:BEFORE-COMMIT" PRIVATE?
   s" PROCESS-PTY:BEFORE-READY" PRIVATE?
   s" PROCESS-PTY:CLEAN-MASK" PRIVATE?
   s" PROCESS-PTY:SUP-CLEAN-MASK" PRIVATE? ;

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
   CHECK-RECOVERY
   CHECK-AUTH-RECOVERY ;

: RUN ( -- )
   T-RESET
   FIXTURE-SETUP
   s" /bin/cat" >LEN PROCESS-PTY:START
   CHECK-PIDS
   CHECK-IO
   PROCESS-PTY:KILL CHECK-KILLED
   CHECK-HB
   CHECK-RAW-SPAWN
   CHECK-OWNER-DEATH
   CHECK-COMMIT-RECOVERY
   CHECK-DEAD-READER
   CHECK-EXEC-ERROR
   CHECK-FRAME-ERROR
   CHECK-FRAME-RESET
   CHECK-SYSCALL-ERRORS
   CHECK-PRIVATE
   CLEANUP-RUN
   T-REPORT
   s" process-pty-io-test: ok" type cr ;

RUN

;package
