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
create ROOT-BUF FS-PATH-CAP allot
create SCRIPT-BUF FS-PATH-CAP allot
create RACE-BUF FS-PATH-CAP allot
create LEADER-BUF FS-PATH-CAP allot

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
variable LEADER-U
variable SENT-R
variable SENT-W
variable OWNER-RACE
variable MASTER-R
variable MASTER-W
variable LIFE-R
variable LIFE-W
variable DONE-R
variable DONE-W
variable ANCHOR-R
variable ANCHOR-W
variable FRAME-DETAIL
variable FRAME-CODE
variable FRAME-EXTRA
variable FRAME-CODE-U
variable FRAME-DETAIL-U
variable ZOMBIE-R
variable ZOMBIE-W
variable ARM-SUP

: SAVE-PIDS ( process-pty-handle pid pid -- process-pty-handle )
   PID>N TARGET !
   PID>N SUP ! ;

: TEST-PIDS ( process-pty-handle -- process-pty-handle pid pid )
   VIEW >r >r >r >r >r >r
   r> drop r> drop r> drop r> drop r> drop r> drop ;

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

: CHECK-EXITED-7 ( outcome -- )
   MATCH outcome
     exited OF 7 T= ENDOF
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

: LEADER$ ( -- ptr u8 n )
   LEADER-BUF LEADER-U @ ;

: FIXTURE-SETUP ( -- )
   CLEANUP-RESET
   s" habu-pty-io" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U PATH!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hold.sh" SCRIPT-BUF JOIN-PATH SCRIPT-U !
   ROOT$ s" race.sh" RACE-BUF JOIN-PATH RACE-U !
   ROOT$ s" leader.sh" LEADER-BUF JOIN-PATH LEADER-U !
   SCRIPT$ S\" #!/bin/sh\ntrap '' HUP\n( sleep 30 ) &\necho ready\nwait\n" WRITE-ALL
   SCRIPT$ CHMOD-X
   RACE$ S\" #!/bin/sh\ntrap '' HUP\nread line\n( sleep 30 ) &\nkill -9 $$\n" WRITE-ALL
   RACE$ CHMOD-X
   LEADER$ S\" #!/bin/sh\ntrap '' HUP\n( sleep 30 ) &\nexit 7\n" WRITE-ALL
   LEADER$ CHMOD-X ;

: REQUIRE-EOF ( fd -- )
   READ-EOF? 0= if E-PROC-OUTPUT throw then ;

: REQUIRE-NOT-READY ( fd -- )
   0 >MS POLL-ONE 0 T= ;

: REQUIRE-READY ( fd -- )
   0 >MS POLL-ONE 0 > TTRUE ;

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
   TEST-PIDS SAVE-PIDS HANDLE>N RAW !
   OWNER-KEEPER drop
   RAW @ N>HANDLE
   SENT-W @ CLOSE-FD
   KEEP-SENT-W @ CLOSE-FD
   SUP-SENT-W @ CLOSE-FD
   KEEP-R @ CLOSE-FD
   OWNER-W @ >FD TARGET @ FRAME-WRITE
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
   OWNER-R @ >FD FRAME-READ TARGET !
   OWNER-R @ CLOSE-FD
   OWNER-GATE-W @ CLOSE-FD
   owner PROC-WAIT-STATUS 0 T=
   SUP-SENT-R @ >FD OWNER-EXIT-MS >MS POLL-ONE 0 > TTRUE
   SUP-SENT-R @ >FD REQUIRE-EOF
   SUP-SENT-R @ CLOSE-FD
   s" owner group empty" T-LABEL TARGET @ >PID GROUP-PROBE-RC ESRCH# negate T=
   SENT-R @ >FD REQUIRE-READY
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

: CHECK-LEADER-EXIT ( -- )
   SENTINEL-OPEN
   LEADER$ >LEN PROCESS-PTY:START
   CHECK-PIDS
   SENT-W @ CLOSE-FD
   PROCESS-PTY:WAIT CHECK-EXITED-7
   s" leader group empty" T-LABEL TARGET @ >PID GROUP-PROBE-RC ESRCH# negate T=
   SENT-R @ >FD REQUIRE-READY
   SENT-R @ >FD REQUIRE-EOF
   SENT-R @ CLOSE-FD ;

: ZOMBIE-CHILD ( -- )
   ZOMBIE-W @ CLOSE-FD
   ZOMBIE-R @ >FD REQUIRE-EOF
   ZOMBIE-R @ CLOSE-FD
   s" " 0 die ;

: ZOMBIE-SPAWN ( -- pid )
   PROC-FORK dup PID>N 0= if drop ZOMBIE-CHILD then ;

: ZOMBIE-WATCH ( pid -- fd )
   PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then >FD ;

: ZOMBIE-READY ( fd -- )
   OWNER-EXIT-MS >MS POLL-ONE 0 <= if E-PROC-TIMEOUT throw then ;

: CHECK-ZOMBIE-RETIRE ( -- )
   PIPE-PAIR ZOMBIE-W ! ZOMBIE-R !
   ZOMBIE-SPAWN {: anchor:pid :}
   anchor PID>N dup setpgid 0 <> if E-PROC-OUTPUT throw then
   ZOMBIE-SPAWN {: target:pid :}
   target PID>N anchor PID>N setpgid 0 <> if E-PROC-OUTPUT throw then
   anchor ZOMBIE-WATCH {: anchor-watch:fd :}
   target ZOMBIE-WATCH {: target-watch:fd :}
   ZOMBIE-R @ CLOSE-FD
   ZOMBIE-W @ CLOSE-FD
   anchor-watch ZOMBIE-READY
   target-watch ZOMBIE-READY
   anchor-watch FD>N CLOSE-FD
   target-watch FD>N CLOSE-FD
   anchor GROUP-KILL-RC {: first:n :}
   HB-TARGET-MACOS? if first EPERM# negate T= else first KILL-CLEAN? TTRUE then
   target SUP-TARGET CELL-PID!
   anchor SUP-ANCHOR CELL-PID!
   anchor SUP-PGRP CELL-PID!
   true SUP-GROUP !
   SUP-RETIRE-GROUP 0 T=
   s" zombie group empty" T-LABEL anchor GROUP-PROBE-RC ESRCH# negate T= ;

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
   s" commit cleanup mask" T-LABEL
   CLEAN-MASK @ CLEAN-MASTER CLEAN-SUP or T=
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
   s" ready cleanup mask" T-LABEL CLEAN-MASK @ CLEAN-DONE-R and CLEAN-DONE-R T=
   SUP @ wait-status 0 < TTRUE
   CHECK-RECOVERY ;

: BAD-EXEC ( -- )
   s" /no/such/habu-pty-executable" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-EXEC-ERROR ( -- )
   [: BAD-EXEC ;] E-PROC-SPAWN TTHROWSQ
   s" exec supervisor mask" T-LABEL SUP-CLEAN-MASK @ 0 T=
   s" exec parent mask" T-LABEL CLEAN-MASK @ 0 T=
   CHECK-RECOVERY ;

: BREAK-JOIN ( -- )
   E-PROC-OUTPUT throw ;

: INJECT-ANCHOR-JOIN ( -- )
   [: BREAK-JOIN ;] is BEFORE-ANCHOR-JOIN ;

: INJECT-TARGET-JOIN ( -- )
   [: BREAK-JOIN ;] is BEFORE-TARGET-JOIN ;

: RESET-JOINS ( -- )
   ANCHOR-JOIN-DEFAULT
   TARGET-JOIN-DEFAULT
   ANCHOR-RELEASE-DEFAULT ;

: DIE-BEFORE-PUBLISH ( -- )
   POLL-FDS 0 100 poll 0 < if E-PROC-OUTPUT throw then
   s" " 1 die ;

: INJECT-TARGET-PUBLISH ( -- )
   [: DIE-BEFORE-PUBLISH ;] is BEFORE-TARGET-PUBLISH ;

: BAD-ANCHOR-JOIN ( -- )
   SENTINEL-OPEN
   INJECT-ANCHOR-JOIN
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: BAD-TARGET-JOIN ( -- )
   SENTINEL-OPEN
   INJECT-TARGET-JOIN
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-JOIN-SENTINEL ( -- )
   SENT-W @ CLOSE-FD
   SENT-R @ >FD REQUIRE-READY
   SENT-R @ >FD REQUIRE-EOF
   SENT-R @ CLOSE-FD ;

: CHECK-PARTIAL-JOINS ( -- )
   [: BAD-ANCHOR-JOIN ;] E-PROC-OUTPUT TTHROWSQ
   RESET-JOINS
   CHECK-JOIN-SENTINEL
   SUP-CLEAN-MASK @ 0 T=
   [: BAD-TARGET-JOIN ;] E-PROC-OUTPUT TTHROWSQ
   RESET-JOINS
   CHECK-JOIN-SENTINEL
   SUP-CLEAN-MASK @ 0 T=
   s" partial join group empty" T-LABEL TX-PGRP CELL-PID@ GROUP-PROBE-RC ESRCH# negate T=
   CHECK-RECOVERY ;

: DIE-BEFORE-ANCHOR-READY ( -- )
   s" " 1 die ;

: INJECT-ANCHOR-HANDOFF ( -- )
   [: DIE-BEFORE-ANCHOR-READY ;] is BEFORE-ANCHOR-JOIN ;

: BAD-ANCHOR-HANDOFF ( -- )
   INJECT-ANCHOR-HANDOFF
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-ANCHOR-HANDOFF ( -- )
   [: BAD-ANCHOR-HANDOFF ;] E-PROC-OUTPUT TTHROWSQ
   RESET-JOINS
   TX-PGRP CELL-PID@ PID>N 0 > TTRUE
   s" anchor handoff group empty" T-LABEL
   TX-PGRP CELL-PID@ GROUP-PROBE-RC ESRCH# negate T=
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: INJECT-ANCHOR-RELEASE ( -- )
   [: DIE-BEFORE-ANCHOR-READY ;] is BEFORE-ANCHOR-RELEASE ;

: BAD-ANCHOR-RELEASE ( -- )
   INJECT-ANCHOR-RELEASE
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-ANCHOR-RELEASE ( -- )
   [: BAD-ANCHOR-RELEASE ;] E-PROC-OUTPUT TTHROWSQ
   RESET-JOINS
   TX-PGRP CELL-PID@ PID>N 0 > TTRUE
   s" anchor release group empty" T-LABEL
   TX-PGRP CELL-PID@ GROUP-PROBE-RC ESRCH# negate T=
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: BAD-TARGET-PUBLISH ( -- )
   SENTINEL-OPEN
   INJECT-TARGET-PUBLISH
   SCRIPT$ >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-TARGET-PUBLISH-DEATH ( -- )
   [: BAD-TARGET-PUBLISH ;] E-PROC-OUTPUT TTHROWSQ
   TARGET-PUBLISH-DEFAULT
   CHECK-JOIN-SENTINEL
   s" publish group empty" T-LABEL TX-PGRP CELL-PID@ GROUP-PROBE-RC ESRCH# negate T=
   CHECK-RECOVERY ;

: FAIL-SUP-GUARD ( pid -- fd )
   PID>N ARM-SUP !
   -1 >FD ;

: INJECT-SUP-GUARD ( -- )
   [: FAIL-SUP-GUARD ;] is OPEN-SUP-GUARD ;

: BAD-SUP-GUARD-ARM ( -- )
   INJECT-SUP-GUARD
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-SUP-GUARD-ARM ( -- )
   [: BAD-SUP-GUARD-ARM ;] E-PROC-OUTPUT TTHROWSQ
   OPEN-SUP-GUARD-DEFAULT
   TX-PGRP CELL-PID@ PID>N -1 T=
   ARM-SUP @ wait-status 0 < TTRUE
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: KILL-BEFORE-SUP-RELEASE ( -- )
   TX-SUP CELL-PID@ {: sup:pid :}
   sup PID>N ARM-SUP !
   sup PID>N SIGKILL kill-errno 0 <> if E-PROC-OUTPUT throw then
   TX-SUP-GUARD CELL-FD@ OWNER-EXIT-MS >MS POLL-ONE
   0 <= if E-PROC-TIMEOUT throw then ;

: INJECT-SUP-RELEASE ( -- )
   [: KILL-BEFORE-SUP-RELEASE ;] is BEFORE-SUP-RELEASE ;

: BAD-SUP-RELEASE ( -- )
   INJECT-SUP-RELEASE
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-SUP-RELEASE ( -- )
   [: BAD-SUP-RELEASE ;] E-PROC-OUTPUT TTHROWSQ
   SUP-RELEASE-DEFAULT
   TX-PGRP CELL-PID@ PID>N -1 T=
   ARM-SUP @ wait-status 0 < TTRUE
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: KILL-BEFORE-GROUP-ACK ( -- )
   TX-PGRP CELL-PID@ {: anchor:pid :}
   anchor PID>N SIGKILL kill-errno 0 <> if E-PROC-OUTPUT throw then
   TX-GUARD CELL-FD@ OWNER-EXIT-MS >MS POLL-ONE
   0 <= if E-PROC-TIMEOUT throw then ;

: INJECT-GROUP-ACK ( -- )
   [: KILL-BEFORE-GROUP-ACK ;] is BEFORE-GROUP-ACK ;

: BAD-GROUP-ACK ( -- )
   INJECT-GROUP-ACK
   s" /bin/cat" >LEN PROCESS-PTY:START
   PROCESS-PTY:KILL drop ;

: CHECK-GROUP-ACK ( -- )
   [: BAD-GROUP-ACK ;] E-PROC-OUTPUT TTHROWSQ
   GROUP-ACK-DEFAULT
   s" group ack group empty" T-LABEL
   TX-PGRP CELL-PID@ GROUP-PROBE-RC ESRCH# negate T=
   CHECK-RECOVERY ;

: BREAK-SUP-CLEAN ( -- )
   SUP-PROC CELL-FD@ FD>N close-rc 0 <> if E-PROC-OUTPUT throw then ;

: INJECT-SUP-CLEAN ( -- )
   [: BREAK-SUP-CLEAN ;] is BEFORE-SUP-CLEAN ;

: RESET-SUP-CLEAN ( -- )
   SUP-CLEAN-DEFAULT ;

: BAD-SUP-CLEAN ( -- )
   INJECT-SUP-CLEAN
   s" /usr/bin/true" >LEN PROCESS-PTY:START
   RESET-SUP-CLEAN
   PROCESS-PTY:WAIT drop ;

: CHECK-SUP-CLEAN-ERROR ( -- )
   [: BAD-SUP-CLEAN ;] E-PROC-OUTPUT TTHROWSQ
   RESET-SUP-CLEAN
   s" supervisor cleanup mask" T-LABEL SUP-CLEAN-MASK @ SUP-CLEAN-PROC T=
   CHECK-RECOVERY ;

: REQUIRE-TARGET-READY ( process-pty-handle -- process-pty-handle )
   IO-WAIT-MS >MS PROCESS-PTY:POLL-IN COUNT>N 0 <= if E-PROC-OUTPUT throw then
   BUF BUF-CAP PROCESS-PTY:READ 0 <= if E-PROC-OUTPUT throw then ;

: BAD-SUPERVISOR-DEATH ( -- )
   SENTINEL-OPEN
   SCRIPT$ >LEN PROCESS-PTY:START
   SAVE-RAW
   CHECK-PIDS
   REQUIRE-TARGET-READY
   SENT-W @ CLOSE-FD
   SUP @ SIGKILL kill-errno 0 <> if E-PROC-OUTPUT throw then
   PROCESS-PTY:WAIT drop ;

: CHECK-SUPERVISOR-DEATH ( -- )
   [: BAD-SUPERVISOR-DEATH ;] E-PROC-OUTPUT TTHROWSQ
   s" supervisor death mask" T-LABEL SUP-CLEAN-MASK @ SUP-CLEAN-PROTOCOL T=
   SENT-R @ >FD REQUIRE-READY
   SENT-R @ >FD REQUIRE-EOF
   SENT-R @ CLOSE-FD
   s" dead supervisor group empty" T-LABEL TARGET @ >PID GROUP-PROBE-RC ESRCH# negate T=
   CHECK-INACTIVE
   CHECK-RECOVERY ;

: FRAME-PIPES ( -- )
   PIPE-PAIR MASTER-W ! MASTER-R !
   PIPE-PAIR LIFE-W ! LIFE-R !
   PIPE-PAIR DONE-W ! DONE-R !
   PIPE-PAIR ANCHOR-W ! ANCHOR-R ! ;

: FRAME-TARGET ( -- pid )
   PROC-FORK dup PID>N 0= if drop s" " 0 die then ;

: FRAME-WRITE-U ( fd n n -- ) {: fd:fd value:n u:n :}
   value FRAME !
   fd FRAME u WRITE-EXACT ;

: FRAME-SUP-CHILD ( -- )
   DONE-R @ CLOSE-FD
   FRAME-CODE-U @ 0 > if
      DONE-W @ >FD FRAME-CODE @ FRAME-CODE-U @ FRAME-WRITE-U
   then
   FRAME-DETAIL-U @ 0 > if
      DONE-W @ >FD FRAME-DETAIL @ FRAME-DETAIL-U @ FRAME-WRITE-U
   then
   FRAME-EXTRA @ if DONE-W @ >FD 1 FRAME-WRITE then
   DONE-W @ CLOSE-FD
   s" " 0 die ;

: FRAME-SUP ( -- pid )
   PROC-FORK dup PID>N 0= if drop FRAME-SUP-CHILD then ;

: FRAME-START-SIZED ( n n bool n n -- process-pty-handle )
   {: code:n detail:n extra:bool code-u:n detail-u:n :}
   extra FRAME-EXTRA !
   detail FRAME-DETAIL !
   code FRAME-CODE !
   code-u FRAME-CODE-U !
   detail-u FRAME-DETAIL-U !
   FRAME-PIPES
   FRAME-TARGET dup PID>N TARGET ! {: target:pid :}
   FRAME-SUP dup PID>N SUP ! {: sup:pid :}
   MASTER-W @ CLOSE-FD
   LIFE-R @ CLOSE-FD
   DONE-W @ CLOSE-FD
   ANCHOR-R @ CLOSE-FD
   sup PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then >FD {: guard:fd :}
   sup PID>N proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then >FD {: sup-guard:fd :}
   RESERVE sup target MASTER-R @ >FD LIFE-W @ >FD DONE-R @ >FD
   ANCHOR-W @ >FD guard sup-guard COMMIT ;

: FRAME-START-CODE ( n n bool -- process-pty-handle )
   {: code:n detail:n extra:bool :}
   code detail extra 1 cells
   detail 0 >= if 1 cells else 0 then
   FRAME-START-SIZED ;

: FRAME-START ( bool -- process-pty-handle )
   if SUP-CLEAN-WAIT else -1 then
   E-PROC-SPAWN swap false FRAME-START-CODE ;

: BAD-FRAME ( -- )
   true FRAME-START PROCESS-PTY:WAIT drop ;

: BAD-SHORT-FRAME ( -- )
   false FRAME-START PROCESS-PTY:WAIT drop ;

: BAD-CONTRADICT-FRAME ( -- )
   0 SUP-CLEAN-WAIT false FRAME-START-CODE PROCESS-PTY:WAIT drop ;

: BAD-POSITIVE-SHORT-FRAME ( -- )
   0 -1 false FRAME-START-CODE PROCESS-PTY:WAIT drop ;

: BAD-EXTRA-FRAME ( -- )
   0 0 true FRAME-START-CODE PROCESS-PTY:WAIT drop ;

: BAD-PARTIAL-FIRST-FRAME ( -- )
   E-PROC-SPAWN 0 false 1 0 FRAME-START-SIZED PROCESS-PTY:WAIT drop ;

: BAD-PARTIAL-SECOND-FRAME ( -- )
   E-PROC-SPAWN 0 false 1 cells 1 FRAME-START-SIZED PROCESS-PTY:WAIT drop ;

: CHECK-FRAME-CHILDREN ( -- )
   TARGET @ >PID PROC-WAIT-STATUS 0 T=
   SUP @ wait-status 0 < TTRUE ;

: CHECK-FRAME-CLEAN ( n -- )
   s" fake frame mask" T-LABEL SUP-CLEAN-MASK @ swap T=
   CHECK-FRAME-CHILDREN
   s" fake frame group empty" T-LABEL
   TARGET @ >PID GROUP-PROBE-RC ESRCH# negate T=
   CHECK-RECOVERY ;

: CHECK-FRAME-ERROR ( -- )
   [: BAD-FRAME ;] catch E-PROC-SPAWN T=
   SUP-CLEAN-WAIT CHECK-FRAME-CLEAN
   [: BAD-SHORT-FRAME ;] catch E-PROC-SPAWN T=
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN
   [: BAD-CONTRADICT-FRAME ;] catch E-PROC-OUTPUT T=
   SUP-CLEAN-WAIT CHECK-FRAME-CLEAN
   [: BAD-POSITIVE-SHORT-FRAME ;] E-PROC-OUTPUT TTHROWSQ
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN
   [: BAD-EXTRA-FRAME ;] E-PROC-OUTPUT TTHROWSQ
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN
   [: BAD-PARTIAL-FIRST-FRAME ;] E-PROC-OUTPUT TTHROWSQ
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN
   [: BAD-PARTIAL-SECOND-FRAME ;] E-PROC-SPAWN TTHROWSQ
   SUP-CLEAN-PROTOCOL CHECK-FRAME-CLEAN ;

: BAD-FRAME-LIVE ( -- )
   s" /bin/cat" >LEN PROCESS-PTY:START HANDLE>N RAW !
   true FRAME-START PROCESS-PTY:WAIT drop ;

: CHECK-FRAME-RESET ( -- )
   [: BAD-FRAME-LIVE ;] catch E-PROC-SPAWN T=
   s" live frame mask" T-LABEL SUP-CLEAN-MASK @ SUP-CLEAN-WAIT T=
   CHECK-FRAME-CHILDREN
   RAW @ N>HANDLE PROCESS-PTY:KILL CHECK-KILLED
   s" frame recovery mask" T-LABEL SUP-CLEAN-MASK @ 0 T= ;

: PRIVATE? ( ptr u8 n -- )
   XREF-FIND XREF-FOUND? TFALSE ;

: CHECK-PRIVATE ( -- )
   s" PROCESS-PTY:START-RAW" PRIVATE?
   s" PROCESS-PTY:START-SAVE" PRIVATE?
   s" PROCESS-PTY:START-GUARD" PRIVATE?
   s" PROCESS-PTY:BEFORE-COMMIT" PRIVATE?
   s" PROCESS-PTY:BEFORE-READY" PRIVATE?
   s" PROCESS-PTY:BEFORE-SUP-CLEAN" PRIVATE?
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
   CHECK-FRAME-ERROR
   CHECK-FRAME-RESET
   s" /bin/cat" >LEN PROCESS-PTY:START
   CHECK-PIDS
   CHECK-IO
   PROCESS-PTY:KILL CHECK-KILLED
   CHECK-HB
   CHECK-RAW-SPAWN
   CHECK-OWNER-DEATH
   CHECK-LEADER-EXIT
   CHECK-ZOMBIE-RETIRE
   CHECK-COMMIT-RECOVERY
   CHECK-DEAD-READER
   CHECK-EXEC-ERROR
   CHECK-PARTIAL-JOINS
   CHECK-ANCHOR-HANDOFF
   CHECK-ANCHOR-RELEASE
   CHECK-TARGET-PUBLISH-DEATH
   CHECK-SUP-GUARD-ARM
   CHECK-SUP-RELEASE
   CHECK-GROUP-ACK
   CHECK-SUP-CLEAN-ERROR
   CHECK-SUPERVISOR-DEATH
   CHECK-SYSCALL-ERRORS
   CHECK-PRIVATE
   CLEANUP-RUN
   T-REPORT
   s" process-pty-io-test: ok" type cr ;

RUN

;package
