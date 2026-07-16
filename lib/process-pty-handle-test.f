\ process-pty-handle-test.f - PTY authority lifecycle coverage.

require lib/process-pty-handle.f
require lib/process-fork.f
require lib/test.f
require test/checker-assert.f

package PROCESS-PTY

create HELD SLOT-CAP cells allot
create WAIT-BYTE 1 allot

variable RAW
variable NEXT-RAW
variable SUP-PID-V
variable TARGET-PID-V
variable WAIT-R
variable WAIT-W
variable BAD-MASTER
variable BAD-LIFE
variable BAD-DONE

: CLOSE-BAD ( fd -- n ) {: fd:fd :}
   fd FD>N 0 < if 0 exit then
   fd FD>N close-rc 0 <> if 1 exit then
   0 ;

: CLOSE-OK ( fd -- )
   CLOSE-BAD 0 T= ;

: CHILD-DIE ( n -- ) {: rc:n :}
   s" " rc die ;

: WAITER-CHILD ( -- )
   WAIT-W @ >FD CLOSE-BAD 0 <> if 1 CHILD-DIE then
   WAIT-R @ WAIT-BYTE 1 read 0= if 0 CHILD-DIE then
   1 CHILD-DIE ;

: SPAWN-WAITER ( -- pid )
   PROC-FORK dup PID>N 0= if
      drop WAITER-CHILD
   then ;

: SETUP-PIDS ( -- )
   PIPE-PAIR WAIT-W ! WAIT-R !
   SPAWN-WAITER SUP-PID-V !
   SPAWN-WAITER TARGET-PID-V ! ;

: WAIT-OK ( pid -- )
   PROC-WAIT-STATUS 0 T= ;

: TEARDOWN-PIDS ( -- )
   WAIT-W @ >FD CLOSE-OK
   SUP-PID-V @ WAIT-OK
   TARGET-PID-V @ WAIT-OK
   WAIT-R @ >FD CLOSE-OK ;

: SUP-PID ( -- pid )
   SUP-PID-V @ ;

: TARGET-PID ( -- pid )
   TARGET-PID-V @ ;

: MAKE-FDS ( -- fd fd fd )
   PIPE-PAIR {: master:fd life:fd :}
   PIPE-PAIR {: done:fd spare:fd :}
   spare CLOSE-OK
   master life done ;

: MAKE ( -- process-pty-handle )
   RESERVE SUP-PID TARGET-PID MAKE-FDS COMMIT ;

: RETIRE ( process-pty-handle -- )
   TAKE TEARDOWN-VIEW
   >r >r >r 2drop
   r> CLOSE-BAD r> CLOSE-BAD + r> CLOSE-BAD +
   swap TEARDOWN-DONE
   0 T= ;

: TEST-TARGET ( process-pty-handle -- process-pty-handle pid )
   VIEW >r >r >r swap drop r> drop r> drop r> drop ;

: TEST-PIDS ( process-pty-handle -- process-pty-handle pid pid )
   VIEW >r >r >r r> drop r> drop r> drop ;

: CHECK-RAW ( -- )
   RAW @ N>HANDLE TEST-TARGET
   PID>N drop RETIRE ;

: CHECK-RESERVATION ( -- )
   RAW @ N>RESERVATION CANCEL ;

: CHECK-TEARDOWN ( -- )
   RAW @ N>TEARDOWN TEARDOWN-VIEW
   >r >r >r 2drop
   r> drop r> drop r> drop
   TEARDOWN-DONE ;

: SAVE ( process-pty-handle -- )
   HANDLE>N RAW ! ;

: SAVE-NEXT ( process-pty-handle -- )
   HANDLE>N NEXT-RAW ! ;

: CHECK-PIDS ( -- )
   MAKE TEST-PIDS
   PID>N TARGET-PID PID>N T=
   PID>N SUP-PID PID>N T=
   RETIRE ;

: CHECK-CANCEL ( -- )
   RESERVE RESERVATION>N dup RAW ! N>RESERVATION CANCEL
   [: CHECK-RESERVATION ;] E-PROC-PTY-HANDLE TTHROWSQ ;

: CHECK-INACTIVE ( -- )
   MAKE SAVE
   RAW @ N>HANDLE RETIRE
   [: CHECK-RAW ;] E-PROC-PTY-HANDLE TTHROWSQ ;

: CHECK-REUSE ( -- )
   MAKE SAVE
   RAW @ N>HANDLE RETIRE
   MAKE SAVE-NEXT
   RAW @ UNPACK-IDX NEXT-RAW @ UNPACK-IDX IDX>N swap IDX>N T=
   RAW @ UNPACK-GEN NEXT-RAW @ UNPACK-GEN T<>
   [: CHECK-RAW ;] E-PROC-PTY-HANDLE TTHROWSQ
   NEXT-RAW @ N>HANDLE RETIRE ;

: CHECK-FORGED ( -- )
   1 8 lshift SLOT-CAP or RAW !
   [: CHECK-RAW ;] E-PROC-PTY-HANDLE TTHROWSQ ;

: HELD! ( n n -- ) {: raw:n i:n :}
   raw HELD i cells + ! ;

: HELD@ ( n -- n )
   cells HELD + @ ;

: HOLD ( n -- ) {: i:n :}
   RESERVE RESERVATION>N i HELD! ;

: RELEASE-HELD ( n -- )
   HELD@ N>RESERVATION CANCEL ;

: RESERVE-AND-CANCEL ( -- )
   RESERVE CANCEL ;

: CHECK-CAPACITY ( -- )
   0 begin dup SLOT-CAP < while dup HOLD 1+ repeat drop
   [: RESERVE-AND-CANCEL ;] E-PROC-PTY-CAPACITY TTHROWSQ
   0 begin dup SLOT-CAP < while dup RELEASE-HELD 1+ repeat drop ;

: CHECK-GEN-MAX ( -- )
   GEN-MAX 0 >IDX GEN!
   RESERVE-AND-CANCEL
   GEN-MAX 0 >IDX GEN!
   1 begin dup SLOT-CAP < while
      GEN-MAX over >IDX GEN!
      1+
   repeat drop
   [: RESERVE-AND-CANCEL ;] E-PROC-PTY-CAPACITY TTHROWSQ
   0 begin dup SLOT-CAP < while 0 over >IDX GEN! 1+ repeat drop ;

: OWNER-CHILD ( -- )
   [: CHECK-RAW ;] catch E-PROC-PTY-HANDLE =
   if 0 else 1 then
   CHILD-DIE ;

: FORK-OWNER ( -- pid )
   PROC-FORK dup PID>N 0= if
      drop OWNER-CHILD
   then ;

: CHECK-OWNER ( -- )
   MAKE SAVE
   FORK-OWNER WAIT-OK
   RAW @ N>HANDLE TEST-TARGET
   PID>N TARGET-PID PID>N T=
   RETIRE ;

: BAD-FDS! ( -- )
   MAKE-FDS BAD-DONE ! BAD-LIFE ! BAD-MASTER ! ;

: BAD-FDS-CLOSE ( -- )
   BAD-MASTER @ CLOSE-OK
   BAD-LIFE @ CLOSE-OK
   BAD-DONE @ CLOSE-OK ;

: BAD-PID-COMMIT ( -- )
   RESERVE SUP-PID -1 >PID
   BAD-MASTER @ BAD-LIFE @ BAD-DONE @ COMMIT
   RETIRE ;

: BAD-FD-COMMIT ( -- )
   RESERVE SUP-PID TARGET-PID
   BAD-MASTER @ BAD-LIFE @ -1 >FD COMMIT
   RETIRE ;

: CHECK-COMMIT-VALIDATION ( -- )
   BAD-FDS!
   [: BAD-PID-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   BAD-FDS-CLOSE
   BAD-FDS!
   [: BAD-FD-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   BAD-FDS-CLOSE ;

: CHECK-TEARDOWN-ONCE ( -- )
   MAKE TAKE TEARDOWN>N dup RAW ! N>TEARDOWN
   TEARDOWN-VIEW >r >r >r 2drop
   r> CLOSE-OK r> CLOSE-OK r> CLOSE-OK
   TEARDOWN-DONE
   [: CHECK-TEARDOWN ;] E-PROC-PTY-HANDLE TTHROWSQ ;

: CHECK-STATIC ( -- )
   s" BAD-RES ( process-pty-reservation -- process-pty-reservation process-pty-reservation ) dup"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-HANDLE ( process-pty-handle -- process-pty-handle process-pty-handle ) dup"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-TEARDOWN ( process-pty-teardown -- process-pty-teardown process-pty-teardown ) dup"
   CHECK-QUIET-CANDIDATE! 0 T= ;

: PRIVATE? ( ptr u8 n -- )
   XREF-FIND XREF-FOUND? TFALSE ;

: CHECK-PRIVATE ( -- )
   s" PROCESS-PTY:N>HANDLE" PRIVATE?
   s" PROCESS-PTY:HANDLE>N" PRIVATE?
   s" PROCESS-PTY:N>RESERVATION" PRIVATE?
   s" PROCESS-PTY:RESERVATION>N" PRIVATE?
   s" PROCESS-PTY:N>TEARDOWN" PRIVATE?
   s" PROCESS-PTY:TEARDOWN>N" PRIVATE?
   s" PROCESS-PTY:RESERVE" PRIVATE?
   s" PROCESS-PTY:COMMIT" PRIVATE?
   s" PROCESS-PTY:VIEW" PRIVATE?
   s" PROCESS-PTY:TAKE" PRIVATE?
   s" PROCESS-PTY:TEARDOWN-VIEW" PRIVATE?
   s" PROCESS-PTY:TEARDOWN-DONE" PRIVATE? ;

: RUN ( -- )
   T-RESET
   SETUP-PIDS
   CHECK-PIDS
   CHECK-CANCEL
   CHECK-INACTIVE
   CHECK-REUSE
   CHECK-FORGED
   CHECK-CAPACITY
   CHECK-GEN-MAX
   CHECK-OWNER
   CHECK-COMMIT-VALIDATION
   CHECK-TEARDOWN-ONCE
   CHECK-STATIC
   CHECK-PRIVATE
   TEARDOWN-PIDS
   T-REPORT
   s" process-pty-handle-test: ok" type cr ;

RUN

;package
