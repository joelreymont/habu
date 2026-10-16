\ process-pty-handle-test.f - PTY authority lifecycle coverage.

require src/lib/process-pty-handle.f
require src/lib/process-fork.f
require src/lib/test.f
require test/checker-assert.f

package PROCESS-PTY

create HELD SLOT-CAP cells allot
create WAIT-BYTE 1 allot

variable RAW
variable NEXT-RAW
variable TARGET-PID-V
variable WAIT-R
variable WAIT-W
variable BAD-MASTER
variable BAD-DONE
variable BAD-WATCH
variable BAD-GATE

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
   PROC-FORK:CHECKED dup PID>N 0= if drop WAITER-CHILD then ;

: SETUP-PID ( -- )
   PIPE-PAIR WAIT-W ! WAIT-R !
   SPAWN-WAITER TARGET-PID-V ! ;

: TEARDOWN-PID ( -- )
   WAIT-W @ >FD CLOSE-OK
   TARGET-PID-V @ >PID PROC-WAIT-STATUS 0 T=
   WAIT-R @ >FD CLOSE-OK ;

: TARGET-PID ( -- pid )
   TARGET-PID-V @ >PID ;

: MAKE-FDS ( -- fd fd fd fd )
   PIPE-PAIR {: master:fd done:fd :}
   PIPE-PAIR {: watch:fd gate:fd :}
   master done watch gate ;

: MAKE ( -- process-pty-handle )
   RESERVE TARGET-PID MAKE-FDS COMMIT ;

: RETIRE-TEARDOWN ( process-pty-teardown -- )
   TEARDOWN-MASTER-TAKE CLOSE-BAD >r
   TEARDOWN-DONE-TAKE CLOSE-BAD r> + >r
   TEARDOWN-WATCH-TAKE CLOSE-BAD r> + >r
   TEARDOWN-GATE-TAKE CLOSE-BAD r> +
   swap TEARDOWN-DONE
   0 T= ;

: RETIRE ( process-pty-handle -- )
   TAKE RETIRE-TEARDOWN ;

: TEST-TARGET ( process-pty-handle -- process-pty-handle pid )
   HANDLE-TARGET@ ;

: CHECK-RAW ( -- )
   RAW @ N>HANDLE TEST-TARGET
   PID>N drop RETIRE ;

: CHECK-RESERVATION ( -- )
   RAW @ N>RESERVATION CANCEL ;

: CHECK-TEARDOWN ( -- )
   RAW @ N>TEARDOWN TEARDOWN-MASTER-TAKE CLOSE-OK
   TEARDOWN-DONE ;

: SAVE ( process-pty-handle -- )
   HANDLE>N RAW ! ;

: SAVE-NEXT ( process-pty-handle -- )
   HANDLE>N NEXT-RAW ! ;

: CHECK-PID ( -- )
   MAKE HANDLE-TARGET@ PID>N TARGET-PID PID>N T= RETIRE ;

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
   PROC-FORK:CHECKED dup PID>N 0= if drop OWNER-CHILD then ;

: CHECK-OWNER ( -- )
   MAKE SAVE
   FORK-OWNER PROC-WAIT-STATUS 0 T=
   RAW @ N>HANDLE TEST-TARGET
   PID>N TARGET-PID PID>N T=
   RETIRE ;

: BAD-FDS! ( -- )
   MAKE-FDS BAD-GATE ! BAD-WATCH ! BAD-DONE ! BAD-MASTER ! ;

: BAD-PID-COMMIT ( -- )
   RESERVE -1 >PID
   BAD-MASTER @ BAD-DONE @ BAD-WATCH @ BAD-GATE @ COMMIT RETIRE ;

: OWNER-PID-COMMIT ( -- )
   RESERVE getpid >PID
   BAD-MASTER @ BAD-DONE @ BAD-WATCH @ BAD-GATE @ COMMIT RETIRE ;

: BAD-FD-COMMIT ( -- )
   RESERVE TARGET-PID
   BAD-MASTER @ BAD-DONE @ -1 >FD BAD-GATE @ COMMIT RETIRE ;

: CLOSED-FD-COMMIT ( -- )
   RESERVE TARGET-PID
   BAD-MASTER @ BAD-DONE @ BAD-WATCH @ BAD-GATE @ COMMIT RETIRE ;

: DUP-FD-COMMIT ( -- )
   RESERVE TARGET-PID
   BAD-MASTER @ BAD-MASTER @ BAD-WATCH @ BAD-GATE @ COMMIT RETIRE ;

: CHECK-COMMIT-VALIDATION ( -- )
   BAD-FDS!
   [: BAD-PID-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   [: OWNER-PID-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   [: BAD-FD-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   [: DUP-FD-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   BAD-WATCH @ CLOSE-OK
   [: CLOSED-FD-COMMIT ;] E-PROC-PTY-HANDLE TTHROWSQ
   BAD-MASTER @ CLOSE-OK
   BAD-DONE @ CLOSE-OK
   BAD-GATE @ CLOSE-OK
   MAKE RETIRE ;

: CHECK-GATE ( -- )
   MAKE
   HANDLE-GATE-TAKE CLOSE-OK
   HANDLE-GATE-TAKE FD>N -1 T=
   RETIRE ;

: CHECK-TEARDOWN-ONCE ( -- )
   MAKE TAKE TEARDOWN>N dup RAW ! N>TEARDOWN
   RETIRE-TEARDOWN
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
   s" PROCESS-PTY:TAKE" PRIVATE?
   s" PROCESS-PTY:TEARDOWN-DONE" PRIVATE? ;

: RUN ( -- )
   T-RESET
   SETUP-PID
   CHECK-PID
   CHECK-CANCEL
   CHECK-INACTIVE
   CHECK-REUSE
   CHECK-FORGED
   CHECK-CAPACITY
   CHECK-GEN-MAX
   CHECK-OWNER
   CHECK-COMMIT-VALIDATION
   CHECK-GATE
   CHECK-TEARDOWN-ONCE
   CHECK-STATIC
   CHECK-PRIVATE
   TEARDOWN-PID
   T-REPORT
   s" process-pty-handle-test: ok" type cr ;

RUN

;package
