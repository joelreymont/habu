\ process-pty-handle-test.f - PTY authority lifecycle coverage.

require lib/process-pty-handle.f
require lib/process-fork.f
require lib/test.f
require test/checker-assert.f

package PROCESS-PTY

create HELD SLOT-CAP cells allot

variable RAW
variable NEXT-RAW

: SUP-PID ( -- pid )
   getpid 1+ >PID ;

: TARGET-PID ( -- pid )
   getpid 2 + >PID ;

: MAKE ( -- process-pty-handle )
   RESERVE SUP-PID TARGET-PID -1 >FD COMMIT ;

: DROP-RESOURCES ( pid pid fd -- )
   FD>N drop PID>N drop PID>N drop ;

: RETIRE ( process-pty-handle -- )
   TAKE DROP-RESOURCES ;

: CHECK-RAW ( -- )
   RAW @ N>HANDLE PROCESS-PTY:HANDLE-PID
   PID>N drop HANDLE>N drop ;

: CHECK-RESERVATION ( -- )
   RAW @ N>RESERVATION CANCEL ;

: SAVE ( process-pty-handle -- )
   HANDLE>N RAW ! ;

: SAVE-NEXT ( process-pty-handle -- )
   HANDLE>N NEXT-RAW ! ;

: CHECK-PID ( -- )
   MAKE PROCESS-PTY:HANDLE-PID
   PID>N TARGET-PID PID>N T=
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
   MAKE HANDLE>N i HELD! ;

: RELEASE-HELD ( n -- )
   HELD@ N>HANDLE RETIRE ;

: RESERVE-AND-CANCEL ( -- )
   RESERVE CANCEL ;

: CHECK-CAPACITY ( -- )
   0 begin dup SLOT-CAP < while dup HOLD 1+ repeat drop
   [: RESERVE-AND-CANCEL ;] E-PROC-PTY-CAPACITY TTHROWSQ
   0 begin dup SLOT-CAP < while dup RELEASE-HELD 1+ repeat drop ;

: CHECK-GEN-MAX ( -- )
   GEN-MAX 0 >IDX GEN!
   [: RESERVE-AND-CANCEL ;] E-PROC-PTY-CAPACITY TTHROWSQ
   0 0 >IDX GEN! ;

: CHILD-DIE ( n -- ) {: rc:n :}
   s" " rc die ;

: CHILD ( -- )
   [: CHECK-RAW ;] catch E-PROC-PTY-HANDLE =
   if 0 else 1 then
   CHILD-DIE ;

: FORK ( -- pid )
   PROC-FORK dup PID>N 0= if
      drop CHILD
   then ;

: CHECK-OWNER ( -- )
   MAKE SAVE
   FORK PROC-WAIT-STATUS 0 T=
   RAW @ N>HANDLE PROCESS-PTY:HANDLE-PID
   PID>N TARGET-PID PID>N T=
   RETIRE ;

: CHECK-STATIC ( -- )
   s" BAD-RES ( process-pty-reservation -- process-pty-reservation process-pty-reservation ) dup"
   CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-HANDLE ( process-pty-handle -- process-pty-handle process-pty-handle ) dup"
   CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   CHECK-PID
   CHECK-CANCEL
   CHECK-INACTIVE
   CHECK-REUSE
   CHECK-FORGED
   CHECK-CAPACITY
   CHECK-GEN-MAX
   CHECK-OWNER
   CHECK-STATIC
   T-REPORT
   s" process-pty-handle-test: ok" type cr ;

RUN

;package
