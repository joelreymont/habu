\ process-pty-handle-test.f - PTY handle ownership coverage.

require lib/process-pty-handle.f
require lib/process-fork.f
require lib/test.f

package PROCESS-PTY

variable INHERITED

: CHECK ( -- )
   INHERITED @ N>HANDLE PROCESS-PTY:HANDLE-PID
   PID>N drop
   HANDLE>N drop ;

: CHILD-DIE ( n -- ) {: rc:n :}
   s" " rc die ;

: CHILD ( -- )
   [: CHECK ;] catch E-PROC-PTY-HANDLE =
   if 0 else 1 then
   CHILD-DIE ;

: FORK ( -- pid )
   PROC-FORK dup PID>N 0= if
      drop CHILD
   then ;

: PARENT ( -- )
   INHERITED @ N>HANDLE PROCESS-PTY:HANDLE-PID
   PID>N getpid T=
   HANDLE>N drop ;

: OWNER ( -- )
   RESERVE {: idx:idx :}
   getpid >PID {: pid:pid :}
   pid pid -1 >FD idx STORE
   idx PACK INHERITED !
   FORK PROC-WAIT-STATUS 0 T=
   PARENT
   idx RELEASE
   0 INHERITED ! ;

: RUN ( -- )
   T-RESET
   OWNER
   T-REPORT
   s" process-pty-handle-test: ok" type cr ;

RUN

;package
