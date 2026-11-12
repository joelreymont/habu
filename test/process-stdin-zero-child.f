\ process-stdin-zero-child.f - child program run as
\ `bin/hb test/process-stdin-zero-child.f`. It closes its own fd 0, so the
\ capture's stdin pipe opens with its close-on-exec read end on fd 0, the number
\ the child's stdin needs. It writes the payload, closes the write end and runs
\ /bin/cat on that read end: the payload reaches stdout only if the spawn kept
\ fd 0 open across exec. A pipe that did not land on fd 0 exits 3, so a red is
\ the spawn's alone.

require src/lib/process.f

package PROC-STDIN-ZERO-CHILD

: CAT ( -- n )
   s" /bin/cat" >LEN PROCESS:IN-R @ >FD -1 >FD -1 >FD PROCESS:RUN-IO-RC
   MATCH result
     ok  OF ENDOF
     err OF ENDOF
   ;MATCH ;

public

: RUN ( -- )
   0 close
   PROCESS:CAPTURE-RESET
   PROCESS:SETUP-STDIN-FDS
   PROCESS:IN-R @ 0 <> if s" stdin pipe is not on fd 0" 3 die then
   PROCESS:IN-W @ s" stdin-zero" write 10 <> if s" short write to the stdin pipe" 4 die then
   PROCESS:IN-W PROCESS:CLOSE-CELL
   CAT {: rc:n :}
   rc 0 <> if s" cat failed" rc die then ;

;package

PROC-STDIN-ZERO-CHILD:RUN
