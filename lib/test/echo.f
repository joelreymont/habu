\ echo.f - echo a captured child's output to the parent at line boundaries.
\
\ Layered on lib/test/runner.f.  The runner captures a child into its bounded
\ stdout and stderr buffers; these words write what has arrived through to the
\ parent's own descriptors while the child is still running, cutting only at
\ newlines.  That is the whole point of the split: a parent heartbeat can never
\ land inside a child's line, and a final unterminated fragment is still emitted
\ before the parent prints PASS or FAIL.
\
\ Load after lib/test/runner.f.

require lib/test/runner.f

package ECHO

using GATE

10 constant LF

variable LINE-END
variable SPAN-U
variable TAIL-U

: WRITE-FD ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

public

\ How many bytes of the buffer end at a newline: the length of the longest
\ prefix that is whole lines.  Zero when no newline has arrived yet.
: LINE-SPAN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   0 LINE-END !
   0 begin dup u < while
      dup a + c@ LF = if dup 1+ LINE-END ! then
      1+
   repeat drop
   LINE-END @ ;

\ Write the whole lines waiting in the buffer to the descriptor and move the
\ partial tail back to the front, leaving its length in the count cell.
: LINES-FD ( n ptr u8 ptr n -- ) {: fd:n buf:ptr lenp:ptr :}
   lenp @ 0 < if E-STR-BOUNDS throw then
   buf lenp @ LINE-SPAN SPAN-U !
   SPAN-U @ 0 <= if exit then
   fd buf SPAN-U @ WRITE-FD
   lenp @ SPAN-U @ - TAIL-U !
   TAIL-U @ 0 > if
      buf SPAN-U @ + buf TAIL-U @ BYTE-COPY
   then
   TAIL-U @ lenp ! ;

\ Write whatever is left, newline-terminated or not, and empty the buffer.
: REMAINDER-FD ( n ptr u8 ptr n -- ) {: fd:n buf:ptr lenp:ptr :}
   lenp @ TAIL-U !
   TAIL-U @ 0 < if E-STR-BOUNDS throw then
   TAIL-U @ 0= if exit then
   fd buf TAIL-U @ WRITE-FD
   0 lenp ! ;

private

: CAPTURED-LINES ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN LINES-FD
   2 GT-ERR-BUF PROC-ERR-LEN LINES-FD ;

: CAPTURED-REST ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN REMAINDER-FD
   2 GT-ERR-BUF PROC-ERR-LEN REMAINDER-FD ;

: PUMP ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-CAPTURE-DRAIN
   CAPTURED-LINES
   label labelu GT-PROGRESS-WAIT ;

: STEP? ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
      drop
      label labelu GT-PROGRESS-CAPTURE-TIMEOUT? dup if
         CAPTURED-REST
      then
      exit
   then
   drop
   label labelu PUMP
   0 0= 0= ;

public

\ Run the pending capture to completion, echoing the child's output as it
\ arrives, and store the outcome in the runner's cells.
: CAPTURE ( ptr u8 n -- ) {: label:ptr labelu:n :}
   begin PROC-CAPTURE-DONE? 0= while
      label labelu STEP? if GT-CAPTURE-STORE exit then
   repeat
   PROC-REAP-CAPTURE
   CAPTURED-REST
   GT-CAPTURE-STORE ;

;package
