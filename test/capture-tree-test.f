\ capture-tree-test.f - a capture that ends its child early ends what the
\ child started. An overflow ends the sleeper the child started
\ (test/check-signal-subject.f), which leads its own process group as every
\ spawn does: a kill of the child's pid or group reaches neither.

require src/lib/error.f
require src/lib/fs.f
require src/lib/test.f
require src/lib/test/runner.f
require src/lib/test/fifo.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package CAPTURE-TREE-TEST

1 cells constant REPORT-BYTES        \ the subject's pid
64 constant CAP
8 constant SHORT-CAP                 \ shorter than the subject's line

create REPORT REPORT-BYTES allot
create OUT-BUF CAP allot
create ERR-BUF CAP allot
create FIFO PATH-CAP allot

variable REPORT-U                    \ bytes of REPORT read so far
variable FIFO-U
variable FIFO-RD

TYPED-VARIABLE REPORTED bool         \ the subject's pid arrived
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
false REPORTED !
true FIFO-DONE !
PROCESS:NO-FD FIFO-RD !

\ ---- the report FIFO --------------------------------------------------------
\ The subject first opens the write end, by the path CHECK_SIGNAL_FIFO names,
\ and maps it into the sleeper, so the read end reports end of file only when
\ neither is left. A FIFO no writer has opened reads end of file too, so end of
\ file counts only after the subject's report.

: FIFO$ ( -- ptr u8 n )
   FIFO FIFO-U @ ;

\ A new FIFO for the case, its read end open before the capture starts, so the
\ subject's open of the write end does not wait.
: OPEN-FIFO ( -- )
   GT-ROOT FIFO TEST-FIFO:MAKE {: u:n r:fd :}
   u FIFO-U !
   r FD>N FIFO-RD !
   0 REPORT-U !
   false REPORTED !
   false FIFO-DONE ! ;

\ The read end within ms: a positive count once it has bytes or has reached end
\ of file, 0 when ms ran out first, a negative errno when the poll failed.
: READY-WITHIN ( n -- n )
   {: ms:n :}
   FIFO-RD @ >FD FD:POLLIN PROCESS:PFD!
   1 ms ms >MS PROCESS:DEADLINE-AT PROCESS:POLL-RESTART ;

\ The subject's pid. The capture has reaped the subject, so the pid is in the
\ FIFO already or never comes: anything else read or polled is a failed report.
: REPORT-READ? ( -- bool )
   begin REPORT-U @ REPORT-BYTES < while
      0 READY-WITHIN {: ready:n :}
      ready 0 <= if false exit then
      FIFO-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file: nothing the subject started holds the write end. A failed read
\ or poll answers false.
: GONE? ( -- bool )
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD -1 TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0 <= if false exit then
   true FIFO-DONE !
   true ;

\ ---- the capture ------------------------------------------------------------

: SUBJECT-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/check-signal-subject.f" >LEN PROC-ARGV+
   s" CHECK_SIGNAL_FIFO" >LEN FIFO$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ No case expects the capture to return its result.
: DROP-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop ENDOF
     err OF PCAP-FAILED:UNMAKE drop 2drop ENDOF
   ;MATCH ;

\ The subject reports its sleeper before it writes the line SHORT-CAP overflows.
: OVERFLOW-CAPTURE ( -- )
   SUBJECT-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN OUT-BUF SHORT-CAP >LEN ERR-BUF CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-CAPTURE DROP-RESULT ;

\ ---- the cases ---------------------------------------------------------------

: ASSERT-KILLED ( -- )
   s" the capture reaped its child after a SIGKILL" T-LABEL
   PROCESS:PID @ PROCESS:NO-PID T=
   PROCESS:RC @ 128 PROCESS:SIGKILL + T= ;

: OVERFLOW-BODY ( -- )
   OPEN-FIFO
   s" a line longer than the capture throws E-PROC-TRUNCATED" T-LABEL
   [: OVERFLOW-CAPTURE ;] E-PROC-TRUNCATED TTHROWSQ
   REPORT-READ? REPORTED !
   s" the subject started its sleeper and reported" T-LABEL
   REPORTED @ TTRUE
   ASSERT-KILLED
   s" nothing the subject started is left" T-LABEL
   GONE? TTRUE ;

\ What a failed case left ends on its own.
: LEFT-ENDED ( -- )
   FIFO-DONE @ 0= if
      s" the processes the capture left ended on their own" T-LABEL
      GONE? TTRUE
   then ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed.
: CASE-END ( -- )
   [: LEFT-ENDED ;] [: FIFO-RD PROCESS:CLOSE-CELL ;] finally ;

public

: MAIN ( -- )
   T-RESET
   s" capture-tree" GT-START
   s" capture-tree: an overflow" type cr
   [: OVERFLOW-BODY ;] [: CASE-END ;] finally
   GT-CLEANUP ;

;package

CAPTURE-TREE-TEST:MAIN
T-REPORT
s" capture-tree-test: ok" type cr
