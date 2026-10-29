\ capture-tree-test.f - a capture that ends its child ends what the child started.
\ Each way a capture ends its child early must end the sleeper the child
\ started (test/check-signal-subject.f), which leads its own process group as
\ every spawn does: a kill of the child's pid or group reaches neither. A wait
\ this test runs around the capture is named and thrown as E-PROC-TIMEOUT
\ (src/lib/test/wait.f), so the row ends as a timeout, not a FAIL.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/wait.f
require src/lib/test/runner.f
require src/lib/test/fifo.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package CAPTURE-TREE-TEST

60000 constant READY-MS              \ an engine boot and the subject's load under a loaded pool
1 constant CAPTURE-MS                \ passed before the capture first polls
5000 constant GONE-MS                \ the killed take a moment to finish dying
30000 constant EXPIRE-MS             \ past the sleeper's 20 s, for a case that failed to end it
1 cells constant REPORT-BYTES        \ the subject's pid
64 constant CAP                      \ holds the subject's line
8 constant SHORT-CAP                 \ does not

create REPORT REPORT-BYTES allot
create OUT-BUF CAP allot
create ERR-BUF CAP allot
create FIFO PATH-CAP allot

variable REPORT-U                    \ bytes of REPORT read so far
variable FIFO-U
variable FIFO-RD

TYPED-VARIABLE REPORTED bool         \ the subject's pid arrived before READY-MS ran out
TYPED-VARIABLE REPORT-EXPIRED bool   \ READY-MS ran out before the subject's pid arrived
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-CLOSED bool      \ the subject closes its output first
false REPORTED !
false REPORT-EXPIRED !
true FIFO-DONE !
false CASE-CLOSED !
PROC-NO-FD FIFO-RD !

\ What test/check-signal-subject.f writes before it starts the sleeper.
: LINE$ ( -- ptr u8 n )
   s\" check-signal-subject: up\n" ;

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
   false REPORT-EXPIRED !
   false FIFO-DONE ! ;

\ The read end within ms: a positive count once it has bytes or has reached end
\ of file, 0 when ms ran out first, a negative errno when the poll failed.
: READY-WITHIN ( n -- n )
   {: ms:n :}
   FIFO-RD @ >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART ;

\ The subject's pid, before READY-MS runs out. End of file first means the
\ subject died before it reported: that, a short report or a failed read or
\ poll is a failed report. READY-MS running out first is this wait's own
\ deadline, a timeout: it answers false with REPORT-EXPIRED set. On macOS, whose
\ poll never reports a FIFO's end of file, a subject dying before its report is
\ that timeout.
: REPORT-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      deadline PROC-LEFT-MS MS>N READY-WITHIN {: ready:n :}
      ready 0= if true REPORT-EXPIRED ! false exit then
      ready 0 < if false exit then
      FIFO-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing the subject started holds the write end. A
\ failed read or poll answers false. ms running out first is this wait's own
\ deadline, named and thrown as a timeout.
: GONE-WITHIN? ( n -- bool )
   {: ms:n :}
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD ms TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0= if s" capture-tree-test GONE-WITHIN" TEST-WAIT:EXPIRED then
   eof 0 < if false exit then
   true FIFO-DONE !
   true ;

\ ---- the reaper arm ---------------------------------------------------------

\ A capture runs PROC-REAP-ARM between its spawn and its first poll, after the
\ subject has opened the write end, so this vector waits there for the report,
\ which follows the sleeper's start: CAPTURE-MS has passed by the first poll,
\ which still finds the line the subject wrote. The wait records its deadline in
\ REPORT-EXPIRED rather than throwing it: whatever this vector throws comes out
\ of the capture as its own code, and two cases expect E-PROC-TIMEOUT from it.
: AWAIT-REPORT ( -- )
   REPORT-READ? REPORTED ! ;

\ Names and throws the report wait's deadline if it ran out, once the capture
\ it ran in has returned or thrown and before the case asserts anything.
: REPORT-DEADLINE ( -- )
   REPORT-EXPIRED @ if s" capture-tree-test REPORT-READ" TEST-WAIT:EXPIRED then ;

\ Arms no reaper, as the default vector does, once the subject has reported.
: ARM-AFTER-REPORT ( pid -- pid )
   drop AWAIT-REPORT PROC-NO-PID >PID ;

\ Refuses the capture once the subject has reported.
: REFUSE-AFTER-REPORT ( pid -- pid )
   drop AWAIT-REPORT E-PROC-SPAWN throw ;

\ ---- the capture ------------------------------------------------------------

: SUBJECT-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/check-signal-subject.f" >LEN PROC-ARGV+
   s" CHECK_SIGNAL_FIFO" >LEN FIFO$ >LEN PROC-ENV+
   CASE-CLOSED @ if s" CHECK_SIGNAL_CLOSED" >LEN s" 1" >LEN PROC-ENV+ then
   PROC-ENV-INHERIT-MISSING ;

\ No case expects the capture to return its result.
: DROP-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop ENDOF
     err OF PCAP-FAILED:UNMAKE drop 2drop ENDOF
   ;MATCH ;

\ A capture whose stdout holds n bytes.
: CAPTURE-RC ( n -- ) {: cap:n :}
   SUBJECT-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN OUT-BUF cap >LEN ERR-BUF CAP >LEN CAPTURE-MS >MS
   RUN-ARGV-ENV-CAPTURE DROP-RESULT ;

: CAPTURE-OUTCOME ( -- len len outcome )
   SUBJECT-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN OUT-BUF CAP >LEN ERR-BUF CAP >LEN CAPTURE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

\ The code the capture the xt runs threw, 0 if it returned. The report wait ran
\ inside that capture: its deadline is named and thrown here, outside any
\ assertion, so it never passes for the capture's own E-PROC-TIMEOUT.
: CAPTURE-CODE ( [ -- ] -- n )
   catch REPORT-DEADLINE ;

\ ---- the cases ---------------------------------------------------------------

: CASE-BEGIN ( bool -- ) {: closed:bool :}
   closed CASE-CLOSED !
   OPEN-FIFO ;

: ASSERT-ENDED ( -- )
   s" the subject started its sleeper and reported" T-LABEL
   REPORTED @ TTRUE
   s" the capture reaped the subject after a SIGKILL" T-LABEL
   PROC-PID @ PROC-NO-PID T=
   PROC-STATUS @ PROC-STATUS>RC RC>N 128 SIGKILL + T=
   s" nothing the subject started is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

: POLL-STATUS-BODY ( -- )
   false CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a deadline in the poll throws E-PROC-TIMEOUT" T-LABEL
   [: CAP CAPTURE-RC ;] CAPTURE-CODE E-PROC-TIMEOUT T=
   s" the line the subject wrote stays captured" T-LABEL
   OUT-BUF PROC-OUT-LEN @ LINE$ T$=
   ASSERT-ENDED ;

: POLL-DATA-BODY ( -- )
   false CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   [: CAPTURE-OUTCOME ;] [: REPORT-DEADLINE ;] finally
   s" a deadline in the poll answers the timeout outcome" T-LABEL
   T-OUTCOME-TIMEOUT {: outu:len erru:len :}
   s" the timeout outcome carries the line the subject wrote" T-LABEL
   OUT-BUF outu LEN>N LINE$ T$=
   erru LEN>N 0 T=
   ASSERT-ENDED ;

\ The subject closed its output first, so the capture reads end of file on both
\ pipes and waits for the status (PROC-REAP-CAPTURE-BOUNDED) until the deadline.
: REAP-BODY ( -- )
   true CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a deadline in the reap throws E-PROC-TIMEOUT" T-LABEL
   [: CAP CAPTURE-RC ;] CAPTURE-CODE E-PROC-TIMEOUT T=
   ASSERT-ENDED ;

: OVERFLOW-BODY ( -- )
   false CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a line longer than the capture throws E-PROC-TRUNCATED" T-LABEL
   [: [: SHORT-CAP CAPTURE-RC ;] CAPTURE-CODE throw ;] E-PROC-TRUNCATED TTHROWSQ
   ASSERT-ENDED ;

\ PROC-REAP-ARM throws after the spawn, as src/lib/process-fork.f SPAWN-REAPER
\ does when its fork is refused.
: REFUSED-BODY ( -- )
   false CASE-BEGIN
   [: REFUSE-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a refused reaper arm throws E-PROC-SPAWN" T-LABEL
   [: [: CAP CAPTURE-RC ;] CAPTURE-CODE throw ;] E-PROC-SPAWN TTHROWSQ
   ASSERT-ENDED ;

\ What a failed case left ends on its own, before EXPIRE-MS runs out.
: LEFT-ENDED ( -- )
   FIFO-DONE @ 0= if
      s" the processes the capture left ended on their own" T-LABEL
      EXPIRE-MS GONE-WITHIN? TTRUE
   then ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed. The read end closes even when the wait for what the case left runs
\ out.
: CASE-END ( -- )
   PROC-REAP-ARM-DEFAULT
   [: LEFT-ENDED ;] [: FIFO-RD PROC-CLOSE-CELL ;] finally ;

public

: MAIN ( -- )
   T-RESET
   s" capture-tree" GT-START
   s" capture-tree: a deadline in the poll, read as a status" type cr
   [: POLL-STATUS-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a deadline in the poll, read as data" type cr
   [: POLL-DATA-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a deadline in the reap" type cr
   [: REAP-BODY ;] [: CASE-END ;] finally
   s" capture-tree: an overflow" type cr
   [: OVERFLOW-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a refused reaper arm" type cr
   [: REFUSED-BODY ;] [: CASE-END ;] finally
   GT-CLEANUP ;

;package

CAPTURE-TREE-TEST:MAIN
T-REPORT
s" capture-tree-test: ok" type cr
