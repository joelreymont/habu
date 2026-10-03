\ capture-tree-test.f - a capture that ends its child ends what the child started.
\
\     bin/hb --load test/capture-tree-test.f
\
\ THE SUBJECT is test/check-signal-subject.f, run as the child of a lib/process.f
\ capture. It writes a line, starts a sleeper that leads a process group of its
\ own, as every spawn does, reports its pid and waits for the sleeper. Both
\ hold the write end of a pipe this test leaves open across the spawn, so the
\ read end reports end of file only when neither is left.
\
\ THE WAYS A CAPTURE ENDS ITS CHILD EARLY, each of which must end the sleeper
\ with it; a kill of the child's pid or group reaches neither the sleeper nor
\ anything else the child spawned:
\
\  1. ITS DEADLINE IN THE POLL, read as a status: RUN-ARGV-ENV-CAPTURE throws
\     E-PROC-TIMEOUT, and the line the subject wrote stays captured.
\  2. THE SAME DEADLINE READ AS DATA: RUN-ARGV-ENV-CAPTURE-OUTCOME answers the
\     timeout outcome with that line.
\  3. ITS DEADLINE IN THE REAP: the subject closed its output first, so the
\     capture read end of file on both pipes and waits for the subject's status
\     (PROC-REAP-CAPTURE-BOUNDED) until the deadline.
\  4. AN OVERFLOW: the line is longer than the capture holds, E-PROC-TRUNCATED.
\  5. A REFUSED REAPER ARM: PROC-REAP-ARM throws after the spawn, as
\     lib/process-fork.f SPAWN-REAPER does when its fork is refused.
\
\ Asserted in each: the capture's own answer, the subject reaped after a
\ SIGKILL, and end of file on the pipe within GONE-MS.
\
\ THE DEADLINE RUNS OUT AFTER THE SLEEPER STARTS. A capture runs PROC-REAP-ARM
\ between its spawn and its first poll. The vector each case installs waits
\ there for the subject's report, which comes after the sleeper started, so a
\ deadline of CAPTURE-MS has passed by the first poll however long the engine
\ took to start. That poll still finds the line the subject wrote before it
\ reported.
\
\ A FAILED CASE LEAVES NOTHING EITHER. CASE-END runs whatever the case asserted
\ or threw, and waits for the processes the capture left, which the sleeper's
\ 20 s bounds.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package CAPTURE-TREE-TEST

60000 constant READY-MS              \ an engine boot and the subject's load under a loaded pool
1 constant CAPTURE-MS                \ passed before the capture first polls
5000 constant GONE-MS                \ the killed take a moment to finish dying
30000 constant EXPIRE-MS             \ past the sleeper's 20 s, for a case that failed to end it
1 cells constant REPORT-BYTES        \ the subject's pid
64 constant CAP                      \ holds the subject's line
8 constant SHORT-CAP                 \ does not
64 constant SINK-CAP
-1 constant NO-FD

create REPORT REPORT-BYTES allot
create SINK SINK-CAP allot
create OUT-BUF CAP allot
create ERR-BUF CAP allot

variable REPORT-U                    \ bytes of REPORT read so far
variable PIPE-RD
variable PIPE-WR

TYPED-VARIABLE REPORTED bool         \ the subject's pid arrived before READY-MS ran out
TYPED-VARIABLE PIPE-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-CLOSED bool      \ the subject closes its output first
false REPORTED !
true PIPE-DONE !
false CASE-CLOSED !
NO-FD PIPE-RD !
NO-FD PIPE-WR !

\ What test/check-signal-subject.f writes before it starts the sleeper.
: LINE$ ( -- ptr u8 n )
   s\" check-signal-subject: up\n" ;

: CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;

\ ---- the report pipe --------------------------------------------------------

\ The write end stays inheritable: the subject and its sleeper hold it. The
\ read end is this process's alone.
: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   r FD>N PIPE-RD !
   w FD>N PIPE-WR !
   0 REPORT-U !
   false REPORTED !
   false PIPE-DONE ! ;

\ TRUE once the read end has bytes or has reached end of file, within ms.
: READY-WITHIN? ( n -- bool ) {: ms:n :}
   PIPE-RD @ >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

\ The subject's pid, before READY-MS runs out. End of file first means the
\ subject died before it reported.
: REPORT-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing the subject started holds the write end.
: GONE-WITHIN? ( n -- bool ) {: ms:n :}
   PIPE-DONE @ if true exit then
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ SINK SINK-CAP read {: got:n :}
      got 0= if true PIPE-DONE ! true exit then
      got 0 < if false exit then
   again ;

\ ---- the reaper arm ---------------------------------------------------------

\ The subject has its copy of the write end once the capture has spawned it,
\ so this process lets its own go and waits for the report.
: AWAIT-REPORT ( -- )
   PIPE-WR CLOSE-CELL
   REPORT-READ? REPORTED ! ;

\ Arms no reaper, as the default vector does, once the subject has reported.
: ARM-AFTER-REPORT ( pid -- pid )
   drop AWAIT-REPORT PROC-NO-PID >PID ;

\ Refuses the capture once the subject has reported.
: REFUSE-AFTER-REPORT ( pid -- pid )
   drop AWAIT-REPORT E-PROC-SPAWN throw ;

\ ---- the capture ------------------------------------------------------------

: FD-TEXT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

: SUBJECT-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/check-signal-subject.f" >LEN PROC-ARGV+
   s" CHECK_SIGNAL_FD" >LEN PIPE-WR @ FD-TEXT$ >LEN PROC-ENV+
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

\ ---- the cases ---------------------------------------------------------------

: CASE-BEGIN ( bool -- ) {: closed:bool :}
   closed CASE-CLOSED !
   OPEN-PIPE ;

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
   [: CAP CAPTURE-RC ;] E-PROC-TIMEOUT TTHROWSQ
   s" the line the subject wrote stays captured" T-LABEL
   OUT-BUF PROC-OUT-LEN @ LINE$ T$=
   ASSERT-ENDED ;

: POLL-DATA-BODY ( -- )
   false CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   CAPTURE-OUTCOME
   s" a deadline in the poll answers the timeout outcome" T-LABEL
   T-OUTCOME-TIMEOUT {: outu:len erru:len :}
   s" the timeout outcome carries the line the subject wrote" T-LABEL
   OUT-BUF outu LEN>N LINE$ T$=
   erru LEN>N 0 T=
   ASSERT-ENDED ;

: REAP-BODY ( -- )
   true CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a deadline in the reap throws E-PROC-TIMEOUT" T-LABEL
   [: CAP CAPTURE-RC ;] E-PROC-TIMEOUT TTHROWSQ
   ASSERT-ENDED ;

: OVERFLOW-BODY ( -- )
   false CASE-BEGIN
   [: ARM-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a line longer than the capture throws E-PROC-TRUNCATED" T-LABEL
   [: SHORT-CAP CAPTURE-RC ;] E-PROC-TRUNCATED TTHROWSQ
   ASSERT-ENDED ;

: REFUSED-BODY ( -- )
   false CASE-BEGIN
   [: REFUSE-AFTER-REPORT ;] is PROC-REAP-ARM
   s" a refused reaper arm throws E-PROC-SPAWN" T-LABEL
   [: CAP CAPTURE-RC ;] E-PROC-SPAWN TTHROWSQ
   ASSERT-ENDED ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed.
: CASE-END ( -- )
   PROC-REAP-ARM-DEFAULT
   PIPE-WR CLOSE-CELL
   PIPE-DONE @ 0= if
      s" the processes the capture left ended on their own" T-LABEL
      EXPIRE-MS GONE-WITHIN? TTRUE
   then
   PIPE-RD CLOSE-CELL ;

public

: MAIN ( -- )
   T-RESET
   s" capture-tree: a deadline in the poll, read as a status" type cr
   [: POLL-STATUS-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a deadline in the poll, read as data" type cr
   [: POLL-DATA-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a deadline in the reap" type cr
   [: REAP-BODY ;] [: CASE-END ;] finally
   s" capture-tree: an overflow" type cr
   [: OVERFLOW-BODY ;] [: CASE-END ;] finally
   s" capture-tree: a refused reaper arm" type cr
   [: REFUSED-BODY ;] [: CASE-END ;] finally ;

;package

CAPTURE-TREE-TEST:MAIN
T-REPORT
s" capture-tree-test: ok" type cr
