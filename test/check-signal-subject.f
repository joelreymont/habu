\ check-signal-subject.f - the program test/check-signal-test.f has check.f check,
\ and test/capture-tree-test.f captures.
\
\ tools/check.f runs this in its run stage's child; test/capture-tree-test.f
\ runs it as the child of a src/lib/process.f capture. It writes LINE$ to stdout,
\ starts a SLEEPER, a /bin/sh that becomes `sleep 20` in a process group of its
\ own, as every spawn is, then reports its own pid and waits for the sleeper.
\ Its first act opens the write end of the FIFO CHECK_SIGNAL_FIFO names
\ (src/lib/test/fifo.f), which it maps into the sleeper, so both hold it and
\ the test reads its end of file as "nothing the subject ran is left". The pid
\ report goes to the same FIFO. Neither outlives the sleeper's 20 s, so a parent
\ that leaves them behind leaves nothing for long.
\
\ With CHECK_SIGNAL_CLOSED set, the subject first closes its stdout and stderr,
\ writes nothing, and the sleeper starts without them, so the capture reads
\ end of file on both while the subject still runs: it then waits in the reap
\ after the pipes close (src/lib/process.f PROC-REAP-CAPTURE-BOUNDED), not in the
\ capture's poll.

require src/lib/errors.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/fifo.f

package CHECK-SIGNAL-SUBJECT

64 constant USAGE-RC

create REPORT 1 cells allot

: REPORT-PID ( fd -- ) {: w:fd :}
   getpid REPORT !
   w FD>N REPORT 1 cells write 1 cells <> if
      s" check-signal-subject: report refused" USAGE-RC die
   then ;

: CLOSED? ( -- bool )
   s" CHECK_SIGNAL_CLOSED" GETENV nip 0<> ;

: SLEEPER ( fd -- pid ) {: w:fd :}
   PROC-ARGV-ENV-RESET
   s" -c" >LEN PROC-ARGV+
   s" exec sleep 20" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD w TEST-FIFO:SPAWN ;

\ What the subject writes to stdout before it starts the sleeper.
: LINE$ ( -- ptr u8 n )
   s\" check-signal-subject: up\n" ;

public

: MAIN ( -- )
   s" CHECK_SIGNAL_FIFO" TEST-FIFO:WRITER {: w:fd :}
   CLOSED? if 1 close 2 close else LINE$ type then
   w SLEEPER {: sleeper:pid :}
   w REPORT-PID
   sleeper PROC-WAIT-STATUS drop ;

;package

CHECK-SIGNAL-SUBJECT:MAIN
