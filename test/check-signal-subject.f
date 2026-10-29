\ check-signal-subject.f - the program test/check-signal-test.f has check.f
\ check (in its run stage's child) and test/capture-tree-test.f captures. It
\ writes LINE$, starts a SLEEPER (/bin/sh becoming `sleep 20`, in its own
\ process group as every spawn is), reports its pid on the FIFO
\ CHECK_SIGNAL_FIFO names and waits for the sleeper.

\ With CHECK_SIGNAL_CLOSED set, the subject closes stdout and stderr first and
\ the sleeper starts without them, so the capture reads end of file on both
\ while the subject runs: it then waits in the reap (src/lib/process.f
\ PROC-REAP-CAPTURE-BOUNDED), not in the capture's poll.

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

\ It opens that FIFO's write end first (src/lib/test/fifo.f) and maps it into
\ the sleeper, so end of file means nothing the subject ran is left.
: SLEEPER ( fd -- pid ) {: w:fd :}
   PROC-ARGV-ENV-RESET
   s" -c" >LEN PROC-ARGV+
   s" exec sleep 20" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD w TEST-FIFO:SPAWN ;

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
