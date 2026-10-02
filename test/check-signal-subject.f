\ check-signal-subject.f - the program test/check-signal-test.f has check.f check,
\ and test/capture-tree-test.f captures.
\
\ tools/check.f runs this in its run stage's child; test/capture-tree-test.f
\ runs it as the child of a lib/process.f capture. It writes LINE$ to stdout,
\ starts a SLEEPER, a /bin/sh that becomes `sleep 20` in a process group of its
\ own, as every spawn is, then reports its own pid and waits for the sleeper.
\ Both hold the write end of the pipe CHECK_SIGNAL_FD names, which the test
\ left open across every spawn, so the test reads its end of file as "nothing
\ the subject ran is left". Neither outlives the sleeper's 20 s, so a parent
\ that leaves them behind leaves nothing for long.
\
\ With CHECK_SIGNAL_CLOSED set, the subject first closes its stdout and stderr,
\ writes nothing, and the sleeper starts without them, so the capture reads
\ end of file on both while the subject still runs: it then waits in the reap
\ after the pipes close (lib/process.f PROC-REAP-CAPTURE-BOUNDED), not in the
\ capture's poll.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package CHECK-SIGNAL-SUBJECT

64 constant USAGE-RC

create REPORT 1 cells allot

: REPORT-FD ( -- n )
   s" CHECK_SIGNAL_FD" GETENV STR>NUMBER? MATCH option
      none OF s" check-signal-subject: CHECK_SIGNAL_FD names no descriptor" USAGE-RC die ENDOF
      some OF ENDOF
   ;MATCH ;

: REPORT-PID ( n -- ) {: fd:n :}
   getpid REPORT !
   fd REPORT 1 cells write 1 cells <> if
      s" check-signal-subject: report refused" USAGE-RC die
   then ;

: CLOSED? ( -- bool )
   s" CHECK_SIGNAL_CLOSED" GETENV nip 0<> ;

: SLEEPER ( -- pid )
   PROC-ARGV-ENV-RESET
   s" -c" >LEN PROC-ARGV+
   s" exec sleep 20" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-IO ;

\ What the subject writes to stdout before it starts the sleeper.
: LINE$ ( -- ptr u8 n )
   s\" check-signal-subject: up\n" ;

public

: MAIN ( -- )
   REPORT-FD {: fd:n :}
   CLOSED? if 1 close 2 close else LINE$ type then
   SLEEPER {: sleeper:pid :}
   fd REPORT-PID
   sleeper PROC-WAIT-STATUS drop ;

;package

CHECK-SIGNAL-SUBJECT:MAIN
