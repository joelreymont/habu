\ launch-context.f - print a process's launch context, this one's or a child's.
\
\   bin/hb --load tools/launch-context.f
\       report THIS process. Run it under each launcher whose behaviour differs
\       and diff the reports:
\          bin/hb --load tools/launch-context.f | grep '^ctx ' | sort > /tmp/a
\          <other launcher> ...                                        > /tmp/b
\          diff /tmp/a /tmp/b
\
\   bin/hb --load tools/launch-context.f -- child
\       report what a CAPTURE-SPAWNED CHILD sees: it spawns this same file
\       through the capture path fixtures use (stdout and stderr on fresh pipes,
\       stdin left alone) and prints the child's report. That spawn shape hands
\       the child the launcher's own fd 0, so under a terminal the child's report
\       says `tty yes` - the difference that decides whether a spawned bare
\       engine enters its REPL and stops on SIGTTOU as a background process
\       group. A fixture that needs a fixed answer must pass an explicit stdin.
\
\ See docs/debugging.md § A child-process fixture disagrees with itself.

require lib/errors.f
require lib/string.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/spawn-report.f

package LAUNCH-CONTEXT
private

$8000 constant CAP
20000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot

: SELF$ ( -- ptr u8 n )
   s" tools/launch-context.f" ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then ;

: CHILD-ENV ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SELF$ >LEN PROC-ARGV+ ;

: REPORT-CAPTURE ( len len -- ) {: o:len e:len :}
   s" --- child report ---" type cr
   OUT o LEN>N type
   ERR e LEN>N type ;

: SPAWN-CHILD ( -- )
   CHILD-ENV
   CHILD-ARGV
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE REPORT-CAPTURE ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o e REPORT-CAPTURE
            s" child rc: " type c RC>N . ENDOF
   ;MATCH ;

: CHILD-MODE? ( -- bool )
   SCRIPT-ARGC 0= if 0 0= 0= exit then
   0 SCRIPT-ARGV$ s" child" STR= ;

public

: MAIN ( -- )
   CHILD-MODE? if SPAWN-CHILD exit then
   SPAWN-REPORT:CONTEXT ;

;package

LAUNCH-CONTEXT:MAIN
