\ launch-context.f - print a process's launch context, this one's or a child's.
\ `bin/hb --load tools/launch-context.f` reports this process: diff its `ctx `
\ lines under each launcher. `... -- child` reports a capture-spawned child, whose
\ fd 0 is the launcher's: under a terminal it says `tty yes`, and a bare engine
\ then enters its REPL and stops on SIGTTOU unless given an explicit stdin.
\ See docs/debugging.md "A child-process fixture disagrees with itself".

require src/lib/errors.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/spawn-report.f

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
