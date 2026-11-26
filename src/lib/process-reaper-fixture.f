\ process-reaper-fixture.f - the pool src/lib/process-reaper-test.f builds
\ stripped and kills. With no argument it starts a reaper, spawns two REAPED
\ children, writes each child's pid on stdout and waits. Its own program as
\ `child` spawns a sleeper that leads its own group and writes its pid; the sh
\ child writes its group, the reaper's pid. Every line is one write, so lines
\ from four processes never mix. With `stopped` the sh child stops itself
\ before it execs its sleeper. What nothing kills ends within 30 s.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/process-reaper.f

package PROCESS-REAPER-FIXTURE
private

: LINE ( n -- )
   SB-RESET FMT:SB-INT $0A SB-APPEND-C
   FD:STDOUT SB$ FD:WRITE-FULL ;

: SLEEPER ( -- PROCESS:request )
   s" /bin/sleep" PROCESS:NEW s" 30" PROCESS:ARG ;

\ This program again as `child`, in the reaper's group.
: SELF-CHILD ( PROCESS:reaper -- pid )
   {: rp :}
   PROCESS:OWN-PATH PROCESS:NEW
   s" child" PROCESS:ARG
   rp PROCESS:REAPED PROCESS:SPAWN ;

\ The sleeper's stdout is /dev/null, so only the report holds the pipe.
: SH-RUNS ( -- str )
   s" ps -o pgid= -p $$; exec /bin/sleep 30 >/dev/null" ;

\ A subshell reports once ps shows the sh stopped, so the pool dies with a
\ stopped member.
: SH-STOPS ( -- str )
   s" (until ps -o stat= -p $$ | grep -q T; do :; done; ps -o pgid= -p $$) & kill -s STOP $$; exec /bin/sleep 30 >/dev/null" ;

: SH-CHILD ( PROCESS:reaper str -- pid )
   {: rp script:str :}
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   script PROCESS:ARG
   rp PROCESS:REAPED PROCESS:SPAWN ;

: PARENT ( str -- )
   {: script:str :}
   PROCESS:REAPER-START {: rp :}
   rp SELF-CHILD {: a:pid :}
   rp script SH-CHILD PID>N LINE
   a PID>N LINE
   a PROCESS:WAIT drop ;

\ A spawned child leads a group of its own, which no member of the reaper's
\ group leads: only the walk's parent link reaches it.
: CHILD ( -- )
   SLEEPER PROCESS:SPAWN {: g:pid :}
   g PID>N LINE
   g PROCESS:WAIT drop ;

public

: RUN ( -- )
   SCRIPT-ARGC 0= if SH-RUNS PARENT exit then
   0 SCRIPT-ARGV$ s" stopped" STR:= if SH-STOPS PARENT exit then
   CHILD ;

;package

: MAIN ( -- ) PROCESS-REAPER-FIXTURE:RUN ;
