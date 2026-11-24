split-str
\ process-reaper-fixture.f - the pool src/lib/process-reaper-test.f kills.
\ With no argument it starts a reaper, spawns two REAPED children, writes each
\ child's pid on stdout and waits. The hb child (`child`) spawns a sleeper that
\ leads its own group and writes its pid; the sh child writes its group, the
\ reaper's pid. Every line is one write, so lines from four processes never mix.
\ With `stopped` the sh child stops itself before it execs its sleeper. What
\ nothing kills ends within 30 s.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/process.f

package PROCESS-REAPER-FIXTURE
private

: LINE ( n -- )
   SB-RESET FMT:SB-INT $0A SB-APPEND-C
   FD:STDOUT SB$ FD:WRITE-FULL ;

: SLEEPER ( -- PROCESS:request )
   s" /bin/sleep" PROCESS:NEW s" 30" PROCESS:ARG ;

\ This file again as `child`, in the reaper's group.
: HB-CHILD ( PROCESS:reaper -- pid )
   {: rp :}
   PROCESS:OWN-PATH PROCESS:NEW
   s" --load" PROCESS:ARG
   s" src/lib/process-reaper-fixture.f" PROCESS:ARG
   s" --" PROCESS:ARG
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
   {: rp script:^ scriptu :}
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   script scriptu PROCESS:ARG
   rp PROCESS:REAPED PROCESS:SPAWN ;

: PARENT ( str -- )
   {: script:^ scriptu :}
   PROCESS:REAPER-START {: rp :}
   rp HB-CHILD {: a:pid :}
   rp script scriptu SH-CHILD PID>N LINE
   a PID>N LINE
   a PROCESS:WAIT drop ;

\ A spawned child leads a group of its own, which no member of the reaper's
\ group leads: only the walk's parent link reaches it.
: CHILD ( -- )
   SLEEPER PROCESS:SPAWN {: g:pid :}
   g PID>N LINE
   g PROCESS:WAIT drop ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0= if SH-RUNS PARENT exit then
   0 SCRIPT-ARGV$ s" stopped" STR:= if SH-STOPS PARENT exit then
   CHILD ;

;package

PROCESS-REAPER-FIXTURE:MAIN
