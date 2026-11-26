\ process-reaper-test.f - a reaper kills what its pool spawned REAPED, a
\ grandchild leading its own group included, when the pool dies by SIGKILL or
\ stops it; REAPER-STOP names a reaper that did not exit 0; a program started
\ with the reaper's mark in a group it does not lead kills nothing. A test reads
\ the reaper's pid as the group a REAPED child reports. The pool that dies is
\ the fixture built stripped outside any tree, so its reaper is that program's.

require src/lib/error.f
require src/lib/string.f
require src/lib/fd.f
require src/lib/task.f
require src/lib/engine-candidate.f
require src/lib/test/assert.f
require src/lib/test/runner.f
require src/lib/process.f
require src/lib/process-reaper.f
require src/lib/spawn.f

package PROCESS-REAPER-TEST
private

4 constant REPORTS                   \ the fixture's: reaper, two children, a grandchild
5000 constant GONE-MS
$1000 constant CAP
$0A constant LF-BYTE

CAP BUFFER: OUT
PATH-CAP BUFFER: IMAGE-BUF
variable IMAGE-U
variable OUT-LEN
REPORTS TYPED-BUFFER PIDS n
variable PID-N
variable ACC

: LINES ( -- n )
   0 OUT-LEN @ 0 ?do OUT i + c@ LF-BYTE = if 1+ then loop ;

\ Reads until n lines have come or the writers are gone, then closes r.
: READ-LINES ( fd n -- )
   {: r:fd want:n :}
   0 OUT-LEN !
   begin LINES want < OUT-LEN @ CAP < and while
      r FD>N OUT OUT-LEN @ + CAP OUT-LEN @ - SPAN:MAKE read
      dup 0 <= if drop r FD>N close exit then
      OUT-LEN +!
   repeat
   r FD>N close ;

: DIGIT ( n -- )
   {: c:n :}
   c [char] 0 >= c [char] 9 <= and if
      ACC @ 10 * c [char] 0 - + ACC !
   then ;

: PID+ ( -- )
   PID-N @ REPORTS < if ACC @ PID-N @ PIDS ! 1 PID-N +! then
   0 ACC ! ;

\ The numbers OUT holds, one a line, into PIDS.
: PARSE ( -- )
   0 PID-N !
   0 ACC !
   OUT-LEN @ 0 ?do
      OUT i + c@ dup LF-BYTE = if drop PID+ else DIGIT then
   loop ;

: ALIVE? ( n -- bool ) 0 kill-errno 0= ;

: ALIVE ( -- n )
   0 PID-N @ 0 ?do i PIDS @ ALIVE? if 1+ then loop ;

\ The reported processes still alive after up to GONE-MS.
: ALIVE-AFTER ( -- n )
   mono-ns GONE-MS 1000000 * + {: deadline:n :}
   begin ALIVE dup 0<> mono-ns deadline < and while
      drop 10 >MS TASK:SLEEP
   repeat ;

\ A failed case leaves no reported process running.
: KILL-LEFT ( -- )
   PID-N @ 0 ?do
      i PIDS @ dup ALIVE? if PROCESS:SIGKILL kill drop else drop then
   loop ;

: RC ( pid -- n ) PROCESS:WAIT PROCESS:STATUS>RC RC>N ;

\ The child's ending within GONE-MS as an rc; one still running then is
\ killed and reaped.
: ENDED-RC ( pid -- n )
   {: pid:pid :}
   pid GONE-MS >MS PROCESS:WAIT-BOUNDED PROCESS:ENDING>DEADLINE-RC RC>N {: rc :}
   rc 124 = if
      pid PROCESS:SIGKILL PROCESS:KILL-RAW drop
      pid RC drop
   then
   rc ;

: IMAGE$ ( -- str ) IMAGE-BUF IMAGE-U @ >str ;

\ A stripped program starts at its own entry, with no engine or tree to run
\ its reaper from, so the fixture is built as one, once for every pool.
: BUILD ( -- )
   s" process-reaper" GT-START
   s" pool" IMAGE-BUF GT-PATH IMAGE-U !
   ENGINE-CANDIDATE:PATH$ PROCESS:NEW
   s" --load" PROCESS:ARG
   s" tools/hb-build.f" PROCESS:ARG
   s" --" PROCESS:ARG
   s" src/lib/process-reaper-fixture.f" PROCESS:ARG
   s" -o" PROCESS:ARG
   IMAGE$ PROCESS:ARG
   PROCESS:SPAWN RC
   s" hb-build links the fixture stripped" T-LABEL
   0 T= ;

\ The stripped pool, run in the temporary directory.
: FIXTURE ( fd -- PROCESS:request )
   {: w:fd :}
   IMAGE$ PROCESS:NEW
   GT-ROOT PROCESS:CWD
   1 >FD w PROCESS:FD ;

\ A pool's SIGKILL closes the write end only it held.
: POOL-DEATH ( fd fd pid str -- )
   {: r:fd w:fd p:pid label:str :}
   w FD>N close
   r REPORTS READ-LINES
   PARSE
   s" the pool reported its reaper, two children and a grandchild" T-LABEL
   PID-N @ REPORTS T=
   p PROCESS:SIGKILL PROCESS:KILL-RAW drop
   s" the pool died of SIGKILL" T-LABEL
   p RC 137 T=
   label T-LABEL
   ALIVE-AFTER 0 T=
   KILL-LEFT ;

\ 1
: TEST-POOL-DEATH ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r w w FIXTURE PROCESS:SPAWN
   s" every process the pool reported is gone within 5 s of its death"
   POOL-DEATH ;

\ 2
: TEST-STOP ( -- )
   s" REAPER-STOP kills a REAPED child still running" T-LABEL
   PROCESS:REAPER-START {: rp :}
   s" /bin/sleep" PROCESS:NEW s" 30" PROCESS:ARG
   rp PROCESS:REAPED PROCESS:SPAWN {: c:pid :}
   rp PROCESS:REAPER-STOP
   c ENDED-RC 137 T= ;

\ 3: the child reports the reaper's pid and exits; the reaper's SIGKILL leaves
\ the group with nobody to kill it.
: TEST-STOP-KILLED ( -- )
   PROCESS:REAPER-START {: rp :}
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   s" ps -o pgid= -p $$" PROCESS:ARG
   1 >FD w PROCESS:FD
   rp PROCESS:REAPED PROCESS:SPAWN {: c:pid :}
   w FD>N close
   r 1 READ-LINES
   PARSE
   s" a REAPED child reports one group" T-LABEL
   PID-N @ 1 T=
   0 PIDS @ {: g:n :}
   g 1 > if g PROCESS:SIGKILL kill drop then
   c RC 0 T=
   s" REAPER-STOP after the reaper's SIGKILL throws E-PROC-WAIT" T-LABEL
   [: rp PROCESS:REAPER-STOP ;] E-PROC-WAIT TTHROWSQ ;

\ 4: the pool's death orphans the reaper's group with a member stopped, so the
\ kernel sends the group SIGHUP, which every member starts with blocked.
: TEST-POOL-DEATH-STOPPED ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r w
   w FIXTURE s" stopped" PROCESS:ARG PROCESS:SPAWN
   s" with a member stopped, every process the pool reported is gone within 5 s"
   POOL-DEATH ;

\ bash, which leads its own group, runs the fixture as a reaper in a subshell:
\ a member of bash's group, not its leader, with fd 3 closed.
: FAKE-REAPER ( -- str )
   s\" (exec -a habu-reaper \"$0\" 3<&- 2>/dev/null); printf %s $?" ;

\ 5: a reaper that leads no group kills nothing and exits with its error.
: TEST-NO-LEADER ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   s" /bin/bash" PROCESS:NEW
   s" -c" PROCESS:ARG
   FAKE-REAPER PROCESS:ARG
   IMAGE$ PROCESS:ARG
   1 >FD w PROCESS:FD
   PROCESS:SPAWN {: c:pid :}
   w FD>N close
   r 1 READ-LINES
   s" a reaper leading no group exits 85 and leaves bash alive" T-LABEL
   OUT OUT-LEN @ >str s" 85" T$=
   c RC 0 T= ;

: CASES ( -- )
   BUILD
   TEST-POOL-DEATH
   TEST-STOP
   TEST-STOP-KILLED
   TEST-POOL-DEATH-STOPPED
   TEST-NO-LEADER ;

public

: PROCESS-REAPER-TEST-MAIN ( -- )
   [: CASES ;] [: GT-CLEANUP ;] finally
   T-REPORT
   s" process-reaper-test: ok" type cr ;

;package

PROCESS-REAPER-TEST:PROCESS-REAPER-TEST-MAIN
