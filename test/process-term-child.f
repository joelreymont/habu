\ process-term-child.f - a capture child for the end-of-capture cases.
\
\ Run: bin/hb --load test/process-term-child.f -- <marker> <mode>
\ Writes `ready` to stdout once its signal disposition is set, then waits for
\ ever. Modes: `exit` catches SIGTERM and answers it by removing <marker> and
\ dying of it; `stay` catches SIGTERM and never ends; `plain` catches nothing.

require src/lib/string.f
require src/lib/process.f
require src/lib/signal.f
require src/lib/fs-mutate.f
require src/lib/task.f

package PROCESS-TERM-CHILD

100 constant WAIT-MS

: READY ( -- )
   1 s" ready" write drop ;

: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   1 SCRIPT-ARGV$ a u STR= ;

: ANSWER ( n -- ) {: sig:n :}
   s" exit" MODE? 0= if exit then
   0 SCRIPT-ARGV$ FS:REMOVE-FILE
   s" process-term-child: signal" sig SIGNAL:DIE-OF ;

: WAIT-SIGNAL ( -- )
   begin
      SIGNAL:FD WAIT-MS >MS PROCESS:POLL-IN drop
      SIGNAL:TAKE MATCH SIGNAL:signal-result
         signal OF ANSWER ENDOF
         timeout OF ENDOF
      ;MATCH
   again ;

: WAIT-PLAIN ( -- )
   begin WAIT-MS >MS TASK:SLEEP again ;

public

: MAIN ( -- )
   s" plain" MODE? if READY WAIT-PLAIN then
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   READY
   WAIT-SIGNAL ;

;package

PROCESS-TERM-CHILD:MAIN
