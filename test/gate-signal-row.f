\ gate-signal-row.f - the row test/gate-signal-test.f puts under a gate.
\
\ With no script argument this is the ROW: it reports its pid and runs the LEAF,
\ this file again with `leaf` as its one argument. The leaf starts the HELPER,
\ reports its pid and starts BURST-N writers one after another. Each writer is a
\ /bin/sh that makes a file under HB_TMP and becomes `sleep`, so it holds the
\ row's scratch and outlives everything above it unless something ends it.
\ Every spawn here makes a process group of its own, which is the shape a row
\ has while it builds an engine: two spawns deep and, for the length of the
\ burst, between two spawns.
\
\ The helper starts one more writer in the background and exits at once, and
\ the leaf does not reap it. It stays a zombie under the leaf, leading the
\ group its writer keeps while init takes the writer as a child, so the only
\ link from the tree to that writer is its group id: the zombie's pid. That is
\ a row whose child forked and exited while what it forked holds the row's
\ capture pipe.
\
\ Both engines report on the descriptor GATE_SIGNAL_FD names: a pipe write end
\ the test left open across every spawn. Nothing here closes it, and the test
\ reads its end of file as "no process under the row is left".

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package GATE-SIGNAL-ROW

24 constant BURST-N
15 constant BURST-GAP-MS             \ the burst outlasts the test's reaction to the second report
64 constant USAGE-RC

create WRITERS BURST-N cells allot
create REPORT 1 cells allot
create PAUSE-PFD 8 allot
create SINK 64 allot
variable HELPER

: REPORT-FD ( -- n )
   s" GATE_SIGNAL_FD" GETENV STR>NUMBER? MATCH option
      none OF s" gate-signal-row: GATE_SIGNAL_FD names no descriptor" USAGE-RC die ENDOF
      some OF ENDOF
   ;MATCH ;

: REPORT-PID ( -- )
   getpid REPORT !
   REPORT-FD REPORT 1 cells write 1 cells <> if
      s" gate-signal-row: report refused" USAGE-RC die
   then ;

: PAUSE ( n -- ) {: ms:n :}
   PAUSE-PFD 0 ms poll drop ;

\ The writer lives 20 s: longer than the test waits for it to be ended, and
\ short enough that a run which fails to end it still leaves nothing behind.
: WRITER$ ( -- ptr u8 n )
   s\" mkdir -p \"$HB_TMP/use\" && : > \"$HB_TMP/use/$$\" && exec sleep 20" ;

: START-WRITER ( n -- ) {: i:n :}
   PROC-ARGV-ENV-RESET
   s" -c" >LEN PROC-ARGV+
   WRITER$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-IO PID>N
   i cells WRITERS + ! ;

\ The helper's writer sends its output to /dev/null, so the pipe the helper
\ writes to reaches its end when the helper has exited: a zombie, since nothing
\ reaps it.
: HELPER$ ( -- ptr u8 n )
   s\" { mkdir -p \"$HB_TMP/use\" && : > \"$HB_TMP/use/$$\" && exec sleep 20; } >/dev/null & exit" ;

: START-HELPER ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
   PROC-ARGV-ENV-RESET
   s" -c" >LEN PROC-ARGV+
   HELPER$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" /bin/sh" >LEN -1 >FD w -1 >FD PROC-SPAWN-ARGV-ENV-IO PID>N HELPER !
   w FD>N close
   begin r FD>N SINK 64 read dup 0 > while drop repeat
   r FD>N close
   0< if
      s" gate-signal-row: the helper's pipe refused a read" USAGE-RC die
   then ;

: LEAF ( -- )
   START-HELPER
   REPORT-PID
   BURST-N 0 ?do
      i START-WRITER
      BURST-GAP-MS PAUSE
   loop
   BURST-N 0 ?do
      i cells WRITERS + @ >PID PROC-WAIT-STATUS drop
   loop
   HELPER @ >PID PROC-WAIT-STATUS drop ;

: ROW ( -- )
   REPORT-PID
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-signal-row.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" leaf" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-ENV-IO-RC
   MATCH result
      ok OF drop ENDOF
      err OF s" gate-signal-row: the leaf failed" rot die ENDOF
   ;MATCH ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0= if ROW exit then
   0 SCRIPT-ARGV$ s" leaf" STR= if LEAF exit then
   s" gate-signal-row: unknown mode" USAGE-RC die ;

;package

GATE-SIGNAL-ROW:MAIN
