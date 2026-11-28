\ process-cpu-test.f - one process's own CPU time against the clock and the
\ kernel's own report.
\ Run: bin/hb --load src/lib/process-cpu-test.f
\ Failure modes: a binding that reads the wrong clock, field or scale, so a
\ spinning child's time does not rise, rises faster than the wall clock, or
\ disagrees with /proc/<pid>/stat on Linux; one that counts what is not the
\ process's own, so an idle child reads more than its exec; a reaped pid or a
\ pid at or below 1 read as a time instead of throwing E-PROC-OUTPUT.

require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/task.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-cpu.f

package PROC-CPU-TEST

1000000 constant NS-PER-MS
10000000 constant NS-PER-TICK       \ USER_HZ is 100 on every Linux target
200 constant GAP-MS
10 constant RISE-SLACK-MS
20 constant STAT-SLACK-MS
50 constant IDLE-MS
512 constant STAT-CAP
41 constant CLOSE-PAREN
48 constant DIGIT-0

create STAT-BUF STAT-CAP allot
variable SPIN                       \ the spinning child until it is reaped
variable IDLE                       \ the sleeping child until it is reaped
variable GONE                       \ the spinner's pid once reaped
variable FIELD#
variable TICKS
variable NUM
PROCESS:NO-PID SPIN !
PROCESS:NO-PID IDLE !

: SPIN-START ( -- )
   PROC-ARGV-RESET
   s" -c" PROC-ARGV+
   s" while :; do :; done" PROC-ARGV+
   s" /bin/sh" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N SPIN ! ;

: IDLE-START ( -- )
   PROC-ARGV-RESET
   s" 30" PROC-ARGV+
   s" /bin/sleep" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N IDLE ! ;

\ SIGKILL the child the cell holds, reap it and clear the cell.
: END-CHILD ( ^n -- ) {: cell:^ :}
   cell @ PROCESS:NO-PID = if exit then
   cell @ PROCESS:SIGKILL kill drop
   cell @ >PID PROCESS:WAIT drop
   PROCESS:NO-PID cell ! ;

: END-ALL ( -- )
   SPIN END-CHILD
   IDLE END-CHILD ;

: CPU ( n -- n ) >PID PROC-CPU:CPU-NS ;

\ /proc/<pid>/stat's line; field 2 is the command in parentheses, which may
\ hold spaces, so fields are counted from the last ')'.
: STAT$ ( n -- str ) {: pid:n :}
   SB-RESET s" /proc/" SB-APPEND pid FMT:SB-INT s" /stat" SB-APPEND
   SB$ STAT-BUF STAT-CAP FS:READ-ALL {: got:n :}
   STAT-BUF got >str ;

: LAST-PAREN ( str -- n ) {: s:str :}
   -1 s STR:LEN 0 ?do
      s STR:DATA i + c@ CLOSE-PAREN = if drop i then
   loop ;

\ One byte past the ')': a space ends a field, adding fields 14 and 15.
: STAT-BYTE ( n -- ) {: c:n :}
   c STR:SPACE = if
      FIELD# @ 14 = FIELD# @ 15 = or if NUM @ TICKS +! then
      0 NUM !  1 FIELD# +!  exit
   then
   NUM @ 10 * c DIGIT-0 - + NUM ! ;

\ utime + stime, fields 14 and 15, in nanoseconds.
: STAT-NS ( n -- n )
   STAT$ {: line:str :}
   2 FIELD# !  0 TICKS !  0 NUM !
   line LAST-PAREN 1+ {: from:n :}
   line STR:LEN from ?do line STR:DATA i + c@ STAT-BYTE loop
   TICKS @ NS-PER-TICK * ;

\ Read first, then the kernel's report: a spinning child gains between the
\ two, and the report truncates each field to a tick.
: AGREES ( n -- ) {: pid:n :}
   pid CPU {: ns:n :}
   pid STAT-NS {: stat:n :}
   ns stat - abs STAT-SLACK-MS NS-PER-MS * <= TTRUE ;

: AGREES-ON-LINUX ( n -- )
   HB-TARGET-LINUX? if AGREES exit then drop ;

\ The wall window brackets both reads, so it is never shorter than the CPU
\ time the child can gain between them.
: SPINS ( -- )
   SPIN-START
   mono-ns {: t0:n :}
   SPIN @ CPU {: first:n :}
   GAP-MS >MS TASK:SLEEP
   SPIN @ CPU {: second:n :}
   mono-ns {: t1:n :}
   s" spin: a spinning child's time rises" T-LABEL
   second first > TTRUE
   s" spin: and by no more than the wall time between the reads" T-LABEL
   second first - t1 t0 - RISE-SLACK-MS NS-PER-MS * + <= TTRUE
   s" spin: each read agrees with /proc/<pid>/stat" T-LABEL
   SPIN @ AGREES-ON-LINUX
   SPIN @ AGREES-ON-LINUX ;

: IDLES ( -- )
   IDLE-START
   s" idle: a sleeping child has run under 50 ms" T-LABEL
   IDLE @ CPU IDLE-MS NS-PER-MS * < TTRUE ;

: GONE-READ ( -- ) GONE @ CPU drop ;
: PID0-READ ( -- ) 0 CPU drop ;
: PID1-READ ( -- ) 1 CPU drop ;

: REJECTS ( -- )
   SPIN @ GONE !
   SPIN END-CHILD
   s" reaped: a reaped child's pid throws E-PROC-OUTPUT" T-LABEL
   [: GONE-READ ;] E-PROC-OUTPUT TTHROWSQ
   s" low: pid 0 throws E-PROC-OUTPUT" T-LABEL
   [: PID0-READ ;] E-PROC-OUTPUT TTHROWSQ
   s" low: pid 1 throws E-PROC-OUTPUT" T-LABEL
   [: PID1-READ ;] E-PROC-OUTPUT TTHROWSQ ;

: BODY ( -- )
   SPINS
   IDLES
   REJECTS ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: END-ALL ;] finally
   T-REPORT ;

RUN
;package
