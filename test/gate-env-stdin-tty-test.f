\ gate-env-stdin-tty-test.f - controlling-terminal regression for GE-RUN-ENV.
\
\ The outer mode forks a non-session-leader worker, which opens a real Linux
\ pseudoterminal without accidentally acquiring it as the outer test's own
\ controlling terminal.  It then uses setsid(1) to run this file's driver mode
\ as the foreground process of a new session with that terminal as its
\ controlling terminal.  The driver then calls GE-RUN-ENV.
\ PROC-SPAWN-ARGV-ENV-RAW makes the captured child its own process-group leader,
\ so that child is a background group relative to the terminal.  Its ordinary
\ startup terminal ioctl must not stop it with SIGTTOU: GE-RUN-ENV promises an
\ explicit empty standard-input pipe, not the driver's ambient terminal.
\
\ Run: bin/hb --load test/gate-env-stdin-tty-test.f

require test/gate-runner-support.f

package GATE-ENV-STDIN-TTY-TEST

$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
2 constant PTY-OPEN-RDWR
2000 constant CHILD-TIMEOUT-MS
10 constant OUTPUT-POLL-MS
100 constant OUTPUT-MAX-POLLS
4096 constant OUTPUT-CAP

create PTY-NAME 128 allot
create PTY-NUM 4 allot
create OUTPUT OUTPUT-CAP allot

variable PTY-U
variable MASTER-FD
variable SLAVE-FD
variable SESSION-PID
variable SESSION-RC
variable OUTPUT-U
variable OUTPUT-RD

: PTY-NAME-C ( n -- ) {: c:n :}
   c PTY-NAME PTY-U @ + c!
   PTY-U @ 1+ PTY-U ! ;

: PTY-NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ PTY-NAME-C
      1+
   repeat drop ;

: PTY-NAME-U+ ( n -- ) {: n:n :}
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + PTY-NAME-C ;

: PTY-NAME-BUILD ( -- )
   0 PTY-U !
   s" /dev/pts/" PTY-NAME+
   PTY-NUM @ PTY-NAME-U+
   0 PTY-NAME-C ;

: OPEN-PTY ( -- )
   s" /dev/ptmx" >LEN PROC-PATHZ PTY-OPEN-RDWR 0 open MASTER-FD !
   MASTER-FD @ 2 <= if E-FS-OPEN throw then
   MASTER-FD @ >FD FD-CLOEXEC!
   0 PTY-NUM !
   MASTER-FD @ LINUX-TIOCSPTLCK PTY-NUM ioctl 0 <> if E-PROC-PTY-HANDLE throw then
   MASTER-FD @ LINUX-TIOCGPTN PTY-NUM ioctl 0 <> if E-PROC-PTY-HANDLE throw then
   PTY-NAME-BUILD
   PTY-NAME PTY-OPEN-RDWR 0 open SLAVE-FD !
   SLAVE-FD @ 2 <= if E-FS-OPEN throw then ;

: SESSION-ARGV ( -- )
   PROC-ARGV-RESET
   s" -w" >LEN PROC-ARGV+
   s" -c" >LEN PROC-ARGV+
   s" bin/hb" >LEN PROC-ARGV+
   s" --load" >LEN PROC-ARGV+
   s" test/gate-env-stdin-tty-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" driver" >LEN PROC-ARGV+ ;

: SPAWN-SESSION ( -- )
   SESSION-ARGV
   s" /usr/bin/setsid" >LEN
   SLAVE-FD @ >FD SLAVE-FD @ >FD SLAVE-FD @ >FD
   PROC-SPAWN-ARGV-IO PID>N SESSION-PID !
   SLAVE-FD @ close ;

: WAIT-SESSION ( -- )
   SESSION-PID @ >PID PROC-WAIT-RC
   MATCH result
     ok  OF SESSION-RC ! ENDOF
     err OF SESSION-RC ! ENDOF
   ;MATCH ;

: DRIVER-OK? ( -- bool )
   OUTPUT OUTPUT-U @ s" gate-env-stdin-tty-driver: ok" CONTAINS? ;

: READ-OUTPUT-STEP ( -- bool )
   MASTER-FD @ >FD OUTPUT-POLL-MS >MS POLL-IN COUNT>N {: ready:n :}
   ready 0 < if E-PROC-OUTPUT throw then
   ready 0= if false exit then
   OUTPUT-U @ OUTPUT-CAP >= if E-STR-CAPACITY throw then
   MASTER-FD @ OUTPUT OUTPUT-U @ + OUTPUT-CAP OUTPUT-U @ - read OUTPUT-RD !
   OUTPUT-RD @ 0 <= if true exit then
   OUTPUT-U @ OUTPUT-RD @ + OUTPUT-U !
   DRIVER-OK? ;

: READ-OUTPUT ( -- )
   0 OUTPUT-U !
   0 begin dup OUTPUT-MAX-POLLS < while
      READ-OUTPUT-STEP if drop MASTER-FD @ close exit then
      1+
   repeat drop
   MASTER-FD @ close
   E-PROC-TIMEOUT throw ;

: SHOW-DRIVER-FAILURE ( -- )
   SESSION-RC @ 0= if exit then
   s" controlling-terminal driver output:" type cr
   OUTPUT OUTPUT-U @ type cr ;

: CHECK-SESSION ( -- )
   SHOW-DRIVER-FAILURE
   s" controlling-terminal driver exits cleanly" T-LABEL
   SESSION-RC @ 0 T=
   s" background no-input child completes without SIGTTOU" T-LABEL
   DRIVER-OK? TTRUE ;

: RUN-CASE ( -- )
   T-RESET
   OPEN-PTY
   SPAWN-SESSION
   WAIT-SESSION
   READ-OUTPUT
   CHECK-SESSION
   T-REPORT
   s" gate-env-stdin-tty-test: ok" type cr ;

: RUN-CASE-CHILD ( -- )
   RUN-CASE
   s" " 0 die ;

: RUN-OUTER ( -- )
   HB-TARGET-LINUX? 0= if
      s" gate-env-stdin-tty-test: Linux controlling-terminal case skipped" type cr
      exit
   then
   T-RESET
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if RUN-CASE-CHILD then
   s" controlling-terminal case worker exits cleanly" T-LABEL
   pid >PID PROC-WAIT-RC
   MATCH result
     ok  OF 0 T= ENDOF
     err OF drop false TTRUE ENDOF
   ;MATCH
   T-REPORT ;

: RUN-DRIVER ( -- )
   PROC-ARGV-RESET
   GE-ARGV-RESET
   s" bin/hb" CHILD-TIMEOUT-MS GE-RUN-ENV
   s" background no-input child exits" GE-EXPECT-OK
   s" background no-input child is silent" GE-EXPECT-SILENT
   s" gate-env-stdin-tty-driver: ok" type cr ;

: DRIVER? ( -- bool )
   SCRIPT-ARGC 1 <> if false exit then
   0 SCRIPT-ARGV$ s" driver" STR= ;

: MAIN ( -- )
   DRIVER? if RUN-DRIVER exit then
   RUN-OUTER ;

MAIN

;package
