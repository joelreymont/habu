\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb < test/proc-pty.f

package PROC-PTY

$20007454 constant TIOCPTYGRANT
$40807453 constant TIOCPTYGNAME
$20007452 constant TIOCPTYUNLK
$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
2 constant PTY-OPEN-RDWR
10 constant PTY-POLL-MS
300 constant PTY-DRAIN-MAX-POLLS
200 constant PTY-EXPECT-MAX-POLLS
1 constant PTY-QUIET-POLLS

create RBUF 4096 allot
create NL 1 allot
create EOT 1 allot
create CH 1 allot
create PTYNAME 128 allot

variable #FAIL
variable #CASE
variable RN
variable QUIET
variable IN-R
variable IN-W
variable OUT-R
variable OUT-W
variable ERR-R
variable ERR-W
variable PID
variable MFD
variable SFD
variable PTY-U
variable PTYNUM

: HB-ARG? ( -- bool )
   SCRIPT-ARGC 0 > ;

: HB-EXE$ ( -- ptr u8 n )
   HB-ARG? if 0 SCRIPT-ARGV$ exit then
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: RBUF-DUMP ( -- )
   s" rbuf bytes: " type RN @ . cr
   RN @ 0 > if
      s" rbuf:" type cr
      RBUF RN @ type cr
   then ;

: T-FAIL ( -- )
   [char] F emit #CASE @ . cr
   RBUF-DUMP
   #FAIL @ 1 + #FAIL ! ;

: T= {: got want :} ( n n -- )
   #CASE @ 1 + #CASE !
   got want <> if T-FAIL then ;

: TTRUE ( bool -- )
   #CASE @ 1 + #CASE !
   0= if T-FAIL then ;

: RCLR ( -- )
   0 RN ! ;

: READ+ {: fd :} ( fd -- )
   fd FD>N RBUF RN @ + 4096 RN @ - read
   dup 0 > if RN @ + RN ! else drop then ;

: MATCH-AT {: ha:ptr na:ptr nu off :} ( ptr u8 ptr u8 n n -- bool )
   0 0=
   nu 0 ?do
      ha off + i + c@  na i + c@  <> if drop 0 0= 0= leave then
   loop ;

: CONTAINS? {: ha:ptr hu na:ptr nu :} ( ptr u8 n ptr u8 n -- bool )
   nu 0= if 0 0= exit then
   hu nu < if 0 0= 0= exit then
   hu nu - 1 + 0 ?do
      ha na nu i MATCH-AT if 0 0= unloop exit then
   loop
   0 0= 0= ;

: TCONTAINS {: a:ptr u :} ( ptr u8 n -- )
   RBUF RN @ a u CONTAINS? TTRUE ;

: FD-WRITE {: fd a:ptr u :} ( fd ptr u8 n -- )
   fd FD>N a u write u T= ;

: FD-WRITE-LN {: fd a:ptr u :} ( fd ptr u8 n -- )
   fd a u FD-WRITE
   fd NL 1 FD-WRITE ;

: DRAIN {: fd :} ( fd -- )
   RCLR
   0 QUIET !
   0 begin dup PTY-DRAIN-MAX-POLLS < QUIET @ PTY-QUIET-POLLS < and while
      fd PTY-POLL-MS >MS POLL-IN COUNT>N 0 > if fd READ+ 0 QUIET ! else QUIET @ 1 + QUIET ! then
      1 +
   repeat drop ;

: MFD-DRAIN ( -- )
   MFD @ >FD DRAIN ;

: RBUF-HAS? {: a:ptr u :} ( ptr u8 n -- bool )
   RBUF RN @ a u CONTAINS? ;

: MFD-READ-READY? ( -- bool )
   MFD @ >FD PTY-POLL-MS >MS POLL-IN COUNT>N 0 > if
      MFD @ >FD READ+
      0 0= exit
   then
   0 0= 0= ;

: EXPECT-WAIT? {: a:ptr u :} ( ptr u8 n -- bool )
   a u RBUF-HAS? if 0 0= exit then
   0 begin dup PTY-EXPECT-MAX-POLLS < while
      MFD-READ-READY? drop
      a u RBUF-HAS? if drop 0 0= exit then
      1 +
   repeat drop
   0 0= 0= ;

: SEND-C {: c :} ( c -- )
   c CH c!
   MFD @ >FD CH 1 FD-WRITE ;

: SEND-S {: a:ptr u :} ( ptr u8 n -- )
   MFD @ >FD a u FD-WRITE ;

: SEND-LN {: a:ptr u :} ( ptr u8 n -- )
   MFD @ >FD a u FD-WRITE-LN ;

: SEND-ESC ( c -- )
   27 SEND-C
   91 SEND-C
   SEND-C ;

: STEP-LN {: a:ptr u :} ( ptr u8 n -- )
   a u SEND-LN
   MFD-DRAIN ;

: STEP-S {: a:ptr u :} ( ptr u8 n -- )
   a u SEND-S
   MFD-DRAIN ;

: EXPECT ( ptr u8 n -- )
   EXPECT-WAIT? TTRUE ;

: REJECT {: a:ptr u :} ( ptr u8 n -- )
   RBUF RN @ a u CONTAINS? 0= TTRUE ;

: EXPECT-OK ( -- )
   s"  ok" EXPECT ;

: EXPECT-PROMPT ( -- )
   s" habu> " EXPECT ;

: REJECT-OK ( -- )
   s"  ok" REJECT ;

: CAPTURE-PIPES ( -- )
   PIPE-PAIR IN-W ! IN-R !
   PIPE-PAIR OUT-W ! OUT-R !
   PIPE-PAIR ERR-W ! ERR-R !
   IN-W @ >FD FD-CLOEXEC!
   OUT-R @ >FD FD-CLOEXEC!
   ERR-R @ >FD FD-CLOEXEC! ;

: CAPTURE-START-HB ( -- )
   HB-EXE$ >LEN IN-R @ >FD OUT-W @ >FD ERR-W @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE ;

: CAPTURE-CLOSE-CHILD-ENDS ( -- )
   IN-R @ close
   OUT-W @ close
   ERR-W @ close ;

: CAPTURE-SEND-SOURCE ( -- )
   IN-W @ >FD s" 2 3 + ." FD-WRITE-LN
   IN-W @ close ;

: CAPTURE-EXPECT-RC ( -- )
   PID @ >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: CAPTURE-EXPECT-OUT ( -- )
   RCLR
   OUT-R @ >FD READ+
   s" 5" TCONTAINS
   OUT-R @ close ;

: CAPTURE-EXPECT-ERR ( -- )
   RCLR
   ERR-R @ >FD READ+
   RN @ 0 T=
   ERR-R @ close ;

: CAPTURE-VERIFY ( -- )
   CAPTURE-EXPECT-RC
   CAPTURE-EXPECT-OUT
   CAPTURE-EXPECT-ERR ;

: CAPTURE-HB ( -- )
   CAPTURE-PIPES
   CAPTURE-START-HB
   CAPTURE-CLOSE-CHILD-ENDS
   CAPTURE-SEND-SOURCE
   CAPTURE-VERIFY ;

: PTY-PATH-C ( n -- ) {: c :}
   c PTYNAME PTY-U @ + c!
   PTY-U @ 1 + PTY-U ! ;

: PTY-PATH+ ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ PTY-PATH-C
      1 +
   repeat drop ;

: PTY-PATH-U+ ( n -- ) {: n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + PTY-PATH-C ;

: PTY-PATH-BUILD ( -- )
   0 PTY-U !
   s" /dev/pts/" PTY-PATH+
   PTYNUM @ PTY-PATH-U+
   0 PTY-PATH-C ;

: OPEN-PTY-MASTER ( n -- ) {: flags :}
   s" /dev/ptmx" >LEN PROC-PATHZ flags 0 open MFD !
   MFD @ 2 > TTRUE
   MFD @ >FD FD-CLOEXEC! ;

: OPEN-PTY-DARWIN ( -- )
   PTY-OPEN-RDWR OPEN-PTY-MASTER
   MFD @ TIOCPTYGRANT NULL$ drop ioctl 0 T=
   MFD @ TIOCPTYUNLK NULL$ drop ioctl 0 T=
   MFD @ TIOCPTYGNAME PTYNAME ioctl 0 T=
   PTYNAME PTY-OPEN-RDWR 0 open SFD !
   SFD @ 2 > TTRUE ;

: OPEN-PTY-LINUX-MASTER ( -- )
   PTY-OPEN-RDWR OPEN-PTY-MASTER
   0 PTYNUM !
   MFD @ LINUX-TIOCSPTLCK PTYNUM ioctl 0 T=
   MFD @ LINUX-TIOCGPTN PTYNUM ioctl 0 T= ;

: OPEN-PTY-LINUX-SLAVE ( -- )
   PTY-PATH-BUILD
   PTYNAME PTY-OPEN-RDWR 0 open SFD !
   SFD @ 2 > TTRUE ;

: OPEN-PTY-LINUX ( -- )
   OPEN-PTY-LINUX-MASTER
   OPEN-PTY-LINUX-SLAVE ;

: PTY-TARGET-UNKNOWN ( -- )
   s" proc-pty: unknown target" 64 die ;

: OPEN-PTY ( -- )
   HB-TARGET-LINUX? if OPEN-PTY-LINUX exit then
   HB-TARGET-MACOS? if OPEN-PTY-DARWIN exit then
   PTY-TARGET-UNKNOWN ;

: PTY-START-HB ( -- )
   OPEN-PTY
   HB-EXE$ >LEN SFD @ >FD SFD @ >FD SFD @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE
   SFD @ close
   MFD-DRAIN ;

: PTY-PROMPT ( -- )
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-ARITH ( -- )
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-UNKNOWN ( -- )
   s" frobnicate" STEP-LN
   s" E-UNDEFINED: frobnicate" EXPECT
   s" ?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT ;

: PTY-SQUARE ( -- )
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" EXPECT ;

: PTY-BACKSPACE ( -- )
   s" 1 2 + .." SEND-S
   127 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 3" EXPECT
   s"  ok" EXPECT ;

: PTY-CANCEL ( -- )
   s" garbage" SEND-S
   3 SEND-C
   MFD-DRAIN
   s" habu> " EXPECT
   s" garbage?" REJECT ;

: PTY-EDIT-SEED ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-LEFT3 ( -- )
   s" 13 ." SEND-S
   68 SEND-ESC
   68 SEND-ESC
   68 SEND-ESC ;

: PTY-EDIT-INSERT-RUN ( -- )
   48 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-HOME ( -- )
   PTY-EDIT-SEED
   PTY-EDIT-LEFT3
   PTY-EDIT-INSERT-RUN ;

: PTY-HISTORY-UP ( -- )
   65 SEND-ESC
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

: PTY-BP-SOURCE ( -- )
   s" : SQB dup * ;" STEP-LN
   s"  ok" EXPECT
   s" : IN1 1 + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-ARM-ONESHOT ( -- )
   s" ' SQB BP+" STEP-LN
   s"  ok" EXPECT
   s" ' IN1 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-RUN-SQ ( -- )
   s" 7 SQB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 49" EXPECT ;

: PTY-BP-RUN-IN1 ( -- )
   s" 9 IN1 ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-BP-RUN-SQ-CLEARED ( -- )
   s" 6 SQB ." STEP-LN
   s" 36" EXPECT
   s" habu-bp:" REJECT ;

: PTY-BP-ONESHOT ( -- )
   PTY-BP-ARM-ONESHOT
   PTY-BP-RUN-SQ
   PTY-BP-RUN-IN1
   PTY-BP-RUN-SQ-CLEARED ;

: PTY-PB-SOURCE ( -- )
   s" : PB dup + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-ARM ( -- )
   s" ' PB BP*" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-FIRST ( -- )
   s" 5 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-PB-SECOND ( -- )
   s" 6 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 12" EXPECT ;

: PTY-PB-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-PERSISTENT ( -- )
   PTY-PB-SOURCE
   PTY-PB-ARM
   PTY-PB-FIRST
   PTY-PB-SECOND
   PTY-PB-CLEAR ;

: PTY-WATCH-VAR ( -- )
   s" variable WV" STEP-LN
   s"  ok" EXPECT
   s" 17 WV !" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-ADD ( -- )
   s" WV BPW+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-WORD ( -- )
   s" : WID dup WV @ + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-SOURCE ( -- )
   PTY-WATCH-VAR
   PTY-WATCH-ADD
   PTY-WATCH-WORD ;

: PTY-WATCH-ARM ( -- )
   s" ' WID BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-RUN ( -- )
   s" 2 WID ." STEP-LN
   s" habu-bp-stack:" EXPECT
   s" habu-bp-watch:" EXPECT
   s" 0000000000000011" EXPECT
   s" 19" EXPECT ;

: PTY-WATCH-CLEAR ( -- )
   s" WV BPW-" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCHPOINT ( -- )
   PTY-WATCH-SOURCE
   PTY-WATCH-ARM
   PTY-WATCH-RUN
   PTY-WATCH-CLEAR ;

: PTY-BPN-ARM ( -- )
   s" 2 ' PB BPN" STEP-LN
   s"  ok" EXPECT ;

: PTY-BPN-SKIP ( -- )
   s" 3 PB ." STEP-LN
   s" 6" EXPECT
   s" habu-bp:" REJECT ;

: PTY-BPN-FIRE ( -- )
   s" 3 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 6" EXPECT ;

: PTY-BPN-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-NTH ( -- )
   PTY-BPN-ARM
   PTY-BPN-SKIP
   PTY-BPN-SKIP
   PTY-BPN-FIRE
   PTY-BPN-CLEAR ;

: PTY-DEFINE-F0-F2 ( -- )
   s" : F0 0 ;" STEP-LN
   s"  ok" EXPECT
   s" : F1 1 ;" STEP-LN
   s"  ok" EXPECT
   s" : F2 2 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F3-F4 ( -- )
   s" : F3 3 ;" STEP-LN
   s"  ok" EXPECT
   s" : F4 4 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F0-F4 ( -- )
   PTY-DEFINE-F0-F2
   PTY-DEFINE-F3-F4 ;

: PTY-DEFINE-F5-F6 ( -- )
   s" : F5 5 ;" STEP-LN
   s"  ok" EXPECT
   s" : F6 6 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F7-F8 ( -- )
   s" : F7 7 ;" STEP-LN
   s"  ok" EXPECT
   s" : F8 8 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F5-F8 ( -- )
   PTY-DEFINE-F5-F6
   PTY-DEFINE-F7-F8 ;

: PTY-BP-F0-F2 ( -- )
   s" ' F0 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F1 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F2 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F3-F4 ( -- )
   s" ' F3 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F4 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F0-F4 ( -- )
   PTY-BP-F0-F2
   PTY-BP-F3-F4 ;

: PTY-BP-F5-F6 ( -- )
   s" ' F5 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F6 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F7-F8 ( -- )
   s" ' F7 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F8 BP+" STEP-LN
   s" table full" EXPECT ;

: PTY-BP-F5-F8 ( -- )
   PTY-BP-F5-F6
   PTY-BP-F7-F8 ;

: PTY-BP-TABLE-FULL ( -- )
   PTY-DEFINE-F0-F4
   PTY-DEFINE-F5-F8
   PTY-BP-F0-F4
   PTY-BP-F5-F8 ;

: PTY-STEP-BASELINE ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-STEP-TOKENS ( -- )
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT ;

: PTY-STEP-DEFINE ( -- )
   s" step : SD dup * ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-STEP-RUN ( -- )
   s" 4 SD ." STEP-LN
   s" 16" EXPECT ;

: PTY-STEP-RECOVER ( -- )
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" EXPECT ;

: PTY-STEPPER ( -- )
   PTY-STEP-BASELINE
   PTY-STEP-TOKENS
   PTY-STEP-DEFINE
   PTY-STEP-RUN
   PTY-STEP-RECOVER ;

: PTY-THROW-LINE ( -- )
   s" 99 throw" STEP-LN
   s" ?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT ;

: PTY-THROW-AFTER ( -- )
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" EXPECT ;

: PTY-THROW-RECOVERY ( -- )
   PTY-THROW-LINE
   PTY-THROW-AFTER ;

: PTY-STOP-HB ( -- )
   4 SEND-C
   PID @ >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   MFD @ close ;

: PTY-BASIC ( -- )
   PTY-START-HB
   PTY-PROMPT
   PTY-ARITH
   PTY-UNKNOWN
   PTY-SQUARE ;

: PTY-EDITOR ( -- )
   PTY-BACKSPACE
   PTY-CANCEL
   PTY-EDIT-HOME
   PTY-HISTORY-UP ;

: PTY-BREAKPOINTS ( -- )
   PTY-BP-SOURCE
   PTY-BP-ONESHOT
   PTY-BP-PERSISTENT
   PTY-WATCHPOINT
   PTY-BP-NTH
   PTY-BP-TABLE-FULL ;

: PTY-TOOLS ( -- )
   PTY-STEPPER
   PTY-THROW-RECOVERY ;

: PTY-HB ( -- )
   PTY-BASIC
   PTY-EDITOR
   PTY-BREAKPOINTS
   PTY-TOOLS
   PTY-STOP-HB ;

: REPORT ( -- )
   #FAIL @ 0 = if s" PASS: process/pty primitives" type cr exit then
   #FAIL @ . s" proc-pty: failures" 1 die ;

10 NL c!
4 EOT c!
CAPTURE-HB
PTY-HB
REPORT
;package
