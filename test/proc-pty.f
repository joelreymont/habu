\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb --load test/proc-pty.f

require lib/engine-candidate.f
require lib/process-pty-io.f

package PROC-PTY

10 constant PTY-POLL-MS
300 constant PTY-DRAIN-MAX-POLLS
200 constant PTY-EXPECT-MAX-POLLS
1 constant PTY-QUIET-POLLS

create RBUF 4096 allot
create NL 1 allot
create EOT 1 allot
create CH 1 allot

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
variable EXPECT-A
variable EXPECT-U
variable DRAIN-I
variable EXPECT-I

: HB-ARG? ( -- bool )
   SCRIPT-ARGC 0 > ;

: HB-EXE$ ( -- ptr u8 n )
   HB-ARG? if 0 SCRIPT-ARGV$ ENGINE-CANDIDATE:VALIDATE$ exit then
   ENGINE-CANDIDATE:PATH$ ;

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

: EXPECT-A-FIELD ( -- ptr ptr u8 )
   EXPECT-A 0 ptr-field ;

: EXPECT-A@ ( -- ptr u8 )
   EXPECT-A-FIELD @ ;

: EXPECT! ( ptr u8 n -- )
   EXPECT-U ! EXPECT-A-FIELD ! ;

: PTY-READ+ ( process-pty-handle -- process-pty-handle )
   RBUF RN @ + 4096 RN @ - PROCESS-PTY:READ
   dup 0 > if RN @ + RN ! else drop then ;

: PTY-DRAIN ( process-pty-handle -- process-pty-handle )
   RCLR
   0 QUIET !
   0 DRAIN-I !
   begin DRAIN-I @ PTY-DRAIN-MAX-POLLS < QUIET @ PTY-QUIET-POLLS < and while
      PTY-POLL-MS >MS PROCESS-PTY:POLL-IN COUNT>N 0 >
      if PTY-READ+ 0 QUIET ! else QUIET @ 1 + QUIET ! then
      DRAIN-I @ 1 + DRAIN-I !
   repeat ;

: MFD-DRAIN ( process-pty-handle -- process-pty-handle )
   PTY-DRAIN ;

: RBUF-HAS? {: a:ptr u :} ( ptr u8 n -- bool )
   RBUF RN @ a u CONTAINS? ;

: MFD-READ-READY? ( process-pty-handle -- process-pty-handle bool )
   PTY-POLL-MS >MS PROCESS-PTY:POLL-IN COUNT>N 0 > if
      PTY-READ+
      0 0= exit
   then
   0 0= 0= ;

: EXPECT-WAIT? ( process-pty-handle ptr u8 n -- process-pty-handle bool )
   EXPECT!
   EXPECT-A@ EXPECT-U @ RBUF-HAS? if 0 0= exit then
   0 EXPECT-I !
   begin EXPECT-I @ PTY-EXPECT-MAX-POLLS < while
      MFD-READ-READY? drop
      EXPECT-A@ EXPECT-U @ RBUF-HAS? if 0 0= exit then
      EXPECT-I @ 1 + EXPECT-I !
   repeat
   0 0= 0= ;

: SEND-C ( process-pty-handle n -- process-pty-handle )
   CH c!
   CH 1 PROCESS-PTY:WRITE ;

: SEND-S ( process-pty-handle ptr u8 n -- process-pty-handle )
   PROCESS-PTY:WRITE ;

: SEND-LN ( process-pty-handle ptr u8 n -- process-pty-handle )
   PROCESS-PTY:WRITE
   NL 1 PROCESS-PTY:WRITE ;

: SEND-ESC ( process-pty-handle n -- process-pty-handle )
   >r
   27 SEND-C
   91 SEND-C
   r> SEND-C ;

: STEP-LN ( process-pty-handle ptr u8 n -- process-pty-handle )
   SEND-LN
   MFD-DRAIN ;

: STEP-S ( process-pty-handle ptr u8 n -- process-pty-handle )
   SEND-S
   MFD-DRAIN ;

: EXPECT ( process-pty-handle ptr u8 n -- process-pty-handle )
   EXPECT-WAIT? TTRUE ;

: REJECT ( process-pty-handle ptr u8 n -- process-pty-handle )
   EXPECT!
   RBUF RN @ EXPECT-A@ EXPECT-U @ CONTAINS? 0= TTRUE ;

: EXPECT-OK ( process-pty-handle -- process-pty-handle )
   s"  ok" EXPECT ;

: EXPECT-PROMPT ( process-pty-handle -- process-pty-handle )
   s" habu> " EXPECT ;

: REJECT-OK ( process-pty-handle -- process-pty-handle )
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

: PTY-START-HB ( -- process-pty-handle )
   HB-EXE$ >LEN PROCESS-PTY:START
   MFD-DRAIN ;

: PTY-PROMPT ( process-pty-handle -- process-pty-handle )
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-ARITH ( process-pty-handle -- process-pty-handle )
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-UNKNOWN ( process-pty-handle -- process-pty-handle )
   s" frobnicate" STEP-LN
   s" E-UNDEFINED: frobnicate" EXPECT
   s" ?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT ;

: PTY-SQUARE ( process-pty-handle -- process-pty-handle )
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" EXPECT ;

\ Certified word on an empty interpret stack: named underdepth reject, then
\ the REPL recovers and the next line evaluates (LDIAGRET recovery leg; dot
\ habu-habu-certified-words-84e84eaf).
: PTY-UNDERDEPTH ( process-pty-handle -- process-pty-handle )
   s" SQ" STEP-LN
   s" hb: interpret stack underdepth: SQ" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT
   s" 6 SQ ." STEP-LN
   s" 36" EXPECT
   s"  ok" EXPECT ;

: PTY-BACKSPACE ( process-pty-handle -- process-pty-handle )
   s" 1 2 + .." SEND-S
   127 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 3" EXPECT
   s"  ok" EXPECT ;

: PTY-CANCEL ( process-pty-handle -- process-pty-handle )
   s" garbage" SEND-S
   3 SEND-C
   MFD-DRAIN
   s" habu> " EXPECT
   s" garbage?" REJECT ;

: PTY-EDIT-SEED ( process-pty-handle -- process-pty-handle )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-LEFT3 ( process-pty-handle -- process-pty-handle )
   s" 13 ." SEND-S
   68 SEND-ESC
   68 SEND-ESC
   68 SEND-ESC ;

: PTY-EDIT-INSERT-RUN ( process-pty-handle -- process-pty-handle )
   48 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-HOME ( process-pty-handle -- process-pty-handle )
   PTY-EDIT-SEED
   PTY-EDIT-LEFT3
   PTY-EDIT-INSERT-RUN ;

: PTY-HISTORY-UP ( process-pty-handle -- process-pty-handle )
   65 SEND-ESC
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

: PTY-BP-SOURCE ( process-pty-handle -- process-pty-handle )
   s" : SQB dup * ;" STEP-LN
   s"  ok" EXPECT
   s" : IN1 1 + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-ARM-ONESHOT ( process-pty-handle -- process-pty-handle )
   s" ' SQB BP+" STEP-LN
   s"  ok" EXPECT
   s" ' IN1 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-RUN-SQ ( process-pty-handle -- process-pty-handle )
   s" 7 SQB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 49" EXPECT ;

: PTY-BP-RUN-IN1 ( process-pty-handle -- process-pty-handle )
   s" 9 IN1 ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-BP-RUN-SQ-CLEARED ( process-pty-handle -- process-pty-handle )
   s" 6 SQB ." STEP-LN
   s" 36" EXPECT
   s" habu-bp:" REJECT ;

: PTY-BP-ONESHOT ( process-pty-handle -- process-pty-handle )
   PTY-BP-ARM-ONESHOT
   PTY-BP-RUN-SQ
   PTY-BP-RUN-IN1
   PTY-BP-RUN-SQ-CLEARED ;

: PTY-PB-SOURCE ( process-pty-handle -- process-pty-handle )
   s" : PB dup + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-ARM ( process-pty-handle -- process-pty-handle )
   s" ' PB BP*" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-FIRST ( process-pty-handle -- process-pty-handle )
   s" 5 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-PB-SECOND ( process-pty-handle -- process-pty-handle )
   s" 6 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 12" EXPECT ;

: PTY-PB-CLEAR ( process-pty-handle -- process-pty-handle )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-PERSISTENT ( process-pty-handle -- process-pty-handle )
   PTY-PB-SOURCE
   PTY-PB-ARM
   PTY-PB-FIRST
   PTY-PB-SECOND
   PTY-PB-CLEAR ;

: PTY-WATCH-VAR ( process-pty-handle -- process-pty-handle )
   s" variable WV" STEP-LN
   s"  ok" EXPECT
   s" 17 WV !" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-ADD ( process-pty-handle -- process-pty-handle )
   s" WV BPW+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-WORD ( process-pty-handle -- process-pty-handle )
   s" : WID dup WV @ + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-SOURCE ( process-pty-handle -- process-pty-handle )
   PTY-WATCH-VAR
   PTY-WATCH-ADD
   PTY-WATCH-WORD ;

: PTY-WATCH-ARM ( process-pty-handle -- process-pty-handle )
   s" ' WID BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-RUN ( process-pty-handle -- process-pty-handle )
   s" 2 WID ." STEP-LN
   s" habu-bp-stack:" EXPECT
   s" habu-bp-watch:" EXPECT
   s" 0000000000000011" EXPECT
   s" 19" EXPECT ;

: PTY-WATCH-CLEAR ( process-pty-handle -- process-pty-handle )
   s" WV BPW-" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCHPOINT ( process-pty-handle -- process-pty-handle )
   PTY-WATCH-SOURCE
   PTY-WATCH-ARM
   PTY-WATCH-RUN
   PTY-WATCH-CLEAR ;

: PTY-BPN-ARM ( process-pty-handle -- process-pty-handle )
   s" 2 ' PB BPN" STEP-LN
   s"  ok" EXPECT ;

: PTY-BPN-SKIP ( process-pty-handle -- process-pty-handle )
   s" 3 PB ." STEP-LN
   s" 6" EXPECT
   s" habu-bp:" REJECT ;

: PTY-BPN-FIRE ( process-pty-handle -- process-pty-handle )
   s" 3 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 6" EXPECT ;

: PTY-BPN-CLEAR ( process-pty-handle -- process-pty-handle )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-NTH ( process-pty-handle -- process-pty-handle )
   PTY-BPN-ARM
   PTY-BPN-SKIP
   PTY-BPN-SKIP
   PTY-BPN-FIRE
   PTY-BPN-CLEAR ;

: PTY-DEFINE-F0-F2 ( process-pty-handle -- process-pty-handle )
   s" : F0 0 ;" STEP-LN
   s"  ok" EXPECT
   s" : F1 1 ;" STEP-LN
   s"  ok" EXPECT
   s" : F2 2 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F3-F4 ( process-pty-handle -- process-pty-handle )
   s" : F3 3 ;" STEP-LN
   s"  ok" EXPECT
   s" : F4 4 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F0-F4 ( process-pty-handle -- process-pty-handle )
   PTY-DEFINE-F0-F2
   PTY-DEFINE-F3-F4 ;

: PTY-DEFINE-F5-F6 ( process-pty-handle -- process-pty-handle )
   s" : F5 5 ;" STEP-LN
   s"  ok" EXPECT
   s" : F6 6 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F7-F8 ( process-pty-handle -- process-pty-handle )
   s" : F7 7 ;" STEP-LN
   s"  ok" EXPECT
   s" : F8 8 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F5-F8 ( process-pty-handle -- process-pty-handle )
   PTY-DEFINE-F5-F6
   PTY-DEFINE-F7-F8 ;

: PTY-BP-F0-F2 ( process-pty-handle -- process-pty-handle )
   s" ' F0 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F1 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F2 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F3-F4 ( process-pty-handle -- process-pty-handle )
   s" ' F3 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F4 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F0-F4 ( process-pty-handle -- process-pty-handle )
   PTY-BP-F0-F2
   PTY-BP-F3-F4 ;

: PTY-BP-F5-F6 ( process-pty-handle -- process-pty-handle )
   s" ' F5 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F6 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F7-F8 ( process-pty-handle -- process-pty-handle )
   s" ' F7 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F8 BP+" STEP-LN
   s" table full" EXPECT ;

: PTY-BP-F5-F8 ( process-pty-handle -- process-pty-handle )
   PTY-BP-F5-F6
   PTY-BP-F7-F8 ;

: PTY-BP-TABLE-FULL ( process-pty-handle -- process-pty-handle )
   PTY-DEFINE-F0-F4
   PTY-DEFINE-F5-F8
   PTY-BP-F0-F4
   PTY-BP-F5-F8 ;

: PTY-STEP-BASELINE ( process-pty-handle -- process-pty-handle )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-STEP-TOKENS ( process-pty-handle -- process-pty-handle )
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT ;

: PTY-STEP-DEFINE ( process-pty-handle -- process-pty-handle )
   s" step : SD dup * ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-STEP-RUN ( process-pty-handle -- process-pty-handle )
   s" 4 SD ." STEP-LN
   s" 16" EXPECT ;

: PTY-STEP-RECOVER ( process-pty-handle -- process-pty-handle )
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" EXPECT ;

: PTY-STEPPER ( process-pty-handle -- process-pty-handle )
   PTY-STEP-BASELINE
   PTY-STEP-TOKENS
   PTY-STEP-DEFINE
   PTY-STEP-RUN
   PTY-STEP-RECOVER ;

: PTY-THROW-LINE ( process-pty-handle -- process-pty-handle )
   s" 99 throw" STEP-LN
   s" ?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT ;

: PTY-THROW-AFTER ( process-pty-handle -- process-pty-handle )
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" EXPECT ;

: PTY-THROW-RECOVERY ( process-pty-handle -- process-pty-handle )
   PTY-THROW-LINE
   PTY-THROW-AFTER ;

: PTY-EXITED ( outcome -- )
   MATCH outcome
     exited OF 0 T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: PTY-STOP-HB ( process-pty-handle -- )
   4 SEND-C
   PROCESS-PTY:WAIT PTY-EXITED ;

: PTY-BASIC ( -- process-pty-handle )
   PTY-START-HB
   PTY-PROMPT
   PTY-ARITH
   PTY-UNKNOWN
   PTY-SQUARE
   PTY-UNDERDEPTH ;

: PTY-EDITOR ( process-pty-handle -- process-pty-handle )
   PTY-BACKSPACE
   PTY-CANCEL
   PTY-EDIT-HOME
   PTY-HISTORY-UP ;

: PTY-BREAKPOINTS ( process-pty-handle -- process-pty-handle )
   PTY-BP-SOURCE
   PTY-BP-ONESHOT
   PTY-BP-PERSISTENT
   PTY-WATCHPOINT
   PTY-BP-NTH
   PTY-BP-TABLE-FULL ;

: PTY-TOOLS ( process-pty-handle -- process-pty-handle )
   PTY-STEPPER
   PTY-THROW-RECOVERY ;

: PTY-HB ( -- )
   PTY-BASIC
   PTY-EDITOR
   PTY-BREAKPOINTS
   PTY-TOOLS
   PTY-STOP-HB ;

: PTY-RUN ( -- )
   [: PTY-HB ;] catch dup 0= if drop exit then
   s" proc-pty: throw after case " type #CASE @ .
   s" proc-pty: throw code " type dup .
   throw ;

: REPORT ( -- )
   #FAIL @ 0 = if s" PASS: process/pty primitives" type cr exit then
   #FAIL @ . s" proc-pty: failures" 1 die ;

10 NL c!
4 EOT c!
CAPTURE-HB
PTY-RUN
REPORT
;package
