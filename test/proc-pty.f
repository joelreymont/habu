\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb < test/proc-pty.f

$20007454 constant TIOCPTYGRANT
$40807453 constant TIOCPTYGNAME
$20007452 constant TIOCPTYUNLK
1 constant POLLIN
2 constant F-SETFD
1 constant FD-CLOEXEC

create PATH-BUF 256 allot
create RBUF 4096 allot
create PFD 8 allot
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

: T= {: got want :} ( got want -- )
   #CASE @ 1 + #CASE !
   got want <> if
      [char] F emit #CASE @ .
      #FAIL @ 1 + #FAIL !
   then ;

: TTRUE ( f -- )
   -1 T= ;

: PATHZ {: a u :} ( a u -- pathz )
   0 begin dup u < while
      dup a + c@  over PATH-BUF + c!
      1 +
   repeat drop
   0 PATH-BUF u + c!
   PATH-BUF ;

: MKPIPE {: rvar wvar :} ( rvar wvar -- )
   pipe 0 T=
   wvar !
   rvar ! ;

: RCLR ( -- )
   0 RN ! ;

: READ+ {: fd :} ( fd -- )
   fd RBUF RN @ + 4096 RN @ - read
   dup 0 > if RN @ + RN ! else drop then ;

: MATCH-AT {: ha na nu off :} ( ha na nu off -- f )
   -1
   nu 0 ?do
      ha off + i + c@  na i + c@  <> if drop 0 leave then
   loop ;

: CONTAINS? {: ha hu na nu :} ( ha hu na nu -- f )
   nu 0= if -1 exit then
   hu nu < if 0 exit then
   hu nu - 1 + 0 ?do
      ha na nu i MATCH-AT if -1 unloop exit then
   loop
   0 ;

: TCONTAINS {: a u :} ( a u -- )
   RBUF RN @ a u CONTAINS? TTRUE ;

: FD-WRITE {: fd a u :} ( fd a u -- )
   fd a u write u T= ;

: FD-WRITE-LN {: fd a u :} ( fd a u -- )
   fd a u FD-WRITE
   fd NL 1 FD-WRITE ;

: PFD! {: fd events :} ( fd events -- )
   events 32 lshift  fd $FFFFFFFF and  or  PFD ! ;

: CLOEXEC {: fd :} ( fd -- )
   fd F-SETFD FD-CLOEXEC fcntl 0 T= ;

: POLL-IN {: fd ms :} ( fd ms -- rc )
   fd POLLIN PFD!
   PFD 1 ms poll ;

: DRAIN {: fd :} ( fd -- )
   RCLR
   0 QUIET !
   0 begin dup 60 < QUIET @ 6 < and while
      fd 50 POLL-IN 0 > if fd READ+ 0 QUIET ! else QUIET @ 1 + QUIET ! then
      1 +
   repeat drop ;

: SEND-C {: c :} ( c -- )
   c CH c!
   MFD @ CH 1 FD-WRITE ;

: SEND-S {: a u :} ( a u -- )
   MFD @ a u FD-WRITE ;

: SEND-LN {: a u :} ( a u -- )
   MFD @ a u FD-WRITE-LN ;

: SEND-ESC ( c -- )
   27 SEND-C
   91 SEND-C
   SEND-C ;

: STEP-LN {: a u :} ( a u -- )
   a u SEND-LN
   MFD @ DRAIN ;

: STEP-S {: a u :} ( a u -- )
   a u SEND-S
   MFD @ DRAIN ;

: EXPECT ( a u -- )
   TCONTAINS ;

: REJECT {: a u :} ( a u -- )
   RBUF RN @ a u CONTAINS? 0 T= ;

: CAPTURE-HB ( -- )
   IN-R IN-W MKPIPE
   OUT-R OUT-W MKPIPE
   ERR-R ERR-W MKPIPE
   IN-W @ CLOEXEC
   OUT-R @ CLOEXEC
   ERR-R @ CLOEXEC
   s" bin/hb" PATHZ IN-R @ OUT-W @ ERR-W @ spawn-io PID !
   PID @ 0 > TTRUE
   IN-R @ close
   OUT-W @ close
   ERR-W @ close
   IN-W @ s" 2 3 + ." FD-WRITE-LN
   IN-W @ close
   PID @ wait-rc 0 T=
   RCLR
   OUT-R @ READ+
   s" 5" TCONTAINS
   OUT-R @ close
   RCLR
   ERR-R @ READ+
   RN @ 0 T=
   ERR-R @ close ;

: OPEN-PTY ( -- )
   s" /dev/ptmx" PATHZ 2 0 open MFD !
   MFD @ 2 > TTRUE
   MFD @ CLOEXEC
   MFD @ TIOCPTYGRANT 0 ioctl 0 T=
   MFD @ TIOCPTYUNLK 0 ioctl 0 T=
   MFD @ TIOCPTYGNAME PTYNAME ioctl 0 T=
   PTYNAME 2 0 open SFD !
   SFD @ 2 > TTRUE ;

: PTY-HB ( -- )
   OPEN-PTY
   s" bin/hb" PATHZ SFD @ SFD @ SFD @ spawn-io PID !
   PID @ 0 > TTRUE
   SFD @ close
   MFD @ DRAIN
   s"  ok" EXPECT
   s" habu> " EXPECT
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" habu> " EXPECT
   s" frobnicate" STEP-LN
   s" frobnicate?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" EXPECT
   s" 1 2 + .." SEND-S
   127 SEND-C
   10 SEND-C
   MFD @ DRAIN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" garbage" SEND-S
   3 SEND-C
   MFD @ DRAIN
   s" habu> " EXPECT
   s" garbage?" REJECT
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT
   s" 13 ." SEND-S
   68 SEND-ESC
   68 SEND-ESC
   68 SEND-ESC
   48 SEND-C
   10 SEND-C
   MFD @ DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT
   65 SEND-ESC
   10 SEND-C
   MFD @ DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" : IN1 1 + ;" STEP-LN
   s"  ok" EXPECT
   s" ' SQ BP+" STEP-LN
   s"  ok" EXPECT
   s" ' IN1 BP+" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" habu-bp:" EXPECT
   s" 49" EXPECT
   s" 9 IN1 ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT
   s" 6 SQ ." STEP-LN
   s" 36" EXPECT
   s" habu-bp:" REJECT
   s" : PB dup + ;" STEP-LN
   s"  ok" EXPECT
   s" ' PB BP*" STEP-LN
   s"  ok" EXPECT
   s" 5 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT
   s" 6 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 12" EXPECT
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT
   s" 2 ' PB BPN" STEP-LN
   s"  ok" EXPECT
   s" 3 PB ." STEP-LN
   s" 6" EXPECT
   s" habu-bp:" REJECT
   s" 3 PB ." STEP-LN
   s" 6" EXPECT
   s" habu-bp:" REJECT
   s" 3 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 6" EXPECT
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT
   s" : F0 0 ;" STEP-LN
   s"  ok" EXPECT
   s" : F1 1 ;" STEP-LN
   s"  ok" EXPECT
   s" : F2 2 ;" STEP-LN
   s"  ok" EXPECT
   s" : F3 3 ;" STEP-LN
   s"  ok" EXPECT
   s" : F4 4 ;" STEP-LN
   s"  ok" EXPECT
   s" : F5 5 ;" STEP-LN
   s"  ok" EXPECT
   s" : F6 6 ;" STEP-LN
   s"  ok" EXPECT
   s" : F7 7 ;" STEP-LN
   s"  ok" EXPECT
   s" : F8 8 ;" STEP-LN
   s"  ok" EXPECT
   s" ' F0 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F1 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F2 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F3 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F4 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F5 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F6 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F7 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F8 BP+" STEP-LN
   s" table full" EXPECT
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT
   s" step : SD dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 4 SD ." STEP-LN
   s" 16" EXPECT
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" EXPECT
   s" 99 throw" STEP-LN
   s" ?" EXPECT
   s" habu> " EXPECT
   s"  ok" REJECT
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" EXPECT
   4 SEND-C
   PID @ wait-rc 0 T=
   MFD @ close ;

: REPORT ( -- )
   #FAIL @ 0 = if s" PASS: process/pty primitives" type cr exit then
   #FAIL @ . s" proc-pty: failures" 1 die ;

10 NL c!
4 EOT c!
CAPTURE-HB
PTY-HB
REPORT
