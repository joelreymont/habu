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
create PTYNAME 128 allot

variable #FAIL
variable #CASE
variable RN
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
   0 begin dup 20 < while
      fd 100 POLL-IN 0 > if fd READ+ then
      1 +
   repeat drop ;

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
   s" habu> " TCONTAINS
   MFD @ s" 1 2 + ." FD-WRITE-LN
   MFD @ DRAIN
   s" 3" TCONTAINS
   s"  ok" TCONTAINS
   EOT MFD @ swap 1 write 1 T=
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
