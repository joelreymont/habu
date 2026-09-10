\ Build once, then run and recapture outside the source checkout.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f

package APP-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
create SECOND-BUF FS-PATH-CAP allot
create STARTUP-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable SECOND-U
variable STARTUP-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;
: STARTUP$ ( -- ptr u8 n ) STARTUP-BUF STARTUP-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" app-image-test" TMPDIR-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U !
   ROOT$ s" startup" STARTUP-BUF JOIN-PATH STARTUP-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N
      ENDOF
   ;MATCH ;

: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" app-image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T=
   outu ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN
   S\" require src/habu/app-image.f\nrequire test/app-image-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN drop
   IMAGE$ EXECUTABLE? TTRUE ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- n n n )
   {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT ;

: CHECK-APPLICATION ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   S\" APP-IMAGE-SUBJECT:RUN . cr\n: FRESH-INCREMENT ( n -- n ) 1+ ;\nAPP-IMAGE-SUBJECT:RUN FRESH-INCREMENT . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 43\n\n44\n\n" T$= ;

: CHECK-REJECTION ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ S\" : FRESH-BAD ( n -- n ) 0= ;\n" RUN-INPUT
   {: outu:n erru:n rc:n :}
   rc 70 T= outu 0 T=
   ERR erru s" expected:" CONTAINS? TTRUE
   ERR erru s" actual:" CONTAINS? TTRUE ;

: RECAPTURE ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" RUN-INPUT CLEAN drop
   SECOND$ EXECUTABLE? TTRUE ;

: CAPTURE-STARTUP ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   STARTUP$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$
   S\" package IMAGE-START-TEST\nvariable STARTS\n: MAIN ( -- ) 1 STARTS +! STARTS @ . cr SCRIPT-ARGC . cr SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ type cr loop SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s\q quit\q STR= if s\q \q 0 die then 0 SCRIPT-ARGV$ s\q throw\q STR= if -123 throw then 0 SCRIPT-ARGV$ s\q eval-throw\q STR= if s\q -124 throw\q INCLUDE-EVALUATE then then ;\n' MAIN\n;package\nAPP-IMAGE:START!\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT CLEAN drop
   STARTUP$ EXECUTABLE? TTRUE ;

: CHECK-STARTUP ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n0\n\n43\n\n" T$= ;

: STARTUP-ARGS ( -- )
   s" alpha" >LEN PROC-ARGV+
   s" two words" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   STARTUP$ S\" APP-IMAGE-SUBJECT:RUN . cr\n" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n2\n\nalpha\ntwo words\n43\n\n" T$= ;

: CHECK-STARTUP-ARGS ( -- )
   PROC-ARGV-ENV-RESET STARTUP-ARGS
   PROC-ARGV-ENV-RESET s" --" >LEN PROC-ARGV+ STARTUP-ARGS ;

: CHECK-STARTUP-EXIT ( -- )
   PROC-ARGV-ENV-RESET
   s" quit" >LEN PROC-ARGV+ PROC-ENV-INHERIT-MISSING
   STARTUP$ s" STARTUP-SHOULD-NOT-EXECUTE" RUN-INPUT CLEAN
   OUT swap S\" 1\n\n1\n\nquit\n" T$= ;

\ fd 0 must be a real terminal: startup failures used to enter REPL recovery
\ before its source frame existed. Output uses a small pipe so the diagnostic
\ remains byte-exact on Linux and macOS.
create PTY-NAME 128 allot
variable PTY-U
variable PTY-NUM

: PTY-C, ( n -- )
   PTY-NAME PTY-U @ + c! 1 PTY-U +! ;

: PTY-U, ( n -- ) {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod 48 + PTY-C, ;

\ Habu uses Darwin-style flags on both targets: O_RDWR | O_NOCTTY.
$20002 constant PTY-OPEN-FLAGS

: OPEN-PTY ( -- n n )
   s" /dev/ptmx" FS-PATHZ PTY-OPEN-FLAGS 0 open {: master:n :}
   master 0 >= TTRUE
   master >FD FD-CLOEXEC!
   HB-TARGET-LINUX? if
      0 PTY-NUM !
      master $40045431 PTY-NUM ioctl 0 T=
      master $80045430 PTY-NUM ioctl 0 T=
      s" /dev/pts/" PTY-NAME swap BYTE-COPY
      9 PTY-U ! PTY-NUM @ PTY-U, 0 PTY-C,
   else
      master $20007454 NULL-PTR ioctl 0 T=
      master $20007452 NULL-PTR ioctl 0 T=
      master $40807453 PTY-NAME ioctl 0 T=
   then
   PTY-NAME PTY-OPEN-FLAGS 0 open {: slave:n :}
   slave 0 >= TTRUE
   master slave ;

: CHECK-STARTUP-THROW ( ptr u8 n ptr u8 n -- )
   {: arg:ptr argu:n diagnostic:ptr diagnosticu:n :}
   PROC-ARGV-ENV-RESET arg argu >LEN PROC-ARGV+ PROC-ENV-INHERIT-MISSING
   OPEN-PTY {: master:n slave:n :}
   PIPE-PAIR {: reader writer :}
   STARTUP$ >LEN slave >FD writer writer PROC-SPAWN-ARGV-ENV-IO {: child :}
   slave close writer FD>N close
   child PROC-WAIT-RC MATCH result
      ok OF 67 T= ENDOF
      err OF 67 T= ENDOF
   ;MATCH
   reader FD>N OUT CAP read {: outu:n :}
   reader FD>N close master close
   outu 0 >= TTRUE
   OUT outu diagnostic diagnosticu CONTAINS? TTRUE ;

: CHECK-STARTUP-THROWS ( -- )
   s" throw" s" hb: uncaught throw code -123" CHECK-STARTUP-THROW
   s" eval-throw" s" hb: uncaught throw code -124" CHECK-STARTUP-THROW ;

: RUN ( -- )
   T-RESET PREPARE
   BUILD
   IMAGE$ CHECK-APPLICATION
   CHECK-REJECTION
   RECAPTURE
   SECOND$ CHECK-APPLICATION
   CAPTURE-STARTUP
   CHECK-STARTUP
   CHECK-STARTUP-ARGS
   CHECK-STARTUP-EXIT
   CHECK-STARTUP-THROWS
   CLEANUP-RUN
   T-REPORT ;

RUN
;package
