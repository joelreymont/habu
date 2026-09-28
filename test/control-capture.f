\ Capture the live checker through its owner, save an application image, then
\ test the saved boundary and current control states in two fresh boots.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require test/whitebox-child.f

package CONTROL-CAPTURE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: ROOT-BUF   variable ROOT-U
FS-PATH-CAP BUFFER: FIRST-BUF  variable FIRST-U
FS-PATH-CAP BUFFER: SECOND-BUF variable SECOND-U
FS-PATH-CAP BUFFER: DENIED-BUF variable DENIED-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;
: DENIED$ ( -- ptr u8 n ) DENIED-BUF DENIED-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" control-capture" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U !
   ROOT$ s" denied" DENIED-BUF JOIN-PATH DENIED-U !
   ROOT$ WHITEBOX-CHILD:PROVIDE-IN ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;

: CHECK ( n n n ptr u8 n -- )
   {: outu:n erru:n rc:n marker:ptr mu:n :}
   rc 0<> OUT outu marker mu CONTAINS? 0= or if
      s" control-capture child rc " type rc . cr
      OUT outu type ERR erru type cr
   then
   rc 0 T=
   OUT outu marker mu CONTAINS? TTRUE ;

\ The fixture needs the private boundary mark, so only the source process uses
\ the whitebox engine. Its saved images must stand alone in fresh processes.
: BUILD-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   s" HABU_CONTROL_IMAGE" >LEN FIRST$ >LEN PROC-ENV+ ;

: BUILD ( -- )
   BUILD-ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require src/habu/app-image.f\nrequire test/control-capture-subject.f\ns\q HABU_CONTROL_IMAGE\q GETENV APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n erru:n rc:n :}
   outu erru rc
   s" control capture: prepared" CHECK
   FIRST$ EXECUTABLE? TTRUE ;

: IMAGE-INPUT ( ptr u8 n ptr u8 n -- n n n )
   {: image:ptr imageu:n input:ptr inputu:n :}
   image imageu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT ;

: RECAPTURE ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   FIRST$
   S\" CONTROL-CAPTURE-SUBJECT:RECAPTURE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   IMAGE-INPUT
   s" control capture: recaptured" CHECK
   SECOND$ EXECUTABLE? TTRUE ;

: REFUSE-IN-SCOPE ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   DENIED$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   FIRST$
   S\" CONTROL-CAPTURE-SUBJECT:REFUSE-IN-SCOPE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   IMAGE-INPUT {: outu:n erru:n rc:n :}
   s" an open rollback scope refuses capture" T-LABEL
   rc 76 T=
   ERR erru s" checker: snapshot inside rollback scope" CONTAINS? TTRUE
   DENIED$ EXECUTABLE? 0= TTRUE ;

: RESTORE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   SECOND$
   S\" CONTROL-CAPTURE-SUBJECT:FINAL\n"
   IMAGE-INPUT
   s" control capture: restored" CHECK ;

public

: RUN ( -- )
   T-RESET
   [: PREPARE BUILD REFUSE-IN-SCOPE RECAPTURE RESTORE ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

;package

CONTROL-CAPTURE-TEST:RUN
