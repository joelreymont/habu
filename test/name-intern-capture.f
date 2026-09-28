\ Save, boot, mutate, recapture and boot the checker with homonymous names.
\ Behavior is asserted in both restored boots. The last phase prints the
\ symbol offsets and pool growth for separate representation measurement.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require test/whitebox-child.f

package NAME-INTERN-CAPTURE

$10000 constant CAP
600000 constant TIMEOUT-MS
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: ROOT-BUF   variable ROOT-U
FS-PATH-CAP BUFFER: FIRST-BUF  variable FIRST-U
FS-PATH-CAP BUFFER: SECOND-BUF variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" name-intern-capture" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U !
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
      s" name intern child rc " type rc . cr
      OUT outu type ERR erru type cr
   then
   rc 0 T=
   OUT outu marker mu CONTAINS? TTRUE ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   s" HABU_NAME_INTERN_IMAGE" >LEN FIRST$ >LEN PROC-ENV+
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require src/habu/app-image.f\nrequire src/habu/verify-source.f\nrequire test/name-intern-capture-subject.f\ns\q HABU_NAME_INTERN_IMAGE\q GETENV APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n erru:n rc:n :}
   outu erru rc s" name intern: prepared" CHECK
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
   S\" NAME-INTERN-SUBJECT:RECAPTURE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   IMAGE-INPUT
   s" name intern: recaptured" CHECK
   SECOND$ EXECUTABLE? TTRUE ;

: RESTORE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   SECOND$
   S\" NAME-INTERN-SUBJECT:FINAL\n"
   IMAGE-INPUT {: outu:n erru:n rc:n :}
   outu erru rc s" name intern: restored" CHECK
   OUT outu type ;

public
: RUN ( -- )
   T-RESET
   [: PREPARE BUILD RECAPTURE RESTORE ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;
;package

NAME-INTERN-CAPTURE:RUN
