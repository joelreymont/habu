\ Capture unused caches, then warm and recapture them twice outside the checkout.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f

package PROCESS-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create FIRST-BUF FS-PATH-CAP allot
create SECOND-BUF FS-PATH-CAP allot
variable ROOT-U
variable FIRST-U
variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" process-image-test" TMPDIR-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N
      ENDOF
   ;MATCH ;

: CLEAN ( n n n -- ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" process image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T=
   OUT outu s" test: ok" CONTAINS? TTRUE ;

\ Use a small explicit environment for the image-building and restored children.
: ENVIRONMENT ( -- )
   s" PATH" >LEN s" /usr/bin:/bin" >LEN PROC-ENV+
   s" LANG" >LEN s" C" >LEN PROC-ENV+ ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   FIRST$ >LEN PROC-ARGV+
   ENVIRONMENT
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/habu/app-image.f\nrequire test/process-image-subject.f\nPROCESS-IMAGE-SUBJECT:CLEAN\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN
   FIRST$ EXECUTABLE? TTRUE ;

: RESTORE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n input:ptr inputu:n :}
   ENVIRONMENT
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT CLEAN ;

: RECAPTURE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n next:ptr nextu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   next nextu >LEN PROC-ARGV+
   path pathu
   S\" PROCESS-IMAGE-SUBJECT:CLEAN\nPROCESS-IMAGE-SUBJECT:EMPTY-ARGV\nPROCESS-IMAGE-SUBJECT:USE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" RESTORE
   next nextu EXECUTABLE? TTRUE ;

: CHECK-IMAGES ( -- )
   BUILD
   FIRST$ SECOND$ RECAPTURE
   SECOND$ FIRST$ RECAPTURE
   PROC-ARGV-ENV-RESET
   FIRST$ S\" PROCESS-IMAGE-SUBJECT:CLEAN\nPROCESS-IMAGE-SUBJECT:USE\n" RESTORE ;

: RUN ( -- )
   T-RESET PREPARE
   [: CHECK-IMAGES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
