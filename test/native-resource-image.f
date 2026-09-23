\ Warm F64 and ZIP, save, then use fresh native resources outside the checkout.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require lib/zip-test-fixture.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package NATIVE-RESOURCE-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
create ZIP-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable ZIP-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: ZIP$ ( -- ptr u8 n ) ZIP-BUF ZIP-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" native-resource-image" HB-TMP-MKDIR {: path:ptr bytes:n :}
   path ROOT-BUF bytes BYTE-COPY bytes ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" input.zip" ZIP-BUF JOIN-PATH ZIP-U !
   ZIP$ ZIP-TEST:ORIGINAL$ WRITE-ALL ;

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
      s" native-resource image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T= outu ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ZIP$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/habu/app-image.f\nrequire test/native-resource-image-subject.f\n1 SCRIPT-ARGV$ NATIVE-RESOURCE-SUBJECT:WARM\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN drop
   IMAGE$ EXECUTABLE? TTRUE ;

: RELAUNCH ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   ZIP$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN ROOT$ >LEN
   S\" 0 SCRIPT-ARGV$ NATIVE-RESOURCE-SUBJECT:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT CLEAN
   OUT swap s" test: ok" CONTAINS? TTRUE ;

: RUN ( -- )
   T-RESET PREPARE BUILD RELAUNCH CLEANUP-RUN T-REPORT ;

RUN
;package
