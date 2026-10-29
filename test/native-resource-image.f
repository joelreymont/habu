\ Warm F64 and ZIP, save, then use fresh native resources outside the checkout.
\ The image is saved from the keyed host with the saver already loaded
\ (test/app-image-engine.f).
require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/zip-test-fixture.f
require test/app-image-engine.f
require test/pcap-result.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package NATIVE-RESOURCE-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot
create IMAGE-BUF PATH-CAP allot
create ZIP-BUF PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable ZIP-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: ZIP$ ( -- ptr u8 n ) ZIP-BUF ZIP-U @ ;

: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" native-resource-image" FS:MKTEMP-DIR {: path:ptr bytes:n :}
   path ROOT-BUF bytes BYTE-COPY bytes ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" application" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   ROOT$ s" input.zip" ZIP-BUF FS:JOIN-PATH ZIP-U !
   ZIP$ ZIP-TEST:ORIGINAL$ FS:WRITE-ALL ;

: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if
      s" native-resource image child failed: " type rc . cr
      OUT outu type ERR erru type
   then
   rc 0 T= erru 0 T= outu ;

: BUILD ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ZIP$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   host hostu >LEN
   S\" 1 set-tier\nrequire test/native-resource-image-subject.f\n1 SCRIPT-ARGV$ NATIVE-RESOURCE-SUBJECT:WARM\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PCAP-RESULT:>N CLEAN drop
   IMAGE$ FS:EXECUTABLE? TTRUE ;

: RELAUNCH ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   ZIP$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN ROOT$ >LEN
   S\" 0 SCRIPT-ARGV$ NATIVE-RESOURCE-SUBJECT:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE PCAP-RESULT:>N CLEAN
   OUT swap s" test: ok" CONTAINS? TTRUE ;

: RUN ( -- )
   T-RESET PREPARE BUILD RELAUNCH FS:CLEANUP-RUN T-REPORT ;

RUN
;package
