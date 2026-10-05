\ Save deferred quotation columns, reopen, grow, save and reopen again.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f

package DEFER-QUOT-IMAGE-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create FIRST-BUF FS-PATH-CAP allot variable FIRST-U
create SECOND-BUF FS-PATH-CAP allot variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   s" deferred-quotation-image" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         erru LEN>N 0<> if OUT outu LEN>N type ERR erru LEN>N type then
         erru LEN>N 0 T=
         outu LEN>N
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         OUT outu LEN>N type ERR erru LEN>N type
         rc RC>N throw
      ENDOF
   ;MATCH ;

: ENVIRONMENT ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: BUILD ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   FIRST$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   HB-TARGET-LINUX-X86-64? if
      S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/deferred-quotation-subject.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   else
      S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/deferred-quotation-subject.f\nrequire src/habu/app-image.f\n0 set-tier\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   then >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   FIRST$ EXECUTABLE? TTRUE ;

: REOPEN ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   FIRST$ >LEN
   S\" DEFER-QUOT-TEST:SECOND\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   SECOND$ EXECUTABLE? TTRUE ;

: FINAL ( -- )
   ENVIRONMENT
   SECOND$ >LEN
   HB-TARGET-LINUX-X86-64? if
      S\" DEFER-QUOT-TEST:FINAL\npackage DEFER-QUOT-TEST\n1 set-tier\n: FINAL-T1 ( -- ) CHECK-SWITCH ;\nT-RESET FINAL-T1 T-REPORT\n;package\n"
   else
      S\" DEFER-QUOT-TEST:FINAL\npackage DEFER-QUOT-TEST\n0 set-tier\n: FINAL-T0 ( -- ) CHECK-SWITCH ;\n1 set-tier\n: FINAL-T1 ( -- ) CHECK-SWITCH ;\nT-RESET FINAL-T0 FINAL-T1 T-REPORT\n;package\n"
   then >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$= ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: PREPARE BUILD REOPEN FINAL ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
