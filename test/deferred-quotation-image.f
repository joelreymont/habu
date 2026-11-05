\ Save deferred quotation columns, reopen, grow, save and reopen again.
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f

package DEFER-QUOT-IMAGE-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot variable ROOT-U
create FIRST-BUF PATH-CAP allot variable FIRST-U
create SECOND-BUF PATH-CAP allot variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   s" deferred-quotation-image" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF FS:JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF FS:JOIN-PATH SECOND-U ! ;

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
   S\" require test/deferred-quotation-subject.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   FIRST$ FS:EXECUTABLE? TTRUE ;

: REOPEN ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   FIRST$ >LEN
   S\" DEFER-QUOT-TEST:SECOND\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   SECOND$ FS:EXECUTABLE? TTRUE ;

: FINAL ( -- )
   ENVIRONMENT
   SECOND$ >LEN
   S\" DEFER-QUOT-TEST:FINAL\npackage DEFER-QUOT-TEST\n: FINAL-T0 ( -- ) CHECK-SWITCH ;\nT-RESET FINAL-T0 T-REPORT\n;package\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$= ;

: RUN ( -- )
   T-RESET FS:CLEANUP-RESET
   [: PREPARE BUILD REOPEN FINAL ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
