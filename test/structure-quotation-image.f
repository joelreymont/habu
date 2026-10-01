\ Compile the private route record at tier 1, save it, and dispatch again in a
\ fresh process. The restored process also compiles one new native caller.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f

package QUOT-FIELD-IMAGE-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create IMAGE-BUF FS-PATH-CAP allot variable IMAGE-U
create PERSIST-BUF FS-PATH-CAP allot variable PERSIST-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: PERSIST$ ( -- ptr u8 n ) PERSIST-BUF PERSIST-U @ ;

: PREPARE ( -- )
   s" structure-quotation-image" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" route-image" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" stored-image" PERSIST-BUF JOIN-PATH PERSIST-U ! ;

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
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/structure-quotation-field.f\n0 set-tier\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   IMAGE$ EXECUTABLE? TTRUE ;

: RESTORE ( -- )
   ENVIRONMENT
   IMAGE$ >LEN
   S\" require test/structure-quotation-restored.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$= ;

: BUILD-PERSIST ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   PERSIST$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/structure-quotation-persist-producer.f\n0 set-tier\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   PERSIST$ EXECUTABLE? TTRUE ;

: RESTORE-PERSIST ( -- )
   ENVIRONMENT
   PERSIST$ >LEN
   S\" package QUOT-PERSIST-TEST\n0 set-tier\n: SAVED-T0 ( -- ) 35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T= ;\n1 set-tier\n: SAVED-T1 ( -- ) 35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T= ;\nCHECK\nT-RESET SAVED-T0 SAVED-T1 T-REPORT\n;package\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$= ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: PREPARE BUILD RESTORE BUILD-PERSIST RESTORE-PERSIST ;]
      [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
