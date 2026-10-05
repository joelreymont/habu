\ Compile the private route record at tier 1, save it, and dispatch again in a
\ fresh process. The restored process also compiles one new native caller.
require lib/test.f
require lib/string.f
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
create SECOND-BUF FS-PATH-CAP allot variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: PERSIST$ ( -- ptr u8 n ) PERSIST-BUF PERSIST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   s" structure-quotation-image" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" route-image" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" stored-image" PERSIST-BUF JOIN-PATH PERSIST-U !
   ROOT$ s" switched-image" SECOND-BUF JOIN-PATH SECOND-U ! ;

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

: SAVE-SOURCE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET SB-APPEND
   S\" require src/habu/app-image.f\n" SB-APPEND
   HB-TARGET-LINUX-X86-64? 0= if S\" 0 set-tier\n" SB-APPEND then
   S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   SB$ ;

: RESAVE-SOURCE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET SB-APPEND
   HB-TARGET-LINUX-X86-64? 0= if S\" 0 set-tier\n" SB-APPEND then
   S\" require src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   SB$ ;

: PERSIST-CHECK$ ( -- ptr u8 n )
   S\" package QUOT-PERSIST-TEST\n: PERSIST-FIRST-TIER ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else 0 then ;\nPERSIST-FIRST-TIER set-tier\n: SAVED-FIRST ( -- ) 35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T= PRESENT-ROW @ READ-CHOICE 42 T= SCALAR-ROW @ READ-CHOICE LOOKALIKE @ 10 + T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 41 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\n1 set-tier\n: SAVED-T1 ( -- ) 35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 T= PRESENT-ROW @ READ-CHOICE 42 T= SCALAR-ROW @ READ-CHOICE LOOKALIKE @ 10 + T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 41 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\nCHECK\nT-RESET SAVED-FIRST SAVED-T1 T-REPORT\n;package\n" ;

: SECOND-CHECK$ ( -- ptr u8 n )
   S\" package QUOT-PERSIST-TEST\n: SECOND-FIRST-TIER ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else 0 then ;\nSECOND-FIRST-TIER set-tier\n: SECOND-FIRST ( -- ) PRESENT-ROW @ READ-CHOICE 466 T= SCALAR-ROW @ READ-CHOICE 44 T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 46 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\n1 set-tier\n: SECOND-T1 ( -- ) PRESENT-ROW @ READ-CHOICE 466 T= SCALAR-ROW @ READ-CHOICE 44 T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 46 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\nT-RESET SECOND-FIRST SECOND-T1 T-REPORT\n;package\n" ;

: BUILD ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/structure-quotation-field.f\n" SAVE-SOURCE$ >LEN
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
   S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/structure-quotation-persist-producer.f\n" SAVE-SOURCE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   PERSIST$ EXECUTABLE? TTRUE ;

: CHECK-PERSIST-RESTORE ( -- )
   ENVIRONMENT
   PERSIST$ >LEN
   PERSIST-CHECK$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$= ;

: RESTORE-PERSIST ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   PERSIST$ >LEN
   S\" package QUOT-PERSIST-TEST\n1 set-tier\n: SWITCH-SAVED ( -- ) 456 construct choice scalar HOLDER-MAKE ENVELOPE-MAKE PRESENT-ROW ! [: 9 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE SCALAR-ROW ! [: 11 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE 0 BATCH ! ;\n: CHECK-SWITCH ( -- ) T-RESET PRESENT-ROW @ READ-CHOICE 466 T= SCALAR-ROW @ READ-CHOICE 44 T= 0 BATCH @ READ-CHOICE 46 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= T-REPORT ;\nSWITCH-SAVED CHECK-SWITCH\n;package\n" RESAVE-SOURCE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$=
   SECOND$ EXECUTABLE? TTRUE ;

: RESTORE-SECOND ( -- )
   ENVIRONMENT
   SECOND$ >LEN
   SECOND-CHECK$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\n" T$= ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: PREPARE BUILD RESTORE BUILD-PERSIST CHECK-PERSIST-RESTORE RESTORE-PERSIST RESTORE-SECOND ;]
      [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
