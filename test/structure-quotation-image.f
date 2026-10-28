\ Compile the private route record and the whole-record storage producer at
\ tier 1 into one image. A fresh restore dispatches the saved routes, compiles
\ new native callers and checks the stored records; another switches the
\ records and saves a second image, and a final restore checks the switch.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f

package QUOT-FIELD-IMAGE-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create IMAGE-BUF FS-PATH-CAP allot variable IMAGE-U
create SECOND-BUF FS-PATH-CAP allot variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   s" structure-quotation-image" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" route-image" IMAGE-BUF JOIN-PATH IMAGE-U !
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

: SECOND-CHECK$ ( -- ptr u8 n )
   S\" package QUOT-PERSIST-TEST\n: SECOND-FIRST-TIER ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else 0 then ;\nSECOND-FIRST-TIER set-tier\n: SECOND-FIRST ( -- ) PRESENT-ROW @ READ-CHOICE 466 T= SCALAR-ROW @ READ-CHOICE 44 T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 46 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\n1 set-tier\n: SECOND-T1 ( -- ) PRESENT-ROW @ READ-CHOICE 466 T= SCALAR-ROW @ READ-CHOICE 44 T= EMPTY-ROW @ READ-CHOICE 0 T= 0 BATCH @ READ-CHOICE 46 T= 1 BATCH @ READ-CHOICE LOOKALIKE @ 10 + T= 2 BATCH @ READ-CHOICE 0 T= ;\nT-RESET SECOND-FIRST SECOND-T1 T-REPORT\n;package\n" ;

: BUILD ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/compiler/native/compiler.f\n1 set-tier\nrequire test/structure-quotation-field.f\nrequire test/structure-quotation-persist-producer.f\n" SAVE-SOURCE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\n" T$=
   IMAGE$ EXECUTABLE? TTRUE ;

: RESTORE ( -- )
   ENVIRONMENT
   IMAGE$ >LEN
   S\" require test/structure-quotation-restored.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   OUT outu S\" test: ok\ntest: ok\ntest: ok\ntest: ok\n" T$= ;

: RESTORE-PERSIST ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   IMAGE$ >LEN
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
   [: PREPARE BUILD RESTORE RESTORE-PERSIST RESTORE-SECOND ;]
      [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
