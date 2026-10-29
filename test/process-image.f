\ Capture live signals and caches, then reinitialise and recapture them twice.
\ Every restored generation starts cold: no signal state, descriptor or
\ argv or environment rows of the process that saved it (PROCESS-IMAGE-SUBJECT
\ CLEAN, FIRST-FD). The first generation is saved from the keyed host with
\ the saver already loaded (test/app-image-engine.f).
require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/test/subject.f
require test/app-image-engine.f
require test/pcap-result.f

package PROCESS-IMAGE-TEST

$10000 constant CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot
create FIRST-BUF PATH-CAP allot
create SECOND-BUF PATH-CAP allot
variable ROOT-U
variable FIRST-U
variable SECOND-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: FIRST$ ( -- ptr u8 n ) FIRST-BUF FIRST-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" process-image-test" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" first" FIRST-BUF FS:JOIN-PATH FIRST-U !
   ROOT$ s" second" SECOND-BUF FS:JOIN-PATH SECOND-U ! ;

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
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   FIRST$ >LEN PROC-ARGV+
   ENVIRONMENT
   host hostu >LEN
   S\" 1 set-tier\nrequire test/process-image-subject.f\nPROCESS-IMAGE-SUBJECT:CLEAN\nSIGNAL:INIT\nSIGNAL:SIGUSR1 SIGNAL:CATCH\ns\q HABU_SAVED\q >LEN s\q x\q >LEN PROC-ENV+\ns\q HABU_SAVED\q >LEN s\q x\q >LEN PROC-ENV-DEFAULT+\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PCAP-RESULT:>N CLEAN
   FIRST$ FS:EXECUTABLE? TTRUE ;

: RESTORE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n input:ptr inputu:n :}
   ENVIRONMENT
   s" HABU_IMAGE_RESTORED" >LEN s" different-envp" >LEN PROC-ENV+
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE PCAP-RESULT:>N {: outu:n erru:n rc:n :}
   input inputu OUT outu ERR erru rc SUBJECT:RC-TIMED-OUT
   outu erru rc CLEAN ;

: RECAPTURE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n next:ptr nextu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   next nextu >LEN PROC-ARGV+
   path pathu
   S\" PROCESS-IMAGE-SUBJECT:CLEAN\nPROCESS-IMAGE-SUBJECT:FIRST-FD\nPROCESS-IMAGE-SUBJECT:EMPTY-ARGV\nPROCESS-IMAGE-SUBJECT:USE\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" RESTORE
   next nextu FS:EXECUTABLE? TTRUE ;

: CHECK-IMAGES ( -- )
   BUILD
   FIRST$ SECOND$ RECAPTURE
   SECOND$ FIRST$ RECAPTURE
   PROC-ARGV-ENV-RESET
   FIRST$ S\" PROCESS-IMAGE-SUBJECT:CLEAN\nPROCESS-IMAGE-SUBJECT:FIRST-FD\nPROCESS-IMAGE-SUBJECT:USE\n" RESTORE ;

: RUN ( -- )
   T-RESET PREPARE
   [: CHECK-IMAGES ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
