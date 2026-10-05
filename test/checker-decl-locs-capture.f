\ checker-decl-locs-capture.f - a saved image carries no navigation state.
\
\ The checker keeps where each named declaration was written beside its record
\ store (src/core/checker.f DECL-LOCS), from the location the verifier arms.
\ The table, the arm and the uses handler belong to the process, so the
\ capture seam releases them (CHECKER-CAPTURE-PREPARE). The producer arms,
\ replays a declaration that takes the location, and saves an image while
\ still armed; the image answers for that row with no location, stamps nothing
\ it was not armed for, and locates again once armed
\ (test/checker-decl-locs-capture-subject.f). The subject reads checker
\ internals, so both halves run the unsealed engine (test/whitebox-child.f)
\ and this file is a plain SUITE.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require test/whitebox-child.f

package NAVL-CAPTURE

$10000 constant CAP
600000 constant TIMEOUT-MS
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: ROOT-BUF    variable ROOT-U
FS-PATH-CAP BUFFER: IMAGE-BUF   variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" checker-decl-locs-capture" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" image" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ WHITEBOX-CHILD:PROVIDE-IN ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;

\ The child exited 0 and its own tests passed.
: CHECK ( n n n -- )
   {: outu:n erru:n rc:n :}
   rc 0<> OUT outu s" test: ok" CONTAINS? 0= or if
      s" checker-decl-locs-capture child rc " type rc . cr
      OUT outu type ERR erru type cr
   then
   rc 0 T=
   OUT outu s" test: ok" CONTAINS? TTRUE ;

: SAVE ( -- )
   s" the producer locates a row, then saves its image armed" T-LABEL
   PROC-ARGV-ENV-RESET
   WHITEBOX-CHILD:ENV!
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENGINE$ >LEN
   S\" require src/habu/app-image.f\nrequire test/checker-decl-locs-capture-subject.f\nNAVL:KEEP\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CHECK
   IMAGE$ EXECUTABLE? TTRUE ;

: RESTORE ( -- )
   s" the image carries no location, arm or table" T-LABEL
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN ROOT$ >LEN S\" NAVL:RESTORED\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT CHECK ;

public
: RUN ( -- )
   T-RESET
   [: PREPARE SAVE RESTORE ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;
;package

NAVL-CAPTURE:RUN
