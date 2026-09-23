\ The public first-store path survives contention and a fresh snapshot process.
require lib/test.f
require lib/fs-mutate.f
require lib/engine-candidate.f
require lib/codesign.f
require src/habu/address-cells.f
require test/snapshot-file.f
package ADDRESS-CELL-TASK-TEST
using ADDRESS-CELLS
$10000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: PREPARE ( -- )
   CLEANUP-RESET
   s" address-cell-tasks" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" restored" IMAGE-BUF JOIN-PATH IMAGE-U ! ;
: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;
: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if OUT outu type ERR erru type then
   rc 0 T= erru 0 T= outu ;
: EXEC ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n input:ptr inputu:n :}
   exe exeu >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN ;
: CHECK-OUTPUT ( n -- )
   OUT swap S\" test: ok\naddress-cell-tasks: ok\n" T$= ;
: FRESH ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$
   S\" require test/address-cell-tasks-subject.f\nADDRESS-CELL-TASKS:RUN\n"
   EXEC CHECK-OUTPUT ;
: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+ IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire test/address-cell-tasks-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   EXEC 0 T=
   IMAGE$ EXECUTABLE? TTRUE ;
: POISON-CAPTURED-LOCK ( -- )
   IMAGE$ SNAPSHOT-FILE:READ-IMAGE
   SNAPSHOT-FILE:FORMAT SNAP-FORMAT-VERSION T=
   LOCK-CELL SNAPSHOT-FILE:CELL@ 0 T=
   LOCK-CELL 1 SNAPSHOT-FILE:BYTE!
   \ Process index pointers are never carried. A damaged incoming value must
   \ still be cleared without dereferencing it after the DATA copy.
   INDEX-CELL SNAPSHOT-FILE:CELL@ 0 T=
   INDEX-CELL 1 SNAPSHOT-FILE:BYTE!
   IMAGE$ SNAPSHOT-FILE:WRITE
   SNAPSHOT-FILE:RELEASE ;
: RESTORED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   IMAGE$ S\" ADDRESS-CELL-TASKS:RUN\n" EXEC CHECK-OUTPUT ;
: CHECK ( -- )
   s" every first registration survives lookup, growth and duplicate contention" T-LABEL
   FRESH BUILD
   s" captured mutex and index cells do not retain process ownership" T-LABEL
   POISON-CAPTURED-LOCK RESTORED ;
: CLEANUP ( -- ) SNAPSHOT-FILE:RELEASE CLEANUP-RUN ;
: RUN ( -- )
   T-RESET PREPARE
   [: CHECK ;] [: CLEANUP ;] finally
   T-REPORT ;
RUN
;using
;package
