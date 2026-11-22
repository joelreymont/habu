split-str
\ A word defined after a caught failed text gets its own ordinal, code and
\ profiler counters, and the failed text's words keep their samples; the child
\ emits JSON as the artifact.

require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package PROF-REWIND-TEST
private

$20000 constant OUT-CAP
$4000 constant ERR-CAP
create OUT OUT-CAP allot
create ERR ERR-CAP allot

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" test/prof-rewind-child.f" PROC-ARGV+ ;

: RUN ( -- )
   T-RESET
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ OUT OUT-CAP >LEN ERR ERR-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE PCAP-RESULT:>N {: outu:n erru:n rc:n :}
   s" profiler rewind child exits successfully" T-LABEL  rc 0 T=
   s" a word after a failed text gets fresh counters" T-LABEL
   OUT outu s\" \qword\q:\qPROF-TRIM-NEW\q" CONTAINS? TFALSE
   s" prof-row resolves a word a failed text defined" T-LABEL
   OUT outu s" PROF-TRIM-NEW" CONTAINS? TTRUE
   s" a failed text's word keeps its deferred samples" T-LABEL
   OUT outu s\" \qword\q:\qPROF-LATE-TRIM\q" CONTAINS? TTRUE
   s" a word after a failed text is sampled as itself" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-TARGET\q" CONTAINS? TTRUE
   s" that word receives exclusive samples" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-TARGET\q,\qexcl\q:0" CONTAINS? TFALSE
   s" a failed text's word that never ran is never charged" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-DONOR\q" CONTAINS? TFALSE
   s" profiler child writes no diagnostic" T-LABEL  erru 0 T=
   s" prof-rewind report artifact:" type cr
   OUT outu type
   erru 0<> if s" prof-rewind child stderr:" type cr ERR erru type then
   T-REPORT
   s" prof-rewind: ok" type cr ;

public
: MAIN ( -- ) RUN ;
;package

PROF-REWIND-TEST:MAIN
