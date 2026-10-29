\ The profiler must retain its sampled donor's identity across a real seed
\ rewind and give a new word at the reused ordinal a fresh profile identity.
\ The child emits JSON as the repeatable report artifact and tests both PCs.

require src/lib/test.f
require src/lib/string.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package PROF-REWIND-TEST
private

$20000 constant OUT-CAP
$4000 constant ERR-CAP
60000 constant TIMEOUT-MS
create OUT OUT-CAP allot
create ERR ERR-CAP allot

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/prof-rewind-child.f" >LEN PROC-ARGV+ ;

: RUN ( -- )
   T-RESET
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ >LEN OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE PCAP-RESULT:>N {: outu:n erru:n rc:n :}
   s" profiler rewind child exits successfully" T-LABEL  rc 0 T=
   s" profiler report retains donor samples" T-LABEL
   OUT outu s\" \qword\q:\qPROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-HOT-WITH-EXTERNAL-NAME\q" CONTAINS? TTRUE
   s" profiler report does not give donor samples to target" T-LABEL
   OUT outu s\" \qword\q:\qPROF-REWIND-TARGET-QUALIFIER-LONG:TARGET-NEW\q" CONTAINS? TFALSE
   s" an ordinary retired word is not revived by a seed rewind" T-LABEL
   OUT outu s\" \qword\q:\qPROF-REWIND-DONOR-QUALIFIER-LONG:RETIRED-HOT\q" CONTAINS? TFALSE
   s" retained caller samples keep their donor identity" T-LABEL
   OUT outu s\" \qcaller\q:\qPROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-CALLER-WITH-EXTERNAL-NAME\q" CONTAINS? TTRUE
   s" retained donor code executes after the rewind" T-LABEL
   OUT outu s" retained donor caller ran" CONTAINS? TTRUE
   s" ordinary trimming gives the replacement fresh counters" T-LABEL
   OUT outu s\" \qword\q:\qPROF-TRIM-NEW\q" CONTAINS? TFALSE
   s" prof-row resolves the replacement after an ordinary trim" T-LABEL
   OUT outu s" PROF-TRIM-NEW" CONTAINS? TTRUE
   s" prof-row resolves the current ordinal" T-LABEL
   OUT outu s" PROF-REWIND-TARGET-QUALIFIER-LONG:TARGET-NEW" CONTAINS? TTRUE
   s" ordinary trimming preserves deferred word samples" T-LABEL
   OUT outu s\" \qword\q:\qPROF-LATE-TRIM\q" CONTAINS? TTRUE
   s" reused code is sampled as its executable target" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-TARGET\q" CONTAINS? TTRUE
   s" reused target receives exclusive samples" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-TARGET\q,\qexcl\q:0" CONTAINS? TFALSE
   s" reused code never charges its reclaimed donor" T-LABEL
   OUT outu s\" \qword\q:\qPROF-RECLAIM-DONOR\q" CONTAINS? TFALSE
   s" profiler child has no diagnostic" T-LABEL  erru 0 T=
   s" prof-rewind report artifact:" type cr
   OUT outu type
   erru 0<> if s" prof-rewind child stderr:" type cr ERR erru type then
   T-REPORT
   s" prof-rewind: ok" type cr ;

public
: MAIN ( -- ) RUN ;
;package

PROF-REWIND-TEST:MAIN
