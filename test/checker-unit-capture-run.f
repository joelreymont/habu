\ The capture case must run after its source load returns to outer stdin.
\ The native gate loads this driver and runs the same candidate engine there.

require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package CHECKER-UNIT-CAPTURE-RUN

$1000 constant CAP
10000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot

: REPORT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N {: outu:n erru:n rc:n :} OUT outu type ERR erru type rc 0 T= ;

public
\ The child inherits this process's environment, so the artifact it makes
\ lies under the HB_TMP this run was given.
: RUN ( -- )
   T-RESET
   s" prepared checker unit capture child" T-LABEL
   PROC-ARGV-RESET
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require test/checker-unit-capture-e2e.f\nCHECKER-UNIT-CAPTURE-TEST:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE REPORT
   T-REPORT ;

;package

CHECKER-UNIT-CAPTURE-RUN:RUN
