\ The capture case must run after its source load returns to outer stdin.
\ The native gate loads this driver and runs the same candidate engine there.

require src/lib/process-argv.f
require src/lib/test.f
require test/pcap-result.f

package CHECKER-UNIT-CAPTURE-RUN

$1000 constant CAP
10000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: REPORT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N {: outu:n erru:n rc:n :} OUT outu type ERR erru type rc 0 T= ;

public
: RUN ( -- )
   T-RESET
   s" prepared checker unit capture child" T-LABEL
   PROC-ARGV-RESET
   HB$ >LEN
   S\" require test/checker-unit-capture-e2e.f\nCHECKER-UNIT-CAPTURE-TEST:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE REPORT
   T-REPORT ;

;package

CHECKER-UNIT-CAPTURE-RUN:RUN
