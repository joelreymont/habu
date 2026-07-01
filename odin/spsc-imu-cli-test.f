\ spsc-imu-cli-test.f - no-hardware smoke for the SPSC IMU CLI entry.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f

package SPSCIMU-CLI-TEST

4096 constant SIC-OUT-CAP
1024 constant SIC-ERR-CAP
120000 constant SIC-TIMEOUT-MS

create SIC-OUT SIC-OUT-CAP allot
create SIC-ERR SIC-ERR-CAP allot

: SIC-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SIC-CAPTURE>N ( len len rc -- n n n )
   {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: SIC-RUN-CLI ( -- n n n )
   PROC-ARGV-RESET
   s" --load" SIC-ARG+
   s" odin/spsc-imu-cli.f" SIC-ARG+
   s" --" SIC-ARG+
   s" --dry-run" SIC-ARG+
   s" --device" SIC-ARG+ s" /tmp/spsc-test" SIC-ARG+
   s" --logical" SIC-ARG+ s" cam_test" SIC-ARG+
   s" --output" SIC-ARG+ s" /tmp/spsc-test-out" SIC-ARG+
   s" --samples" SIC-ARG+ s" 1" SIC-ARG+
   s" --timeout-ms" SIC-ARG+ s" 1" SIC-ARG+
   s" --poll-ms" SIC-ARG+ s" 1" SIC-ARG+
   s" --no-start" SIC-ARG+
   s" ../habu/bin/hb" >LEN SIC-OUT SIC-OUT-CAP >LEN SIC-ERR SIC-ERR-CAP >LEN
   SIC-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   SIC-CAPTURE>N ;

: SIC-TEST-DRY-RUN ( -- )
   SIC-RUN-CLI 0 T= 0 T= {: outu:n :}
   SIC-OUT outu s" # SPSC BMI088 IMU Capture" CONTAINS? TTRUE
   SIC-OUT outu s" logical name: cam_test" CONTAINS? TTRUE
   SIC-OUT outu s" samples requested: 1" CONTAINS? TTRUE ;

: SIC-TEST-RUN ( -- )
   T-RESET
   SIC-TEST-DRY-RUN ;

SIC-TEST-RUN
T-REPORT

end-package
