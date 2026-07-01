\ spsc-imu-cli.f - CLI entry for Habu SPSC BMI088 capture.

require lib/errors.f
require lib/string.f
require lib/process-argv.f
require odin/spsc-imu.f

package SPSCIMU
private

64 constant SI-CLI-USAGE-RC

variable SI-CLI-ARG-I
variable SI-CLI-DRY-RUN

: LINE ( ptr u8 n -- )
   type cr ;

: USAGE ( -- )
   s" usage: odin/spsc-imu-cli.f -- --device PATH --logical NAME --output DIR [options]" LINE
   s" options: --dry-run --samples N --timeout-ms N --poll-ms N --start --no-start" LINE
   s"          --time-domain NAME --frame NAME" LINE ;

: DIE-USAGE ( -- )
   USAGE s" spsc-imu usage" SI-CLI-USAGE-RC die ;

: ARG$ ( -- ptr u8 n )
   SI-CLI-ARG-I @ SCRIPT-ARGV$ ;

: VALUE$ ( -- ptr u8 n )
   SI-CLI-ARG-I @ 1+ SCRIPT-ARGC >= if DIE-USAGE then
   SI-CLI-ARG-I @ 1+ SCRIPT-ARGV$ ;

: ADVANCE ( n -- )
   SI-CLI-ARG-I @ + SI-CLI-ARG-I ! ;

: NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop DIE-USAGE then ;

: OPTION ( -- )
   ARG$ s" -h" STR= if DIE-USAGE then
   ARG$ s" --help" STR= if DIE-USAGE then
   ARG$ s" --dry-run" STR= if 1 SI-CLI-DRY-RUN ! 1 ADVANCE exit then
   ARG$ s" --device" STR= if VALUE$ DEVICE! 2 ADVANCE exit then
   ARG$ s" --logical" STR= if VALUE$ LOGICAL! 2 ADVANCE exit then
   ARG$ s" --output" STR= if VALUE$ OUTPUT! 2 ADVANCE exit then
   ARG$ s" --samples" STR= if VALUE$ NUM SAMPLES! 2 ADVANCE exit then
   ARG$ s" --timeout-ms" STR= if VALUE$ NUM TIMEOUT-MS! 2 ADVANCE exit then
   ARG$ s" --poll-ms" STR= if VALUE$ NUM POLL-MS! 2 ADVANCE exit then
   ARG$ s" --time-domain" STR= if VALUE$ TIME-DOMAIN! 2 ADVANCE exit then
   ARG$ s" --frame" STR= if VALUE$ FRAME! 2 ADVANCE exit then
   ARG$ s" --start" STR= if START! 1 ADVANCE exit then
   ARG$ s" --no-start" STR= if NO-START! 1 ADVANCE exit then
   DIE-USAGE ;

: PARSE ( -- )
   RESET
   0 SI-CLI-DRY-RUN !
   SCRIPT-ARGC 0= if DIE-USAGE then
   0 SI-CLI-ARG-I !
   begin SI-CLI-ARG-I @ SCRIPT-ARGC < while OPTION repeat ;

: RUN-MAIN ( -- )
   PARSE
   SI-CLI-DRY-RUN @ if SUMMARY type exit then
   RUN dup 0 <> if s" spsc imu capture failed" rot die then
   drop ;

public

: MAIN ( -- )
   RUN-MAIN ;

end-package

package SPSCIMU
MAIN
end-package
