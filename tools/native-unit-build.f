\ Load the launcher at the caller's tier. The saved driver's child selects
\ tier 1 for explicit FILE compilation; no launcher code enters that image.
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-id.f
require src/os/script-argv.f
require tools/native-file-driver-image.f

package NATIVE-UNIT-BUILD-ENTRY
private
$10000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

public
: FILE-CHILD ( -- )
   SCRIPT-ARGC 0= if s" native-build: missing mode" 74 die then
   0 SCRIPT-ARGV$ s" --export-file" STR=
   0 SCRIPT-ARGV$ s" --import-file" STR= or 0= if
      s" native-build: unsupported mode" 74 die
   then
   NATIVE-FILE-DRIVER-IMAGE:PATH$ {: engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+
   SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ >LEN PROC-ARGV+ loop
   engine engineu >LEN
   S\" 1 set-tier\nNATIVE-FILE-EXPLICIT:RUN\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
      {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   s" " rc die ;

;package
NATIVE-UNIT-BUILD-ENTRY:FILE-CHILD
