\ Load the production entry and its driver through the native build guard.
\ No output argument reaches the driver's own refusal after the callback and
\ dynamic source-load boundary have compiled; it does not start a full build.
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package NATIVE-BUILD-ENTRY-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot

: RUN ( -- )
   T-RESET
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" tools/native-build.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 74 <> if
      OUT outu LEN>N type ERR erru LEN>N type
   then
   s" the native driver reaches its missing-output refusal" T-LABEL
   rc 74 T=
   outu LEN>N 0 T=
   ERR erru LEN>N S\" native-build: one explicit output path is required\n" T$=
   T-REPORT ;

RUN
;package
