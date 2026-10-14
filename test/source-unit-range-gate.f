\ Run the range fixture on stdin, preserving its top-level source-frame contract.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package SOURCE-UNIT-RANGE-GATE

$10000 constant CAP
create SOURCE CAP allot
create OUT CAP allot
create ERR CAP allot

public
: MAIN ( -- )
   T-RESET
   s" source-unit ranges through stdin" T-LABEL
   s" test/source-unit-range-e2e.f" SOURCE CAP READ-ALL {: sourceu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN SOURCE sourceu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0 T=
   OUT outu LEN>N s" test: ok" CONTAINS? TTRUE
   T-REPORT ;
;package

SOURCE-UNIT-RANGE-GATE:MAIN
