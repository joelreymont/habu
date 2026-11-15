\ Loads test/loop-obligations-subject.f on the engine under test at its default
\ tier, through the normal load path.
require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/subject.f

package LOOP-OBLIGATIONS-TEST

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: CASES ( -- )
   s" loop obligations through the load path" T-LABEL
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/loop-obligations-subject.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   s" test/loop-obligations-subject.f" OUT outu LEN>N ERR erru LEN>N rc SUBJECT:RC-TIMED-OUT
   OUT outu LEN>N S\" loop-runtime: ok\n" STR= 0= rc 0 <> or if
      ERR erru LEN>N type cr
   then
   rc 0 T=
   OUT outu LEN>N S\" loop-runtime: ok\n" T$=
   \ The subject asserts each exact catch code; the load path must also diagnose.
   erru LEN>N 0 > TTRUE ;

: RUN ( -- )
   T-RESET
   CASES
   T-REPORT ;

RUN
;package
